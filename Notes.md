# Runtime notes

## FPS

### Metriche task periodiche

Si assume la seguente task periodica, che inizializza le proprietà utilizzando procedure del runtime. Tale codice è basato sugli esempi di uso di "task cicliche" di Dilan e Carletto.

```Ada
with Ada.Real_Time; use Ada.Real_Time;
with Activation_Manager;
with Ada.Text_IO;
with System.BB.Time;
with System.BB.Threads; use System.BB.Threads;
with System.BB.Threads.Queues;
with Regular_Producer_Parameters; use Regular_Producer_Parameters;

package body Regular_Producer is
   Period : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (Regular_Producer_Period);

   task body Regular_Producer is
      --  for periodic suspension
      Next_Time : Ada.Real_Time.Time := Activation_Manager.Get_Activation_Time;
   begin
      --  Setting artificial deadline
      Set_Period (System.BB.Time.Milliseconds (Regular_Producer_Period));
      Set_Starting_Time (Activation_Manager.Time_Conversion (Next_Time));
      Set_Relative_Deadline (System.BB.Time.Milliseconds (Regular_Producer_Deadline));
      Set_Fake_Number_ID (1);
      System.BB.Threads.Queues.Initialize_Task_Table (1);

      delay until Next_Time;
      loop
         Next_Time := Next_Time + Period;
         Regular_Producer_Operation;
         delay until Next_Time;
      end loop;
   end Regular_Producer;
end Regular_Producer;
```

Il metodo `Set_Relative_Deadline` esegue `Change_Relative_Deadline` in `s-bbthqu.adb`, il quale cambia la deadline relativa e setta quella assoluta al valore baseline seguente. 

```Ada
System.BB.Time.Time_First + Thread.Active_Starting_Time -
   (Thread.Active_Period - Thread.Active_Relative_Deadline)
                  + Global_Interrupt_Delay
```

Tale valore porta ad una deadline assoluta corretta dal primo risveglio in poi in quanto la deadline assoluta viene calcolata al risveglio come baseline + $(n+1)T$, ove $n$ è il numero di esecuzioni del task e $T$ è il periodo. Tuttavia tale codice provoca la registrazione di 2 Deadline Miss (DM) ancor prima che parta il primo job (1 di `Delay_Until` e 1 di `Context_Switch_Needed`). Questo perché l'istruzione `delay until Next_Time` per la sincronizzazione a tempo logico 0, non è differenziabile dall'identica istruzione a termine di un job e in entrambi `Delay_Until` e `Context_Switch_Needed` avviene il controllo di DM.

La soluzione è stata spostare l'inizializzazione della tabelle delle metriche ad un istante successivo al tempo zero logico e antecedente l'inizio del primo job. Questo ha anche portato ad una modifica del valore iniziale di Execution da -1 a 0 in `Initialize_Task_Table` in `s-bbthqu.adb`, un valore difatti più corretto e meno "ad hoc".

```Ada
delay until Next_Time;
System.BB.Threads.Queues.Initialize_Task_Table (1);
loop
```

### Alcuni controlli di Deadline Miss sembrano eccessivi

Il controllo di deadline miss avviene in 4 punti diversi: `Delay_Until` in `s-bbtime.adb`, `Context_Switch_Needed`, `Wakeup_Expired_Alarms`, `Yield` in `s-bbthqu.adb`.

Ci sembra eccessivo controllare una potenziale DM durante un controllo di Context Switch o al risveglio dal `Delay_Until`, in quanto è sufficiente farlo solo prima di entrare in sospensione (`Delay_Until`). Questo sembra anche provocare un duplice conteggio di DM.

Ad esempio, prima di andare in stato `Delayed` in `Delay_Until`, viene giustamente fatto un controllo di DM. In caso positivo viene aumentato il contatore e messo a `True` la flag `Check` per la task che è andata in DM. Questo supponiamo dovrebbe evitare gli ulteriori controlli nei punti menzionati, secondo noi ridondanti. Tuttavia appena il thread è messo in stato `Delayed`, tale flag viene resettato a `False`. Immaginiamo questo sia invece per indicare che normalmente la task, al suo successivo risveglio, sarà nuovamente pronta per essere sottoposta a controlli di DM. Però, una volta in stato `Delayed`, il runtime esegue `Context_Switch_Needed` per verificare se c'è un task `Runnable` in coda ready. Poiché c'è un controllo ridondante di DM e la flag `Check` è stata resettata erroneamente a `False`, verrà rilevata una ulteriore DM, già contata in `Delay_Until`.

Questa situazione spiega anche i due DM (invece che uno solo) che vi sono all'inizio di una task periodica, come menzionato nella sezione precedente.

Questa ridonanza dei controlli non è solo non necessaria, ma anche dannosa. Abbiamo rilevato infatti che con un workload prossimo al limite, la task riesce a completare sempre in tempo prima di passare allo stato `Delayed` in `Delay_Until` ma ciò provoca comunque una falsa DM da parte del controllo in `Context_Switch_Needed`. Questo perché quest'ultima funzione viene invocata al prossimo Interrupt/SysTick, che avviene 1ms più tardi nel caso peggiore. Quel ms potrebbe provocare un falso positivo.

Per tali motivi, la nostra decisione è stata di rimuovere i controlli di DM tranne che quello in `Delay_Until`. Questo rimuove la necessità di avere un corretto valore del flag `Check`, un approccio facilmente prono ad errori e la cui correttezza d'uso è difficile da valutare in quanto fortemente dipendente dall'ordine di esecuzione dei vari punti. È sicuramente possibile valutare di rimettere i controlli nei vari punti laddove si voglia rilevare una DM tempestivamente, in tal caso ragionando su un approccio migliore per evitare conteggi multipli. Ad esempio un approccio ragionevole è usare un `Timing_Event`, il quale viene attivato all'istante di DM senza dover aspettare che il job in overrun arrivi all'istruzione `Delay_Until`. Tant'è che è possibile che, in caso di overload, un job non arrivi mai al completamento se subisce interferenza infinita da parte di task a priorità più alta.

### Task sporadiche

La strumentazione non gestisce le metriche per task sporadiche, poiché non ci sono controlli all'inizio di un'entry.

Innanzitutto le task sporadiche non hanno periodo, per cui non vi è l'invocazione del metodo `Set_Period` e `Change_Relative_Deadline` setterà una deadline assoluta pari al valore baseline seguente.

```Ada
Thread, System.BB.Time.Time_First + Thread.Active_Starting_Time +
   (Thread.Active_Relative_Deadline - Thread.Active_Period)
                  + Global_Interrupt_Delay
```

Il calcolo della deadline assoluta utilizzando il meccanismo della baseline è sbagliato per task sporadiche che non hanno periodo. Per quest'ultime, la Deadline Assoluta è pari al tempo di rilascio della barriera associata alla entry (`Now`) + Deadline Relativa.

In seguito al risveglio dopo il `Delay_Until` di sincronizzazione a tempo logico zero, la task sporadica si vedrà anche aumentata la Deadline Assoluta di un valore pari a `Active_Period`, che essendo indefinito è pari a zero. Il risultato di questi calcoli non provoca danni di DM, in quanto le nostre successive modifiche permettono un calcolo corretto della Deadline Assoluta per task sporadiche. Tuttavia è evidente che questi meccanismi di Deadline Assoluta sono ideati per task periodiche e siano fragili/error-prone per task sporadiche. 

Abbiamo, per cominciare, aggiunto il controllo di DM in `Protected_Single_Entry_Call` in `s-tposen.adb`, per controllare se vi è una deadline miss all'entrata di una Entry Call. In caso positivo viene incrementato il contatore. Nel metodo viene anche incrementato di 1 l'esecuzione della task sporadica.

Il metodo `Initialize_Task_Table` in `s-bbthqu.adb` è stato modificato per accogliere il parametro `Is_Sporadic` per differenziare il caso di task sporadico da quello periodico. Il primo avrà nelle tabella delle metriche valori iniziali di DM ed Executions diversi da quelli di una task periodica. Tale soluzione non è ingegneristicamente pulita, ma è stata adottata per evitare di stravolgere il lavoro di Dilan.

In seguito abbiamo modificato il metodo `Wakeup` in `s-bbthre.adb`, modificando l'aggiornamento della Deadline Assoluta da `Active_Period + Active_Absolute_Deadline` a `Active_Relative_Deadline + Now`. Questo è stato necessario perché il metodo `Wakeup` è invocato per il risveglio di una task sospesa su una Entry. Al risveglio di tale task sporadica, essa deve avere la Deadline Assoluta aggiornata a `Active_Relative_Deadline + Now`.

Ci chiediamo tuttavia se tale modifica sia corretta per le task periodiche, la cui deadline assoluta dovrebbe giustamente essere `Active_Period + Active_Absolute_Deadline`. Queste al momento sono risvegliate dallo stato `Delayed` dal metodo `Wakeup_Expired_Alarms` in `s-bbthqu.adb`, che non fa uso del metodo `Wakeup` e già setta la Deadline Assolute usando il periodo. Il metodo `Wakeup` invece risveglia solo thread in stato `Suspended`, ovvero sospesi per una entry, ma in altre applicazioni real-time è possibile che anche task periodiche facciano chiamate di entry e quindi possano essere sospese. Per quest'ultime non ci dovrebbe essere alcun aggiornamento della Deadline Assoluta al risveglio da una entry, in quanto tale valore per loro è calcolato al risveglio del `Delay_Until`. 

### Preemptions

L'aumento del contatore delle preemptions avviene all'interno della funzione `Context_Switch_Needed` in `s-bbthqu.adb` ove il test è il seguente.

```Ada
First_Thread /= Running_Thread and Running_Thread.Preemption_Needed
```

Il campo `Preemption_Needed`, ideato da Carletto, serve per evitare di contare come preemption i casi in cui una task periodica vada in stato `Delayed` e vi sia una task in stato `Runnable` nella coda dei ready. Per il runtime, la funzione `Context_Switch_Needed` restituisce `True` in questo caso perché effettivamente c'è da fare un context switch, ma ai nostri fini non va contato come preemption.

Tale meccanismo dovrebbe tuttavia essere esteso anche per le task sporadiche, le quali vanno in stato `Suspended` e possono essere "pre-rilasciate" da una task `Runnable`. Per tale motivo abbiamo aggiunto `Id.Preemption_Needed := True` a `Wakeup` in `s-bbthre.adb` per abilitare il conteggio delle preemptions al risveglio da una sospensione, e `Running_Thread.Preemption_Needed = False` in `s-tposen.adb` prima di una sospensione per disattivare il conteggio. Questo meccanismo è analogo all'uso fatto da Carletto prima e dopo i `Delay_Until`.

### Release Jitter

Dilan calcola il release jitter nel seguente modo per task periodiche, ove `Change_Jitters` è una procedura che salva il valore di jitter nella tabelle delle metriche. Il secondo parametro è il jitter sul Response Time del job, che tuttavia non abbiamo usato in quanto causava eccezioni di overflow.

```Ada
loop
   Release_Jitter := Ada.Real_Time.Time_First +
      (Ada.Real_Time.Clock - Next_Time);

   Regular_Producer_Operation;
   Next_Time := Next_Time + Period;

   System.BB.Threads.Queues.Change_Jitters (Running_Thread, Time_Conversion (Ada.Real_Time.Time_First), Time_Conversion (Release_Jitter));
   delay until Next_Time; --  delay statement at end of loop
end loop;
```

Tuttavia tale metodo ovviamente non è applicabile per task sporadiche che non hanno il valore `Next_Time`. Il metodo che abbiamo trovato è stato il seguente:

```Ada
package body On_Call_Producer is
   use Ada.Real_Time;

   Release_Time : Ada.Real_Time.Time;

   function Start (Activation_Parameter : Positive) return Boolean is
      Response : Boolean;
   begin
      Response := Request_Buffer.Deposit (Activation_Parameter);
      Release_Time := Ada.Real_Time.Clock;
      return Response;
   end Start;

   task body On_Call_Producer is
      Current_Workload : Positive;
      Next_Time : Ada.Real_Time.Time := Activation_Manager.Get_Activation_Time;
      Release_Jitter : Ada.Real_Time.Time;
   begin
      ...

      loop
         Current_Workload := Request_Buffer.Extract;

         Release_Jitter := Ada.Real_Time.Time_First +
            (Ada.Real_Time.Clock - Release_Time);
         On_Call_Producer_Operation (Current_Workload);

         Change_Jitters (Running_Thread, Time_Conversion (Ada.Real_Time.Time_First), Time_Conversion (Release_Jitter));
      end loop;
end On_Call_Producer;
```

Salviamo il tempo di rilascio `Release_Time` dopo l'apertura della barriera sulla entry da parte di Regular Producer tramite la funzione `Start`. Il Release Jitter è quindi calcolato come differenza rispetto a tale istante. Questo approccio funziona fintanto che il sistema non è in stato di overloading. In quel caso, laddove Regular Producer rilasci il successivo job di On Call Producer prima che quello corrente abbia ancora cominciato, l'istante di `Release_Time` potrebbe non essere più corretto.

### Sidenotes

- `Avarage_Work_Jitter` in `s-bbthqu.adb` è stato rinominato in `Average_Work_Jitter`
- `Active_Period` è stato rinominato semplicemente in `Period` essendo costante dopo la prima inizializzazione
- `Active_Starting_Time` è stato rinominato semplicemente in `Starting_Time` essendo costante dopo la prima inizializzazione

## EDF

Le precedenti note sono in genere valide analogamente anche per EDF. Le modifiche alla strumentazione sono state riportate anche in EDF.

### Cambio di Deadline Relativa a causa della Deadline Floor

Il metodo `Change_Relative_Deadline` in `s-bbthqu.adb` non differenzia tra cambio di Deadline Relativa come inizializzazione di una task e come acquisizione/rilascio di una risorsa protetta.

Attenzione inoltre che la task rilasciata a tempo $s$ con deadline relativa $d$ e che accede alla risorsa protetta con deadline floor $D$ a tempo $t$ ($s \lt t$), dovrebbe vedersi la deadline relativa cambiata in $D$ e la deadline assoluta calcolata come $min\{t + D, s + d\}$.

La nuova implementazione seguente, aggiunge un parametro formale `Is_Floor` per differenziare la chiamata `Change_Relative_Deadline` utilizzata, sotto alias `Set_Relative_Deadline`, durante l'inizializzazione di una task rispetto alla chiamata effettuata nella procedura `Lock` in `s-taprob.adb`. Inoltre calcola correttamente il valore di Deadline Assoluta con la formula sopra menzionata.

```Ada
procedure Change_Relative_Deadline
   (Thread       : Thread_Id;
   Rel_Deadline : Relative_Deadline;
   Is_Floor     : Boolean := False)
is
   Aux_Pointer  : Thread_Id;
   CPU_Id       : constant CPU := Get_CPU (Thread);
   Now          : constant System.BB.Time.Time := System.BB.Time.Clock;
begin
   --  A CPU can only change the relative deadline of its own tasks

   pragma Assert (CPU_Id = Current_CPU);

   --  We can only change the priority of the thread that is
   --  currently executing.

   pragma Assert (Thread = Running_Thread_Table (CPU_Id));

   --  Change the active relative deadline. The base relative deadline does
   --  not change
   Thread.Active_Relative_Deadline := Rel_Deadline;

   if Is_Floor then
      Change_Absolute_Deadline (Thread, Absolute_Deadline'Min (
         Now + Thread.Active_Relative_Deadline,
         Thread.Active_Absolute_Deadline));
   else
      if Thread.Active_Relative_Deadline <= Thread.Period then
         Change_Absolute_Deadline (Thread, System.BB.Time.Time_First +
            Thread.Starting_Time -
            (Thread.Period - Thread.Active_Relative_Deadline)
               + Global_Interrupt_Delay);
      else
         Change_Absolute_Deadline (Thread, System.BB.Time.Time_First +
            Thread.Starting_Time +
            (Thread.Active_Relative_Deadline - Thread.Period)
               + Global_Interrupt_Delay);
      end if;
   end if;

   --  Thread.Active_Absolute_Deadline := (Rel_Deadline + Now);
```

### I Suspension Objects sono senza deadline floor

Spesso ci possono essere altri costrutti che per loro natura hanno bisogno di un protected object, come `Suspension_Object` e `Timing_Event`, che tuttavia avrebbero Deadline Floor a zero implicitamente.

## Conclusioni

Riteniamo che il rilevamento delle metriche DM ed Executions sia idealmente interessante da rilevare in alcuni punti precisi del runtime, sfruttando pattern delle task periodiche e sporadiche come l'utilizzo di `Delay_Until` e Entry Call rispettivamente. Questo permetterebbe di rilevare metriche senza "inquinare" il corpo dei jobs. Tuttavia troviamo che la soluzione corrente non generalizzi bene a tutti i possibili casi di applicazioni real-time, pur rimanendo nei vincoli del profilo Ravenscar. 

Rimaniamo convinti che farlo nel runtime sia l'approccio corretto, in quanto il corpo di un job può subire jitter, tuttavia è necessario pensare ad un approccio più robusto. Un compito che abbiamo anche noi compreso non essere per nulla banale.

### Unificazione

Abbiamo anche modificato il runtime FPS per fornire le stesse API di quello EDF in modo da usare un'unica versione dell'applicazione GEE. È stato sufficiente aggiungere in `s-taprob` le seguenti dichiarazioni:

```Ada
Current_Object : Protection_Access;

procedure Initialize_Protection_Deadline
   (Object           : Protection_Access;
   Floor_Deadline   : Integer);
```

L'implementazione della procedura è vuota mentre `Current_Object` è assegnato al valore di `Object` in `Initialize_Protection`, come avviene nel corrispettivo EDF. Tale campo non ha utilizzo in FPS, per cui la modifica è innocua.

```Ada
procedure Initialize_Protection_Deadline
     (Object           : Protection_Access;
      Floor_Deadline   : Integer)
   is null;
```