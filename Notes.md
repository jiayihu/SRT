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

Tale valore porta ad una deadline assoluta corretta dal primo risveglio in poi in quanto la deadline assoluta viene calcolata al risveglio e come baseline + $(n+1)T$, ove $n$ è il numero di esecuzioni del task. Tuttavia tale codice provoca la registrazione di 2 deadline miss ancor prima che parta il primo job (1 di `Delay_Until` e 1 di `Context_Switch_Needed`). Questo perché l'istruzione `delay until Next_Time` per la sincronizzazione a tempo logico 0, non è differenziabile dall'identica istruzione a termine di un job.

La soluzione è stata spostare l'inizializzazione della tabelle delle metriche ad un istante successivo al tempo zero logico e antecedente l'inizio del primo job. Questo ha anche portato ad una modifica del valore iniziale di Execution da -1 a 0 in `Initialize_Task_Table` in `s-bbthqu.adb`, un valore difatti più corretto e meno "ad hoc".

### Alcuni controlli di Deadline Miss sembrano eccessivi

Ci sembra eccessivo controllare potenziale Deadline Miss durante un controllo di Context Switch o al risveglio dal `Delay_Until`, in quanto ci sembra sufficiente prima di entrare in sospensione. Questo sembra provocare anche un eccessivo conteggio di Deadline Miss qualora la presenza di questi sia rilevata.

Ad esempio, prima di andare in stato `Delayed` in `Delay_Until`, viene giustamente fatto un controllo di Deadline Miss. In caso positivo viene aumentato il contatore e messo a `True` la flag `Check` per la task che è andata in Deadline Miss. Questo supponiamo dovrebbe evitare gli ulteriori controlli nei punti menzionati, secondo noi ridondanti. Tuttavia appena il thread è messo in stato `Delayed`, tale flag viene resettato a `False`. Immaginiamo questo sia per indicare che normalmente la task, al suo successivo risveglio, sarà nuovamente pronta per essere sottoposta a controlli di Deadline Miss. Però, una volta in stato `Delayed`, il runtime esegue `Context_Switch_Needed` per verificare se c'è un task `Runnable` in coda ready. Poiché c'è un controllo ridondante di Deadline Miss e la flag `Check` è stata resettata erroneamente a `False`, verrà rilevato una ulteriore Deadline Miss, già contata.

Questa situazione spiega anche i due Deadline Miss (invece che uno solo) che vi sono all'inizio di una task periodica, come menzionato nella sezione precedente.

Questa ridonanza dei controlli non è solo non necessaria, ma anche dannosa. Abbiamo rilevato infatti che con un workload prossimo al limite, la task riesce a completare sempre in tempo prima di passare allo stato `Delayed` in `Delay_Until` ma ciò provoca comunque in una falsa Deadline Miss da parte del controllo in `Context_Switch_Needed`. Questo perché quest'ultima funzione viene invocata al prossimo Interrupt/SysTick, che avviene almeno 1ms più tardi. Quel ms potrebbe provocare ad un falso positivo.

### Task sporadiche

La strumentazione non gestisce le metriche per task sporadici, quindi non ci sono controlli all'inizio di un'entry.

Innanzitutto le task sporadiche non hanno periodo, per cui non vi è l'invocazione del metodo `Set_Period` e `Change_Relative_Deadline` setterà una deadline assoluta pari al valore baseline seguente.

```Ada
Thread, System.BB.Time.Time_First + Thread.Active_Starting_Time +
   (Thread.Active_Relative_Deadline - Thread.Active_Period)
                  + Global_Interrupt_Delay)
```

Il calcolo della deadline assoluta utilizzando il meccanismo della baseline è sbagliato per task sporadiche che non hanno periodo. Per un "caso fortuito", il valore iniziale di `Active_Period` in assenza di inizializzazione, sembra essere zero in Ada. Per cui in realtà la Deadline Assoluta iniziale settata è già quella del primo job, non una baseline.

In seguito al risveglio dopo il `Delay_Until`, la task sporadica si vedrà aumentata la Deadline Assoluta di un valore pari a `Active_Period`, pari "fortuitamente" sempre a zero quindi senza conseguenze. Tuttavia è evidente che questi meccanismi di Deadline Assoluta sono ideati per task periodiche e siano fragili per task sporadiche. 

Abbiamo per cominciare aggiunto il controllo di Deadline Miss in `Protected_Single_Entry_Call` in `s-tposen.adb`, per controllare se vi è una deadline miss all'entrata di una Entry Call. In caso positivo viene incrementato il contatore. Nel metodo viene anche incrementato di 1 l'esecuzione della task sporadica.

Il metodo `Initialize_Task_Table` è stato modificato per aggiungere il parametro `Is_Sporadic` per differenziare il caso di task sporadico da quello periodico. Il primo avrà nelle tabella delle metriche valori iniziali di Deadline Miss ed Executions diversi da quelli di una task periodica. Tale soluzione non è ingegneristicamente pulita, ma visti i ristretti tempi non abbiamo trovato soluzioni migliori.

In seguito abbiamo modificato il metodo `Wakeup` in `s-bbthre.adb`, modificando l'aggiornamento della Deadline Assoluta da `Active_Period + Active_Absolute_Deadline` a `Active_Relative_Deadline + Now`. Questo è stato necessario perché il metodo `Wakeup` è invocato per il risveglio di una task sospesa in una Entry. Al risveglio di tale task sporadica, essa deve avere la Deadline Assoluta aggiornata a `Active_Relative_Deadline + Now`.

Ci chiediamo tuttavia se tale modifica sia corretta per le task periodiche, la cui deadline assoluta dovrebbe giustamente essere `Active_Period + Active_Absolute_Deadline`. Queste al momento sono risvegliate dal metodo `Wakeup_Expired_Alarms` in `s-bbthqu.adb`, che non fa uso del metodo `Wakeup` e già setta la Deadline Assolute usando il periodo. Il metodo `Wakeup` invece risveglia solo thread in stato `Suspended`, ovvero sospesi per una entry, ma immaginiano che in altre applicazioni sia possibile che anche task periodiche facciano chiamate di entry e quindi possano essere sospese.

### Preemptions

L'aumento del contatore delle preemptions avviene all'interno della funzione `Context_Switch_Needed` in `s-bbthqu.adb` ove il test è il seguente.

```Ada
First_Thread /= Running_Thread and Running_Thread.Preemption_Needed
```

Il campo `Preemption_Needed`, ideato da Carletto, serve per evitare di contare come preemption i casi in cui una task periodica vada in stato `Delayed` e vi sia una task in stato `Runnable` nella coda dei ready. Per il runtime, la funzione `Context_Switch_Needed` restituisce `True` in questo caso perché effettivamente c'è da fare un context switch, ma ai nostri fini non va contato come preemption.

Tale meccanismo dovrebbe tuttavia essere esteso anche per le task sporadiche, le quali vanno in stato `Suspended` e possono essere "pre-rilasciate" da una task `Runnable`. Per tale motivo abbiamo aggiunto `Id.Preemption_Needed := True` a `Wakeup` in `s-bbthre.adb` per abilitare il conteggio delle preemptions al risveglio da una sospensione, e `Running_Thread.Preemption_Needed = False` in `s-tposen.adb` prima di una sospensione per disattivare il conteggio. Questo meccanismo è analogo all'uso fatto da Carletto per prima e dopo i `Delay_Until`.

### Sidenotes

- `Avarage_Work_Jitter` in `s-bbthqu.adb` è stato rinominato in `Average_Work_Jitter`
- `Active_Period` è stato rinominato semplicemente in `Period` essendo costante dopo la prima inizializzazione
- `Active_Starting_Time` è stato rinominato semplicemente in `Starting_Time` essendo costante dopo la prima inizializzazione

## EDF

Le precedenti note sono in genere valide analogamente anche per EDF. Le modifiche alla strumentazione sono state riportate anche in EDF.

### Cambio di Deadline Relativa a causa della Deadline Floor

Il metodo `Change_Relative_Deadline` in `s-bbthqu.adb` non differenzia tra cambio di Deadline Relativa come inizializzazione di una task e come acquisizione/rilascio di una risorsa protetta.

Attenzione inoltre che la task rilasciata a tempo $s$ con deadline relativa $d$ e che accede alla risorsa protetta con deadline floor $D$ a tempo $t$ ($s \lt t$), dovrebbe vedersi la deadline relativa cambiata in $D$ e la deadline assoluta calcolata come $min\{t + D, s + d\}$.

La nuova implementazione seguente, aggiunge un parametro formale `Is_Floor` per differenziare dalla chiamata `Change_Relative_Deadline` utilizzata, sotto alias `Set_Relative_Deadline`, durante l'inizializzazione di una task dalla chiamata effettuata nella procedura `Lock` in `s-taprob.adb`. Inoltre calcola correttamente il valore di Deadline Assoluta con la formula sopra menzionata.

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
      if Thread.Active_Relative_Deadline <= Thread.Active_Period then
         Change_Absolute_Deadline (Thread, System.BB.Time.Time_First +
            Thread.Active_Starting_Time -
            (Thread.Active_Period - Thread.Active_Relative_Deadline)
               + Global_Interrupt_Delay);
      else
         Change_Absolute_Deadline (Thread, System.BB.Time.Time_First +
            Thread.Active_Starting_Time +
            (Thread.Active_Relative_Deadline - Thread.Active_Period)
               + Global_Interrupt_Delay);
      end if;
   end if;

   --  Thread.Active_Absolute_Deadline := (Rel_Deadline + Now);
```

### I Suspension Objects sono senza deadline floor

Spesso ci possono essere altri costrutti che per loro natura hanno bisogno di un protected object, come `Suspension_Object` e `Timing_Event`, che tuttavia avrebbero Deadline Floor a zero implicitamente.

## Conclusioni

Riteniamo che il rilevamento delle metriche Deadline Miss ed Executions sia idealmente interessante da rilevare in punti del runtime, sfruttando pattern delle task periodiche e sporadiche come l'utilizzo di `Delay_Until` e Entry Call rispettivamente. Questo permetterebbe di rilevare metriche senza "inquinare" il corpo dei jobs. Tuttavia troviamo che una tale soluzione non possa essere generalizzata a tutti i possibili casi di applicazioni real-time, pur rimanendo nei vincoli del profilo Ravenscar. 

Come è stato fatto per attributi delle task, quali Periodo, Deadline Relativa e tempo logico zero, e per la rilevazione del jitter, riteniamo che la definizione di procedure specifiche per segnalare l'inizio e la fine di un job sia una soluzione più robusta.
