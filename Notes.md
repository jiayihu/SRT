# Runtime notes

1. Il metodo `Change_Relative_Deadline` in `s-bbthqu.adb` non differenzia tra cambio di deadline relativa come inizializzazione di una task e come acquisizione/rilascio di una risorsa protetta
   - Attenzione: la task rilasciata a tempo $s$ con deadline relativa $d$ e che accede alla risorsa protetta con deadline floor $D$ a tempo $t$ ($s \lt t$), dovrebbe vedersi la deadline relativa cambiata in $D$ e la deadline assoluta calcolata come $min\{t + D, s + d\}$
2. Perché il controllo di deadline miss avviene solo all'interno di una entry? E non ad esempio anche quando una task viene messa in coda di sospensione a causa del `delay until`?
3. I Suspension Objects sono senza deadline floor
