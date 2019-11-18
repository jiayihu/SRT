with Ada.Interrupts.Names;
with System;
package External_Event_Server_Parameters is
   --  a target-specific interrupt
   The_Interrupt : constant Ada.Interrupts.Interrupt_ID :=
     Ada.Interrupts.Names.EXTI0_Interrupt;

   --  the interrupt priority should be at the appropriate level
   Event_Queue_Priority : constant System.Interrupt_Priority :=
     System.Interrupt_Priority'First;

   --  the interrupt sporadic priority is determined by deadline
   --  monotonic analysis
   External_Event_Server_Priority : constant System.Priority := 11;
   procedure Server_Operation;
end External_Event_Server_Parameters;
