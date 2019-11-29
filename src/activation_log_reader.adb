with Ada.Synchronous_Task_Control;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Overrun;

package body Activation_Log_Reader is
   Local_Suspension_Object : Ada.Synchronous_Task_Control.Suspension_Object;
   procedure Signal is
   begin
      Ada.Synchronous_Task_Control.Set_True (Local_Suspension_Object);
   end Signal;
   procedure Wait is
   begin
      Ada.Synchronous_Task_Control.Suspend_Until_True
        (Local_Suspension_Object);
   end Wait;
   task body Activation_Log_Reader is
   begin
      --  for tasks to achieve simultaneous activation
      Activation_Manager.Activation_Sporadic;
      loop
         Overrun.Start (2, Ada.Real_Time.Milliseconds (1000));
         --  suspending parameterless request of activation event
         Wait;
         --  non-suspending operation code
         Activation_Log_Reader_Parameters.Activation_Log_Reader_Operation;
         Overrun.Check (2);
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("ALR: Something has gone wrong here: " & Exception_Information (Error));
   end Activation_Log_Reader;
end Activation_Log_Reader;
