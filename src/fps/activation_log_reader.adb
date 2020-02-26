with Ada.Synchronous_Task_Control;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Deadline_Miss;
with Task_Metrics;

package body Activation_Log_Reader is
   use Ada.Real_Time;

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
         --  Task_Metrics.Start_Tracking;
         --  suspending parameterless request of activation event
         Wait;
         Deadline_Miss.Set_Deadline_Handler (Deadline_Miss.ALR, Ada.Real_Time.Clock +
            Ada.Real_Time.Milliseconds (Activation_Log_Reader_Parameters.Activation_Log_Reader_Deadline));
         --  non-suspending operation code
         Activation_Log_Reader_Parameters.Activation_Log_Reader_Operation;
         Deadline_Miss.Cancel_Deadline_handler (Deadline_Miss.ALR);
         --  Task_Metrics.End_Tracking;
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("ALR: Something has gone wrong here: " & Exception_Information (Error));
   end Activation_Log_Reader;

end Activation_Log_Reader;
