with Request_Buffer;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Deadline_Miss;
with Task_Metrics;

package body On_Call_Producer is
   use Ada.Real_Time;

   Local_Deadline : Deadline_Miss.Deadline_Handler;

   --  to hide the implementation of the event buffer
   function Start (Activation_Parameter : Positive) return Boolean is
   begin
      return Request_Buffer.Deposit (Activation_Parameter);
   end Start;
   task body On_Call_Producer is
      Current_Workload : Positive;
   begin
      --  for tasks to achieve simultaneous activation
      Activation_Manager.Activation_Sporadic;
      loop
         --  Task_Metrics.Start_Tracking;
         --  suspending request for activation event with data exchange
         Current_Workload := Request_Buffer.Extract;
         --  non-suspending operation code
         On_Call_Producer_Parameters.On_Call_Producer_Operation
           (Current_Workload);
         Deadline_Miss.Cancel_Deadline_Handler (Local_Deadline);
         --  Task_Metrics.End_Tracking;
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("OCP: Something has gone wrong here: " & Exception_Information (Error));
   end On_Call_Producer;
end On_Call_Producer;
