with Request_Buffer;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Deadline_Miss;
with Task_Overhead;
with System.BB.Time;
with System.BB.Threads; use System.BB.Threads;
with On_Call_Producer_Parameters; use On_Call_Producer_Parameters;

package body On_Call_Producer is
   use Ada.Real_Time;

   --  to hide the implementation of the event buffer
   function Start (Activation_Parameter : Positive) return Boolean is
   begin
      return Request_Buffer.Deposit (Activation_Parameter);
   end Start;
   task body On_Call_Producer is
      Current_Workload : Positive;
      --  for tasks to achieve simultaneous activation
      Next_Time : Ada.Real_Time.Time := Activation_Manager.Get_Activation_Time;
   begin
      --  Setting artificial deadline
      Set_Starting_Time (Activation_Manager.Time_Conversion (Next_Time));
      Set_Relative_Deadline (System.BB.Time.Milliseconds (On_Call_Producer_Deadline));

      delay until Next_Time;
      loop
         --  Task_Overhead.Start_Tracking;
         --  suspending request for activation event with data exchange
         Current_Workload := Request_Buffer.Extract;
         Deadline_Miss.Set_Deadline_Handler (Deadline_Miss.OCP, Ada.Real_Time.Clock +
            Ada.Real_Time.Milliseconds (On_Call_Producer_Deadline));
         --  non-suspending operation code
         On_Call_Producer_Operation (Current_Workload);
         Deadline_Miss.Cancel_Deadline_Handler (Deadline_Miss.OCP);
         --  Task_Overhead.End_Tracking;
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("OCP: Something has gone wrong here: " & Exception_Information (Error));
   end On_Call_Producer;
end On_Call_Producer;
