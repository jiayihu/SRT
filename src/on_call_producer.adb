with Request_Buffer;
with Activation_Manager; use Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Task_Metrics;
with System.BB.Time;
with System.BB.Threads; use System.BB.Threads;
with System.BB.Threads.Queues; use System.BB.Threads.Queues;
with On_Call_Producer_Parameters; use On_Call_Producer_Parameters;

package body On_Call_Producer is
   use Ada.Real_Time;
   use System.BB.Time;

   Release_Time : System.BB.Time.Time := System.BB.Time.Time'First;

   --  to hide the implementation of the event buffer
   function Start (Activation_Parameter : Positive) return Boolean is
      Response : Boolean;
   begin
      Response := Request_Buffer.Deposit (Activation_Parameter);

      if Release_Time = System.BB.Time.Time'First then
         Release_Time := System.BB.Time.Clock;
      end if;

      return Response;
   end Start;
   task body On_Call_Producer is
      Current_Workload : Positive;
      Next_Time : Ada.Real_Time.Time := Activation_Manager.Get_Activation_Time;
      Release_Jitter : System.BB.Time.Time_Span;
      Response_Time : System.BB.Time.Time_Span;
   begin
      --  Setting artificial deadline
      Set_Starting_Time (Activation_Manager.Time_Conversion (Next_Time));
      Set_Relative_Deadline (System.BB.Time.Milliseconds (On_Call_Producer_Deadline));
      Set_Fake_Number_ID (2);

      delay until Next_Time;
      System.BB.Threads.Queues.Initialize_Task_Table (2, True);
      loop
         --  Task_Metrics.Start_Tracking;
         --  suspending request for activation event with data exchange
         Current_Workload := Request_Buffer.Extract;
         Release_Jitter := System.BB.Time.Clock - Release_Time;

         --  non-suspending operation code
         On_Call_Producer_Operation (Current_Workload);

         Response_Time := System.BB.Time.Clock - Release_Time;
         Release_Time := System.BB.Time.Time'First;
         Change_Jitters (Running_Thread, Response_Time, Release_Jitter);
         --  Task_Metrics.End_Tracking;
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("OCP: Something has gone wrong here: " & Exception_Information (Error));
   end On_Call_Producer;
end On_Call_Producer;
