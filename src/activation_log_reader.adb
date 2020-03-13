with Ada.Synchronous_Task_Control;
with Activation_Manager; use Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Task_Metrics;
with System.BB.Time;
with System.BB.Threads; use System.BB.Threads;
with System.BB.Threads.Queues; use System.BB.Threads.Queues;
with Activation_Log_Reader_Parameters; use Activation_Log_Reader_Parameters;
with System.Tasking.Protected_Objects;

package body Activation_Log_Reader is
   use Ada.Real_Time;
   use System.BB.Time;

   Release_Time : System.BB.Time.Time := System.BB.Time.Time'First;

   Local_Suspension_Object : Ada.Synchronous_Task_Control.Suspension_Object;
   procedure Signal is
   begin
      Ada.Synchronous_Task_Control.Set_True (Local_Suspension_Object);

      if Release_Time = System.BB.Time.Time'First then
         Release_Time := System.BB.Time.Clock;
      end if;

      Ada.Text_IO.Put_Line ("Signal");
   end Signal;

   procedure Wait is
   begin
      Ada.Synchronous_Task_Control.Suspend_Until_True
        (Local_Suspension_Object);
   end Wait;

   task body Activation_Log_Reader is
      --  for tasks to achieve simultaneous activation
      Next_Time : Ada.Real_Time.Time := Activation_Manager.Get_Activation_Time;
      Release_Jitter : System.BB.Time.Time_Span;
      Response_Time : System.BB.Time.Time_Span;
   begin
      --  Setting artificial deadline
      Set_Starting_Time (Activation_Manager.Time_Conversion (Next_Time));
      Set_Relative_Deadline (System.BB.Time.Milliseconds (Activation_Log_Reader_Deadline));
      Set_Fake_Number_ID (3);

      delay until Next_Time;
      System.BB.Threads.Queues.Initialize_Task_Table (3, True);
      loop
         --  suspending parameterless request of activation event
         Wait;
         --  Task_Metrics.Start_Tracking;
         Release_Jitter := System.BB.Time.Clock - Release_Time;
         --  non-suspending operation code
         Activation_Log_Reader_Operation;

         Response_Time := System.BB.Time.Clock - Release_Time;
         Release_Time := System.BB.Time.Time'First;
         Change_Jitters (Running_Thread, Response_Time, Release_Jitter);
         --  Task_Metrics.End_Tracking;
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("ALR: Something has gone wrong here: " & Exception_Information (Error));
   end Activation_Log_Reader;

begin
   System.Tasking.Protected_Objects.Initialize_Protection_Deadline
      (System.Tasking.Protected_Objects.Current_Object, 90000000); -- 0.5 * 180Mhz
end Activation_Log_Reader;
