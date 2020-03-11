with Ada.Real_Time; use Ada.Real_Time;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Task_Metrics;
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

      delay until Next_Time;
      System.BB.Threads.Queues.Initialize_Task_Table (1, False);
      loop
         --  Task_Metrics.Start_Tracking;
         Next_Time := Next_Time + Period;
         --  non-suspending operation code
         Regular_Producer_Operation;
         --  time-based activation event
         delay until Next_Time; --  delay statement at end of loop
         System.BB.Threads.Queues.Print_Table (1);
         --  Task_Metrics.End_Tracking;
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("RP: Something has gone wrong here: " & Exception_Information (Error));
   end Regular_Producer;
end Regular_Producer;
