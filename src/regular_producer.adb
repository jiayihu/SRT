with Ada.Real_Time; use Ada.Real_Time;
with Activation_Manager; use Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Task_Metrics;
with System.BB.Time;
with System.BB.Threads; use System.BB.Threads;
with System.BB.Threads.Queues; use System.BB.Threads.Queues;
with Regular_Producer_Parameters; use Regular_Producer_Parameters;

package body Regular_Producer is
   Period : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (Regular_Producer_Period);

   task body Regular_Producer is
      --  for periodic suspension
      Next_Time : Ada.Real_Time.Time := Get_Activation_Time;
      Release_Jitter : Ada.Real_Time.Time;
   begin
      --  Setting artificial deadline
      Set_Period (System.BB.Time.Milliseconds (Regular_Producer_Period));
      Set_Starting_Time (Time_Conversion (Next_Time));
      Set_Relative_Deadline (System.BB.Time.Milliseconds (Regular_Producer_Deadline));
      Set_Fake_Number_ID (1);

      delay until Next_Time;
      Initialize_Task_Table (1, False);
      loop
         --  Task_Metrics.Start_Tracking;
         Release_Jitter := Ada.Real_Time.Time_First +
           (Ada.Real_Time.Clock - Next_Time);

         --  non-suspending operation code
         Regular_Producer_Operation;

         Next_Time := Next_Time + Period;

         Change_Jitters (Running_Thread, Time_Conversion (Ada.Real_Time.Time_First), Time_Conversion (Release_Jitter));

         --  time-based activation event
         delay until Next_Time; --  delay statement at end of loop
         Print_Table (1);
         --  Task_Metrics.End_Tracking;
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("RP: Something has gone wrong here: " & Exception_Information (Error));
   end Regular_Producer;
end Regular_Producer;
