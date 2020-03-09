with Ada.Real_Time; use Ada.Real_Time;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with System.BB.Time;
with System.BB.Threads; use System.BB.Threads;
with System.BB.Threads.Queues;

package body Print_Metrics is
   Period : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (5000);

   task body Print_Metrics is
      --  for periodic suspension
      Next_Time : Ada.Real_Time.Time := Activation_Manager.Get_Activation_Time;
   begin
      --  Setting artificial deadline
      Set_Period (System.BB.Time.Milliseconds (5000));
      Set_Starting_Time (Activation_Manager.Time_Conversion (Next_Time));
      Set_Relative_Deadline (System.BB.Time.Milliseconds (100));
      Set_Fake_Number_ID (-1);
      System.BB.Threads.Queues.Initialize_Task_Table (-1);

      delay until Next_Time;
      loop
         Next_Time := Next_Time + Period;

         System.BB.Threads.Queues.Print_Table (1);

         delay until Next_Time; --  delay statement at end of loop
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("Print_Metrics: Something has gone wrong here: " & Exception_Information (Error));
   end Print_Metrics;
end Print_Metrics;
