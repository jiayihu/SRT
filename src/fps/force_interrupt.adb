with Ada.Real_Time; use Ada.Real_Time;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with ST;              use ST;
with ST.EXTI; use ST.EXTI;
with Task_Metrics;
with System.BB.Time;
with System.BB.Threads; use System.BB.Threads;
with System.BB.Threads.Queues;

package body Force_Interrupt is
   Period : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (5000);
   Button_Line : constant Interrupt_Line := 0;
   --  The User Button is connected to EXTI0 (aka Line 0)

   task body Force_Interrupt is
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
         --  Task_Metrics.Start_Tracking;
         Next_Time := Next_Time + Period;

         EXTI.Software_Interrupt_Event_Register.Line :=
           (Button_Line => True, others => False);
         Ada.Text_IO.Put_Line ("Interrupt generated");

         delay until Next_Time; --  delay statement at end of loop
         --  Task_Metrics.End_Tracking;
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("Interrupt: Something has gone wrong here: " & Exception_Information (Error));
   end Force_Interrupt;
end Force_Interrupt;
