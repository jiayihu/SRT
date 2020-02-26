with Ada.Real_Time; use Ada.Real_Time;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with ST;              use ST;
with ST.EXTI; use ST.EXTI;
with Task_Metrics;

package body Force_Interrupt is
   Period : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (5000);
   Button_Line : constant Interrupt_Line := 0;
   --  The User Button is connected to EXTI0 (aka Line 0)

   task body Force_Interrupt is
      --  for periodic suspension
      Next_Time : Ada.Real_Time.Time;
   begin
      --  for tasks to achieve simultaneous activation
      Activation_Manager.Activation_Cyclic (Next_Time);
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
