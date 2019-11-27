with Ada.Real_Time;
with Ada.Execution_Time;
with Ada.Text_IO;

package body Task_Overhead is
   use Ada.Real_Time;
   use Ada.Execution_Time;
   Last : CPU_Time;

   procedure Start_Tracking is
   begin
      Last := Ada.Execution_Time.Clock;
   end Start_Tracking;

   procedure End_Tracking (Item : String := "") is
      Exe_Time : Duration;
   begin
      Exe_Time := To_Duration (Ada.Execution_Time.Clock - Last);
      Ada.Text_IO.Put_Line ("Elapsed time:" & Duration'Image (Exe_Time));
      Last := Ada.Execution_Time.Clock;
   end End_Tracking;
end Task_Overhead;
