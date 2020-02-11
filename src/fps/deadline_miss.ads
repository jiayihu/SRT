with System;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Real_Time.Timing_Events; use Ada.Real_Time.Timing_Events;

package Deadline_Miss is
   type Task_Name is (RP, OCP, ALR);
   type Deadline_Events_Array is array (Task_Name) of Timing_Event;

   protected Handler
     with Priority =>
       System.Interrupt_Priority'Last
   is
      procedure Notify_Deadline_Miss (Event : in out Timing_Event);
   end Handler;

   procedure Set_Deadline_Handler (Name : Task_Name; At_Time : in Time);

   procedure Cancel_Deadline_Handler (Name : Task_Name);
end Deadline_Miss;
