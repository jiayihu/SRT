with System;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Real_Time.Timing_Events; use Ada.Real_Time.Timing_Events;

package Deadline_Miss is
   type Deadline_Handler is limited private;

   procedure Set_Deadline_Handler (
      H : in out Deadline_Handler;
      Name : String;
      At_Time : Time);
   procedure Cancel_Deadline_Handler (H : in out Deadline_Handler);

private
   protected type Deadline_Handler
     with Priority =>
       System.Interrupt_Priority'Last
   is
      procedure Notify_Deadline_Miss (Event : in out Timing_Event);

      procedure Set_Deadline_Handler (Name : String; At_Time : Time);
      procedure Cancel_Deadline_Handler;

      private
         Tag : String (1 .. 3) := "N/A";
         Event : Timing_Event;
   end Deadline_Handler;
end Deadline_Miss;
