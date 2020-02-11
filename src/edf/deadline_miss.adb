with Ada.Real_Time; use Ada.Real_Time;

package body Deadline_Miss is
   Deadline_Events : Deadline_Events_Array;

   protected body Handler is
      procedure Notify_Deadline_Miss (Event : in out Timing_Event) is
      begin
         raise Program_Error with "Detected deadline miss";
      end Notify_Deadline_Miss;
   end Handler;

   procedure Set_Deadline_Handler (Name : Task_Name; At_Time : in Time) is
   begin
      Set_Handler (Deadline_Events (Name),
         At_Time, Handler.Notify_Deadline_Miss'Access);
   end Set_Deadline_Handler;

   procedure Cancel_Deadline_Handler (Name : Task_Name) is
      Cancelled : Boolean;
      pragma Unreferenced (Cancelled);
   begin
      Cancel_Handler (Deadline_Events (Name), Cancelled);
   end Cancel_Deadline_Handler;
end Deadline_Miss;