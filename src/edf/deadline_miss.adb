with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

package body Deadline_Miss is
   protected body Deadline_Handler is
      procedure Notify_Deadline_Miss (Event : in out Timing_Event) is
      begin
         --raise Program_Error with "Detected deadline miss";
         Ada.Text_IO.Put_Line ("Deadline Miss Detected - " & Tag );
      end Notify_Deadline_Miss;

      procedure Set_Deadline_Handler (Name : String; At_Time : in Time) is
      begin
         Tag := Name;

         Set_Handler (Event, At_Time, Notify_Deadline_Miss'Access);
      end Set_Deadline_Handler;

      procedure Cancel_Deadline_Handler is
         Cancelled : Boolean;
         pragma Unreferenced (Cancelled);
      begin
         Cancel_Handler (Event, Cancelled);
      end Cancel_Deadline_Handler;
   end Deadline_Handler;

   procedure Set_Deadline_Handler (H: in out Deadline_Handler; Name : String; At_Time : in Time) is
   begin
      H.Set_Deadline_Handler (Name, At_Time);
   end Set_Deadline_Handler;

   procedure Cancel_Deadline_Handler (H : in out Deadline_Handler) is
   begin
      H.Cancel_Deadline_Handler;
   end Cancel_Deadline_Handler;
end Deadline_Miss;
