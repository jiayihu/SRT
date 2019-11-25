with System.BB.Time; use System.BB.Time;
with System.Semihosting;

package body Overhead is
   Initial_Value : Time;

   procedure Start_Tracking is
   begin
      Initial_Value := Clock;
      Put_Line ("Initial_Value" & Time'Image (Initial_Value));
   end Start_Tracking;

   procedure End_Tracking is
      Now : constant Time := Clock;
      --  Elapsed : Time;
   begin
      --  Elapsed := Now - Initial_Value;
      Put_Line (Time'Image (Initial_Value) & Time'Image (Now));
   end End_Tracking;

   procedure Put_Line (Item : String) is
   begin
      System.Semihosting.Put (Item & ASCII.CR & ASCII.LF);
   end Put_Line;
end Overhead;
