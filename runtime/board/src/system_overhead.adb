with System.BB.Time; use System.BB.Time;
with System.Semihosting;

package body System_Overhead is
   Initial_Value : Time := 0;
   Start_Sub_Value : Time := 0;
   End_Sub_Value : Time := 0;

   procedure Start_Tracking is
   begin
      Initial_Value := Clock;
      Start_Sub_Value := 0;
      End_Sub_Value := 0;
   end Start_Tracking;

   procedure Start_Sub_Program is
   begin
      Start_Sub_Value := Clock;
   end Start_Sub_Program;

   procedure End_Sub_Program is
   begin
      End_Sub_Value := Clock;
   end End_Sub_Program;

   procedure End_Tracking (Item : String := "") is
      Now : constant Time := Clock;
      Sub_Program : Time;
      Elapsed : Time;
   begin
      --  Sometimes End_Tracking may be called before Start_Tracking
      if Initial_Value = 0 then
         return;
      end if;

      Sub_Program := End_Sub_Value - Start_Sub_Value;
      Elapsed := Now - Initial_Value - Sub_Program;

      Put_Line (Item & Time'Image (Elapsed));
   end End_Tracking;

   procedure Log_Time is
   begin
      Put_Line (Time'Image (Clock));
   end Log_Time;

   procedure Put_Line (Item : String) is
   begin
      System.Semihosting.Put (Item & ASCII.CR & ASCII.LF);
   end Put_Line;
end System_Overhead;
