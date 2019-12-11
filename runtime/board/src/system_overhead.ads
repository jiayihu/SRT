package System_Overhead is
   pragma Preelaborate;

   procedure Start_Tracking;

   --  Avoid counting sub-program execution time
   procedure Start_Sub_Program;
   procedure End_Sub_Program;

   procedure End_Tracking (Item : String := "");

   procedure Log_Time;
   --  Just log the current clock time

   procedure Put_Line (Item : String);
end System_Overhead;
