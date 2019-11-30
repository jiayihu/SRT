with Ada.Real_Time; use Ada.Real_Time;
with Ada.Execution_Time; use Ada.Execution_Time;

package body Overrun is
   use Ada.Real_Time;
   use Ada.Execution_Time;

   Limits : Limits_Array := (CPU_Time_First, CPU_Time_First, CPU_Time_First);

   procedure Start (Index : Natural; Budget : Time_Span) is
   begin
      Limits (Index) := Ada.Execution_Time.Clock + Budget;
   end Start;

   procedure Check (Index : Natural) is
   begin
      if Ada.Execution_Time.Clock > Limits (Index) then
         raise Program_Error with "Detected overrun";
      end if;
   end Check;
end Overrun;
