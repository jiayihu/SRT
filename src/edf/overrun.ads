with Ada.Real_Time;
with Ada.Execution_Time;

package Overrun is
   type Limits_Array is array (0 .. 2) of Ada.Execution_Time.CPU_Time;

   procedure Start (Index : Natural; Budget : Ada.Real_Time.Time_Span);
   procedure Check (Index : Natural);
end Overrun;
