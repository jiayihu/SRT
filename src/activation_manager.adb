with Ada.Real_Time;
with System.BB.Time;

package body Activation_Manager is
   function Get_Activation_Time return Ada.Real_Time.Time is
   begin
      return Activation_Time;
   end Get_Activation_Time;

   function Time_Conversion (Time_in  : Ada.Real_Time.Time) return System.BB.Time.Time_Span is
      Time_in_to_Time_Span : Ada.Real_Time.Time_Span;
      Time_out : System.BB.Time.Time_Span;
   begin
      Time_in_to_Time_Span := Time_in - Ada.Real_Time.Time_First;
      Time_out := System.BB.Time.To_Time_Span
         (Ada.Real_Time.To_Duration (Time_in_to_Time_Span));
      return Time_out;
   end Time_Conversion;
end Activation_Manager;
