--
--  This package defines common types used to interact with the registers on
--  STMicroelectronic microcontrollers.
--

--  The enumeration types have explicit representation clauses to ensure the
--  compiler assigns the correct values to each enumeration literal.
--

package ST with Pure is

   type Enable_Type is (Disable, Enable);
   for Enable_Type use (Disable => 0, Enable => 1);
   --  Enables or disables the desired operation

   type Enabled_Type is (Disabled, Enabled);
   for Enabled_Type use (Disabled => 0, Enabled => 1);
   --  Indicates if the object is enabled or disabled

   type Occurred_Type is (Not_Occurred, Occurred);
   for Occurred_Type use (Not_Occurred => 0, Occurred => 1);
   --  Indicates if an event occurred or not

   type Clear_Type is (Do_Not_Clear, Clear);
   for Clear_Type use (Do_Not_Clear => 0, Clear => 1);
   --  Indicates that a flag should be cleared or not

   type Reset_Type is (Do_Not_Reset, Reset);
   for Reset_Type use (Do_Not_Reset => 0, Reset => 1);
   --  Indicates whether the feature should be reset or not

end ST;
