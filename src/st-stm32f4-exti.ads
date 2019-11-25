--
--  Access to the STM32F4 Interrupts and Events (EXTI) registers
--

--  For register details see:
--  Chapter 12 - Interrupts and events,
--  STMicroelectronic RM0090 Reference Manual

with System; use System;

package ST.STM32F4.EXTI is

   ---------------------------
   -- EXTI Memory Addresses --
   ---------------------------

   EXTI_Base_Address    : constant := 16#4001_3C00#;
   IMR_Offset_Address   : constant := 16#00#;
   EMR_Offset_Address   : constant := 16#04#;
   RTSR_Offset_Address  : constant := 16#08#;
   FTSR_Offset_Address  : constant := 16#0C#;
   SWIER_Offset_Address : constant := 16#10#;
   PR_Offset_Address    : constant := 16#14#;

   -----------------------
   -- Hardware Features --
   -----------------------

   type Interrupt_Line is mod 23;

   type Mask_Type is (Masked, Not_Masked);
   type Masked_Line_Set is array (Interrupt_Line) of Mask_Type with Pack;

   type Enabled_Line_Set is array (Interrupt_Line) of Enabled_Type with Pack;

   type Boolean_Line_Set is array (Interrupt_Line) of Boolean with Pack;

   type Occurred_Line_Set is array (Interrupt_Line) of Occurred_Type with Pack;

   --------------------
   -- Register Types --
   --------------------

   type EXTI_Mask_Register is record
      Line : Masked_Line_Set;
   end record with Size => Word_Size;

   type EXTI_Enabled_Register is record
      Line : Enabled_Line_Set;
   end record with Size => Word_Size;

   type Software_Interrupt_Event is record
      Line : Boolean_Line_Set;
   end record with Size => Word_Size;

   type Interrupt_Pending is record
      Line : Occurred_Line_Set;
   end record with Size => Word_Size;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for EXTI_Mask_Register use record
      Line at 0 range 0 .. 22;
   end record;

   for EXTI_Enabled_Register use record
      Line at 0 range 0 .. 22;
   end record;

   for Software_Interrupt_Event use record
      Line at 0 range 0 .. 22;
   end record;

   for Interrupt_Pending use record
      Line at 0 range 0 .. 22;
   end record;

   --------------------
   -- GPIO Registers --
   --------------------

   Interrupt_Mask_Register : EXTI_Mask_Register
     with Volatile, Address =>
       System'To_Address (EXTI_Base_Address + IMR_Offset_Address);

   Event_Mask_Register : EXTI_Mask_Register
     with Volatile, Address =>
       System'To_Address (EXTI_Base_Address + EMR_Offset_Address);

   Rising_Trigger_Selection_Register : EXTI_Enabled_Register
     with Volatile, Address =>
       System'To_Address (EXTI_Base_Address + RTSR_Offset_Address);

   Falling_Trigger_Selection_Register : EXTI_Enabled_Register
     with Volatile, Address =>
       System'To_Address (EXTI_Base_Address + FTSR_Offset_Address);

   Software_Interrupt_Event_Register : Software_Interrupt_Event
     with Volatile, Address =>
       System'To_Address (EXTI_Base_Address + SWIER_Offset_Address);

   Interrupt_Pending_Register : Interrupt_Pending
     with Volatile, Address =>
       System'To_Address (EXTI_Base_Address + PR_Offset_Address);

end ST.STM32F4.EXTI;
