--
--  Access to the STM32F4 General-Purpose I/O (GPIO) registers
--

--  For register details see:
--  Chapter 8 - General-Purpose I/Os, STMicroelectronic RM0090 Reference Manual

with System; use System;

package ST.STM32F4.GPIO with Preelaborate is

   --  This package provides access to the STM32F4 GPIO registers. It takes
   --  advantage of Ada's ability to specify hardware registers using
   --  enumeration types to represent options and states, and records and
   --  arrays to describe register layouts. This approach leads to more
   --  readable and verifiable code.
   --
   --  The layout of this package is broken into five sections and is organized
   --  with sections typically relying on types and constants defined in
   --  preceding sections:
   --     * Memory Addresses
   --     * Hardware Features
   --     * Register Types
   --     * Type Representations
   --     * GPIO Registers

   --  Memory Addresses:
   --    Defines the base address of the GPIO registers and the offset from
   --    this address for each individual GPIO register. This section appears
   --    at the start in common with the format used by many reference manuals.
   --    This style allows memory addresses to be easily cross-referenced and
   --    verified against the reference manual, and simplifies the reuse of the
   --    package across different microcontrollers where the memory location
   --    of the registers may differ.
   --
   --  Hardware Features:
   --    Defines types and constants related to the microcontroller or
   --    peripheral. This contains states that are selectable or readable via
   --    the unit's registers. Typically the states or options are represented
   --    by enumerated types or constrained integer types. For the former, this
   --    approach ensures the right set of options are being used by the
   --    program, while the later ensures the correct number of bits are
   --    assigned to objects of the type.
   --
   --  Register Types:
   --    These types define the individual registers. Typically this is done
   --    using record types, but in some cases arrays, packed arrays and
   --    register-sized numbers may be more suitable.
   --
   --  Type Representations:
   --    The hardware representation for above defined types are collected
   --    here. Grouping the representations together makes the type definitions
   --    in the earlier sections clearer and simpler to read, particularly
   --    when trying to discover the settings that are available for a
   --    particular register or component.
   --
   --   GPIO Registers:
   --     The definition of the GPIO registers. For this unit, only a single
   --     register is declared: GPIO_Port_Register. This has been done since
   --     the STM32F4 has nine GPIO ports with an identical set of registers.
   --     Thus, instead of defining nine unique GPIO port registers ourselves,
   --     we can define a single register consisting of an array of nine GPIO
   --     ports to provide more programming flexibility. For example, the
   --     IO_Mode_Register in Port_A can be read by:
   --
   --       Result := GPIO_Port_Register (Port_A).IO_Mode_Register;
   --
   --     In this example the enumeration item Port_A could be replaced by a
   --     GPIO_Port object and thus allowing the user to define the port to use
   --     elsewhere, or even delay the assignment of the GPIO_Port until
   --     run-time.

   --  To use this package, start with the GPIO Registers section to discover
   --  the registers available and then refer to the Register Types and
   --  Hardware Features as required to resolve the types used in GPIO
   --  Registers. This package uses the full names for registers and their
   --  components to make the code accessing these register readable.

   ---------------------------
   -- GPIO Memory Addresses --
   ---------------------------

   GPIO_Base_Address      : constant := 16#4002_0000#;
   MODER_Offset_Address   : constant := 16#00#;
   OTYPER_Offset_Address  : constant := 16#04#;
   OSPEEDR_Offset_Address : constant := 16#08#;
   PUPDR_Offset_Address   : constant := 16#0C#;
   IDR_Offset_Address     : constant := 16#10#;
   ODR_Offset_Address     : constant := 16#14#;
   BSRR_Offset_Address    : constant := 16#18#;
   LCKR_Offset_Address    : constant := 16#1C#;
   AFR_Offset_Address     : constant := 16#20#;

   -----------------------
   -- Hardware Features --
   -----------------------

   type GPIO_Ports is (Port_A, Port_B, Port_C, Port_D, Port_E,
                       Port_F, Port_G, Port_H, Port_I);

   type GPIO_Pins is mod 16;

   type IO_Mode is (Input, Output, Alternative_Function, Analog);
   type IO_Mode_Set is array (GPIO_Pins) of IO_Mode with Pack;

   type Output_Type is (Push_Pull, Open_Drain);
   type Output_Type_Set is array (GPIO_Pins) of Output_Type with Pack;

   type Speeds is (Low, Medium, High, Fast);
   type Speeds_Set is array (GPIO_Pins) of Speeds with Pack;

   type Pull_Direction is (No_Pull, Pull_Up, Pull_Down);
   type Pull_Direction_Set is array (GPIO_Pins) of Pull_Direction with Pack;

   type GPIO_State is (Low, High);
   type GPIO_State_Set is array (GPIO_Pins) of GPIO_State with Pack;

   type High_State is (No_Change, High);
   type Low_State is (No_Change, Low);

   type High_Set is array (GPIO_Pins) of High_State with Pack;
   type Low_Set is array (GPIO_Pins) of Low_State with Pack;

   type Active_Type is (Inactive, Active);
   type Lock_Type is (Unlocked, Locked);
   type Lock_Set is array (GPIO_Pins) of Lock_Type with Pack;

   type Alternative_Function_Type is mod 2 ** 4;
   type Alternative_Function_Set is array (GPIO_Pins)
     of Alternative_Function_Type with Pack;

   --------------------
   -- Register Types --
   --------------------

   type IO_Mode_Pins is record
      Pin : IO_Mode_Set;
   end record;

   type Output_Driver_Pins is record
      Pin : Output_Type_Set;
   end record;

   type Speeds_Pins is record
      Pin : Speeds_Set;
   end record;

   type Pull_Direction_Pins is record
      Pin : Pull_Direction_Set;
   end record;

   type GPIO_State_Pins is record
      Pin : GPIO_State_Set;
   end record;

   type GPIO_Set_Clear is record
      Pin_To_Set_High : High_Set;
      Pin_To_Set_Low  : Low_Set;
   end record;

   type Configuration_Lock is record
      Lock_Key : Active_Type;
      Pin      : Lock_Set;
   end record;

   type Pin_Alternative_Function is record
      Pin : Alternative_Function_Set;
   end record;

   type GPIO_Port_Registers is record
      IO_Mode_Register              : IO_Mode_Pins;
      Output_Driver_Register        : Output_Driver_Pins;
      Output_Pin_Speed_Register     : Speeds_Set;
      Pull_Direction_Register       : Pull_Direction_Pins;
      Input_State_Register          : GPIO_State_Pins;
      Output_State_Register         : GPIO_State_Pins;
      Set_Pin_State_Register        : GPIO_Set_Clear;
      Configuration_Lock_Register   : Configuration_Lock;
      Alternative_Function_Register : Pin_Alternative_Function;
   end record;

   --------------------------
   -- Type Representations --
   --------------------------

   --  GNAT assigns sequential codes to the enumeration values in the order
   --  they appear within a type, with the first enumeration value assigned the
   --  code 0. For most enumeration representations these representations are
   --  not strictly necessary as the GNAT behaviour achieves the assignment
   --  required by the hardware. However, if there are gaps in an enumerated
   --  type's code, then a representation is required for the type. This
   --  section demonstrates how this is done.

   for IO_Mode use (Input                => 2#00#,
                    Output               => 2#01#,
                    Alternative_Function => 2#10#,
                    Analog               => 2#11#);
   --  Typically reference manuals describe the code used for each enumeration
   --  value in binary. Ada allows us to transcribe these into the program
   --  directly.

   for Speeds use (Low => 0, Medium => 1, High => 2, Fast => 3);
   --  But if it is more convenient, decimals numbers can be used as well

   for GPIO_Set_Clear use record
      Pin_To_Set_High at 0 range  0 .. 15;
      Pin_To_Set_Low  at 0 range 16 .. 31;
   end record;

   for Configuration_Lock use record
      Lock_Key at 0 range 16 .. 16;
      Pin      at 0 range  0 .. 15;
   end record;

   for GPIO_Port_Registers use record
      IO_Mode_Register              at MODER_Offset_Address   range 0 .. 31;
      Output_Driver_Register        at OTYPER_Offset_Address  range 0 .. 15;
      Output_Pin_Speed_Register     at OSPEEDR_Offset_Address range 0 .. 31;
      Pull_Direction_Register       at PUPDR_Offset_Address   range 0 .. 31;
      Input_State_Register          at IDR_Offset_Address     range 0 .. 15;
      Output_State_Register         at ODR_Offset_Address     range 0 .. 15;
      Set_Pin_State_Register        at BSRR_Offset_Address    range 0 .. 31;
      Configuration_Lock_Register   at LCKR_Offset_Address    range 0 .. 16;
      Alternative_Function_Register at AFR_Offset_Address     range 0 .. 63;
   end record;

   --------------------
   -- GPIO Registers --
   --------------------

   pragma Warnings (Off, "*component of*");
   type GPIO_Port_Set is array (GPIO_Ports) of GPIO_Port_Registers
     with Volatile_Components, Component_Size => 16#400# * Storage_Unit;
   pragma Warnings (On, "*component of*");
   --  A type that defines the array of GPIO port registers. A type is required
   --  here so we can specify the spacing between each set of port registers.
   --  The spacing is achieved by specifying the size of each component. Since
   --  the components are larger than the underlying type, we suppress the
   --  compiler warning that is raised in this case since we know what we are
   --  doing.

   GPIO_Port_Register : GPIO_Port_Set
     with Address => System'To_Address (GPIO_Base_Address);
   --  Access to the GPIO port registers
   --
   --  We use System'To_Address to do the conversion from the memory address
   --  constant to a System.Address object as the
   --  System.Storage_Elements.To_Address function is dynamic.

end ST.STM32F4.GPIO;
