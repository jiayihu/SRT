--
--  Access to the STM32F407 Reset and Clock Control (RCC) registers
--

--  For register details see:
--  Chapter 7 - Reset and clock control for STM32F405xx/07xx and
--  STM32F415xx/17xx, STMicroelectronic RM0090 Reference Manual

with System; use System;

package ST.STM32F4.RCC with Preelaborate is

   --  This package provides access to the STM32F4 Reset and Clock Control
   --  registers. It takes advantage of Ada's ability to specify hardware
   --  registers using enumeration types to represent options and states, and
   --  records and arrays to describe register layouts. This approach leads to
   --  more readable and verifiable code.
   --
   --  The layout of this package is broken into five sections and is organized
   --  with sections typically relying on types and constants defined in
   --  preceding sections:
   --     * Memory Addresses
   --     * Hardware Features
   --     * Register Types
   --     * Type Representations
   --     * RCC Registers

   --  Memory Addresses:
   --    Defines the base address of the RCC registers and the offset from
   --    this address for each individual register. This section appears at
   --    the start in common with the format used by many reference manuals.
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
   --   RCC Registers:
   --     The definition of the RCC registers. For this unit,

   --  To use this package, start with the RCC Registers section to discover
   --  the registers available and then refer to the Register Types and
   --  Hardware Features as required to resolve the types used in RCC
   --  registers. This package uses the full names for registers and their
   --  components to make the code accessing these register readable.

   --------------------------
   -- RCC Memory Addresses --
   --------------------------

   RCC_Base_Address         : constant := 16#4002_3800#;
   CR_Offset_Address        : constant := 16#00#;
   PLLCFGR_Offset_Address   : constant := 16#04#;
   CFGR_Offset_Address      : constant := 16#08#;
   CIR_Offset_Address       : constant := 16#0C#;
   AHB1RSTR_Offset_Address  : constant := 16#10#;
   AHB2RSTR_Offset_Address  : constant := 16#14#;
   AHB3RSTR_Offset_Address  : constant := 16#18#;
   APB1RSTR_Offset_Address  : constant := 16#20#;
   APB2RSTR_Offset_Address  : constant := 16#24#;
   AHB1ENR_Offset_Address   : constant := 16#30#;
   AHB2ENR_Offset_Address   : constant := 16#34#;
   AHB3ENR_Offset_Address   : constant := 16#38#;
   APB1ENR_Offset_Address   : constant := 16#40#;
   APB2ENR_Offset_Address   : constant := 16#44#;
   AHB1LPENR_Offset_Address : constant := 16#50#;
   AHB2LPENR_Offset_Address : constant := 16#54#;
   AHB3LPENR_Offset_Address : constant := 16#58#;
   APB1LPENR_Offset_Address : constant := 16#60#;
   APB2LPENR_Offset_Address : constant := 16#64#;
   BDCR_Offset_Address      : constant := 16#70#;
   CSR_Offset_Address       : constant := 16#74#;
   SSCGR_Offset_Address     : constant := 16#80#;
   PLLI2SCF_Offset_Address  : constant := 16#84#;

   -----------------------
   -- Hardware Features --
   -----------------------

   type HSI_Clock_Calibration is mod 2 ** 8;
   type HSI_Clock_Trimming is mod 2 ** 5;

   type Peripheral_Division_Factor is mod 2 ** 4;
   type Entry_Clock_Source_Type is
     (High_Speed_Internal_Clock, High_Speed_External_Clock);
   type System_Division_Factor is (Divide_By_2, Divide_By_4,
                                   Divide_By_6, Divide_By_8);
   type Multiplication_Factor is mod 2 ** 9;
   type Input_Clock_Factor is mod 2 ** 6;

   type MCO2_Clock_Source is (System_Clock, PLL_I2S, High_Speed_External_Clock,
                         PLL_Clock);
   type MCO1_Clock_Source is (High_Speed_Internal_Clock,
                              Low_Speed_External_Clock,
                              High_Speed_External_Clock,
                              PLL_Clock);
   type MCO_Prescale_Factor is (No_Division, Divide_By_2, Divide_By_3,
                                Divide_By_4, Divide_By_5);
   type I2S_Clock_Source is (PLL_I2S, External);
   type RTC_Prescale_Factor is mod 2 ** 5;
   type APB_Prescale_Factor is (No_Division, Divide_By_2, Divide_By_4,
                                Divide_By_8, Divide_By_16);
   type AHB_Prescale_Factor is (No_Division, Divide_By_2, Divide_By_4,
                                Divide_By_8, Divide_By_16, Divide_By_64,
                                Divide_By_128, Divide_By_256, Divide_By_512);
   type System_Clock_Source_Type is (High_Speed_Internal_Clock,
                                     High_Speed_External_Clock,
                                     PLL);

   type Reset_Type is (Do_Not_Reset, Reset);

   type RTC_Clock_Source_Type is (No_Clock,
                                  Low_Speed_External_Clock,
                                  Low_Speed_Internal_Clock,
                                  High_Speed_External_Clock_Derived);

   type Spread_Selection is (Center_Spread, Down_Spread);
   type Incrementation_Step_Size is mod 2 ** 15;
   type Modulation_Period_Type is mod 2 ** 13;

   type I2S_Clock_Division_Factor is mod 2 ** 3;
   type VCO_Multiplaction_Factor is mod 2 ** 9;

   --------------------
   -- Register Types --
   --------------------

   type Clock_Control is record
      PLL_I2S_Locked                        : Boolean;
      PLL_I2S                               : Enable_Type;
      PLL_Main_Locked                       : Boolean;
      PLL_Main                              : Enable_Type;
      Clock_Security_System                 : Enable_Type;
      High_Speed_External_Clock_Bypass      : Enable_Type;
      High_Speed_External_Clock_Ready       : Boolean;
      High_Speed_External_Clock             : Enable_Type;
      High_Speed_Internal_Clock_Calibration : HSI_Clock_Calibration;
      High_Speed_Internal_Clock_Trimming    : HSI_Clock_Trimming;
      High_Speed_Internal_Clock_Ready       : Boolean;
      High_Speed_Internal_Clock             : Enable_Type;
   end record;

   type PLL_Configuration is record
      Peripheral_Clock_Divider : Peripheral_Division_Factor;
      Clock_Source             : Entry_Clock_Source_Type;
      System_Clock_Divider     : System_Division_Factor;
      VCO_Multiplier           : Multiplication_Factor;
      Input_Clock_Divider      : Input_Clock_Factor;
   end record;

   type Clock_Configuration is record
      Microcontroller_Clock_Output_2           : MCO2_Clock_Source;
      Microcontroller_Clock_Output_2_Prescaler : MCO_Prescale_Factor;
      Microcontroller_Clock_Output_1_Prescaler : MCO_Prescale_Factor;
      I2S_Clock_Selection                      : I2S_Clock_Source;
      Microcontroller_Clock_Output_1           : MCO1_Clock_Source;
      HSE_Clock_Divider_For_RTC                : RTC_Prescale_Factor;
      APB2_Prescaler                           : APB_Prescale_Factor;
      APB1_Prescaler                           : APB_Prescale_Factor;
      AHB_Prescaler                            : AHB_Prescale_Factor;
      System_Clock_Status                      : System_Clock_Source_Type;
      System_Clock_Source                      : System_Clock_Source_Type;
   end record;

   type Clock_Interrupt is record
      Clock_Security_System_Clear            : Boolean;
      PLL_I2S_Ready_Clear                    : Boolean;
      Main_PLL_Ready_Clear                   : Boolean;
      High_Speed_External_Clock_Ready_Clear  : Boolean;
      High_Speed_Internal_Clock_Ready_Clear  : Boolean;
      Low_Speed_External_Clock_Ready_Clear   : Boolean;
      Low_Speed_Internal_Clock_Ready_Clear   : Boolean;
      PLL_I2S_Ready_Enable                   : Enable_Type;
      Main_PLL_Read_Enable                   : Enable_Type;
      High_Speed_External_Clock_Ready_Enable : Enable_Type;
      High_Speed_Internal_Clock_Ready_Enable : Enable_Type;
      Low_Speed_External_Clock_Ready_Enable  : Enable_Type;
      Low_Speed_Internal_Clock_Ready_Enable  : Enable_Type;
      Clock_Security_System                  : Boolean;
      PLL_I2S_Ready                          : Boolean;
      Main_PLL_Ready                         : Boolean;
      High_Speed_External_Clock_Ready        : Boolean;
      High_Speed_Internal_Clock_Ready        : Boolean;
      Low_Speed_External_Clock_Ready         : Boolean;
      Low_Speed_Internal_Clock_Ready         : Boolean;
   end record;

   type AHB1_Peripheral_Reset is record
      USB_OTG_HS   : Reset_Type;
      Ethernet_Mac : Reset_Type;
      DMA2         : Reset_Type;
      DMA1         : Reset_Type;
      CRC          : Reset_Type;
      IO_Port_I    : Reset_Type;
      IO_Port_H    : Reset_Type;
      IO_Port_G    : Reset_Type;
      IO_Port_F    : Reset_Type;
      IO_Port_E    : Reset_Type;
      IO_Port_D    : Reset_Type;
      IO_Port_C    : Reset_Type;
      IO_Port_B    : Reset_Type;
      IO_Port_A    : Reset_Type;
   end record;

   type AHB2_Peripheral_Reset is record
      USB_OTG_FS              : Reset_Type;
      Random_Number_Generator : Reset_Type;
      Hash                    : Reset_Type;
      Cryptographic           : Reset_Type;
      Camera_Interface        : Reset_Type;
   end record;

   type AHB3_Peripheral_Reset is record
      Flexible_Static_Memory_Controller : Reset_Type;
   end record;

   type APB1_Peripheral_Reset is record
      DAC             : Reset_Type;
      Power_Interface : Reset_Type;
      CAN2            : Reset_Type;
      CAN1            : Reset_Type;
      I2C3            : Reset_Type;
      I2C2            : Reset_Type;
      I2C1            : Reset_Type;
      UART5           : Reset_Type;
      UART4           : Reset_Type;
      UART3           : Reset_Type;
      UART2           : Reset_Type;
      SPI3            : Reset_Type;
      SPI2            : Reset_Type;
      Window_Watchdog : Reset_Type;
      TIM14           : Reset_Type;
      TIM13           : Reset_Type;
      TIM12           : Reset_Type;
      TIM7            : Reset_Type;
      TIM6            : Reset_Type;
      TIM5            : Reset_Type;
      TIM4            : Reset_Type;
      TIM3            : Reset_Type;
      TIM2            : Reset_Type;
   end record;

   type APB2_Peripheral_Reset is record
      TIM11                           : Reset_Type;
      TIM10                           : Reset_Type;
      TIM9                            : Reset_Type;
      System_Configuration_Controller : Reset_Type;
      SPI1                            : Reset_Type;
      SDIO                            : Reset_Type;
      ADC                             : Reset_Type;
      USART6                          : Reset_Type;
      USART1                          : Reset_Type;
      TIM8                            : Reset_Type;
      TIM1                            : Reset_Type;
   end record;

   type AHB1_Peripheral_Clock_Enable is record
      USB_OTG_HS_ULPI       : Enable_Type;
      USB_OTG_HS            : Enable_Type;
      Ethernet_PTP          : Enable_Type;
      Ethernet_Reception    : Enable_Type;
      Ethernet_Transmission : Enable_Type;
      Ethernet_Mac          : Enable_Type;
      DMA2                  : Enable_Type;
      DMA1                  : Enable_Type;
      CCM_Data_Ram          : Enable_Type;
      Backup_SRAM           : Enable_Type;
      CRC                   : Enable_Type;
      IO_Port_I             : Enable_Type;
      IO_Port_H             : Enable_Type;
      IO_Port_G             : Enable_Type;
      IO_Port_F             : Enable_Type;
      IO_Port_E             : Enable_Type;
      IO_Port_D             : Enable_Type;
      IO_Port_C             : Enable_Type;
      IO_Port_B             : Enable_Type;
      IO_Port_A             : Enable_Type;
   end record;

   type AHB2_Peripheral_Clock_Enable is record
      USB_OTG_FS              : Enable_Type;
      Random_Number_Generator : Enable_Type;
      Hash                    : Enable_Type;
      Cryptographic           : Enable_Type;
      Camera_Interface        : Enable_Type;
   end record;

   type AHB3_Peripheral_Clock_Enable is record
      Flexible_Static_Memory_Controller : Enable_Type;
   end record;

   type APB1_Peripheral_Clock_Enable is record
      DAC             : Enable_Type;
      Power_Interface : Enable_Type;
      CAN2            : Enable_Type;
      CAN1            : Enable_Type;
      I2C3            : Enable_Type;
      I2C2            : Enable_Type;
      I2C1            : Enable_Type;
      UART5           : Enable_Type;
      UART4           : Enable_Type;
      UART3           : Enable_Type;
      UART2           : Enable_Type;
      SPI3            : Enable_Type;
      SPI2            : Enable_Type;
      Window_Watchdog : Enable_Type;
      TIM14           : Enable_Type;
      TIM13           : Enable_Type;
      TIM12           : Enable_Type;
      TIM7            : Enable_Type;
      TIM6            : Enable_Type;
      TIM5            : Enable_Type;
      TIM4            : Enable_Type;
      TIM3            : Enable_Type;
      TIM2            : Enable_Type;
   end record;

   type APB2_Peripheral_Clock_Enable is record
      TIM11                           : Enable_Type;
      TIM10                           : Enable_Type;
      TIM9                            : Enable_Type;
      System_Configuration_Controller : Enable_Type;
      SPI1                            : Enable_Type;
      SDIO                            : Enable_Type;
      ADC3                            : Enable_Type;
      ADC2                            : Enable_Type;
      ADC1                            : Enable_Type;
      USART6                          : Enable_Type;
      USART1                          : Enable_Type;
      TIM8                            : Enable_Type;
      TIM1                            : Enable_Type;
   end record;

   type Backup_Domain_Control is record
      Reset                           : Reset_Type;
      RTC_Clock                       : Enable_Type;
      RTC_Clock_Source                : RTC_Clock_Source_Type;
      Low_Speed_External_Clock_Bypass : Enable_Type;
      Low_Speed_External_Clock_Ready  : Boolean;
      Low_Speed_External_Clock        : Enable_Type;
   end record;

   type Clock_Control_And_Status is record
      Low_Power_Reset                : Occurred_Type;
      Window_Watchdog_Reset          : Occurred_Type;
      Independent_Watchdog_Reset     : Occurred_Type;
      Software_Reset                 : Occurred_Type;
      POR_PDR_Reset                  : Occurred_Type;
      PIN_Reset                      : Occurred_Type;
      BOR_Reset                      : Occurred_Type;
      Remove_Reset_Flag              : Boolean;
      Low_Speed_Internal_Clock_Ready : Boolean;
      Low_Speed_Internal_Clock       : Enable_Type;
   end record;

   type Spread_Spectrum_Clock_Generation is record
      Spread_Spectrum_Modulation : Enable_Type;
      Spread                     : Spread_Selection;
      Incrementation_Step        : Incrementation_Step_Size;
      Modulation_Period          : Modulation_Period_Type;
   end record;

   type PLL_I2S_Configuration is record
      I2S_Clocks_Divider : I2S_Clock_Division_Factor;
      VCO_Multiplier     : VCO_Multiplaction_Factor;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Clock_Control use record
      PLL_I2S_Locked                        at 0 range 27 .. 27;
      PLL_I2S                               at 0 range 26 .. 26;
      PLL_Main_Locked                       at 0 range 25 .. 25;
      PLL_Main                              at 0 range 24 .. 24;
      Clock_Security_System                 at 0 range 19 .. 19;
      High_Speed_External_Clock_Bypass      at 0 range 18 .. 18;
      High_Speed_External_Clock_Ready       at 0 range 17 .. 17;
      High_Speed_External_Clock             at 0 range 16 .. 16;
      High_Speed_Internal_Clock_Calibration at 0 range  8 .. 15;
      High_Speed_Internal_Clock_Trimming    at 0 range  3 .. 7;
      High_Speed_Internal_Clock_Ready       at 0 range  1 .. 1;
      High_Speed_Internal_Clock             at 0 range  0 .. 0;
   end record;
   --  The record representations follow the format of the STM32F4 reference
   --  manual where the bits of the record are documented in decreasing bit
   --  order.

   for PLL_Configuration use record
      Peripheral_Clock_Divider at 0 range 24 .. 27;
      Clock_Source             at 0 range 22 .. 22;
      System_Clock_Divider     at 0 range 16 .. 17;
      VCO_Multiplier           at 0 range  6 .. 14;
      Input_Clock_Divider      at 0 range  0 .. 5;
   end record;

   for MCO_Prescale_Factor use (No_Division => 0,
                                Divide_By_2 => 2#100#,
                                Divide_By_3 => 2#101#,
                                Divide_By_4 => 2#110#,
                                Divide_By_5 => 2#111#);
   --  GNAT assigns sequential codes to the enumeration values in the order
   --  they appear within a type, with the first enumeration value assigned the
   --  code 0. For most enumeration representations these representations are
   --  not strictly necessary as the GNAT behaviour achieves the assignment
   --  required by the hardware. However, if there are gaps in an enumerated
   --  type's code, then a representation is required for the type. The bit
   --  representation can be express in binary (as commonly described by
   --  a microcontroller's reference manual).

   for APB_Prescale_Factor use (No_Division   => 0,
                                Divide_By_2   => 4,
                                Divide_By_4   => 5,
                                Divide_By_8   => 6,
                                Divide_By_16  => 7);
   --  Or as decimals if more convenient.

   for AHB_Prescale_Factor use (No_Division   => 0,
                                Divide_By_2   => 8,
                                Divide_By_4   => 9,
                                Divide_By_8   => 10,
                                Divide_By_16  => 11,
                                Divide_By_64  => 12,
                                Divide_By_128 => 13,
                                Divide_By_256 => 14,
                                Divide_By_512 => 15);

   for Clock_Configuration use record
      Microcontroller_Clock_Output_2           at 0 range 30 .. 31;
      Microcontroller_Clock_Output_2_Prescaler at 0 range 27 .. 29;
      Microcontroller_Clock_Output_1_Prescaler at 0 range 24 .. 26;
      I2S_Clock_Selection                      at 0 range 23 .. 23;
      Microcontroller_Clock_Output_1           at 0 range 21 .. 22;
      HSE_Clock_Divider_For_RTC                at 0 range 16 .. 20;
      APB2_Prescaler                           at 0 range 13 .. 15;
      APB1_Prescaler                           at 0 range 10 .. 12;
      AHB_Prescaler                            at 0 range  4 .. 7;
      System_Clock_Status                      at 0 range  2 .. 3;
      System_Clock_Source                      at 0 range  0 .. 1;
   end record;

   for Clock_Interrupt use record
      Clock_Security_System_Clear            at 0 range 23 .. 23;
      PLL_I2S_Ready_Clear                    at 0 range 21 .. 21;
      Main_PLL_Ready_Clear                   at 0 range 20 .. 20;
      High_Speed_External_Clock_Ready_Clear  at 0 range 19 .. 19;
      High_Speed_Internal_Clock_Ready_Clear  at 0 range 18 .. 18;
      Low_Speed_External_Clock_Ready_Clear   at 0 range 17 .. 17;
      Low_Speed_Internal_Clock_Ready_Clear   at 0 range 16 .. 16;
      PLL_I2S_Ready_Enable                   at 0 range 13 .. 13;
      Main_PLL_Read_Enable                   at 0 range 12 .. 12;
      High_Speed_External_Clock_Ready_Enable at 0 range 11 .. 11;
      High_Speed_Internal_Clock_Ready_Enable at 0 range 10 .. 10;
      Low_Speed_External_Clock_Ready_Enable  at 0 range  9 .. 9;
      Low_Speed_Internal_Clock_Ready_Enable  at 0 range  8 .. 8;
      Clock_Security_System                  at 0 range  7 .. 7;
      PLL_I2S_Ready                          at 0 range  5 .. 5;
      Main_PLL_Ready                         at 0 range  4 .. 4;
      High_Speed_External_Clock_Ready        at 0 range  3 .. 3;
      High_Speed_Internal_Clock_Ready        at 0 range  2 .. 2;
      Low_Speed_External_Clock_Ready         at 0 range  1 .. 1;
      Low_Speed_Internal_Clock_Ready         at 0 range  0 .. 0;
   end record;

   for AHB1_Peripheral_Reset use record
      USB_OTG_HS   at 0 range 29 .. 29;
      Ethernet_Mac at 0 range 25 .. 25;
      DMA2         at 0 range 22 .. 22;
      DMA1         at 0 range 21 .. 21;
      CRC          at 0 range 12 .. 12;
      IO_Port_I    at 0 range  8 .. 8;
      IO_Port_H    at 0 range  7 .. 7;
      IO_Port_G    at 0 range  6 .. 6;
      IO_Port_F    at 0 range  5 .. 5;
      IO_Port_E    at 0 range  4 .. 4;
      IO_Port_D    at 0 range  3 .. 3;
      IO_Port_C    at 0 range  2 .. 2;
      IO_Port_B    at 0 range  1 .. 1;
      IO_Port_A    at 0 range  0 .. 0;
   end record;

   for AHB2_Peripheral_Reset use record
      USB_OTG_FS              at 0 range 7 .. 7;
      Random_Number_Generator at 0 range 6 .. 6;
      Hash                    at 0 range 5 .. 5;
      Cryptographic           at 0 range 4 .. 4;
      Camera_Interface        at 0 range 0 .. 0;
   end record;

   for AHB3_Peripheral_Reset use record
      Flexible_Static_Memory_Controller at 0 range 0 .. 0;
   end record;

   for APB1_Peripheral_Reset use record
      DAC             at 0 range 29 .. 29;
      Power_Interface at 0 range 28 .. 28;
      CAN2            at 0 range 26 .. 26;
      CAN1            at 0 range 25 .. 25;
      I2C3            at 0 range 23 .. 23;
      I2C2            at 0 range 22 .. 22;
      I2C1            at 0 range 21 .. 21;
      UART5           at 0 range 20 .. 20;
      UART4           at 0 range 19 .. 19;
      UART3           at 0 range 18 .. 18;
      UART2           at 0 range 17 .. 17;
      SPI3            at 0 range 15 .. 15;
      SPI2            at 0 range 14 .. 14;
      Window_Watchdog at 0 range 11 .. 11;
      TIM14           at 0 range  8 .. 8;
      TIM13           at 0 range  7 .. 7;
      TIM12           at 0 range  6 .. 6;
      TIM7            at 0 range  5 .. 5;
      TIM6            at 0 range  4 .. 4;
      TIM5            at 0 range  3 .. 3;
      TIM4            at 0 range  2 .. 2;
      TIM3            at 0 range  1 .. 1;
      TIM2            at 0 range  0 .. 0;
   end record;

   for APB2_Peripheral_Reset use record
      TIM11                           at 0 range 18 .. 18;
      TIM10                           at 0 range 17 .. 17;
      TIM9                            at 0 range 16 .. 16;
      System_Configuration_Controller at 0 range 14 .. 14;
      SPI1                            at 0 range 12 .. 12;
      SDIO                            at 0 range 11 .. 11;
      ADC                             at 0 range  8 .. 8;
      USART6                          at 0 range  5 .. 5;
      USART1                          at 0 range  4 .. 4;
      TIM8                            at 0 range  1 .. 1;
      TIM1                            at 0 range  0 .. 0;
   end record;

   for AHB1_Peripheral_Clock_Enable use record
      USB_OTG_HS_ULPI       at 0 range 30 .. 30;
      USB_OTG_HS            at 0 range 29 .. 29;
      Ethernet_PTP          at 0 range 28 .. 28;
      Ethernet_Reception    at 0 range 27 .. 27;
      Ethernet_Transmission at 0 range 26 .. 26;
      Ethernet_Mac          at 0 range 25 .. 25;
      DMA2                  at 0 range 22 .. 22;
      DMA1                  at 0 range 21 .. 21;
      CCM_Data_Ram          at 0 range 20 .. 20;
      Backup_SRAM           at 0 range 18 .. 18;
      CRC                   at 0 range 12 .. 12;
      IO_Port_I             at 0 range  8 .. 8;
      IO_Port_H             at 0 range  7 .. 7;
      IO_Port_G             at 0 range  6 .. 6;
      IO_Port_F             at 0 range  5 .. 5;
      IO_Port_E             at 0 range  4 .. 4;
      IO_Port_D             at 0 range  3 .. 3;
      IO_Port_C             at 0 range  2 .. 2;
      IO_Port_B             at 0 range  1 .. 1;
      IO_Port_A             at 0 range  0 .. 0;
   end record;

   for AHB2_Peripheral_Clock_Enable use record
      USB_OTG_FS              at 0 range 7 .. 7;
      Random_Number_Generator at 0 range 6 .. 6;
      Hash                    at 0 range 5 .. 5;
      Cryptographic           at 0 range 4 .. 4;
      Camera_Interface        at 0 range 0 .. 0;
   end record;

   for AHB3_Peripheral_Clock_Enable use record
      Flexible_Static_Memory_Controller at 0 range 0 .. 0;
   end record;

   for APB1_Peripheral_Clock_Enable use record
      DAC             at 0 range 29 .. 29;
      Power_Interface at 0 range 28 .. 28;
      CAN2            at 0 range 26 .. 26;
      CAN1            at 0 range 25 .. 25;
      I2C3            at 0 range 23 .. 23;
      I2C2            at 0 range 22 .. 22;
      I2C1            at 0 range 21 .. 21;
      UART5           at 0 range 20 .. 20;
      UART4           at 0 range 19 .. 19;
      UART3           at 0 range 18 .. 18;
      UART2           at 0 range 17 .. 17;
      SPI3            at 0 range 15 .. 15;
      SPI2            at 0 range 14 .. 14;
      Window_Watchdog at 0 range 11 .. 11;
      TIM14           at 0 range  8 .. 8;
      TIM13           at 0 range  7 .. 7;
      TIM12           at 0 range  6 .. 6;
      TIM7            at 0 range  5 .. 5;
      TIM6            at 0 range  4 .. 4;
      TIM5            at 0 range  3 .. 3;
      TIM4            at 0 range  2 .. 2;
      TIM3            at 0 range  1 .. 1;
      TIM2            at 0 range  0 .. 0;
   end record;

   for APB2_Peripheral_Clock_Enable use record
      TIM11                           at 0 range 18 .. 18;
      TIM10                           at 0 range 17 .. 17;
      TIM9                            at 0 range 16 .. 16;
      System_Configuration_Controller at 0 range 14 .. 14;
      SPI1                            at 0 range 12 .. 12;
      SDIO                            at 0 range 11 .. 11;
      ADC3                            at 0 range 10 .. 10;
      ADC2                            at 0 range  9 .. 9;
      ADC1                            at 0 range  8 .. 8;
      USART6                          at 0 range  5 .. 5;
      USART1                          at 0 range  4 .. 4;
      TIM8                            at 0 range  1 .. 1;
      TIM1                            at 0 range  0 .. 0;
   end record;

   for Backup_Domain_Control use record
      Reset                           at 0 range 16 .. 16;
      RTC_Clock                       at 0 range 15 .. 15;
      RTC_Clock_Source                at 0 range  8 .. 9;
      Low_Speed_External_Clock_Bypass at 0 range  2 .. 2;
      Low_Speed_External_Clock_Ready  at 0 range  1 .. 1;
      Low_Speed_External_Clock        at 0 range  0 .. 0;
   end record;

   for Clock_Control_And_Status use record
      Low_Power_Reset                at 0 range 31 .. 31;
      Window_Watchdog_Reset          at 0 range 30 .. 30;
      Independent_Watchdog_Reset     at 0 range 29 .. 29;
      Software_Reset                 at 0 range 28 .. 28;
      POR_PDR_Reset                  at 0 range 27 .. 27;
      PIN_Reset                      at 0 range 26 .. 26;
      BOR_Reset                      at 0 range 25 .. 25;
      Remove_Reset_Flag              at 0 range 24 .. 24;
      Low_Speed_Internal_Clock_Ready at 0 range  1 .. 1;
      Low_Speed_Internal_Clock       at 0 range  0 .. 0;
   end record;

   for Spread_Spectrum_Clock_Generation use record
      Spread_Spectrum_Modulation at 0 range 31 .. 31;
      Spread                     at 0 range 30 .. 30;
      Incrementation_Step        at 0 range 13 .. 27;
      Modulation_Period          at 0 range  0 .. 12;
   end record;

   for PLL_I2S_Configuration use record
      I2S_Clocks_Divider at 0 range 28 .. 30;
      VCO_Multiplier     at 0 range  6 .. 14;
   end record;

   -------------------
   -- RCC Registers --
   -------------------

   --  This section defines the RCC registers that are accessed as record
   --  objects.

   --  Here we use System'To_Address to do the conversion from memory address
   --  constant to a System.Address object, because the usually used
   --  System.Storage_Elements.To_Address function is dynamic.

   Clock_Control_Register : Clock_Control
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + CR_Offset_Address);

   PLL_Configuration_Register : PLL_Configuration
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + PLLCFGR_Offset_Address);

   Clock_Configuration_Register : Clock_Configuration
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + CFGR_Offset_Address);

   Clock_Interrupt_Register : Clock_Interrupt
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + CIR_Offset_Address);

   AHB1_Peripheral_Reset_Register : AHB1_Peripheral_Reset
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + AHB1RSTR_Offset_Address);

   AHB2_Peripheral_Reset_Register : AHB2_Peripheral_Reset
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + AHB2RSTR_Offset_Address);

   AHB3_Peripheral_Reset_Register : AHB3_Peripheral_Reset
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + AHB3RSTR_Offset_Address);

   APB1_Peripheral_Reset_Register : APB1_Peripheral_Reset
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + APB1RSTR_Offset_Address);

   APB2_Peripheral_Reset_Register : APB2_Peripheral_Reset
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + APB2RSTR_Offset_Address);

   AHB1_Peripheral_Clock_Enable_Register : AHB1_Peripheral_Clock_Enable
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + AHB1ENR_Offset_Address);

   AHB2_Peripheral_Clock_Enable_Register : AHB2_Peripheral_Clock_Enable
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + AHB2ENR_Offset_Address);

   AHB3_Peripheral_Clock_Enable_Register : AHB3_Peripheral_Clock_Enable
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + AHB3ENR_Offset_Address);

   APB1_Peripheral_Clock_Enable_Register : APB1_Peripheral_Clock_Enable
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + APB1ENR_Offset_Address);

   APB2_Peripheral_Clock_Enable_Register : APB2_Peripheral_Clock_Enable
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + APB2ENR_Offset_Address);

   AHB1_Peripheral_Clock_In_Sleep_Mode_Register : AHB1_Peripheral_Clock_Enable
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + AHB1LPENR_Offset_Address);

   AHB2_Peripheral_Clock_In_Sleep_Mode_Register : AHB2_Peripheral_Clock_Enable
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + AHB2LPENR_Offset_Address);

   AHB3_Peripheral_Clock_In_Sleep_Mode_Register : AHB3_Peripheral_Clock_Enable
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + AHB3LPENR_Offset_Address);

   APB1_Peripheral_Clock_In_Sleep_Mode_Register : APB1_Peripheral_Clock_Enable
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + APB1LPENR_Offset_Address);

   APB2_Peripheral_Clock_In_Sleep_Mode_Register : APB2_Peripheral_Clock_Enable
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + APB2LPENR_Offset_Address);

   Backup_Domain_Control_Register : Backup_Domain_Control
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + BDCR_Offset_Address);

   Clock_Control_And_Status_Register : Clock_Control_And_Status
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + CSR_Offset_Address);

   Spread_Spectrum_Clock_Generation_Register : Spread_Spectrum_Clock_Generation
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + SSCGR_Offset_Address);

   PLL_I2S_Configuration_Register : PLL_I2S_Configuration
     with Volatile, Address =>
       System'To_Address (RCC_Base_Address + PLLI2SCF_Offset_Address);
end ST.STM32F4.RCC;
