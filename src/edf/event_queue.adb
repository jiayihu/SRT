with ST;              use ST;
with ST.EXTI; use ST.EXTI;
with System.Tasking.Protected_Objects;

package body Event_Queue is
   Button_Line : constant Interrupt_Line := 0;
   --  The User Button is connected to EXTI0 (aka Line 0)

   protected body Handler is
      procedure Signal is
      begin
         --  Clear the raised interrupt by writing "Occurred" to the correct
         --  position in the EXTI Pending Register.
         EXTI.Interrupt_Pending_Register.Line :=
           (Button_Line => Occurred, others => Not_Occurred);

         Barrier := True;
      end Signal;
      entry Wait when Barrier is
      begin
         Barrier := False;
      end Wait;
   end Handler;
begin
   --  Initialization code for the Button package. This will be executed before
   --  the tasks are run and enables the interrupt raised by the User Button
   --  to be received by the STM32F4's interrupt unit.

   EXTI.Interrupt_Mask_Register.Line (Button_Line) := Not_Masked;
   EXTI.Rising_Trigger_Selection_Register.Line (Button_Line) := Enabled;

   System.Tasking.Protected_Objects.Initialize_Protection_Deadline
     (System.Tasking.Protected_Objects.Current_Object, 18000000); -- 0.1 * 180Mhz
end Event_Queue;
