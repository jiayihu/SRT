------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                         S Y S T E M . B B . T I M E                      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2018, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with System.BB.Interrupts;
with System.BB.Board_Support;
with System.BB.Protection;
with System.BB.Parameters;
with System.BB.Threads.Queues;
with System.BB.Timing_Events;
with System.Multiprocessors.Fair_Locks;
with Ada.Unchecked_Conversion;
with System.OS_Interface;
-------------------------------------------------------------
package body System.BB.Time is

   use Board_Support.Time;
   use Parameters;
   use Board_Support.Multiprocessors;
   use System.Multiprocessors;
   use System.Multiprocessors.Fair_Locks;
   use Threads, Threads.Queues;

   package OSI renames System.OS_Interface;
   -----------------------
   -- Local definitions --
   -----------------------

   Alarm_Lock : Fair_Lock := (Spinning => (others => False),
                              Lock     => (Flag   => 0));
   --  Used to protect access to shared alarm resources
   --  (Timer configuration and Pending_Alarm variable)

   subtype Clock_Interval is Board_Support.Time.Timer_Interval;

   type Clock_Periods is mod 2 ** 32;
   for Clock_Periods'Size use 32;

   function "&" (Left : Clock_Periods; Right : Clock_Interval) return Time is
     (Time (Left) * (Time (Max_Timer_Interval) + Time (1)) + Time (Right));
   --  Combine MSP and LSP of clock to form time

   Update_In_Progress : constant Clock_Periods := 0;
   Periods_In_Epoch   : constant Clock_Periods := 1;
   --  Special value to signify Last_Clock_Update is going on, so on
   --  multiprocessor systems can avoid race conditions during updates.
   --  Choose 0, and have epoch start at 1, so Unsynchronized_Clock can
   --  ignore updates and just return an early time instead.

   type Composite_Time is record
      MSP : Clock_Periods  := Periods_In_Epoch;
      pragma Atomic (MSP);
      LSP : Clock_Interval := 0;
      pragma Atomic (LSP);
   end record;
   --  Time representation used for the software clock, allowing concurrent
   --  updates and reads, see Update_Clock.
   --
   --  Include a default expression for component LSP, even when not needed, to
   --  prevent the need for elaboration code to initialize default-initialized
   --  objects of this type (note that this package has a restriction
   --  No_Elaboration_Code).

   Software_Clock : Composite_Time;
   --  Clock with same time-base as hardware clock, but allowing a larger
   --  range. This is always behind the actual time by less than one hardware
   --  clock period. See Update_Clock for read and update protocol.

   Pending_Alarm : Time := Time'Last;
   --  Time of the current alarm handled by the timer. Used to determine if a
   --  given alarm is before the current one, and so needs to re-configure the
   --  timer.

   Max_Sleep : Time := 0;
   --  The longest time we can sleep without updating the Software_Clock.
   --  Initialized by Initialize_Timers.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID);
   --  Handler for the alarm interrupt

   procedure Update_Clock (Now : out Time);
   --  This procedure has to be executed at least once each period of the
   --  hardware clock. We also require that this procedure be called with
   --  interrupts disabled, to ensure no stale values will be written. Given
   --  that limitation, it is fine to do concurrent updates on SMP systems:
   --  no matter which update ultimately prevails, it can't be old. While, on
   --  SMP systems, the Period_Counter may not always be monotone, the time
   --  returned by Update_Clock and Clock is.

   -------------------
   -- Alarm_Handler --
   -------------------

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID) is

      Now             : Time;
      Next_Alarm      : Time; -- Time

   begin
      --  Make sure there is an alarm pending.

      pragma Assert (Pending_Alarm /= Time'Last
             and then Interrupt = -1);

      Board_Support.Time.Clear_Alarm_Interrupt;

      --  The access to the queues must be protected

      Protection.Enter_Kernel;

      --  Reset Pending_Alarm before computing the next alarm time, as other
      --  processors may set alarms concurrently, and these alarms would be
      --  ignored otherwise. The alarm lock must be held for this.

      if Multiprocessor then
         Lock (Alarm_Lock);
         Pending_Alarm := Time'Last;
         Unlock (Alarm_Lock);

      --  No need for lock if not on multiprocessor

      else
         Pending_Alarm := Time'Last;
      end if;

      Update_Clock (Now);

      --  Ensure alarms will keep going to keep the software clock up-to-date.

      Next_Alarm := Now + Max_Sleep;

      --  Multiprocessor case special processing

      if Parameters.Multiprocessor then

         --  This is the alarm CPU, we have to wake up the other CPUs with
         --  expired alarms.

         for CPU_Id in CPU loop

            if CPU_Id /= Current_CPU then
               declare
                  Alarm_Time : constant Time := Get_Next_Timeout (CPU_Id);

               begin
                  if Alarm_Time <= Now then

                     --  Alarm expired, wake up the CPU

                     Board_Support.Multiprocessors.Poke_CPU (CPU_Id);

                  else
                     --  Check if this is the next non-expired alarm of the
                     --  overall system.

                     if Alarm_Time < Next_Alarm then
                        Next_Alarm := Alarm_Time;
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end if;

      Update_Alarm (Next_Alarm);
      --  Execute expired events of the current CPU

      Timing_Events.Execute_Expired_Timing_Events (Now);

      --  Wake up our alarms, and set any new alarm

      Wakeup_Expired_Alarms (Now);

      --  Next_Alarm := Time'Min (Get_Next_Timeout (Current_CPU), Next_Alarm);
      --  Update_Alarm (Next_Alarm);

      Protection.Leave_Kernel;
   end Alarm_Handler;

   -----------
   -- Clock --
   -----------

   function Clock return Time is
      First_MSP  : Clock_Periods;
      Before_MSP : Clock_Periods;
      Before_LSP : Clock_Interval;
      Now_LSP    : Clock_Interval;
      After_MSP  : Clock_Periods;

   begin
      --  Reading the clock needs to access to the software and the hardware
      --  clock. In a multiprocessor, masking interrupts is not enough because
      --  the software clock can be updated by another processor. Therefore, we
      --  keep reading until we get a consistent value (no updates of the
      --  software MSP while we read the hardware clock).

      --  We can limit the iterations in the loop to 3. In the worst case, if
      --  the MSP keeps increasing within the loop, it means that we are
      --  spending an extremely long time in this function (we get preempted
      --  all the time). If the first time we read 1, and then the MSP gets
      --  increased, we know that the time is between 1 & X and 2 & X (because
      --  the software clock can be behind the actual time by at most one
      --  hardware clock period). It means that the actual time when we entered
      --  this function was before 3 & 0. In the second iteration we can read
      --  2 and then get increased again. Hence actual time is between 2 & X
      --  and 3 & X. Hence, the actual time when we leave function clock is at
      --  least 2 & 0. However, we do not know when between 2 & 0 and 3 & 0.
      --  Hence we read a third time, and if we read 3 and then a change, it
      --  means that the actual time is between 3 & X and 4 & X (so at least
      --  3 & 0). Hence, at the end of the third iteration, we can return 3 & 0
      --  as a safe value that is between the beginning and end of the
      --  execution of this call to Clock.

      for Iteration in 1 .. 3 loop

         --  On multiprocessor systems there may be a concurrent update of the
         --  software clock (signaled with Update_In_Progress). Retry if this
         --  happens. On monoprocessors the loop is performed only once.

         loop
            Before_MSP := Software_Clock.MSP;

            exit when not Multiprocessor
              or else Before_MSP /= Update_In_Progress;
         end loop;

         --  After the loop, Before_MSP cannot be equal to Update_In_Progress.
         --  In the case of multiprocessors because of the exit condition, and
         --  in the case of monoprocessors because the update is done
         --  atomically.

         Before_LSP := Software_Clock.LSP;

         Now_LSP := Clock_Interval (Read_Clock);

         After_MSP := Software_Clock.MSP;

         --  If the MSP in Software_Clock has changed (or is changing), we
         --  do not know the time at which the software clock was updated. It
         --  is important to note that the implementation does not force the
         --  software clock to be updated at a time close to the LSP wraparound
         --  (it needs to be done at least once per hardware clock period, but
         --  we do not know when). Hence, returning (Before_MSP + 1) & 0 is
         --  not correct because the updated LSP in the Software_Clock does
         --  not need to be close to zero.

         --  Normal case, no updates in MSP

         if Before_MSP = After_MSP then

            --  If we know the value of the software clock at the time of the
            --  read of the hardware clock, we know the time of that read,
            --  because the software clock can never be more than one period
            --  behind. Hence, we build a Time value from two consecutive
            --  readings of the hardware clock (Before_LSP and Now_LSP) and one
            --  reading of the MSP from the Software_Clock (and we know that
            --  the MSP did not change between the two readings of Before_LSP
            --  and Now_LSP).

            return
              Before_MSP + (if Now_LSP < Before_LSP then 1 else 0) & Now_LSP;

         --  After the first unsuccessful iteration we store the first MSP
         --  value read to have a reference of the initial time when we entered
         --  the clock function (before First_MSP + 2 & 0).

         elsif Iteration = 1 then
            First_MSP := Before_MSP;

         --  During the second or third iteration, if the clock has been
         --  increased by two or more then Before_MSP & 0 is certainly within
         --  the beginning and end of the execution of this call to Clock.

         elsif Before_MSP - First_MSP >= 2 then
            exit;
         end if;
      end loop;

      pragma Assert (Before_MSP - First_MSP >= 2);

      return Before_MSP & 0;
   end Clock;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Now               : Time;
      Self              : Threads.Thread_Id;
      Inserted_As_First : Boolean;

   begin

      Now := Clock;

      Protection.Enter_Kernel;

      Self := Thread_Self;

      pragma Assert (Self.State = Runnable);

      System.BB.Threads.Queues.Add_Execution (Self.Fake_Number_ID);

      --  Test if the alarm time is in the future

      if Self.Active_Absolute_Deadline < Now then
            --  Se necessario si aumentano le deadline miss
         if System.BB.Threads.Queues.Get_Check (Self.Fake_Number_ID) = False
         then
            System.BB.Threads.Queues.Set_Check (Self.Fake_Number_ID, True);
            System.BB.Threads.Queues.Add_DM (Self.Fake_Number_ID);
         end if;
      end if;

      if T + System.BB.Threads.Queues.Global_Interrupt_Delay > Now then

         --  Extract the thread from the ready queue. When a thread wants to
         --  wait for an alarm it becomes blocked.

         Self.State := Delayed;

         Self.Preemption_Needed := False;
         Extract (Self);

         --  Insert Thread_Id in the alarm queue (ordered by time) and if it
         --  was inserted at head then check if Alarm Time is closer than the
         --  next clock interrupt.

         Insert_Alarm (T, Self, Inserted_As_First);

         if Inserted_As_First then
            --   Update_Alarm (Get_Next_Timeout (Current_CPU));
            Update_Alarm (T);
         end if;

         System.BB.Threads.Queues.Set_Check (Self.Fake_Number_ID, False);
      else
         --  If alarm time is not in the future, the thread must yield the CPU
         Threads.Queues.Change_Absolute_Deadline
           (Self, Self.Active_Absolute_Deadline + Self.Active_Period);
         System.BB.Threads.Queues.Set_Check (Self.Fake_Number_ID, False);
         Yield (Self);
      end if;
      Protection.Leave_Kernel;
   end Delay_Until;

   -----------
   -- Epoch --
   -----------

   function Epoch return Time is
   begin
      return Periods_In_Epoch & 0;
   end Epoch;

   ----------------------
   -- Get_Next_Timeout --
   ----------------------

   function Get_Next_Timeout (CPU_Id : CPU) return Time is
      Alarm_Time : constant Time := Get_Next_Alarm_Time (CPU_Id);
      Event_Time : constant Time := Timing_Events.Get_Next_Timeout (CPU_Id);
   begin
      return Time'Min (Alarm_Time, Event_Time);
   end Get_Next_Timeout;

   -----------------------
   -- Initialize_Timers --
   -----------------------

   procedure Initialize_Timers is
   begin
      --  There may never be more than Max_Timer_Interval clocks between
      --  updates of Software_Clock, or we lose track of time. Allow a 1/8th
      --  period safety for early wakeup. The alarm CPU should never have
      --  alarm interrupts disabled for longer than this, or we may miss
      --  clock updates.

      Max_Sleep := Time (Max_Timer_Interval / 8 * 7);

      --  Install alarm handler

      Board_Support.Time.Install_Alarm_Handler (Alarm_Handler'Access);

      --  It is important to initialize the software LSP with the value coming
      --  from the hardware. There is no guarantee that this hardware value is
      --  close to zero (it may have been initialized by monitor software with
      --  any value and at any moment in time). With this initialization we
      --  ensure that the first alarm is not too far (we need to ensure that
      --  the value in the software LSP is less than a period away from the
      --  actual value in hardware).

      Software_Clock.LSP := Clock_Interval (Read_Clock);

      --  Establish invariant that there always is a pending alarm at most
      --  Max_Sleep time in the future.

      Pending_Alarm := Clock + Max_Sleep;
      Board_Support.Time.Set_Alarm (Clock_Interval (Max_Sleep));
   end Initialize_Timers;

   -------------------
   --  Update_Alarm --
   -------------------

   procedure Update_Alarm (Alarm : Time) is
      Now             : constant Time := Clock;
      Time_Difference : Time;

   begin
      --  On multiprocessors we want to do the entire procedure while holding
      --  the alarm lock, as we shouldn't read or update the Pending_Alarm
      --  variable, or program the alarm, concurrently with another update.

      if Parameters.Multiprocessor then
         Lock (Alarm_Lock);
      end if;

      if Alarm <= Now then

         --  If alarm is in the past, set the minimum timer value so the
         --  interrupt will be triggered as soon as possible.

         Time_Difference := 1;

      else
         Time_Difference := Alarm - Now;
      end if;

      Time_Difference := Time'Min (Time_Difference, Max_Sleep);

      --  If next alarm time is closer than the currently pending alarm,
      --  reprogram the alarm.

      if Alarm < Pending_Alarm then
         pragma Assert (Time_Difference in 1 .. Max_Sleep);

         Board_Support.Time.Set_Alarm (Clock_Interval (Time_Difference));
         Pending_Alarm := Alarm;
      end if;

      if Parameters.Multiprocessor then
         Unlock (Alarm_Lock);
      end if;
   end Update_Alarm;

   ------------------
   -- Update_Clock --
   ------------------

   --  Must be called from within Kernel (interrupts disabled). Must only be
   --  called from one processor at a time.

   procedure Update_Clock (Now : out Time) is
      Update_MSP : constant Clock_Periods := Software_Clock.MSP;
      Update_LSP : constant Clock_Interval := Software_Clock.LSP;
      Now_LSP    : constant Clock_Interval := Clock_Interval (Read_Clock);
      Now_MSP    : Clock_Periods;

   begin
      if Now_LSP < Update_LSP then
         Now_MSP := Update_MSP + 1;

         --  Need to do "atomic" update of both parts of the clock

         --  Mark Software_Clock.MSP as invalid during updates. The read
         --  protocol is to read Software_Clock.MSP both before and after
         --  reading Software_Clock.LSP. Only consider the MSP as that
         --  belonging to the LSP if both values are the same and not equal
         --  to the special Update_In_Progress value.

         --  Because interrupts are disabled, this special read protocol is
         --  only necessary on multiprocessor systems.

         Software_Clock.MSP := Update_In_Progress;
         Software_Clock.LSP := Now_LSP;
         Software_Clock.MSP := Now_MSP;

      else
         Now_MSP := Update_MSP;

         --  Only need to change the LSP, so we can do this atomically

         Software_Clock.LSP := Now_LSP;
      end if;

      Now := Now_MSP & Now_LSP;
   end Update_Clock;

   -----------------------
   -- Local definitions --
   -----------------------

   subtype LLI is Long_Long_Integer;
   type Uint_64 is mod 2 ** 64;
   --  subtype Uint_64 is System.BB.Time.Time;
   --  Type used to represent intermediate results of arithmetic
   --  operations
   Max_Pos_Time_Span : constant := Uint_64 (Time_Span_Last);
   Max_Neg_Time_Span : constant := Uint_64 (2 ** 63);

   function Mul_Div (V : LLI; M : Natural; D : Positive) return LLI;
   function Rounded_Div (L, R : LLI) return LLI;

   ---------
   -- "*" --
   ---------

   function "*" (Left : Time_Span; Right : Integer) return Time_Span is
      Is_Negative : constant Boolean :=
        (if Left > 0 then
            Right < 0
         elsif Left < 0
            then Right > 0
         else
            False);
      --  Sign of the result

      Max_Value : constant Uint_64 :=
        (if Is_Negative then
            Max_Neg_Time_Span
         else
            Max_Pos_Time_Span);
      --  Maximum absolute value that can be returned by the multiplication
      --  taking into account the sign of the operators.

      Abs_Left : constant Uint_64 :=
        (if Left = Time_Span_First then
            Max_Neg_Time_Span
         else
            Uint_64 (abs (Left)));
      --  Remove sign of left operator

      Abs_Right : constant Uint_64 := Uint_64 (abs (LLI (Right)));
      --  Remove sign of right operator

   begin
      --  Overflow check is performed by hand assuming that Time_Span is a
      --  64-bit signed integer. Otherwise these checks would need an
      --  intermediate type with more than 64-bit. The sign of the operators
      --  is removed to simplify the intermediate computation of the overflow
      --  check.

      if Abs_Right /= 0 and then Max_Value / Abs_Right < Abs_Left then
         raise Constraint_Error;
      else
         return Left * Time_Span (Right);
      end if;
   end "*";

   function "*" (Left : Integer; Right : Time_Span) return Time_Span is
   begin
      return Right * Left;
   end "*";

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Time_Span) return Time is
   begin
      --  Overflow checks are performed by hand assuming that Time and
      --  Time_Span are 64-bit unsigned and signed integers respectively.
      --  Otherwise these checks would need an intermediate type with more
      --  than 64 bits.

      if Right >= 0
        and then Uint_64 (Time_Last) - Uint_64 (Left) >= Uint_64 (Right)
      then
         return Time (Uint_64 (Left) + Uint_64 (Right));

      --  The case of Right = Time_Span'First needs to be treated differently
      --  because the absolute value of -2 ** 63 is not within the range of
      --  Time_Span.

      elsif Right = Time_Span'First and then Left >= Max_Neg_Time_Span then
         return Time (Uint_64 (Left) - Max_Neg_Time_Span);

      elsif Right < 0 and then Right > Time_Span'First
        and then Left >= Time (abs (Right))
      then
         return Time (Uint_64 (Left) - Uint_64 (abs (Right)));

      else
         raise Constraint_Error;
      end if;
   end "+";

   function "+" (Left : Time_Span; Right : Time) return Time is
   begin
      --  Overflow checks must be performed by hand assuming that Time and
      --  Time_Span are 64-bit unsigned and signed integers respectively.
      --  Otherwise these checks would need an intermediate type with more
      --  than 64-bit.

      if Left >= 0
        and then Uint_64 (Time_Last) - Uint_64 (Right) >= Uint_64 (Left)
      then
         return Time (Uint_64 (Left) + Uint_64 (Right));

      elsif Left = Time_Span'First and then Right >= Max_Neg_Time_Span then
         return Time (Uint_64 (Right) - Max_Neg_Time_Span);

      elsif Left < 0 and then Left > Time_Span'First
        and then Right >= Time (abs (Left))
      then
         return Time (Uint_64 (Right) - Uint_64 (abs (Left)));

      else
         raise Constraint_Error;
      end if;
   end "+";

   function "+" (Left, Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (LLI (Left) + LLI (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Time_Span) return Time is
   begin
      --  Overflow checks must be performed by hand assuming that Time and
      --  Time_Span are 64-bit unsigned and signed integers respectively.
      --  Otherwise these checks would need an intermediate type with more
      --  than 64-bit.

      if Right >= 0 and then Left >= Time (Right) then
         return Time (Uint_64 (Left) - Uint_64 (Right));

      --  The case of Right = Time_Span'First needs to be treated differently
      --  because the absolute value of -2 ** 63 is not within the range of
      --  Time_Span.

      elsif Right = Time_Span'First
        and then Uint_64 (Time_Last) - Uint_64 (Left) >= Max_Neg_Time_Span
      then
         return Left + Time (Max_Neg_Time_Span);

      elsif Right < 0 and then Right > Time_Span'First
        and then Uint_64 (Time_Last) - Uint_64 (Left) >= Uint_64 (abs (Right))
      then
         return Left + Time (abs (Right));

      else
         raise Constraint_Error;
      end if;
   end "-";

   function "-" (Left, Right : Time) return Time_Span is
   begin
      --  Overflow checks must be performed by hand assuming that Time and
      --  Time_Span are 64-bit unsigned and signed integers respectively.
      --  Otherwise these checks would need an intermediate type with more
      --  than 64-bit.

      if Left >= Right
        and then Uint_64 (Left) - Uint_64 (Right) <= Max_Pos_Time_Span
      then
         return Time_Span (Uint_64 (Left) - Uint_64 (Right));

      elsif Left < Right
        and then Uint_64 (Right) - Uint_64 (Left) <= Max_Neg_Time_Span
      then
         return -1 - Time_Span (Uint_64 (Right) - Uint_64 (Left) - 1);

      else
         raise Constraint_Error;
      end if;
   end "-";

   function "-" (Left, Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (LLI (Left) - LLI (Right));
   end "-";

   function "-" (Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (-LLI (Right));
   end "-";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Time_Span) return Integer is
      pragma Unsuppress (Overflow_Check);
      pragma Unsuppress (Division_Check);
   begin
      return Integer (LLI (Left) / LLI (Right));
   end "/";

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
      pragma Unsuppress (Overflow_Check);
      pragma Unsuppress (Division_Check);
   begin
      return Left / Time_Span (Right);
   end "/";

   ------------------
   -- Microseconds --
   ------------------

   function Microseconds (US : Integer) return Time_Span is
   begin
      --  Overflow can't happen (Ticks_Per_Second is Natural)

      return
        Time_Span (Rounded_Div (LLI (US) * LLI (OSI.Ticks_Per_Second), 1E6));
   end Microseconds;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (MS : Integer) return Time_Span is
   begin
      --  Overflow can't happen (Ticks_Per_Second is Natural)

      return
        Time_Span (Rounded_Div (LLI (MS) * LLI (OSI.Ticks_Per_Second), 1E3));
   end Milliseconds;

   -------------
   -- Mul_Div --
   -------------

   function Mul_Div (V : LLI; M : Natural; D : Positive) return LLI is

      --  We first multiply V * M and then divide the result by D, while
      --  avoiding overflow in intermediate calculations and detecting it in
      --  the final result. To get the rounding to the nearest integer, away
      --  from zero if exactly halfway between two values, we add +/- D/2
      --  (depending on the sign on V) directly at the end of multiplication.
      --
      --  ----------------------------------------
      --  Multiplication (and rounding adjustment)
      --  ----------------------------------------
      --
      --  Since V is a signed 64-bit integer and M is signed (but non-negative)
      --  32-bit integer, their product may not fit in 64-bits. To avoid
      --  overflow we split V and into high and low parts
      --
      --    V_Hi = V  /  2 ** 32
      --    V_Lo = V rem 2 ** 32
      --
      --  where each part is either zero or has the sign of the dividend; thus
      --
      --    V = V_Hi * 2 ** 32 + V_Lo
      --
      --  In either case V_Hi and V_Lo are in range of 32-bit signed integer,
      --  yet stored in 64-bit signed variables. When multiplied by M, which is
      --  in range of 0 .. 2 ** 31 - 1, the results will still fit in 64-bit
      --  integer, even if we extend it by D/2 as required to implement
      --  rounding. We will get the value of V * M ± D/2 as low and high part:
      --
      --    (V * M ± D/2)_Lo = (V_Lo * M ± D/2) with carry zeroed
      --    (V * M ± D/2)_Hi = (V_Hi * M) with carry from (V_Lo * M ± D/2)
      --
      --  (carry flows only from low to high part), or mathematically speaking:
      --
      --    (V * M ± D/2)_Lo = (V * M ± D/2) rem 2 ** 32
      --    (V * M ± D/2)_Hi = (V * M ± D/2)  /  2 ** 32
      --
      --  and thus
      --
      --    V * M ± D/2 = (V * M ± D/2)_Hi * 2 ** 32 + (V * M ± D/2)_Lo
      --
      --  with signs just like described for V_Hi and V_Lo.
      --
      --  --------
      --  Division
      --  --------
      --
      --  The final result (V * M ± D/2) / D is computed as a high and low
      --  parts:
      --
      --    ((V * M ± D/2) / D)_Hi = (V * M ± D/2)_Hi / D
      --    ((V * M ± D/2) / D)_Lo =
      --        ((V * M ± D/2)_Lo + remainder from high part division) / D
      --
      --  (remainder flows only from high to low part, opposite to carry),
      --  or mathematically speaking:
      --
      --    ((V * M ± D/2) / D)_Hi = ((V * M ± D/2) / D)  /  2 ** 32
      --    ((V * M ± D/2) / D)_Lo = ((V * M ± D/2) / D) rem 2 ** 32
      --
      --  and thus
      --
      --    (V * M ± D/2) / D = ((V * M ± D/2) / D)_Hi * 2 ** 32
      --                      + ((V * M ± D/2) / D)_Lo
      --
      --  with signs just like described for V_Hi and V_Lo.
      --
      --  References: this calculation is partly inspired by Knuth's algorithm
      --  in TAoCP Vol.2, section 4.3.1, excercise 16. However, here it is
      --  adapted it for signed arithmetic; has no loop (since the input number
      --  has fixed width); and discard the remainder of the result.

      V_Hi : constant LLI := V  /  2 ** 32;
      V_Lo : constant LLI := V rem 2 ** 32;
      --  High and low parts of V

      V_M_Hi : LLI;
      V_M_Lo : LLI;
      --  High and low parts of V * M (+-) D / 2

      Result_Hi : LLI;
      --  High part of the result

      Result_Lo : LLI;
      --  Low part of the result

      Remainder : LLI;
      --  Remainder of the first division

   begin
      --  Multiply V * M and add/subtract D/2

      V_M_Lo := V_Lo * LLI (M) + (if V >= 0 then 1 else -1) * LLI (D / 2);
      V_M_Hi := V_Hi * LLI (M) + V_M_Lo / 2 ** 32;
      V_M_Lo := V_M_Lo rem 2 ** 32;

      --  First quotient

      Result_Hi := V_M_Hi / LLI (D);

      --  The final result would overflow

      if Result_Hi not in -(2 ** 31) .. 2 ** 31 - 1 then
         raise Constraint_Error;
      end if;

      Remainder := V_M_Hi rem LLI (D);
      Result_Hi := Result_Hi * 2 ** 32;

      --  Second quotient

      Result_Lo := (V_M_Lo + Remainder * 2 ** 32) / LLI (D);

      --  Combine low and high parts of the result

      return Result_Hi + Result_Lo;
   end Mul_Div;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (NS : Integer) return Time_Span is
   begin
      --  Overflow can't happen (Ticks_Per_Second is Natural)

      return
        Time_Span (Rounded_Div (LLI (NS) * LLI (OSI.Ticks_Per_Second), 1E9));
   end Nanoseconds;

   -----------------
   -- Rounded_Div --
   -----------------

   function Rounded_Div (L, R : LLI) return LLI is
      Left : LLI;
   begin
      if L >= 0 then
         Left := L + R / 2;
      else
         Left := L - R / 2;
      end if;

      return Left / R;
   end Rounded_Div;

   -------------------------------------------------------------
   -------------------------------------------------------------
   --  DEBUG

   function To_Duration is
     new Ada.Unchecked_Conversion (Long_Long_Integer, Duration);

   function To_Integer is
     new Ada.Unchecked_Conversion (Duration, Long_Long_Integer);

   function To_Integer is
     new Ada.Unchecked_Conversion (Time_Span, LLI);

   Duration_Units : constant Positive := Positive (1.0 / Duration'Small);
   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : Time_Span) return Duration is
   begin
      return
        To_Duration
          (Mul_Div (To_Integer (TS), Duration_Units, OSI.Ticks_Per_Second));
   end To_Duration;

   function To_Time_Span (D : Duration) return Time_Span is
   begin
      return
        Time_Span
          (Mul_Div (To_Integer (D), OSI.Ticks_Per_Second, Duration_Units));
   end To_Time_Span;

   -------------------------------------------------------------
   -------------------------------------------------------------

end System.BB.Time;
