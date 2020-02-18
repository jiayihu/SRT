with Ada.Synchronous_Task_Control;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Deadline_Miss;
with Task_Overhead;
with System.BB.Time;
with System.BB.Threads; use System.BB.Threads;
with Activation_Log_Reader_Parameters; use Activation_Log_Reader_Parameters;
with System.Tasking.Protected_Objects;

package body Activation_Log_Reader is
   use Ada.Real_Time;

   Local_Suspension_Object : Ada.Synchronous_Task_Control.Suspension_Object;
   procedure Signal is
   begin
      Ada.Synchronous_Task_Control.Set_True (Local_Suspension_Object);
   end Signal;

   procedure Wait is
   begin
      Ada.Synchronous_Task_Control.Suspend_Until_True
        (Local_Suspension_Object);
   end Wait;

   task body Activation_Log_Reader is
      --  for tasks to achieve simultaneous activation
      Next_Time : Ada.Real_Time.Time := Activation_Manager.Get_Activation_Time;
   begin
      --  Setting artificial deadline
      Set_Starting_Time (Activation_Manager.Time_Conversion (Next_Time));
      Set_Relative_Deadline (System.BB.Time.Milliseconds (Activation_Log_Reader_Deadline));


      delay until Next_Time;
      loop
         --  suspending parameterless request of activation event
         Wait;
         Deadline_Miss.Set_Deadline_Handler (Deadline_Miss.ALR, Ada.Real_Time.Clock +
            Ada.Real_Time.Milliseconds (Activation_Log_Reader_Deadline));
         --  Task_Overhead.Start_Tracking;
         --  non-suspending operation code
         Activation_Log_Reader_Operation;
         --  Task_Overhead.End_Tracking;
         Deadline_Miss.Cancel_Deadline_handler (Deadline_Miss.ALR);
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("ALR: Something has gone wrong here: " & Exception_Information (Error));
   end Activation_Log_Reader;

begin
   System.Tasking.Protected_Objects.Initialize_Protection_Deadline
      (System.Tasking.Protected_Objects.Current_Object, 90000000); -- 0.5 * 180Mhz
end Activation_Log_Reader;
