with Ada.Real_Time;

package System_Time is

   function Clock
   return Ada.Real_Time.Time renames Ada.Real_Time.Clock;

   --  Global start time relative to which all periodic events in system
   --  +    will be scheduled.
   System_Start_Time : constant Ada.Real_Time.Time := Clock;


   --  To wait for all tasks to be elaborated and activate them at the
   --  +    same time.
   Task_Activation_Delay : constant Ada.Real_Time.Time_Span
      := Ada.Real_Time.Milliseconds (2000);


end System_Time;
