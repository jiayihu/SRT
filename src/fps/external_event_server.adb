with Event_Queue;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with System.BB.Time;
with System.BB.Threads; use System.BB.Threads;
with System.BB.Threads.Queues;
with External_Event_Server_Parameters; use External_Event_Server_Parameters;

package body External_Event_Server is
   procedure Wait renames Event_Queue.Handler.Wait;
   task body External_Event_Server is
      --  for tasks to achieve simultaneous activation
      Next_Time : Ada.Real_Time.Time := Activation_Manager.Get_Activation_Time;
   begin
      --  Setting artificial deadline
      Set_Starting_Time (Activation_Manager.Time_Conversion (Next_Time));
      Set_Relative_Deadline (System.BB.Time.Milliseconds (External_Event_Server_Deadline));
      Set_Fake_Number_ID (4);

      delay until Next_Time;
      System.BB.Threads.Queues.Initialize_Task_Table (4, True);
      loop
         --  suspending request for external activation event
         Wait;
         --  non-suspending operation code
         Server_Operation;
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("EES: Something has gone wrong here: " & Exception_Information (Error));
   end External_Event_Server;
end External_Event_Server;
