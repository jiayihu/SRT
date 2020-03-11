with Activation_Log_Reader;
with Activation_Manager;
with Event_Queue;
with External_Event_Server;
with On_Call_Producer;
with Regular_Producer;
with Force_Interrupt;
with System.BB.Time;
with System.BB.Threads; use System.BB.Threads;

procedure Gee is
   pragma Priority (1);
begin
   --  Setting artificial deadline
   Set_Relative_Deadline (System.BB.Time.Time_Span_Last);

   loop
      null;
   end loop;
end Gee;
