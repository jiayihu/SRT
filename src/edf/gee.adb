with Activation_Log_Reader;
with Activation_Manager;
with Event_Queue;
with External_Event_Server;
with On_Call_Producer;
with Regular_Producer;
with Force_Interrupt;
with System.Task_Primitives.Operations;
with System.BB.Time;

procedure Gee is
begin
   System.Task_Primitives.Operations.Set_Relative_Deadline
       (System.Task_Primitives.Operations.Self,
        System.BB.Time.Time_Span_Last);
   loop
      null;
   end loop;
end Gee;
