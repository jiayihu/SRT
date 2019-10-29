with Activation_Log_Reader;
with Activation_Manager;
with Event_Queue;
with External_Event_Server;
with On_Call_Producer;
with Regular_Producer;
with Request_Buffer;
with System;

procedure Gee is
   pragma Priority (System.Priority'First);
begin
   loop
      -- delay until Ada.Real_Time.Time_Last;
      null;
   end loop;
end Gee;
