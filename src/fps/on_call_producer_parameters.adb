with Production_Workload;
with Ada.Text_IO;
with Task_Metrics;

package body On_Call_Producer_Parameters is
   procedure On_Call_Producer_Operation (Load : Positive) is
   begin
      -- Task_Metrics.Start_Tracking;
      --  we execute the required amount of excess workload
      Production_Workload.Small_Whetstone (Load);
      -- Task_Metrics.End_Tracking;
      --  then we report nominal completion of current activation
      Ada.Text_IO.Put_Line ("End of sporadic activation.                       ");
   end On_Call_Producer_Operation;
end On_Call_Producer_Parameters;
