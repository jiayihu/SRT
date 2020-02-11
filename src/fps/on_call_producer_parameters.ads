with System;
package On_Call_Producer_Parameters is
   On_Call_Producer_Priority : constant System.Priority := 5;
   On_Call_Producer_Deadline : constant Natural := 800; -- in milliseconds
   procedure On_Call_Producer_Operation (Load : Positive);
end On_Call_Producer_Parameters;
