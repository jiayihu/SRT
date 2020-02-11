with System;
package Activation_Log_Reader_Parameters is
   Activation_Log_Reader_Priority : constant System.Priority := 3;
   Activation_Log_Reader_Deadline : constant Natural := 1_000; -- in milliseconds
   procedure Activation_Log_Reader_Operation;
end Activation_Log_Reader_Parameters;
