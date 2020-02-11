with Ada.Text_IO;

package Task_Overhead is
   procedure Start_Tracking;
   procedure End_Tracking (Item : String := "");

   procedure Put_Line (Item : String) renames Ada.Text_IO.Put_Line;
end Task_Overhead;
