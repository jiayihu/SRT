with System;

package Force_Interrupt is
   task Force_Interrupt
     with Priority => System.Priority'Last;
end Force_Interrupt;
