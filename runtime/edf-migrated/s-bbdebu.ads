------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                         S Y S T E M . B B . D E B U G                    --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2012, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

package System.BB.Debug is
   pragma Preelaborate;

   General : constant Boolean := False;
   Queues  : constant Boolean := False;
   Delays  : constant Boolean := False;
   Clocks  : constant Boolean := False;
   Bench   : constant Boolean := True;

   Debug_Inte  : Boolean := General;
   Debug_Hand  : Boolean := General;
   Debug_Prot  : Boolean := General;
   Debug_Thre  : Boolean := General;
   Debug_Prob  : Boolean := General;
   Debug_Prop  : Boolean := General;
   Debug_Rest  : Boolean := General;
   Debug_Event : Boolean := General;
   Debug_Soft  : Boolean := General;
   Debug_POSE  : Boolean := General;

   Debug_Thqu  : Boolean := General;
   Debug_Timer : Boolean := General;
   Debug_Abs   : Boolean := General;

   Debug_Tasks : Boolean := General;
   Debug_Handler  : Boolean := General;

   Debug_Time  : Boolean := General;

   Debug_Add   : Boolean := Delays;
   Debug_Delay : Boolean := Delays;

   Debug_Delay_Time : Boolean := Clocks;
   Debug_Wakeup_Time : Boolean := Clocks;

   Debug_Insert : Boolean := Queues;
   Debug_Abs_Dead : Boolean := Clocks;

   Debug_Queue : Boolean := Queues;
   Debug_Clock : Boolean := Clocks;

   Print_Miss  : Boolean := Bench;
   Print_Preem : Boolean := Bench;

end System.BB.Debug;
