------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                    S Y S T E M . B B . D E A D L I N E S                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
--                     Copyright (C) 2003-2011, AdaCore                     --
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

--  Package in charge of implementing deadline functionalities

pragma Restrictions (No_Elaboration_Code);

with System.BB.Time;

package System.BB.Deadlines is
   pragma Preelaborate;

   --  Relative Deadline declaration
   --  Any relative deadline can be defined between the shortest deadline
   --  available and the highest defined as time_span'last. Relative_Deadline
   --  represents the length of time intervals for task deadlines, and it is
   --  defined as a 64-bit signed integer as is Time_Span.
   subtype Relative_Deadline  is System.BB.Time.Time_Span;

   --  Default relative deadline value
   Default_Relative_Deadline : constant Relative_Deadline :=
     Relative_Deadline (0);

   --  Type used for unspecified relative deadline
   Unspecified_Relative_Deadline : constant
     System.BB.Deadlines.Relative_Deadline :=
       System.BB.Deadlines.Relative_Deadline (-1);

   --  Absolute Deadline declaration
   --  Any absolute deadline can be defined between the shortest deadline
   --  available and the highest defined as time'last
   subtype Absolute_Deadline  is System.BB.Time.Time;

   --  Default absolute deadline value
   Default_Absolute_Deadline : constant Absolute_Deadline :=
     Absolute_Deadline (0);

end System.BB.Deadlines;
