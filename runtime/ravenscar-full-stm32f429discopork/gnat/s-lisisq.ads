-----------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                S Y S T E M . L I B M _ S I N G L E . S Q R T             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2014-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Ada Cert Math specific implementation of sqrt for IEEE 32 bit

with System.Libm_Prefix;
package System.Libm_Single.Squareroot is
   pragma Pure;

   package SLP renames System.Libm_Prefix;

   function Sqrt (X : Float) return Float
     with Export, Convention => C, External_Name => SLP.Prefix & "sqrtf";
   --  The Sqrt function returns the following special values:
   --  C99 special values:
   --    Sqrt (+-0) = +-0
   --    Sqrt (INF) = INF
   --    Sqrt (X)   = NaN, for X < 0.0

end System.Libm_Single.Squareroot;
