-- ***************************************************************************
--            BWT Index-Based Package Specification
--
--           Copyright (C) 2026 By Ulrik Hørlyk Hjort
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-- ***************************************************************************

with Ada.Streams; use Ada.Streams;

package BWT_Index is

   BWT_Error : exception;

   -- Apply Burrows-Wheeler Transform (index-based).
   -- Groups similar characters together to improve compressibility.
   -- Returns transformed data and primary index (position of original).
   procedure Transform
     (Input         : in  Stream_Element_Array;
      Output        : out Stream_Element_Array;
      Primary_Index : out Stream_Element_Offset;
      Last          : out Stream_Element_Offset);

   -- Inverse Burrows-Wheeler Transform (index-based).
   -- Reconstructs original data from BWT output and primary index.
   procedure Inverse_Transform
     (Input         : in  Stream_Element_Array;
      Primary_Index : in  Stream_Element_Offset;
      Output        : out Stream_Element_Array;
      Last          : out Stream_Element_Offset);

end BWT_Index;
