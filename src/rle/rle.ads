-- ***************************************************************************
--             RLE Compression Package Specification
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

package RLE is

   Compression_Error : exception;

   -- Compress data using Run-Length Encoding.
   -- Replaces runs of identical bytes with (count, byte) pairs.
   procedure Compress
     (Input  : in  Stream_Element_Array;
      Output : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   -- Decompress RLE-encoded data.
   -- Expands (count, byte) pairs back to original byte sequences.
   procedure Decompress
     (Input  : in  Stream_Element_Array;
      Output : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   -- Estimate maximum output buffer size needed for compression.
   -- Worst case: every byte is different (2x expansion plus header).
   function Estimate_Compressed_Size (Input_Size : Stream_Element_Offset) return Stream_Element_Offset;

end RLE;
