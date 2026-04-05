-- ***************************************************************************
--                RLE Compression Package Body
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

with Ada.Unchecked_Conversion;

package body RLE is

   -- File format:
   -- [4 bytes] Magic: "RLE1"
   -- [4 bytes] Original size (big-endian)
   -- [N bytes] Encoded data:
   --   If byte < 128: literal byte value
   --   If byte >= 128: run marker, next byte is value, count = (byte - 127)

   Magic : constant String := "RLE1";

   type Byte is mod 256;
   
   function To_Stream_Element is new Ada.Unchecked_Conversion
     (Source => Byte, Target => Stream_Element);
   
   function To_Byte is new Ada.Unchecked_Conversion
     (Source => Stream_Element, Target => Byte);

   -- Write 4-byte big-endian integer to output
   procedure Write_U32
     (Output : in out Stream_Element_Array;
      Pos    : in out Stream_Element_Offset;
      Value  : Stream_Element_Offset) is
   begin
      Output (Pos)     := Stream_Element ((Value / 16777216) mod 256);
      Output (Pos + 1) := Stream_Element ((Value / 65536) mod 256);
      Output (Pos + 2) := Stream_Element ((Value / 256) mod 256);
      Output (Pos + 3) := Stream_Element (Value mod 256);
      Pos := Pos + 4;
   end Write_U32;

   -- Read 4-byte big-endian integer from input
   function Read_U32
     (Input : Stream_Element_Array;
      Pos   : Stream_Element_Offset) return Stream_Element_Offset is
   begin
      return Stream_Element_Offset (Input (Pos)) * 16777216 +
             Stream_Element_Offset (Input (Pos + 1)) * 65536 +
             Stream_Element_Offset (Input (Pos + 2)) * 256 +
             Stream_Element_Offset (Input (Pos + 3));
   end Read_U32;

   procedure Compress
     (Input  : in  Stream_Element_Array;
      Output : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Out_Pos      : Stream_Element_Offset := Output'First;
      In_Pos       : Stream_Element_Offset := Input'First;
      Run_Start    : Stream_Element_Offset;
      Run_Length   : Natural;
      Current_Byte : Stream_Element;
   begin
      -- Write header
      for I in Magic'Range loop
         Output (Out_Pos) := Character'Pos (Magic (I));
         Out_Pos := Out_Pos + 1;
      end loop;
      
      Write_U32 (Output, Out_Pos, Input'Length);

      -- Empty input case
      if Input'Length = 0 then
         Last := Out_Pos - 1;
         return;
      end if;

      -- Encode runs
      while In_Pos <= Input'Last loop
         Current_Byte := Input (In_Pos);
         Run_Start := In_Pos;
         Run_Length := 1;

         -- Count consecutive identical bytes (max 128 for encoding)
         while In_Pos < Input'Last and then
               Input (In_Pos + 1) = Current_Byte and then
               Run_Length < 128
         loop
            In_Pos := In_Pos + 1;
            Run_Length := Run_Length + 1;
         end loop;

         In_Pos := In_Pos + 1;

         -- Encode the run
         if Run_Length >= 3 then
            -- Run encoding: marker (128 + run_length), value
            Output (Out_Pos) := Stream_Element (127 + Run_Length);
            Output (Out_Pos + 1) := Current_Byte;
            Out_Pos := Out_Pos + 2;
         else
            -- Literal encoding for short runs
            for I in 1 .. Run_Length loop
               if Current_Byte >= 128 then
                  -- Value >= 128 needs special handling (looks like marker)
                  Output (Out_Pos) := 128;  -- Run of length 1
                  Output (Out_Pos + 1) := Current_Byte;
                  Out_Pos := Out_Pos + 2;
               else
                  Output (Out_Pos) := Current_Byte;
                  Out_Pos := Out_Pos + 1;
               end if;
            end loop;
         end if;
      end loop;

      Last := Out_Pos - 1;
   end Compress;

   procedure Decompress
     (Input  : in  Stream_Element_Array;
      Output : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      In_Pos       : Stream_Element_Offset := Input'First;
      Out_Pos      : Stream_Element_Offset := Output'First;
      Original_Size : Stream_Element_Offset;
      Marker       : Stream_Element;
      Value        : Stream_Element;
      Run_Length   : Natural;
   begin
      -- Verify magic
      if Input'Length < 8 then
         raise Compression_Error with "Input too short";
      end if;

      for I in Magic'Range loop
         if Input (In_Pos) /= Character'Pos (Magic (I)) then
            raise Compression_Error with "Invalid RLE magic number";
         end if;
         In_Pos := In_Pos + 1;
      end loop;

      -- Read original size
      Original_Size := Read_U32 (Input, In_Pos);
      In_Pos := In_Pos + 4;

      -- Decode runs
      while In_Pos <= Input'Last and Out_Pos <= Output'Last loop
         Marker := Input (In_Pos);
         In_Pos := In_Pos + 1;

         if Marker >= 128 then
            -- Run encoding
            Run_Length := Natural (Marker) - 127;
            if In_Pos > Input'Last then
               raise Compression_Error with "Truncated run data";
            end if;
            Value := Input (In_Pos);
            In_Pos := In_Pos + 1;

            -- Output the run
            for I in 1 .. Run_Length loop
               Output (Out_Pos) := Value;
               Out_Pos := Out_Pos + 1;
            end loop;
         else
            -- Literal byte
            Output (Out_Pos) := Marker;
            Out_Pos := Out_Pos + 1;
         end if;
      end loop;

      Last := Out_Pos - 1;

      -- Verify size
      if Stream_Element_Offset (Last - Output'First + 1) /= Original_Size then
         raise Compression_Error with "Size mismatch in decompression";
      end if;
   end Decompress;

   function Estimate_Compressed_Size (Input_Size : Stream_Element_Offset) return Stream_Element_Offset is
   begin
      -- Header (8 bytes) + worst case (every byte different and >= 128): 2x expansion
      return 8 + (Input_Size * 2) + 100;
   end Estimate_Compressed_Size;

end RLE;
