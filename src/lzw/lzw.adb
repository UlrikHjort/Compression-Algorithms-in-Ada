-- ***************************************************************************
--               LZW Compression Package Body
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

with Interfaces; use Interfaces;

package body LZW is

   Max_Dict_Size : constant := 4096;
   Max_String_Len : constant := 1000;

   type Dict_Entry is record
      Data   : Stream_Element_Array (1 .. Max_String_Len);
      Length : Natural := 0;
   end record;

   type Dictionary is array (0 .. Max_Dict_Size - 1) of Dict_Entry;

   -- Initialize dictionary with single-byte entries (0-255).
   -- All subsequent entries (256+) added during compression.
   procedure Init_Dict (Dict : out Dictionary; Size : out Natural) is
   begin
      for I in Stream_Element'Range loop
         Dict (Natural (I)).Data (1) := I;
         Dict (Natural (I)).Length := 1;
      end loop;
      Size := 256;
   end Init_Dict;

   function Find_In_Dict
     (Dict : Dictionary;
      Size : Natural;
      S    : Stream_Element_Array) return Integer
   is
   begin
      for I in 0 .. Size - 1 loop
         if Dict (I).Length = S'Length then
            declare
               Match : Boolean := True;
            begin
               for J in 0 .. S'Length - 1 loop
                  declare
                     Offset : constant Stream_Element_Offset := Stream_Element_Offset (S'First) + Stream_Element_Offset (J);
                  begin
                     if S (Offset) /= Dict (I).Data (Stream_Element_Offset (Natural (J) + 1)) then
                        Match := False;
                        exit;
                     end if;
                  end;
               end loop;
               if Match then
                  return I;
               end if;
            end;
         end if;
      end loop;
      return -1;
   end Find_In_Dict;

   -- Add new sequence to dictionary if space available.
   -- Copies sequence into next dictionary slot, increments size.
   -- Maximum 4096 entries (12-bit codes).
   procedure Add_To_Dict
     (Dict : in out Dictionary;
      Size : in out Natural;
      S    : Stream_Element_Array)
   is
   begin
      if Size < Max_Dict_Size and S'Length <= Max_String_Len then
         for I in 0 .. S'Length - 1 loop
            declare
               Offset : constant Stream_Element_Offset := Stream_Element_Offset (S'First) + Stream_Element_Offset (I);
            begin
               Dict (Size).Data (Stream_Element_Offset (Natural (I) + 1)) := S (Offset);
            end;
         end loop;
         Dict (Size).Length := S'Length;
         Size := Size + 1;
      end if;
   end Add_To_Dict;

   -- Calculate bit width needed for current dictionary size.
   -- Returns 9 bits for 0-512, 10 for 513-1024, 11 for 1025-2048, 12 for 2049-4096.
   function Code_Bits (Dict_Size : Natural) return Natural is
   begin
      if Dict_Size <= 512 then
         return 9;
      elsif Dict_Size <= 1024 then
         return 10;
      elsif Dict_Size <= 2048 then
         return 11;
      else
         return 12;
      end if;
   end Code_Bits;

   -- Write variable-width code to output stream.
   -- Packs bits tightly (not byte-aligned) to minimize space.
   -- Uses bit buffer and position tracking.
   procedure Write_Code
     (Output : in out Stream_Element_Array;
      Pos    : in out Stream_Element_Offset;
      Buf    : in out Unsigned_8;
      Bits   : in out Natural;
      Code   : Natural;
      Width  : Natural)
   is
      Code_U : Unsigned_32 := Unsigned_32 (Code);
   begin
      for I in reverse 0 .. Width - 1 loop
         if (Code_U and Shift_Left (Unsigned_32 (1), I)) /= 0 then
            Buf := Buf or Unsigned_8 (Shift_Left (Unsigned_32 (1), 7 - Bits));
         end if;
         Bits := Bits + 1;
         if Bits = 8 then
            Output (Pos) := Stream_Element (Buf);
            Pos := Pos + 1;
            Buf := 0;
            Bits := 0;
         end if;
      end loop;
   end Write_Code;

   -- Read variable-width code from input stream.
   -- Extracts specified number of bits from packed byte stream.
   -- Handles codes spanning byte boundaries.
   function Read_Code
     (Input : Stream_Element_Array;
      Pos   : in out Stream_Element_Offset;
      Bits  : in out Natural;
      Width : Natural) return Natural
   is
      Code : Unsigned_32 := 0;
   begin
      for I in reverse 0 .. Width - 1 loop
         if Pos > Input'Last then
            return 0;
         end if;
         if (Unsigned_8 (Input (Pos)) and Shift_Left (Unsigned_8 (1), 7 - Bits)) /= 0 then
            Code := Code or Shift_Left (Unsigned_32 (1), I);
         end if;
         Bits := Bits + 1;
         if Bits = 8 then
            Pos := Pos + 1;
            Bits := 0;
         end if;
      end loop;
      return Natural (Code);
   end Read_Code;

   procedure Compress
     (Input  : in  Stream_Element_Array;
      Output : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Dict       : Dictionary;
      Dict_Size  : Natural;
      W          : Stream_Element_Array (1 .. Max_String_Len);
      W_Len      : Natural := 0;
      Pos        : Stream_Element_Offset;
      Buf        : Unsigned_8 := 0;
      Bits       : Natural := 0;
      Start_Pos  : Stream_Element_Offset;
   begin
      if Input'Length = 0 then
         Last := Output'First - 1;
         return;
      end if;

      Init_Dict (Dict, Dict_Size);

      Pos := Output'First;
      Output (Pos) := 1;
      Pos := Pos + 1;

      declare
         Len : constant Natural := Natural (Input'Length);
      begin
         Output (Pos) := Stream_Element (Len / 16777216 mod 256);
         Output (Pos + 1) := Stream_Element (Len / 65536 mod 256);
         Output (Pos + 2) := Stream_Element (Len / 256 mod 256);
         Output (Pos + 3) := Stream_Element (Len mod 256);
      end;
      Pos := Pos + 4;
      Start_Pos := Pos;

      W (1) := Input (Input'First);
      W_Len := 1;

      for I in Input'First + 1 .. Input'Last loop
         declare
            WC : Stream_Element_Array (1 .. Stream_Element_Offset (W_Len) + 1);
            Idx : Integer;
            WC_First : constant Stream_Element_Offset := WC'First;
            W_First : constant Stream_Element_Offset := W'First;
         begin
            WC (Stream_Element_Offset (WC_First) .. Stream_Element_Offset (WC_First) + Stream_Element_Offset (W_Len) - 1) := 
               W (Stream_Element_Offset (W_First) .. Stream_Element_Offset (W_First) + Stream_Element_Offset (W_Len) - 1);
            WC (Stream_Element_Offset (W_Len) + 1) := Input (I);

            Idx := Find_In_Dict (Dict, Dict_Size, WC);

            if Idx >= 0 then
               W (Stream_Element_Offset (W'First) .. Stream_Element_Offset (W'First) + Stream_Element_Offset (W_Len)) := 
                  WC (Stream_Element_Offset (WC_First) .. Stream_Element_Offset (WC_First) + Stream_Element_Offset (W_Len));
               W_Len := W_Len + 1;
            else
               Idx := Find_In_Dict (Dict, Dict_Size, W (1 .. Stream_Element_Offset (W_Len)));
               Write_Code (Output, Pos, Buf, Bits, Idx, Code_Bits (Dict_Size));
               Add_To_Dict (Dict, Dict_Size, WC);
               W (1) := Input (I);
               W_Len := 1;
            end if;
         end;
      end loop;

      declare
         Idx : constant Integer := Find_In_Dict (Dict, Dict_Size, W (1 .. Stream_Element_Offset (W_Len)));
      begin
         Write_Code (Output, Pos, Buf, Bits, Idx, Code_Bits (Dict_Size));
      end;

      if Bits > 0 then
         Output (Pos) := Stream_Element (Buf);
         Pos := Pos + 1;
      end if;

      Last := Pos - 1;

      -- If compressed size not smaller, store uncompressed
      if Last >= Output'First + Input'Length then
         Output (Output'First) := 0;
         Output (Output'First + 1 .. Output'First + Input'Length) := Input;
         Last := Output'First + Input'Length;
      end if;
   end Compress;

   -- Main decompression procedure - reverses LZW encoding.
   -- Rebuilds dictionary in sync with encoder, handles special case where
   -- code references not-yet-added entry. Decodes variable-width codes.
   procedure Decompress
     (Input  : in  Stream_Element_Array;
      Output : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Flag      : Stream_Element;
      Dict      : Dictionary;
      Dict_Size : Natural;
      Pos       : Stream_Element_Offset;
      Write_Pos : Stream_Element_Offset := Output'First;
      Bits      : Natural := 0;
      Old_Code  : Natural;
      New_Code  : Natural;
      Out_Size  : Natural;
   begin
      if Input'Length = 0 then
         Last := Output'First - 1;
         return;
      end if;

      Flag := Input (Input'First);

      if Flag = 0 then
         Output (Output'First .. Output'First + Input'Length - 2) :=
           Input (Input'First + 1 .. Input'Last);
         Last := Output'First + Input'Length - 2;
         return;
      end if;

      Init_Dict (Dict, Dict_Size);

      Out_Size := Natural (Input (Input'First + 1)) * 16777216 +
                  Natural (Input (Input'First + 2)) * 65536 +
                  Natural (Input (Input'First + 3)) * 256 +
                  Natural (Input (Input'First + 4));

      Pos := Input'First + 5;

      Old_Code := Read_Code (Input, Pos, Bits, Code_Bits (Dict_Size));

      declare
         Len : constant Natural := Dict (Old_Code).Length;
      begin
         for I in Natural range 1 .. Len loop
            Output (Write_Pos + Stream_Element_Offset (I - 1)) := Dict (Old_Code).Data (Stream_Element_Offset (I));
         end loop;
         Write_Pos := Write_Pos + Stream_Element_Offset (Len);
      end;

      while Write_Pos - Output'First < Stream_Element_Offset (Out_Size) loop
         New_Code := Read_Code (Input, Pos, Bits, Code_Bits (Dict_Size + 1));

         if New_Code < Dict_Size then
            declare
               Len     : constant Natural := Dict (New_Code).Length;
               Old_Len   : constant Natural := Dict (Old_Code).Length;
               Str       : Stream_Element_Array (1 .. Stream_Element_Offset (Old_Len) + 1);
               Str_First : constant Stream_Element_Offset := Str'First;
            begin
               for I in Natural range 1 .. Len loop
                  Output (Write_Pos + Stream_Element_Offset (I - 1)) := Dict (New_Code).Data (Stream_Element_Offset (I));
               end loop;
               Write_Pos := Write_Pos + Stream_Element_Offset (Len);

               for I in Natural range 1 .. Old_Len loop
                  Str (Stream_Element_Offset (Str_First) + Stream_Element_Offset (I - 1)) := Dict (Old_Code).Data (Stream_Element_Offset (I));
               end loop;
               Str (Stream_Element_Offset (Str_First) + Stream_Element_Offset (Old_Len)) := Dict (New_Code).Data (1);
               Add_To_Dict (Dict, Dict_Size, Str);
            end;
         else
            declare
               Old_Len : constant Natural := Dict (Old_Code).Length;
               Str     : Stream_Element_Array (1 .. Stream_Element_Offset (Old_Len) + 1);
               Str_First : constant Stream_Element_Offset := Str'First;
            begin
               for I in 1 .. Old_Len loop
                  Str (Stream_Element_Offset (Str_First) + Stream_Element_Offset (I - 1)) := Dict (Old_Code).Data (Stream_Element_Offset (I));
               end loop;
               Str (Stream_Element_Offset (Str_First) + Stream_Element_Offset (Old_Len)) := Dict (Old_Code).Data (1);

               for I in Natural range 1 .. Natural (Str'Length) loop
                  Output (Write_Pos + Stream_Element_Offset (I - 1)) := Str (Stream_Element_Offset (Str_First) + Stream_Element_Offset (I - 1));
               end loop;
               Write_Pos := Write_Pos + Stream_Element_Offset (Str'Length);

               Add_To_Dict (Dict, Dict_Size, Str);
            end;
         end if;

         Old_Code := New_Code;
      end loop;

      Last := Output'First + Stream_Element_Offset (Out_Size) - 1;
   end Decompress;

   -- Estimate maximum output buffer size needed for compression.
   -- Returns input size plus overhead for metadata.
   function Estimate_Compressed_Size (Input_Size : Stream_Element_Offset) return Stream_Element_Offset is
   begin
      return Input_Size + 1024;
   end Estimate_Compressed_Size;

end LZW;
