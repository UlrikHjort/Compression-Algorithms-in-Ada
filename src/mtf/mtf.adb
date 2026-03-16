-- ***************************************************************************
--          MTF (Move-to-Front) Encoding Implementation
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

package body MTF is

   -- List of all possible byte values, dynamically reordered
   type Byte_List is array (Stream_Element range 0..255) of Stream_Element;

   -- Initialize list to identity mapping: [0, 1, 2, ..., 255]
   procedure Initialize_List (List : out Byte_List) is
   begin
      for I in List'Range loop
         List(I) := I;
      end loop;
   end Initialize_List;

   -- Find position of Value in the list (returns 0-255)
   function Find_Position (List : Byte_List; Value : Stream_Element) return Stream_Element is
   begin
      for I in List'Range loop
         if List(I) = Value then
            return I;
         end if;
      end loop;
      raise Encoding_Error with "Value not found in list";
   end Find_Position;

   -- Move byte at Position to front of list, shift others right
   procedure Move_To_Front (List : in out Byte_List; Position : Stream_Element) is
      Value : constant Stream_Element := List(Position);
   begin
      -- Shift elements [0..Position-1] one position to the right
      for I in reverse 0 .. Position - 1 loop
         List(I + 1) := List(I);
      end loop;
      -- Place value at front
      List(0) := Value;
   end Move_To_Front;

   ---------------------------------------------------------------------------
   -- Encode: Transform input using Move-to-Front
   ---------------------------------------------------------------------------
   procedure Encode
     (Input  : in  Stream_Element_Array;
      Output : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      List : Byte_List;
      Pos  : Stream_Element;
   begin
      if Input'Length = 0 then
         Last := Output'First - 1;
         return;
      end if;

      if Output'Length < Input'Length then
         raise Encoding_Error with "Output buffer too small";
      end if;

      Initialize_List(List);
      Last := Output'First - 1;

      for I in Input'Range loop
         -- Find position of current byte
         Pos := Find_Position(List, Input(I));
         
         -- Output the position
         Last := Last + 1;
         Output(Last) := Pos;
         
         -- Move byte to front
         Move_To_Front(List, Pos);
      end loop;
   end Encode;

   ---------------------------------------------------------------------------
   -- Decode: Reverse the Move-to-Front transform
   ---------------------------------------------------------------------------
   procedure Decode
     (Input  : in  Stream_Element_Array;
      Output : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      List : Byte_List;
      Pos  : Stream_Element;
   begin
      if Input'Length = 0 then
         Last := Output'First - 1;
         return;
      end if;

      if Output'Length < Input'Length then
         raise Encoding_Error with "Output buffer too small";
      end if;

      Initialize_List(List);
      Last := Output'First - 1;

      for I in Input'Range loop
         -- Read position from input
         Pos := Input(I);
         
         -- Validate position
         if Pos > 255 then
            raise Encoding_Error with "Invalid position value";
         end if;
         
         -- Output byte at that position
         Last := Last + 1;
         Output(Last) := List(Pos);
         
         -- Move byte to front
         Move_To_Front(List, Pos);
      end loop;
   end Decode;

end MTF;
