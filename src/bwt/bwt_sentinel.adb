-- ***************************************************************************
--              BWT Sentinel-Based Package Body
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

package body BWT_Sentinel is

   -- Compare two rotations with sentinel at the end
   function Less_Rotation
     (Data   : Stream_Element_Array;
      Index1 : Stream_Element_Offset;
      Index2 : Stream_Element_Offset) return Boolean
   is
      N : constant Stream_Element_Offset := Data'Length;
      Pos1, Pos2 : Stream_Element_Offset;
   begin
      for I in 0 .. N - 1 loop
         Pos1 := Data'First + ((Index1 - Data'First + I) mod N);
         Pos2 := Data'First + ((Index2 - Data'First + I) mod N);
         
         if Data (Pos1) < Data (Pos2) then
            return True;
         elsif Data (Pos1) > Data (Pos2) then
            return False;
         end if;
      end loop;
      
      return False;
   end Less_Rotation;

   procedure Transform
     (Input  : in  Stream_Element_Array;
      Output : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      -- Input data with sentinel appended
      Data : Stream_Element_Array (1 .. Input'Length + 1);
      N : constant Stream_Element_Offset := Data'Length;
      
      -- Array of rotation indices
      type Rotation_Index_Array is array (Stream_Element_Offset range <>) of Stream_Element_Offset;
      Rotations : Rotation_Index_Array (0 .. N - 1);
      
   begin
      -- Check that sentinel doesn't appear in input
      for I in Input'Range loop
         if Input (I) = Sentinel then
            raise BWT_Error with "Sentinel byte (0) found in input data";
         end if;
      end loop;
      
      -- Copy input and append sentinel
      if Input'Length > 0 then
         Data (1 .. Input'Length) := Input;
      end if;
      Data (Data'Last) := Sentinel;
      
      -- Initialize rotation indices
      for I in Rotations'Range loop
         Rotations (I) := Data'First + I;
      end loop;
      
      -- Sort rotations (bubble sort for now)
      for I in Rotations'First .. Rotations'Last - 1 loop
         for J in I + 1 .. Rotations'Last loop
            if Less_Rotation (Data, Rotations (J), Rotations (I)) then
               declare
                  Temp : constant Stream_Element_Offset := Rotations (I);
               begin
                  Rotations (I) := Rotations (J);
                  Rotations (J) := Temp;
               end;
            end if;
         end loop;
      end loop;
      
      -- Extract last column
      for I in Rotations'Range loop
         declare
            Start_Pos : constant Stream_Element_Offset := Rotations (I);
            Last_Pos  : Stream_Element_Offset;
         begin
            -- Last character of rotation (wrapping)
            if Start_Pos = Data'First then
               Last_Pos := Data'Last;
            else
               Last_Pos := Start_Pos - 1;
            end if;
            
            Output (Output'First + I) := Data (Last_Pos);
         end;
      end loop;
      
      Last := Output'First + N - 1;
   end Transform;

   procedure Inverse_Transform
     (Input  : in  Stream_Element_Array;
      Output : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      N : constant Stream_Element_Offset := Input'Length;
      
      -- First column is sorted last column
      First_Column : Stream_Element_Array (Input'Range);
      
      -- Next array
      type Index_Array is array (Stream_Element_Offset range <>) of Stream_Element_Offset;
      Next : Index_Array (0 .. N - 1);
      
      -- Find sentinel position (will be in first column after sort)
      Sentinel_Pos_First : Stream_Element_Offset := 0;
      
   begin
      if N = 0 then
         Last := Output'First - 1;
         return;
      end if;
      
      -- Build first column by sorting last column
      First_Column := Input;
      for I in First_Column'First .. First_Column'Last - 1 loop
         for J in I + 1 .. First_Column'Last loop
            if First_Column (J) < First_Column (I) then
               declare
                  Temp : constant Stream_Element := First_Column (I);
               begin
                  First_Column (I) := First_Column (J);
                  First_Column (J) := Temp;
               end;
            end if;
         end loop;
      end loop;
      
      -- Find sentinel in first column (should be at position 0 after sort)
      for I in First_Column'Range loop
         if First_Column (I) = Sentinel then
            Sentinel_Pos_First := I - First_Column'First;
            exit;
         end if;
      end loop;
      
      -- Build Next array using first-last property
      for I in 0 .. N - 1 loop
         declare
            Last_Char  : constant Stream_Element := Input (Input'First + I);
            First_Char : constant Stream_Element := First_Column (First_Column'First + I);
            Occurrence : Natural := 0;
         begin
            -- Count which occurrence of First_Char this is
            for J in First_Column'First .. First_Column'First + I - 1 loop
               if First_Column (J) = First_Char then
                  Occurrence := Occurrence + 1;
               end if;
            end loop;
            
            -- Find same occurrence in last column
            declare
               Found : Natural := 0;
            begin
               for J in Input'Range loop
                  if Input (J) = First_Char then
                     if Found = Occurrence then
                        Next (I) := J - Input'First;
                        exit;
                     end if;
                     Found := Found + 1;
                  end if;
               end loop;
            end;
         end;
      end loop;
      
      -- Reconstruct original (without sentinel)
      -- Start from row that comes after sentinel in the first column
      declare
         Current_Row : Stream_Element_Offset := Next (Sentinel_Pos_First);
         Out_Pos : Stream_Element_Offset := Output'First;
      begin
         -- Output N-1 characters (skip sentinel)
         for I in 1 .. N - 1 loop
            Output (Out_Pos) := First_Column (First_Column'First + Current_Row);
            Current_Row := Next (Current_Row);
            Out_Pos := Out_Pos + 1;
         end loop;
         
         Last := Out_Pos - 1;
      end;
   end Inverse_Transform;

end BWT_Sentinel;
