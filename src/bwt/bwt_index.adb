-- ***************************************************************************
--                BWT Index-Based Package Body
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

package body BWT_Index is

   -- Compare two rotations of the input starting at different positions
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
      
      return False; -- Equal rotations
   end Less_Rotation;

   procedure Transform
     (Input         : in  Stream_Element_Array;
      Output        : out Stream_Element_Array;
      Primary_Index : out Stream_Element_Offset;
      Last          : out Stream_Element_Offset)
   is
      N : constant Stream_Element_Offset := Input'Length;
      
      -- Array of rotation indices
      type Rotation_Index_Array is array (Stream_Element_Offset range <>) of Stream_Element_Offset;
      Rotations : Rotation_Index_Array (0 .. N - 1);
      
   begin
      if N = 0 then
         Primary_Index := 0;
         Last := Output'First - 1;
         return;
      end if;
      
      -- Initialize rotation indices
      for I in Rotations'Range loop
         Rotations (I) := Input'First + I;
      end loop;
      
      -- Simple bubble sort of rotations
      for I in Rotations'First .. Rotations'Last - 1 loop
         for J in I + 1 .. Rotations'Last loop
            if Less_Rotation (Input, Rotations (J), Rotations (I)) then
               declare
                  Temp : constant Stream_Element_Offset := Rotations (I);
               begin
                  Rotations (I) := Rotations (J);
                  Rotations (J) := Temp;
               end;
            end if;
         end loop;
      end loop;
      
      -- Extract last column and find primary index
      for I in Rotations'Range loop
         declare
            Start_Pos : constant Stream_Element_Offset := Rotations (I);
            Last_Pos  : Stream_Element_Offset;
         begin
            -- Last character of rotation is one before start position (wrapping)
            if Start_Pos = Input'First then
               Last_Pos := Input'Last;
            else
               Last_Pos := Start_Pos - 1;
            end if;
            
            Output (Output'First + I) := Input (Last_Pos);
            
            -- Check if this rotation is the original (starts at Input'First)
            if Start_Pos = Input'First then
               Primary_Index := I;
            end if;
         end;
      end loop;
      
      Last := Output'First + N - 1;
   end Transform;

   procedure Inverse_Transform
     (Input         : in  Stream_Element_Array;
      Primary_Index : in  Stream_Element_Offset;
      Output        : out Stream_Element_Array;
      Last          : out Stream_Element_Offset)
   is
      N : constant Stream_Element_Offset := Input'Length;
      
      -- First column is sorted last column
      First_Column : Stream_Element_Array (Input'Range);
      
      -- Next array: next[i] tells us which row follows row i
      type Index_Array is array (Stream_Element_Offset range <>) of Stream_Element_Offset;
      Next : Index_Array (0 .. N - 1);
      
      -- Count array for building Next
      type Count_Array is array (Stream_Element range 0 .. 255) of Natural;
      Count : Count_Array := (others => 0);
      
   begin
      if N = 0 then
         Last := Output'First - 1;
         return;
      end if;
      
      -- Build first column by sorting last column (Input)
      First_Column := Input;
      
      -- Simple selection sort (can optimize later)
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
      
      -- Build Next array using the first-last property
      -- For each character value, track how many we've seen
      for I in 0 .. N - 1 loop
         declare
            Last_Char  : constant Stream_Element := Input (Input'First + I);
            First_Char : constant Stream_Element := First_Column (First_Column'First + I);
            
            -- Find which occurrence of First_Char this is in First_Column
            Occurrence : Natural := 0;
         begin
            for J in First_Column'First .. First_Column'First + I - 1 loop
               if First_Column (J) = First_Char then
                  Occurrence := Occurrence + 1;
               end if;
            end loop;
            
            -- Find the same occurrence of this character in Input (last column)
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
      
      -- Reconstruct original by following the chain starting from Primary_Index
      declare
         Current_Row : Stream_Element_Offset := Primary_Index;
      begin
         for I in 0 .. N - 1 loop
            Output (Output'First + I) := First_Column (First_Column'First + Current_Row);
            Current_Row := Next (Current_Row);
         end loop;
      end;
      
      Last := Output'First + N - 1;
   end Inverse_Transform;

end BWT_Index;
