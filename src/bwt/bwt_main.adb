-- ***************************************************************************
--                 BWT Command Line Interface
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

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with BWT_Index;

procedure BWT_Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use Ada.Streams;
   
   package SIO renames Ada.Streams.Stream_IO;

   Magic_Index : constant String := "BWT1";

   procedure Print_Usage is
   begin
      Put_Line ("BWT (Burrows-Wheeler Transform) Tool");
      Put_Line ("Usage: bwt <command> <input_file> <output_file>");
      Put_Line ("");
      Put_Line ("Commands:");
      Put_Line ("  -t, --transform   Apply BWT transform");
      Put_Line ("  -i, --inverse     Apply inverse BWT");
      Put_Line ("  -h, --help        Show this help message");
      Put_Line ("");
      Put_Line ("Note: This is index-based BWT (stores primary index)");
   end Print_Usage;

   -- Write 4-byte big-endian integer
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

   -- Read 4-byte big-endian integer
   function Read_U32
     (Input : Stream_Element_Array;
      Pos   : Stream_Element_Offset) return Stream_Element_Offset is
   begin
      return Stream_Element_Offset (Input (Pos)) * 16777216 +
             Stream_Element_Offset (Input (Pos + 1)) * 65536 +
             Stream_Element_Offset (Input (Pos + 2)) * 256 +
             Stream_Element_Offset (Input (Pos + 3));
   end Read_U32;

   procedure Transform_File (Input_Name, Output_Name : String) is
      Input_File   : SIO.File_Type;
      Output_File  : SIO.File_Type;
      Input_Size   : Stream_Element_Offset;
      Input_Data   : Stream_Element_Array (1 .. 100_000);
      Output_Data  : Stream_Element_Array (1 .. 100_000);
      Header       : Stream_Element_Array (1 .. 16);
      Last         : Stream_Element_Offset;
      Primary_Index : Stream_Element_Offset;
      Out_Pos      : Stream_Element_Offset := Header'First;
   begin
      SIO.Open (Input_File, SIO.In_File, Input_Name);
      Input_Size := Stream_Element_Offset (SIO.Size (Input_File));
      
      if Input_Size > Input_Data'Length then
         Put_Line ("Error: Input file too large (max 100KB for now)");
         SIO.Close (Input_File);
         return;
      end if;

      SIO.Read (Input_File, Input_Data (1 .. Input_Size), Last);
      SIO.Close (Input_File);

      -- Apply BWT
      BWT_Index.Transform (Input_Data (1 .. Input_Size), Output_Data, Primary_Index, Last);

      -- Build header
      for I in Magic_Index'Range loop
         Header (Out_Pos) := Character'Pos (Magic_Index (I));
         Out_Pos := Out_Pos + 1;
      end loop;
      
      Write_U32 (Header, Out_Pos, Input_Size);
      Write_U32 (Header, Out_Pos, Primary_Index);

      -- Write output file
      SIO.Create (Output_File, SIO.Out_File, Output_Name);
      SIO.Write (Output_File, Header (1 .. Out_Pos - 1));
      SIO.Write (Output_File, Output_Data (1 .. Last));
      SIO.Close (Output_File);

      Put_Line ("BWT Transform complete:");
      Put_Line ("  Input size:     " & Stream_Element_Offset'Image (Input_Size) & " bytes");
      Put_Line ("  Output size:    " & Stream_Element_Offset'Image (Last) & " bytes");
      Put_Line ("  Primary index:  " & Stream_Element_Offset'Image (Primary_Index));
   exception
      when others =>
         if SIO.Is_Open (Input_File) then
            SIO.Close (Input_File);
         end if;
         if SIO.Is_Open (Output_File) then
            SIO.Close (Output_File);
         end if;
         raise;
   end Transform_File;

   procedure Inverse_File (Input_Name, Output_Name : String) is
      Input_File   : SIO.File_Type;
      Output_File  : SIO.File_Type;
      Input_Size   : Stream_Element_Offset;
      Header       : Stream_Element_Array (1 .. 16);
      Input_Data   : Stream_Element_Array (1 .. 100_000);
      Output_Data  : Stream_Element_Array (1 .. 100_000);
      Last         : Stream_Element_Offset;
      Original_Size : Stream_Element_Offset;
      Primary_Index : Stream_Element_Offset;
      In_Pos       : Stream_Element_Offset := Header'First;
   begin
      SIO.Open (Input_File, SIO.In_File, Input_Name);
      Input_Size := Stream_Element_Offset (SIO.Size (Input_File));
      
      if Input_Size < 12 then
         Put_Line ("Error: Invalid BWT file (too short)");
         SIO.Close (Input_File);
         return;
      end if;

      -- Read header
      SIO.Read (Input_File, Header (1 .. 12), Last);
      
      -- Verify magic
      for I in Magic_Index'Range loop
         if Header (In_Pos) /= Character'Pos (Magic_Index (I)) then
            Put_Line ("Error: Invalid BWT magic number");
            SIO.Close (Input_File);
            return;
         end if;
         In_Pos := In_Pos + 1;
      end loop;
      
      Original_Size := Read_U32 (Header, In_Pos);
      In_Pos := In_Pos + 4;
      Primary_Index := Read_U32 (Header, In_Pos);

      -- Read BWT data
      declare
         BWT_Size : constant Stream_Element_Offset := Input_Size - 12;
      begin
         SIO.Read (Input_File, Input_Data (1 .. BWT_Size), Last);
         SIO.Close (Input_File);

         -- Apply inverse BWT
         BWT_Index.Inverse_Transform (Input_Data (1 .. BWT_Size), Primary_Index, Output_Data, Last);

         -- Write output
         SIO.Create (Output_File, SIO.Out_File, Output_Name);
         SIO.Write (Output_File, Output_Data (1 .. Last));
         SIO.Close (Output_File);

         Put_Line ("BWT Inverse complete:");
         Put_Line ("  Input size:  " & Stream_Element_Offset'Image (Input_Size) & " bytes");
         Put_Line ("  Output size: " & Stream_Element_Offset'Image (Last) & " bytes");
      end;
   exception
      when others =>
         if SIO.Is_Open (Input_File) then
            SIO.Close (Input_File);
         end if;
         if SIO.Is_Open (Output_File) then
            SIO.Close (Output_File);
         end if;
         raise;
   end Inverse_File;

begin
   if Argument_Count < 1 then
      Print_Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   declare
      Command : constant String := Argument (1);
   begin
      if Command = "-h" or Command = "--help" then
         Print_Usage;
      elsif Command = "-t" or Command = "--transform" then
         if Argument_Count /= 3 then
            Put_Line ("Error: Transform requires input and output file names");
            Print_Usage;
            Set_Exit_Status (Failure);
         else
            Transform_File (Argument (2), Argument (3));
         end if;
      elsif Command = "-i" or Command = "--inverse" then
         if Argument_Count /= 3 then
            Put_Line ("Error: Inverse requires input and output file names");
            Print_Usage;
            Set_Exit_Status (Failure);
         else
            Inverse_File (Argument (2), Argument (3));
         end if;
      else
         Put_Line ("Error: Unknown command '" & Command & "'");
         Print_Usage;
         Set_Exit_Status (Failure);
      end if;
   end;
end BWT_Main;
