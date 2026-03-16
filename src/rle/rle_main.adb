-- ***************************************************************************
--                 RLE Command Line Interface
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
with RLE;

procedure RLE_Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use Ada.Streams;
   
   package SIO renames Ada.Streams.Stream_IO;

   procedure Print_Usage is
   begin
      Put_Line ("RLE Compression Tool");
      Put_Line ("Usage: rle <command> <input_file> <output_file>");
      Put_Line ("");
      Put_Line ("Commands:");
      Put_Line ("  -c, --compress    Compress input file");
      Put_Line ("  -d, --decompress  Decompress input file");
      Put_Line ("  -h, --help        Show this help message");
   end Print_Usage;

   procedure Compress_File (Input_Name, Output_Name : String) is
      Input_File  : SIO.File_Type;
      Output_File : SIO.File_Type;
      Input_Size  : Stream_Element_Offset;
      Input_Data  : Stream_Element_Array (1 .. 1_000_000);
      Output_Data : Stream_Element_Array (1 .. 2_100_000);
      Last        : Stream_Element_Offset;
   begin
      SIO.Open (Input_File, SIO.In_File, Input_Name);
      Input_Size := Stream_Element_Offset (SIO.Size (Input_File));
      
      if Input_Size > Input_Data'Length then
         Put_Line ("Error: Input file too large (max 1MB)");
         SIO.Close (Input_File);
         return;
      end if;

      SIO.Read (Input_File, Input_Data (1 .. Input_Size), Last);
      SIO.Close (Input_File);

      RLE.Compress (Input_Data (1 .. Input_Size), Output_Data, Last);

      SIO.Create (Output_File, SIO.Out_File, Output_Name);
      SIO.Write (Output_File, Output_Data (1 .. Last));
      SIO.Close (Output_File);

      Put_Line ("Compression complete:");
      Put_Line ("  Input size:  " & Stream_Element_Offset'Image (Input_Size) & " bytes");
      Put_Line ("  Output size: " & Stream_Element_Offset'Image (Last) & " bytes");
      
      if Input_Size > 0 then
         declare
            Ratio : constant Float := (Float (Last) / Float (Input_Size)) * 100.0;
         begin
            Put_Line ("  Ratio:       " & Float'Image (Ratio) & "%");
         end;
      end if;
   exception
      when others =>
         if SIO.Is_Open (Input_File) then
            SIO.Close (Input_File);
         end if;
         if SIO.Is_Open (Output_File) then
            SIO.Close (Output_File);
         end if;
         raise;
   end Compress_File;

   procedure Decompress_File (Input_Name, Output_Name : String) is
      Input_File  : SIO.File_Type;
      Output_File : SIO.File_Type;
      Input_Size  : Stream_Element_Offset;
      Input_Data  : Stream_Element_Array (1 .. 2_100_000);
      Output_Data : Stream_Element_Array (1 .. 1_000_000);
      Last        : Stream_Element_Offset;
   begin
      SIO.Open (Input_File, SIO.In_File, Input_Name);
      Input_Size := Stream_Element_Offset (SIO.Size (Input_File));
      
      if Input_Size > Input_Data'Length then
         Put_Line ("Error: Input file too large");
         SIO.Close (Input_File);
         return;
      end if;

      SIO.Read (Input_File, Input_Data (1 .. Input_Size), Last);
      SIO.Close (Input_File);

      RLE.Decompress (Input_Data (1 .. Input_Size), Output_Data, Last);

      SIO.Create (Output_File, SIO.Out_File, Output_Name);
      SIO.Write (Output_File, Output_Data (1 .. Last));
      SIO.Close (Output_File);

      Put_Line ("Decompression complete:");
      Put_Line ("  Input size:  " & Stream_Element_Offset'Image (Input_Size) & " bytes");
      Put_Line ("  Output size: " & Stream_Element_Offset'Image (Last) & " bytes");
   exception
      when others =>
         if SIO.Is_Open (Input_File) then
            SIO.Close (Input_File);
         end if;
         if SIO.Is_Open (Output_File) then
            SIO.Close (Output_File);
         end if;
         raise;
   end Decompress_File;

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
      elsif Command = "-c" or Command = "--compress" then
         if Argument_Count /= 3 then
            Put_Line ("Error: Compress requires input and output file names");
            Print_Usage;
            Set_Exit_Status (Failure);
         else
            Compress_File (Argument (2), Argument (3));
         end if;
      elsif Command = "-d" or Command = "--decompress" then
         if Argument_Count /= 3 then
            Put_Line ("Error: Decompress requires input and output file names");
            Print_Usage;
            Set_Exit_Status (Failure);
         else
            Decompress_File (Argument (2), Argument (3));
         end if;
      else
         Put_Line ("Error: Unknown command '" & Command & "'");
         Print_Usage;
         Set_Exit_Status (Failure);
      end if;
   end;
end RLE_Main;
