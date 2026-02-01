-- ***************************************************************************
--               Huffman Command Line Interface
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
with Huffman;

procedure Huffman_Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use Ada.Streams;
   
   package SIO renames Ada.Streams.Stream_IO;

   -- Display command-line usage information and available options.
   procedure Print_Usage is
   begin
      Put_Line ("Huffman Compression Tool");
      Put_Line ("Usage: huffman <command> <input_file> <output_file>");
      Put_Line ("");
      Put_Line ("Commands:");
      Put_Line ("  -c, --compress    Compress input file");
      Put_Line ("  -d, --decompress  Decompress input file");
      Put_Line ("  -h, --help        Show this help message");
   end Print_Usage;

   -- Read file, compress using Huffman, write result.
   -- Displays compression statistics and ratio.
   procedure Compress_File (Input_Name, Output_Name : String) is
      Input_File  : SIO.File_Type;
      Output_File : SIO.File_Type;
      Input_Size  : Stream_Element_Offset;
      Input_Data  : Stream_Element_Array (1 .. 1_000_000);
      Output_Data : Stream_Element_Array (1 .. 1_100_000);
      Last        : Stream_Element_Offset;
   begin
      SIO.Open (Input_File, SIO.In_File, Input_Name);
      Input_Size := Stream_Element_Offset (SIO.Size (Input_File));
      
      SIO.Read (Input_File, Input_Data (1 .. Input_Size), Last);
      SIO.Close (Input_File);

      Put_Line ("Compressing " & Input_Name & " (" & Stream_Element_Offset'Image (Input_Size) & " bytes)...");
      
      Huffman.Compress (Input_Data (1 .. Last), Output_Data, Last);
      
      SIO.Create (Output_File, SIO.Out_File, Output_Name);
      SIO.Write (Output_File, Output_Data (1 .. Last));
      SIO.Close (Output_File);

      Put_Line ("Compressed to " & Output_Name & " (" & Stream_Element_Offset'Image (Last) & " bytes)");
      if Input_Size > 0 then
         Put_Line ("Compression ratio:" & Integer'Image ((Integer (Last) * 100) / Integer (Input_Size)) & "%");
      end if;
   exception
      when SIO.Name_Error =>
         Put_Line ("Error: Cannot open input file: " & Input_Name);
         Set_Exit_Status (Failure);
      when SIO.Use_Error =>
         Put_Line ("Error: Cannot create output file: " & Output_Name);
         Set_Exit_Status (Failure);
   end Compress_File;

   procedure Decompress_File (Input_Name, Output_Name : String) is
      Input_File  : SIO.File_Type;
      Output_File : SIO.File_Type;
      Input_Size  : Stream_Element_Offset;
      Input_Data  : Stream_Element_Array (1 .. 1_100_000);
      Output_Data : Stream_Element_Array (1 .. 1_000_000);
      Last        : Stream_Element_Offset;
   begin
      SIO.Open (Input_File, SIO.In_File, Input_Name);
      Input_Size := Stream_Element_Offset (SIO.Size (Input_File));
      
      SIO.Read (Input_File, Input_Data (1 .. Input_Size), Last);
      SIO.Close (Input_File);

      Put_Line ("Decompressing " & Input_Name & "...");
      
      Huffman.Decompress (Input_Data (1 .. Last), Output_Data, Last);
      
      SIO.Create (Output_File, SIO.Out_File, Output_Name);
      SIO.Write (Output_File, Output_Data (1 .. Last));
      SIO.Close (Output_File);

      Put_Line ("Decompressed to " & Output_Name & " (" & Stream_Element_Offset'Image (Last) & " bytes)");
   exception
      when SIO.Name_Error =>
         Put_Line ("Error: Cannot open input file: " & Input_Name);
         Set_Exit_Status (Failure);
      when SIO.Use_Error =>
         Put_Line ("Error: Cannot create output file: " & Output_Name);
         Set_Exit_Status (Failure);
   end Decompress_File;

begin
   if Argument_Count < 3 then
      Print_Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   declare
      Command : constant String := Argument (1);
      Input   : constant String := Argument (2);
      Output  : constant String := Argument (3);
   begin
      if Command = "-c" or Command = "--compress" then
         Compress_File (Input, Output);
      elsif Command = "-d" or Command = "--decompress" then
         Decompress_File (Input, Output);
      elsif Command = "-h" or Command = "--help" then
         Print_Usage;
      else
         Put_Line ("Error: Unknown command: " & Command);
         Print_Usage;
         Set_Exit_Status (Failure);
      end if;
   end;
exception
   when Huffman.Compression_Error =>
      Put_Line ("Error: Compression/decompression failed");
      Set_Exit_Status (Failure);
end Huffman_Main;
