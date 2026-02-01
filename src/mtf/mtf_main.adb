-- ***************************************************************************
--          MTF (Move-to-Front) Command Line Interface
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
with Ada.Exceptions;
with MTF;

procedure MTF_Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use Ada.Streams;
   
   package SIO renames Ada.Streams.Stream_IO;

   procedure Print_Usage is
   begin
      Put_Line ("MTF (Move-to-Front) Encoding Tool");
      Put_Line ("Usage: mtf <command> <input_file> <output_file>");
      Put_Line ("");
      Put_Line ("Commands:");
      Put_Line ("  -e, --encode     Encode input file");
      Put_Line ("  -d, --decode     Decode input file");
      Put_Line ("  -h, --help       Show this help message");
      Put_Line ("");
      Put_Line ("Note: MTF does not compress by itself - it prepares data");
      Put_Line ("for better compression by other algorithms (like RLE or Huffman).");
   end Print_Usage;

   procedure Write_Header (File : SIO.File_Type; Size : Stream_Element_Offset) is
      Magic  : constant Stream_Element_Array := (77, 84, 70, 49); -- "MTF1"
      Header : Stream_Element_Array (1 .. 8);
   begin
      -- Write magic number
      Header (1 .. 4) := Magic;
      
      -- Write original size (big-endian)
      Header (5) := Stream_Element (Size / 16777216 mod 256);
      Header (6) := Stream_Element (Size / 65536 mod 256);
      Header (7) := Stream_Element (Size / 256 mod 256);
      Header (8) := Stream_Element (Size mod 256);
      
      SIO.Write (File, Header);
   end Write_Header;

   procedure Read_Header (File : SIO.File_Type; Size : out Stream_Element_Offset) is
      Magic    : constant Stream_Element_Array := (77, 84, 70, 49); -- "MTF1"
      Header   : Stream_Element_Array (1 .. 8);
      Last     : Stream_Element_Offset;
   begin
      SIO.Read (File, Header, Last);
      
      if Last < 8 then
         raise MTF.Encoding_Error with "File too short to contain header";
      end if;
      
      if Header (1 .. 4) /= Magic then
         raise MTF.Encoding_Error with "Invalid MTF file (bad magic number)";
      end if;
      
      -- Read original size (big-endian)
      Size := Stream_Element_Offset (Header (5)) * 16777216 +
              Stream_Element_Offset (Header (6)) * 65536 +
              Stream_Element_Offset (Header (7)) * 256 +
              Stream_Element_Offset (Header (8));
   end Read_Header;

   procedure Encode_File (Input_Name, Output_Name : String) is
      Input_File  : SIO.File_Type;
      Output_File : SIO.File_Type;
      Input_Size  : Stream_Element_Offset;
      Input_Data  : Stream_Element_Array (1 .. 1_000_000);
      Output_Data : Stream_Element_Array (1 .. 1_000_000);
      Last        : Stream_Element_Offset;
   begin
      SIO.Open (Input_File, SIO.In_File, Input_Name);
      Input_Size := Stream_Element_Offset (SIO.Size (Input_File));
      
      if Input_Size > Input_Data'Length then
         Put_Line ("Error: Input file too large (max 1MB)");
         SIO.Close (Input_File);
         return;
      end if;

      if Input_Size > 0 then
         SIO.Read (Input_File, Input_Data (1 .. Input_Size), Last);
      end if;
      SIO.Close (Input_File);

      if Input_Size = 0 then
         -- Handle empty file
         SIO.Create (Output_File, SIO.Out_File, Output_Name);
         Write_Header (Output_File, 0);
         SIO.Close (Output_File);
         Put_Line ("Encoding complete (empty file)");
         return;
      end if;

      MTF.Encode (Input_Data (1 .. Input_Size), Output_Data, Last);

      SIO.Create (Output_File, SIO.Out_File, Output_Name);
      Write_Header (Output_File, Input_Size);
      SIO.Write (Output_File, Output_Data (1 .. Last));
      SIO.Close (Output_File);

      Put_Line ("Encoding complete:");
      Put_Line ("  Input size:  " & Stream_Element_Offset'Image (Input_Size) & " bytes");
      Put_Line ("  Output size: " & Stream_Element_Offset'Image (Last + 8) & " bytes");
      Put_Line ("  (MTF output is same size as input + 8 byte header)");
   exception
      when E : others =>
         Put_Line ("Error during encoding: " & Ada.Exceptions.Exception_Message (E));
         if SIO.Is_Open (Input_File) then
            SIO.Close (Input_File);
         end if;
         if SIO.Is_Open (Output_File) then
            SIO.Close (Output_File);
         end if;
   end Encode_File;

   procedure Decode_File (Input_Name, Output_Name : String) is
      Input_File    : SIO.File_Type;
      Output_File   : SIO.File_Type;
      Input_Size    : Stream_Element_Offset;
      Original_Size : Stream_Element_Offset;
      Input_Data    : Stream_Element_Array (1 .. 1_000_000);
      Output_Data   : Stream_Element_Array (1 .. 1_000_000);
      Last          : Stream_Element_Offset;
   begin
      SIO.Open (Input_File, SIO.In_File, Input_Name);
      
      Read_Header (Input_File, Original_Size);
      
      if Original_Size = 0 then
         -- Handle empty file
         SIO.Close (Input_File);
         SIO.Create (Output_File, SIO.Out_File, Output_Name);
         SIO.Close (Output_File);
         Put_Line ("Decoding complete (empty file)");
         return;
      end if;
      
      Input_Size := Stream_Element_Offset (SIO.Size (Input_File)) - 8;
      
      if Input_Size > Input_Data'Length then
         Put_Line ("Error: Encoded data too large");
         SIO.Close (Input_File);
         return;
      end if;

      SIO.Read (Input_File, Input_Data (1 .. Input_Size), Last);
      SIO.Close (Input_File);

      MTF.Decode (Input_Data (1 .. Last), Output_Data, Last);
      
      if Last /= Original_Size then
         Put_Line ("Warning: Decoded size doesn't match header");
      end if;

      SIO.Create (Output_File, SIO.Out_File, Output_Name);
      SIO.Write (Output_File, Output_Data (1 .. Last));
      SIO.Close (Output_File);

      Put_Line ("Decoding complete:");
      Put_Line ("  Output size: " & Stream_Element_Offset'Image (Last) & " bytes");
   exception
      when E : others =>
         Put_Line ("Error during decoding: " & Ada.Exceptions.Exception_Message (E));
         if SIO.Is_Open (Input_File) then
            SIO.Close (Input_File);
         end if;
         if SIO.Is_Open (Output_File) then
            SIO.Close (Output_File);
         end if;
   end Decode_File;

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
      elsif Command = "-e" or Command = "--encode" then
         if Argument_Count /= 3 then
            Put_Line ("Error: encode requires input and output file names");
            Print_Usage;
            Set_Exit_Status (Failure);
         else
            Encode_File (Argument (2), Argument (3));
         end if;
      elsif Command = "-d" or Command = "--decode" then
         if Argument_Count /= 3 then
            Put_Line ("Error: decode requires input and output file names");
            Print_Usage;
            Set_Exit_Status (Failure);
         else
            Decode_File (Argument (2), Argument (3));
         end if;
      else
         Put_Line ("Error: Unknown command '" & Command & "'");
         Print_Usage;
         Set_Exit_Status (Failure);
      end if;
   end;
end MTF_Main;
