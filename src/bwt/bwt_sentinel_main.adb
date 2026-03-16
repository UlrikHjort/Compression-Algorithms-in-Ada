-- ***************************************************************************
--             BWT Sentinel Command Line Interface
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
with BWT_Sentinel;

procedure BWT_Sentinel_Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use Ada.Streams;
   
   package SIO renames Ada.Streams.Stream_IO;

   Magic_Sentinel : constant String := "BWTS";

   procedure Print_Usage is
   begin
      Put_Line ("BWT (Burrows-Wheeler Transform) Tool - Sentinel Version");
      Put_Line ("Usage: bwt_sentinel <command> <input_file> <output_file>");
      Put_Line ("");
      Put_Line ("Commands:");
      Put_Line ("  -t, --transform   Apply BWT transform");
      Put_Line ("  -i, --inverse     Apply inverse BWT");
      Put_Line ("  -h, --help        Show this help message");
      Put_Line ("");
      Put_Line ("Note: This is sentinel-based BWT (uses EOF marker, no index)");
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
      Output_Data  : Stream_Element_Array (1 .. 100_001);  -- +1 for sentinel
      Header       : Stream_Element_Array (1 .. 8);
      Last         : Stream_Element_Offset;
      Out_Pos      : Stream_Element_Offset := Header'First;
   begin
      SIO.Open (Input_File, SIO.In_File, Input_Name);
      Input_Size := Stream_Element_Offset (SIO.Size (Input_File));
      
      if Input_Size > Input_Data'Length then
         Put_Line ("Error: Input file too large (max 100KB for now)");
         SIO.Close (Input_File);
         return;
      end if;

      if Input_Size > 0 then
         SIO.Read (Input_File, Input_Data (1 .. Input_Size), Last);
      else
         Last := 0;
      end if;
      SIO.Close (Input_File);

      -- Apply BWT (sentinel-based)
      BWT_Sentinel.Transform (Input_Data (1 .. Input_Size), Output_Data, Last);

      -- Build header (no primary index needed!)
      for I in Magic_Sentinel'Range loop
         Header (Out_Pos) := Character'Pos (Magic_Sentinel (I));
         Out_Pos := Out_Pos + 1;
      end loop;
      
      Write_U32 (Header, Out_Pos, Input_Size);

      -- Write output file
      SIO.Create (Output_File, SIO.Out_File, Output_Name);
      SIO.Write (Output_File, Header);
      SIO.Write (Output_File, Output_Data (1 .. Last));
      SIO.Close (Output_File);

      Put_Line ("BWT Transform complete (sentinel mode):");
      Put_Line ("  Input size:     " & Stream_Element_Offset'Image (Input_Size) & " bytes");
      Put_Line ("  Output size:    " & Stream_Element_Offset'Image (Last) & " bytes (includes sentinel)");
      Put_Line ("  No index needed - using sentinel byte 0");
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
      Header       : Stream_Element_Array (1 .. 8);
      Input_Data   : Stream_Element_Array (1 .. 100_001);
      Output_Data  : Stream_Element_Array (1 .. 100_000);
      Last         : Stream_Element_Offset;
      Original_Size : Stream_Element_Offset;
      In_Pos       : Stream_Element_Offset := Header'First;
   begin
      SIO.Open (Input_File, SIO.In_File, Input_Name);
      Input_Size := Stream_Element_Offset (SIO.Size (Input_File));
      
      if Input_Size < 8 then
         Put_Line ("Error: Invalid BWT file (too short)");
         SIO.Close (Input_File);
         return;
      end if;

      -- Read header
      SIO.Read (Input_File, Header, Last);
      
      -- Verify magic
      for I in Magic_Sentinel'Range loop
         if Header (In_Pos) /= Character'Pos (Magic_Sentinel (I)) then
            Put_Line ("Error: Invalid BWT magic number (expected BWTS)");
            SIO.Close (Input_File);
            return;
         end if;
         In_Pos := In_Pos + 1;
      end loop;
      
      Original_Size := Read_U32 (Header, In_Pos);

      -- Read BWT data (includes sentinel)
      declare
         BWT_Size : constant Stream_Element_Offset := Input_Size - 8;
      begin
         SIO.Read (Input_File, Input_Data (1 .. BWT_Size), Last);
         SIO.Close (Input_File);

         -- Apply inverse BWT (sentinel-based)
         BWT_Sentinel.Inverse_Transform (Input_Data (1 .. BWT_Size), Output_Data, Last);

         -- Write output
         SIO.Create (Output_File, SIO.Out_File, Output_Name);
         if Last >= Output_Data'First then
            SIO.Write (Output_File, Output_Data (1 .. Last));
         end if;
         SIO.Close (Output_File);

         Put_Line ("BWT Inverse complete (sentinel mode):");
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
end BWT_Sentinel_Main;
