-- ***************************************************************************
--               Huffman Compression Package Body
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

with Ada.Unchecked_Deallocation;

package body Huffman is

   type Frequency_Array is array (Stream_Element) of Natural;

   type Huffman_Node;
   type Huffman_Node_Access is access Huffman_Node;

   type Huffman_Node is record
      Symbol    : Stream_Element;
      Frequency : Natural;
      Is_Leaf   : Boolean;
      Left      : Huffman_Node_Access;
      Right     : Huffman_Node_Access;
   end record;

   type Bit_Array is array (Natural range <>) of Boolean;
   pragma Pack (Bit_Array);

   type Code_Entry is record
      Bits   : Bit_Array (0 .. 255);
      Length : Natural := 0;
   end record;

   type Code_Table is array (Stream_Element) of Code_Entry;

   procedure Free is new Ada.Unchecked_Deallocation (Huffman_Node, Huffman_Node_Access);

   type Priority_Queue is array (1 .. 256) of Huffman_Node_Access;
   type Queue_Size is range 0 .. 256;

   -- Insert node into priority queue maintaining min-heap property.
   -- Nodes with lower frequency bubble up to root.
   procedure Insert_Node (Queue : in out Priority_Queue; Size : in out Queue_Size; Node : Huffman_Node_Access) is
      I : Queue_Size := Size + 1;
   begin
      while I > 1 and then Queue (Integer (I / 2)).Frequency > Node.Frequency loop
         Queue (Integer (I)) := Queue (Integer (I / 2));
         I := I / 2;
      end loop;
      Queue (Integer (I)) := Node;
      Size := Size + 1;
   end Insert_Node;

   -- Remove and return minimum-frequency node from priority queue.
   -- Restores min-heap property after removal.
   procedure Extract_Min (Queue : in out Priority_Queue; Size : in out Queue_Size; Node : out Huffman_Node_Access) is
      Last : Huffman_Node_Access;
      I, Child : Queue_Size;
   begin
      if Size = 0 then
         Node := null;
         return;
      end if;

      Node := Queue (1);
      Last := Queue (Integer (Size));
      Size := Size - 1;
      I := 1;

      loop
         Child := I * 2;
         exit when Child > Size;

         if Child < Size and then Queue (Integer (Child + 1)).Frequency < Queue (Integer (Child)).Frequency then
            Child := Child + 1;
         end if;

         exit when Last.Frequency <= Queue (Integer (Child)).Frequency;

         Queue (Integer (I)) := Queue (Integer (Child));
         I := Child;
      end loop;

      if Size > 0 then
         Queue (Integer (I)) := Last;
      end if;
   end Extract_Min;

   procedure Build_Frequency_Table
     (Input       : Stream_Element_Array;
      Frequencies : out Frequency_Array)
   is
   begin
      Frequencies := (others => 0);
      for I in Input'Range loop
         Frequencies (Input (I)) := Frequencies (Input (I)) + 1;
      end loop;
   end Build_Frequency_Table;

   -- Build optimal Huffman tree from frequency table.
   -- Repeatedly merges two lowest-frequency nodes until single tree.
   function Build_Huffman_Tree (Frequencies : Frequency_Array) return Huffman_Node_Access is
      Queue : Priority_Queue;
      Size  : Queue_Size := 0;
      Node1, Node2, Parent : Huffman_Node_Access;
   begin
      for I in Frequencies'Range loop
         if Frequencies (I) > 0 then
            Insert_Node (Queue, Size,
                        new Huffman_Node'(Symbol    => I,
                                         Frequency => Frequencies (I),
                                         Is_Leaf   => True,
                                         Left      => null,
                                         Right     => null));
         end if;
      end loop;

      if Size = 0 then
         return null;
      end if;

      if Size = 1 then
         Extract_Min (Queue, Size, Node1);
         return new Huffman_Node'(Symbol    => 0,
                                 Frequency => Node1.Frequency,
                                 Is_Leaf   => False,
                                 Left      => Node1,
                                 Right     => null);
      end if;

      while Size > 1 loop
         Extract_Min (Queue, Size, Node1);
         Extract_Min (Queue, Size, Node2);

         Parent := new Huffman_Node'(Symbol    => 0,
                                    Frequency => Node1.Frequency + Node2.Frequency,
                                    Is_Leaf   => False,
                                    Left      => Node1,
                                    Right     => Node2);
         Insert_Node (Queue, Size, Parent);
      end loop;

      Extract_Min (Queue, Size, Parent);
      return Parent;
   end Build_Huffman_Tree;

   procedure Generate_Codes
     (Node   : Huffman_Node_Access;
      Codes  : in out Code_Table;
      Code   : Bit_Array;
      Depth  : Natural)
   is
   begin
      if Node = null then
         return;
      end if;

      if Node.Is_Leaf then
         Codes (Node.Symbol).Bits (0 .. Depth - 1) := Code (0 .. Depth - 1);
         Codes (Node.Symbol).Length := Depth;
      else
         if Node.Left /= null then
            declare
               Left_Code : Bit_Array (0 .. 255) := Code;
            begin
               Left_Code (Depth) := False;
               Generate_Codes (Node.Left, Codes, Left_Code, Depth + 1);
            end;
         end if;

         if Node.Right /= null then
            declare
               Right_Code : Bit_Array (0 .. 255) := Code;
            begin
               Right_Code (Depth) := True;
               Generate_Codes (Node.Right, Codes, Right_Code, Depth + 1);
            end;
         end if;
      end if;
   end Generate_Codes;

   -- Recursively deallocate all nodes in Huffman tree.
   -- Post-order traversal ensures children freed before parent.
   procedure Free_Tree (Node : in out Huffman_Node_Access) is
   begin
      if Node /= null then
         Free_Tree (Node.Left);
         Free_Tree (Node.Right);
         Free (Node);
      end if;
   end Free_Tree;

   -- Serialize Huffman tree to output stream.
   -- Uses markers: 0=internal node, 1=leaf, 2=null.
   procedure Write_Tree (Node : Huffman_Node_Access; Output : in out Stream_Element_Array; Index : in out Stream_Element_Offset) is
   begin
      if Node = null then
         Output (Index) := 2;  -- Marker for null
         Index := Index + 1;
         return;
      end if;

      if Node.Is_Leaf then
         Output (Index) := 1;  -- Marker for leaf
         Index := Index + 1;
         Output (Index) := Node.Symbol;
         Index := Index + 1;
      else
         Output (Index) := 0;  -- Marker for internal node
         Index := Index + 1;
         Write_Tree (Node.Left, Output, Index);
         Write_Tree (Node.Right, Output, Index);
      end if;
   end Write_Tree;

   -- Deserialize Huffman tree from input stream.
   -- Reconstructs tree structure from markers and leaf symbols.
   procedure Read_Tree (Input : Stream_Element_Array; Index : in out Stream_Element_Offset; Node : out Huffman_Node_Access) is
      Marker : Stream_Element;
   begin
      if Index > Input'Last then
         Node := null;
         return;
      end if;

      Marker := Input (Index);
      Index := Index + 1;

      if Marker = 2 then
         -- Null node
         Node := null;
      elsif Marker = 1 then
         -- Leaf node
         Node := new Huffman_Node'(Symbol    => Input (Index),
                                  Frequency => 0,
                                  Is_Leaf   => True,
                                  Left      => null,
                                  Right     => null);
         Index := Index + 1;
      else
         -- Internal node
         Node := new Huffman_Node'(Symbol    => 0,
                                  Frequency => 0,
                                  Is_Leaf   => False,
                                  Left      => null,
                                  Right     => null);
         Read_Tree (Input, Index, Node.Left);
         Read_Tree (Input, Index, Node.Right);
      end if;
   end Read_Tree;

   procedure Compress
     (Input  : in  Stream_Element_Array;
      Output : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Frequencies : Frequency_Array;
      Tree        : Huffman_Node_Access;
      Codes       : Code_Table;
      Initial_Code : Bit_Array (0 .. 255) := (others => False);
      Bit_Buffer  : Stream_Element := 0;
      Bit_Pos     : Natural := 0;
      Write_Pos   : Stream_Element_Offset;
      Tree_Start  : Stream_Element_Offset;
      Data_Start  : Stream_Element_Offset;
   begin
      if Input'Length = 0 then
         Last := Output'First - 1;
         return;
      end if;

      Build_Frequency_Table (Input, Frequencies);
      Tree := Build_Huffman_Tree (Frequencies);

      if Tree = null then
         Output (Output'First) := 0;
         Output (Output'First + 1 .. Output'First + Input'Length) := Input;
         Last := Output'First + Input'Length;
         return;
      end if;

      Codes := (others => (Bits => (others => False), Length => 0));
      Generate_Codes (Tree, Codes, Initial_Code, 0);

      Output (Output'First) := 1;  -- Flag: 1 = compressed
      Tree_Start := Output'First + 1;
      Write_Pos := Tree_Start;
      Write_Tree (Tree, Output, Write_Pos);
      Data_Start := Write_Pos;

      Output (Write_Pos) := Stream_Element (Input'Length / 16777216 mod 256);
      Output (Write_Pos + 1) := Stream_Element (Input'Length / 65536 mod 256);
      Output (Write_Pos + 2) := Stream_Element (Input'Length / 256 mod 256);
      Output (Write_Pos + 3) := Stream_Element (Input'Length mod 256);
      Write_Pos := Write_Pos + 4;

      for I in Input'Range loop
         declare
            Code : Code_Entry renames Codes (Input (I));
         begin
            for J in 0 .. Code.Length - 1 loop
               if Code.Bits (J) then
                  Bit_Buffer := Bit_Buffer or Stream_Element (2 ** (7 - Bit_Pos));
               end if;

               Bit_Pos := Bit_Pos + 1;
               if Bit_Pos = 8 then
                  Output (Write_Pos) := Bit_Buffer;
                  Write_Pos := Write_Pos + 1;
                  Bit_Buffer := 0;
                  Bit_Pos := 0;
               end if;
            end loop;
         end;
      end loop;

      if Bit_Pos > 0 then
         Output (Write_Pos) := Bit_Buffer;
         Write_Pos := Write_Pos + 1;
      end if;

      Last := Write_Pos - 1;
      Free_Tree (Tree);

      if Last >= Output'First + Input'Length then
         Output (Output'First) := 0;
         Output (Output'First + 1 .. Output'First + Input'Length) := Input;
         Last := Output'First + Input'Length;
      end if;
   end Compress;

   procedure Decompress
     (Input  : in  Stream_Element_Array;
      Output : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Flag : Stream_Element;
      Tree : Huffman_Node_Access;
      Index : Stream_Element_Offset;
      Current : Huffman_Node_Access;
      Output_Size : Natural;
      Bit_Byte : Stream_Element;
      Bit_Mask : Stream_Element;
      Output_Count : Natural := 0;
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

      Index := Input'First + 1;
      Read_Tree (Input, Index, Tree);

      if Tree = null then
         raise Compression_Error with "Invalid tree structure";
      end if;

      if Index + 3 > Input'Last then
         raise Compression_Error with "Invalid compressed data";
      end if;

      Output_Size := Natural (Input (Index)) * 16777216 +
                    Natural (Input (Index + 1)) * 65536 +
                    Natural (Input (Index + 2)) * 256 +
                    Natural (Input (Index + 3));
      Index := Index + 4;

      Current := Tree;

      for I in Index .. Input'Last loop
         Bit_Byte := Input (I);
         for Bit in 0 .. 7 loop
            exit when Output_Count >= Output_Size;

            Bit_Mask := Stream_Element (2 ** (7 - Bit));

            if (Bit_Byte and Bit_Mask) /= 0 then
               Current := Current.Right;
            else
               Current := Current.Left;
            end if;

            if Current.Is_Leaf then
               Output (Output'First + Stream_Element_Offset (Output_Count)) := Current.Symbol;
               Output_Count := Output_Count + 1;
               Current := Tree;
            end if;
         end loop;
      end loop;

      Last := Output'First + Stream_Element_Offset (Output_Count) - 1;
      Free_Tree (Tree);
   end Decompress;

   -- Estimate maximum output buffer size needed for compression.
   -- Returns input size plus overhead for tree and metadata.
   function Estimate_Compressed_Size (Input_Size : Stream_Element_Offset) return Stream_Element_Offset is
   begin
      return Input_Size + 1024;  -- Add overhead for header
   end Estimate_Compressed_Size;

end Huffman;
