with Text_IO;

procedure Solution is

   type Piece is record
      Start  : Natural;
      Length : Natural;
      Id     : Natural;
   end record;

   generic
      type Element is private;
   package Generic_Lists is

      type List is limited private;

      type Cursor is private;

      procedure Append (Self : in out List; Value : Element);

      procedure Prepend (Self : in out List; Value : Element);

      function Length (Self : List) return Natural;

      function First (Self : List) return Cursor;

      procedure Next (Self : in out Cursor);

      function Has_Element (Self : Cursor) return Boolean;

      function Get (Self : Cursor) return Element;

   private
      type Node;
      type Node_Access is access Node;

      type Node is record
         Item : Element;
         Next : Node_Access;
      end record;

      type List is record
         Count : Natural;
         Last  : Node_Access;
      end record;

      type Cursor is record
         Count : Natural;
         Node  : Node_Access;
      end record;

   end Generic_Lists;

   -------------------
   -- Generic_Lists --
   -------------------

   package body Generic_Lists is

      procedure Append (Self : in out List; Value : Element) is
         Tail : constant Node_Access := Self.Last;
      begin
         if Tail = null then
            Self.Last := new Node'(Item => Value, Next => null);
            Self.Last.Next := Self.Last;
         else
            Self.Last := new Node'(Item => Value, Next => Tail.Next);
            Tail.Next := Self.Last;
         end if;

         Self.Count := Self.Count + 1;
      end Append;

      function First (Self : List) return Cursor is
      begin
         return (Self.Count, Self.Last.Next);
      end First;

      function Get (Self : Cursor) return Element is
      begin
         return Self.Node.Item;
      end Get;

      function Has_Element (Self : Cursor) return Boolean is
      begin
         return Self.Count /= 0;
      end Has_Element;

      function Length (Self : List) return Natural is
      begin
         return Self.Count;
      end Length;

      procedure Next (Self : in out Cursor) is
      begin
         Self.Node := Self.Node.Next;
         Self.Count := Self.Count - 1;
      end Next;

      procedure Prepend (Self : in out List; Value : Element) is
         Tail : constant Node_Access := Self.Last;
      begin
         if Tail = null then
            Self.Last := new Node'(Item => Value, Next => null);
            Self.Last.Next := Self.Last;
         else
            Tail.Next := new Node'(Item => Value, Next => Tail.Next);
         end if;

         Self.Count := Self.Count + 1;
      end Prepend;

   end Generic_Lists;

   package Piece_Lists is new Generic_Lists (Piece);

   procedure Moved (File : Piece; Result : in out Long_Integer) is
   begin
      for J in File.Start .. File.Start + File.Length - 1 loop
         Result := Result + Long_Integer (J * File.Id);
      end loop;
   end Moved;

begin

   declare
      Busy    : Piece_Lists.List;  --  In reverse order
      Free    : Piece_Lists.List;
      Result  : Long_Integer := 0;
   begin
      declare
         Input   : Text_IO.File_Type;
         Block   : Natural := 0;
         File    : Natural := 0;
         Is_Free : Boolean := False;
         Char    : Character;
         Length  : Natural;
      begin
         Text_IO.Open (Input, Text_IO.In_File, "input.txt");
         while not Text_IO.End_Of_File (Input) loop
            Text_IO.Get (Input, Char);
            Length := Character'Pos (Char) - Character'Pos ('0');
            if Is_Free then
               Piece_Lists.Append (Free, (Block, Length, 0));
            else
               Piece_Lists.Prepend (Busy, (Block, Length, File));
               File := File + 1;
            end if;
            Block := Block + Length;
            Is_Free := not Is_Free;
         end loop;
      end;

      declare
         Space : array (1 .. Piece_Lists.Length (Free)) of Piece;
         A     : Piece_Lists.Cursor := Piece_Lists.First (Busy);
         B     : Piece_Lists.Cursor := Piece_Lists.First (Free);
         File  : Piece := (0, 0, 0);
      begin
         for J in Space'Range loop
            Space (J) := Piece_Lists.Get (B);
            Piece_Lists.Next (B);
         end loop;

         while Piece_Lists.Has_Element (A) loop
            File := Piece_Lists.Get (A);
            Piece_Lists.Next (A);

            for J in Space'Range loop
               if Space (J).Length >= File.Length
                 and Space (J).Start < File.Start
               then
                  Moved ((Space (J).Start, File.Length, File.Id), Result);
                  Space (J).Length := Space (J).Length - File.Length;
                  Space (J).Start := Space (J).Start + File.Length;
                  exit;
               elsif J = Space'Last then
                  Moved (File, Result);
               end if;
            end loop;
         end loop;
      end;

      Text_IO.Put_Line (Long_Integer'Image (Result));
   end;
end Solution;
