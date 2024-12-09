with Text_IO;

procedure Solution is

   type Vector is record
      X, Y : Integer;
   end record;

   type Frequency is
     ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
      'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
      's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
      'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
      'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');

   type Square_Map is array (Positive range <>, Positive range <>) of Boolean;

   function "+" (L, R : Vector) return Vector is
   begin
      return (L.X + R.X, L.Y + R.Y);
   end "+";

   function "-" (L, R : Vector) return Vector is
   begin
      return (L.X - R.X, L.Y - R.Y);
   end "-";

   function To_Frequency (Char : Character) return Frequency is
      Result : Frequency;
   begin
      case Char is
         when 'a' .. 'z' =>
            Result :=
              Frequency'Val (Character'Pos (Char) - Character'Pos ('a'));
         when 'A' .. 'Z' =>
            Result :=
              Frequency'Val
                (Frequency'Pos ('A')
                 + Character'Pos (Char) - Character'Pos ('A'));
         when '0' .. '9' =>
            Result :=
              Frequency'Val
                (Frequency'Pos ('0')
                 + Character'Pos (Char) - Character'Pos ('0'));
         when others =>
            raise Constraint_Error;
      end case;

      return Result;
   end To_Frequency;

   function Is_On_Map (Map   : Square_Map; Point : Vector) return Boolean is
   begin
      return Point.X in Map'Range (2) and Point.Y in Map'Range (1);
   end Is_On_Map;

   procedure Put_On_Map
     (Map   : in out Square_Map;
      Point : Vector;
      Count : in out Natural) is
   begin
      if Is_On_Map (Map, Point) and then not Map (Point.Y, Point.X) then
         Map (Point.Y, Point.X) := True;
         Count := Count + 1;
      end if;
   end Put_On_Map;

   -------------------
   -- Generic_Lists --
   -------------------

   generic
      type Element is private;
   package Generic_Lists is

      type List is limited private;

      type Cursor is private;

      procedure Append (Self : in out List; Value : Element);

      function First (Self : List) return Cursor;

      procedure Next (Self : in out Cursor);

      function Has_Element (Self : Cursor) return Boolean;

      function Get (Self : Cursor) return Element;

   private
      type Node is record
         Item : Element;
         Next : Cursor;
      end record;

      type List is record
         First : Cursor;
      end record;

      type Cursor is access Node;

   end Generic_Lists;

   -------------------
   -- Generic_Lists --
   -------------------

   package body Generic_Lists is

      procedure Append (Self : in out List; Value : Element) is
      begin
         Self.First := new Node'(Item => Value, Next => Self.First);
      end Append;

      function First (Self : List) return Cursor is
      begin
         return Self.First;
      end First;

      function Get (Self : Cursor) return Element is
      begin
         return Self.Item;
      end Get;

      function Has_Element (Self : Cursor) return Boolean is
      begin
         return Self /= null;
      end Has_Element;

      procedure Next (Self : in out Cursor) is
      begin
         Self := Self.Next;
      end Next;

   end Generic_Lists;

begin
   declare
      package Vector_Lists is new Generic_Lists (Vector);

      type Vector_List_Array is array (Frequency) of Vector_Lists.List;

      Anthena : Vector_List_Array;
      Input   : Text_IO.File_Type;
      Row     : Natural := 0;
      Result  : Natural := 0;

   begin
      Text_IO.Open (Input, Text_IO.In_File, "input.txt");

      while not Text_IO.End_Of_File (Input) loop
         declare
            Line   : String (1 .. 80);
            Last   : Natural;
         begin
            Text_IO.Get_Line (Input, Line, Last);
            Row := Row + 1;

            for X in 1 .. Last loop
               if Line (X) /= '.' then
                  Vector_Lists.Append
                    (Anthena (To_Frequency (Line (X))), (X, Row));
               end if;
            end loop;
         end;
      end loop;

      declare
         Map : Square_Map (1 .. Row, 1 .. Row) := (others => (others => False));
      begin
         for F in Frequency loop
            declare
               A : Vector_Lists.Cursor := Vector_Lists.First (Anthena (F));
            begin
               while Vector_Lists.Has_Element (A) loop
                  declare
                     B : Vector_Lists.Cursor := A;
                  begin
                     Vector_Lists.Next (B);
                     Put_On_Map (Map, Vector_Lists.Get (A), Result);

                     while Vector_Lists.Has_Element (B) loop
                        declare
                           L : constant Vector := Vector_Lists.Get (A);
                           R : constant Vector := Vector_Lists.Get (B);
                           N : Vector := L - (R - L);
                           M : Vector := R + (R - L);
                        begin
                           while Is_On_Map (Map, N) loop
                              Put_On_Map (Map, N, Result);
                              N := N - (R - L);
                           end loop;

                           while Is_On_Map (Map, M) loop
                              Put_On_Map (Map, M, Result);
                              M := M + (R - L);
                           end loop;

                           Vector_Lists.Next (B);
                        end;
                     end loop;
                  end;
                  Vector_Lists.Next (A);
               end loop;
            end;
         end loop;
      end;

      Text_IO.Put_Line (Natural'Image (Result));
   end;
end Solution;
