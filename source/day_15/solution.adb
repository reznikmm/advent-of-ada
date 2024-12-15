with Text_IO;

procedure Solution is

   type Vector is record
      X, Y : Integer;
   end record;

   type Direction is (Up, Down, Left, Right);

   Size   : constant := 50;
   Input  : Text_IO.File_Type;
   Result : Integer := 0;
   Robot  : Vector;

   Map : array
     (0 .. Positive (Size - 1),
      0 .. Positive (2 * Size - 1)) of Character;

   Step : constant array (Direction) of Vector :=
     ((0, -1), (0, 1), (-1, 0), (1, 0));

   function "+" (Left, Right : Vector) return Vector is
   begin
      return (Left.X + Right.X, Left.Y + Right.Y);
   end "+";

   function Can_Free_Cell (V : Vector; Dir : Direction) return Boolean is
      Dest : constant Vector := V + Step (Dir);
   begin
      case Map (V.Y, V.X) is
         when '#' =>
            return False;
         when '[' =>
            return Can_Free_Cell (Dest, Dir) and
              (Dir in Left .. Right
               or else Can_Free_Cell (Dest + Step (Right), Dir));
         when ']' =>
            return Can_Free_Cell (Dest, Dir) and
              (Dir in Left .. Right
               or else Can_Free_Cell (Dest + Step (Left), Dir));
         when others =>
            return True;
      end case;
   end Can_Free_Cell;

   procedure Free_Cell (V : Vector; Dir : Direction) is
      Dest : Vector := V + Step (Dir);
   begin
      case Map (V.Y, V.X) is
         when '#' =>
            raise Program_Error;
         when '[' =>
            Free_Cell (Dest, Dir);
            Map (Dest.Y, Dest.X) := Map (V.Y, V.X);
            Map (V.Y, V.X) := '.';
            if Dir in Up .. Down then
               Dest := Dest + Step (Right);
               Free_Cell (Dest, Dir);
               Map (Dest.Y, Dest.X) := Map (V.Y, V.X + 1);
               Map (V.Y, V.X + 1) := '.';
            end if;
         when ']' =>
            Free_Cell (Dest, Dir);
            Map (Dest.Y, Dest.X) := Map (V.Y, V.X);
            Map (V.Y, V.X) := '.';
            if Dir in Up .. Down then
               Dest := Dest + Step (Left);
               Free_Cell (Dest, Dir);
               Map (Dest.Y, Dest.X) := Map (V.Y, V.X - 1);
               Map (V.Y, V.X - 1) := '.';
            end if;
         when others =>
            null;
      end case;
   end Free_Cell;

   package Integer_IO is new Text_IO.Integer_IO (Integer);

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   for Y in Map'Range (1) loop
      for X in Map'Range (1) loop
         Text_IO.Get (Input, Map (Y, 2 * X));
         if Map (Y, 2 * X) = '@' then
            Map (Y, 2 * X) := '.';
            Map (Y, 2 * X + 1) := '.';
            Robot := (2 * X, Y);
         elsif Map (Y, 2 * X) = '#' then
            Map (Y, 2 * X + 1) := '#';
         elsif Map (Y, 2 * X) = 'O' then
            Map (Y, 2 * X) := '[';
            Map (Y, 2 * X + 1) := ']';
         else
            Map (Y, 2 * X + 1) := Map (Y, 2 * X);
         end if;
      end loop;
   end loop;

   while not Text_IO.End_Of_File (Input) loop
      declare
         Raw : Character;
         Dir : Direction;
      begin
         Text_IO.Get (Input, Raw);
         case Raw is
            when '^' => Dir := Up;
            when 'v' => Dir := Down;
            when '<' => Dir := Left;
            when '>' => Dir := Right;
            when others => raise Constraint_Error;
         end case;

         if Can_Free_Cell (Robot + Step (Dir), Dir) then
            Free_Cell (Robot + Step (Dir), Dir);
            Robot := Robot + Step (Dir);
         end if;
      end;
   end loop;

   for Y in Map'Range (1) loop
      for X in Map'Range (2) loop
         Text_IO.Put (Map (Y, X));
         if Map (Y, X) = '[' then
            Result := Result + 100 * Y + X;
         end if;
      end loop;
      Text_IO.New_Line;
   end loop;

   Integer_IO.Put (Result);
end Solution;
