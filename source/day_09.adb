pragma Ada_2022;
pragma Extensions_Allowed (On);

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Hashed_Sets;

procedure Day_09 is
   type Position is array (1 .. 2) of Integer;
   function Hash (V : Position) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (V (1) + 123 * V (2)));

   package Position_Sets is new Ada.Containers.Hashed_Sets
     (Position, Hash, "=");

   type Direction is (Left, Right, Up, Down);

   function To_Direction (C : Character) return Direction is
     (case C is
         when 'L' => Left,
         when 'R' => Right,
         when 'U' => Up,
         when 'D' => Down,
         when others => raise Constraint_Error);

   procedure Step (Head : in out Position; Dir : Direction) is
   begin
      case Dir is
         when Left => Head (1) := Head (1) - 1;
         when Right => Head (1) := Head (1) + 1;
         when Up => Head (2) := Head (2) + 1;
         when Down => Head (2) := Head (2) - 1;
      end case;
   end Step;

   procedure Step
     (Head : Position;
      Tail : in out Position)
   is
      function Sign (Left : Integer) return Integer is
        (if Left < 0 then -1 else 1);
   begin
      for J in 1 .. 2 loop
         if Head (3 - J) = Tail (3 - J) and then
           abs (Head (J) - Tail (J)) > 1
         then
            Tail (J) := Head (J) - Sign (Head (J) - Tail (J));
            return;
         end if;
      end loop;

      if (for all J in 1 .. 2 => Head (J) /= Tail (J)) and then
        (for some J in 1 .. 2 => abs (Head (J) - Tail (J)) > 1)
      then
         for J in 1 .. 2 loop
            Tail (J) := Tail (J) + Sign (Head (J) - Tail (J));
         end loop;
      end if;
   end Step;

   Visited : Position_Sets.Set;

   Rope : array (1 .. 10) of Position := [others => [0, 0]];
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Char : Character;
         Count : Integer;
         Dir   : Direction;
      begin
         Ada.Text_IO.Get (Char);
         Ada.Integer_Text_IO.Get (Count);
         Dir := To_Direction (Char);
         for J in 1 .. Count loop
            Step (Rope (1), Dir);
            for K in 2 .. Rope'Last loop
               Step (Rope (K - 1), Rope (K));
            end loop;
            Ada.Text_IO.Put_Line (Rope (Rope'Last)'Image);
            Visited.Include (Rope (Rope'Last));
         end loop;
      end;
   end loop;

   Ada.Integer_Text_IO.Put (Integer (Visited.Length));
end Day_09;
