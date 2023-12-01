with Ada.Text_IO;
with Ada.Containers.Formal_Hashed_Sets;

procedure Day_09 with SPARK_Mode is
   package Count_IO is new Ada.Text_IO.Integer_IO (Ada.Containers.Count_Type);

   subtype Coordinate is Integer range -9_999 .. 9_999;
   type Position is array (1 .. 2) of Coordinate;
   type Step is array (1 .. 2) of Coordinate range -1 .. 1;
   subtype Safe is Integer range Coordinate'First + 1 .. Coordinate'Last - 1;

   function "+" (Left : Position; Right : Step) return Position
     with Pre => (for all C of Left => C in Safe);

   subtype Step_Encoding is Character
     with Static_Predicate => Step_Encoding in 'L' | 'R' | 'U' | 'D';

   function To_Step (Char : Character) return Step
     with Pre => Char in Step_Encoding;

   procedure Move_Tail
     (Head : Position;
      Tail : in out Position)
     with Pre => (for all C of Tail => C in Safe);
   --  Drag Tail after Head

   function "+" (Left : Position; Right : Step) return Position is
     (Left (1) + Right (1), Left (2) + Right (2));

   function To_Step (Char : Character) return Step is
     (case Char is
         when 'L' => (-1, 0),
         when 'R' => (1, 0),
         when 'U' => (0, 1),
         when 'D' => (0, -1),
         when others => raise Constraint_Error);

   procedure Move_Tail
     (Head : Position;
      Tail : in out Position)
   is
      function Sign (Left : Integer) return Integer is
        (if Left < 0 then -1 elsif Left > 0 then 1 else 0);

      Change : Step;
   begin
      if (for some J in 1 .. 2 => Head (J) - Tail (J) not in -1 .. 1) then
         Change := (Sign (Head (1) - Tail (1)), Sign (Head (2) - Tail (2)));
         Tail := Tail + Change;
      end if;
   end Move_Tail;

   function Hash (V : Position) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Ada.Containers.Hash_Type'Mod (V (1))
        + 123 * Ada.Containers.Hash_Type'Mod (V (2));
   end Hash;

   package Position_Sets is new Ada.Containers.Formal_Hashed_Sets
     (Position, Hash, "=");

   Visited : Position_Sets.Set (Capacity => 5000, Modulus => 5003);

   Rope : array (1 .. 10) of Position := (others => (0, 0));
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         use type Ada.Containers.Count_Type;

         Char  : Character;
         Count : Ada.Containers.Count_Type;
      begin
         Ada.Text_IO.Get (Char);
         Count_IO.Get (Count);

         if Char not in Step_Encoding or else
           Count > Visited.Capacity - Position_Sets.Length (Visited)
         then
            Ada.Text_IO.Put_Line ("Invalid input");
            return;
         end if;

         for J in 1 .. Count loop
            pragma Loop_Invariant
              (Position_Sets.Length (Visited) <
                   Visited.Capacity - Ada.Containers.Count_Type (Count - J));

            if (for some Knot of Rope =>
                 (for some Coordinate of Knot => Coordinate not in Safe))
            then
               Ada.Text_IO.Put_Line ("Out of bounds");
               return;
            end if;

            Rope (1) := Rope (1) + To_Step (Char);

            for Knot in 2 .. Rope'Last loop
               Move_Tail (Head => Rope (Knot - 1), Tail => Rope (Knot));
            end loop;

            Position_Sets.Include (Visited, Rope (Rope'Last));
         end loop;
      end;
   end loop;

   Count_IO.Put (Position_Sets.Length (Visited));
end Day_09;
