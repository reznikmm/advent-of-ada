with Ada.Containers;
with Ada.Containers.Formal_Hashed_Sets;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure Day_18 with SPARK_Mode is

   type Coordinate is range -1 .. 300;
   package Coordinate_IO is new Ada.Text_IO.Integer_IO (Coordinate);
   type Position is array (1 .. 3) of Coordinate;

   function Min (A, B : Position) return Position is
      Result : Position;
   begin
      for J in A'Range loop
         Result (J) := Coordinate'Min (A (J), B (J));
      end loop;
      return Result;
   end Min;

   function Max (A, B : Position) return Position is
      Result : Position;
   begin
      for J in A'Range loop
         Result (J) := Coordinate'Max (A (J), B (J));
      end loop;
      return Result;
   end Max;

   function Hash (V : Position) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (V (1) + 23 * V (2) + 31 * V (3)));

   package Position_Sets is new Ada.Containers.Formal_Hashed_Sets
     (Position, Hash, "=");

   type Position_Array is array (Positive range <>) of Position;

   Lower, Upper : Position;

   function Sides (V : Position) return Position_Array is
      Result : Position_Array (1 .. 6) := (others => Lower);
      Index  : Natural range 0 .. Result'Last := 0;

   begin
      for J in V'Range loop
         pragma Loop_Invariant (Index < 2 * J);
         declare
            Copy : Position := V;
         begin
            if V (J) < Upper (J) then
               Copy (J) := V (J) + 1;
               Index := Index + 1;
               Result (Index) := Copy;
            end if;

            if V (J) > Lower (J) then
               Copy (J) := V (J) - 1;
               Index := Index + 1;
               Result (Index) := Copy;
            end if;
         end;
      end loop;

      return Result (1 .. Index);
   end Sides;

   type Space_Kind is (External, Lava, Internal);

   type Space is array
     (Coordinate range <>, Coordinate range <>, Coordinate range <>) of
       Space_Kind;

   procedure Fill (S : in out Space; Start : Position)
     with Pre => S (Start (1), Start (2), Start (3)) = Internal;

   procedure Fill (S : in out Space; Start : Position) is
   begin
      S (Start (1), Start (2), Start (3)) := External;

      for Cube of Sides (Start) loop
         if S (Cube (1), Cube (2), Cube (3)) = Internal then
            Fill (S, Cube);
         end if;
      end loop;
   end Fill;

   Cubes  : Position_Sets.Set (3500, 3571);
   Result : Natural := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         P_1   : Coordinate;
         P_2   : Coordinate;
         P_3   : Coordinate;
         Comma : Character;
      begin
         Coordinate_IO.Get (P_1);
         Ada.Text_IO.Get (Comma);
         Coordinate_IO.Get (P_2);
         Ada.Text_IO.Get (Comma);
         Coordinate_IO.Get (P_3);

         Position_Sets.Insert (Cubes, (P_1, P_2, P_3));
         Result := Result + 6;
      end;
   end loop;

   Lower := Position_Sets.Element (Cubes, Position_Sets.First (Cubes));
   Upper := Lower;

   for Cube of Cubes loop
      Lower := Min (Lower, Cube);
      Upper := Max (Upper, Cube);
   end loop;

   for V of Lower loop
      V := V - 1;
   end loop;

   for V of Upper loop
      V := V + 1;
   end loop;

   declare
      Lower_1 : constant Coordinate := Lower (1);
      Lower_2 : constant Coordinate := Lower (2);
      Lower_3 : constant Coordinate := Lower (3);
      Upper_1 : constant Coordinate := Upper (1);
      Upper_2 : constant Coordinate := Upper (2);
      Upper_3 : constant Coordinate := Upper (3);

      S : Space (Lower_1 .. Upper_1, Lower_2 .. Upper_2, Lower_3 .. Upper_3) :=
        (others => (others => (others => Internal)));
   begin
      for Cube of Cubes loop
         S (Cube (1), Cube (2), Cube (3)) := Lava;
      end loop;

      Fill (S, Lower);

      for Cube of Cubes loop
         for Side of Sides (Cube) loop
            if S (Side (1), Side (2), Side (3)) /= External then
               Result := Result - 1;
            end if;
         end loop;
      end loop;
   end;

   Ada.Integer_Text_IO.Put (Result);
end Day_18;
