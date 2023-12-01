with Ada.Text_IO;
with Ada.Integer_Text_IO;
--  with Ada.Containers.Vectors;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Day_22 is

   Size : constant := 50;

   type Cube_Face is range 1 .. 6;
   --    1       1
   --  2 3 4   6 2 3 4
   --    5       5
   ---   6
   --  type Edge is range 1 .. 12;

   type Direction is (Right, Down, Left, Up);
   type Rotation is (None, Clockwise, Upside_Down, Counterclockwise);
   NO : constant Rotation := None;
   CW : constant Rotation := Clockwise;
   CC : constant Rotation := Counterclockwise;
   UD : constant Rotation := Upside_Down;

   --  function "+" (L, R : Rotation) return Rotation is
   --     (Rotation'Val ((Rotation'Pos (L) + Rotation'Pos (R)) mod 4));

   --  function "-" (L, R : Rotation) return Rotation is
   --     (Rotation'Val ((Rotation'Pos (L) - Rotation'Pos (R)) mod 4));

   function "+" (L : Direction; R : Rotation) return Direction is
      (Direction'Val ((Direction'Pos (L) + Rotation'Pos (R)) mod 4));

   --  function "-" (L : Direction; R : Rotation) return Direction is
   --     (Direction'Val ((Direction'Pos (L) - Rotation'Pos (R)) mod 4));

   type Jump is record
      Face : Cube_Face;
      Rot  : Rotation;
   end record;

   type Jump_Array is array (Direction) of Jump;

   type Cube_Position is record
      Face : Cube_Face;
      Rot  : Rotation;
   end record;

   type Position is record
      X, Y : Positive;
   end record;

   function "*" (L : Position; R : Rotation) return Position is
     (case R is
         when NO => L,
         when CW => (Size - L.Y + 1, L.X),
         when UD => (Size - L.X + 1, Size - L.Y + 1),
         when CC => (L.Y, Size - L.X + 1));

   type Wall_Map is array (1 .. Size, 1 .. Size) of Boolean;

   type Part is record
      S     : Cube_Position;
      Wall  : Wall_Map := (others => (others => False));
      Pos   : Position;
      Jumps : Jump_Array;
   end record;

   --  type Square_Set is array (Square) of Boolean;
   type Cube_Map is array (Cube_Face) of Part;

   procedure Step
     (Map   : Cube_Map;
      Face  : in out Cube_Face;
      Pos   : in out Position;
      Steps : in out Natural;
      Dir   : in out Direction)
   is
      Saved_Pos : Position;
      Saved_Face : Cube_Face;
      Saved_Dir : Direction;
   begin
      for J in 1 .. Steps loop
         Saved_Pos := Pos;
         Saved_Face := Face;
         Saved_Dir := Dir;
         case Dir is
            when Right =>
               if Pos.X = Size then
                  declare
                     F : constant Cube_Face := Map (Face).Jumps (Dir).Face;
                     R : constant Rotation := Map (Face).Jumps (Dir).Rot;
                  begin
                     Face := F;
                     Pos.X := 1;
                     Pos := Pos * R;
                     Dir := Dir + R;
                  end;
               else
                  Pos.X := Pos.X + 1;
               end if;
            when Left =>
               if Pos.X = 1 then
                  declare
                     F : constant Cube_Face := Map (Face).Jumps (Dir).Face;
                     R : constant Rotation := Map (Face).Jumps (Dir).Rot;
                  begin
                     Face := F;
                     Pos.X := Size;
                     Pos := Pos * R;
                     Dir := Dir + R;
                  end;
               else
                  Pos.X := Pos.X - 1;
               end if;
            when Down =>
               if Pos.Y = Size then
                  declare
                     F : constant Cube_Face := Map (Face).Jumps (Dir).Face;
                     R : constant Rotation := Map (Face).Jumps (Dir).Rot;
                  begin
                     Face := F;
                     Pos.Y := 1;
                     Pos := Pos * R;
                     Dir := Dir + R;
                  end;
               else
                  Pos.Y := Pos.Y + 1;
               end if;
            when Up =>
               if Pos.Y = 1 then
                  declare
                     F : constant Cube_Face := Map (Face).Jumps (Dir).Face;
                     R : constant Rotation := Map (Face).Jumps (Dir).Rot;
                  begin
                     Face := F;
                     Pos.Y := Size;
                     Pos := Pos * R;
                     Dir := Dir + R;
                  end;
               else
                  Pos.Y := Pos.Y - 1;
               end if;
         end case;

         if Map (Face).Wall (Pos.X, Pos.Y) then
            Face := Saved_Face;
            Pos := Saved_Pos;
            Dir := Saved_Dir;
            exit;
         end if;
      end loop;
      Steps := 0;
   end Step;

   type Cube_Face_Array is array (Positive range <>) of Cube_Face;

   procedure Read
     (Map   : in out Cube_Map;
      Row   : in out Positive;
      Faces : Cube_Face_Array) is
   begin
      for Face of Faces loop
         Map (Face).S := (Face, None);
      end loop;

      for Y in 1 .. Size loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line;
            Pos  : Positive := Line'First;
         begin
            Row := Row + 1;
            while Line (Pos) = ' ' loop
               Pos := Pos + 1;
            end loop;
            for Face of Faces loop
               for X in 1 .. Size loop
                  Map (Face).Wall (X, Y) := Line (Pos) = '#';
                  Map (Face).Pos.X := Pos;
                  Map (Face).Pos.Y := Row - 1;
                  Pos := Pos + 1;
               end loop;
            end loop;
         end;
      end loop;
   end Read;

   --  (Right, Down, Left, Up);
   Map  : Cube_Map :=
     (1 => (Jumps => ((2, NO), (3, NO), (4, UD), (6, CW)), others => <>),
      2 => (Jumps => ((5, UD), (3, CW), (1, NO), (6, NO)), others => <>),
      3 => (Jumps => ((2, CC), (5, NO), (4, CC), (1, NO)), others => <>),
      4 => (Jumps => ((5, NO), (6, NO), (1, UD), (3, CW)), others => <>),
      5 => (Jumps => ((2, UD), (6, CW), (4, NO), (3, NO)), others => <>),
      6 => (Jumps => ((5, CC), (2, NO), (1, CC), (4, NO)), others => <>));

   --  Map  : Cube_Map :=
   --    (5 => (Jumps => ((2, UD), (6, NO), (4, CC), (3, UD)), others => <>),
   --     3 => (Jumps => ((4, NO), (1, UD), (2, CW), (5, UD)), others => <>),
   --     4 => (Jumps => ((6, NO), (1, CC), (3, NO), (5, CW)), others => <>),
   --     6 => (Jumps => ((2, CW), (1, NO), (4, NO), (5, NO)), others => <>),
   --     1 => (Jumps => ((2, NO), (3, UD), (4, CW), (6, NO)), others => <>),
   --     2 => (Jumps => ((5, UD), (3, CC), (1, NO), (6, CW)), others => <>));
   --
   Start : Cube_Face := 5;
   Row   : Positive := 1;
begin
   Read (Map, Row, (1, 2));
   Read (Map, Row, (1 => 3));
   Read (Map, Row, (4, 5));
   Read (Map, Row, (1 => 6));

   declare
      Line : constant String := Ada.Text_IO.Get_Line;
   begin
      pragma Assert (Line = "");
   end;

   declare
      Dir  : Direction := Right;
      Pos  : Position := (1, 1);
      Zero : constant Positive := Character'Pos ('0');
      Path : constant String := Ada.Text_IO.Get_Line;
      Number : Natural := 0;
   begin
      for Item of Path loop
         case Item is
            when '0' .. '9' =>
               Number := Number * 10 + Character'Pos (Item) - Zero;
            when 'L' =>
               Step (Map, Start, Pos, Number, Dir);
               Dir := (if Dir = Direction'First then Direction'Last
                       else Direction'Pred (Dir));
               Ada.Text_IO.Put (Pos.X'Image);
               Ada.Text_IO.Put (Pos.Y'Image);
               Ada.Text_IO.Put (Dir'Image);
               Ada.Text_IO.New_Line;
            when 'R' =>
               Step (Map, Start, Pos, Number, Dir);
               Dir := (if Dir = Direction'Last then Direction'First
                       else Direction'Succ (Dir));
               Ada.Text_IO.Put (Pos.X'Image);
               Ada.Text_IO.Put (Pos.Y'Image);
               Ada.Text_IO.Put (Dir'Image);
               Ada.Text_IO.New_Line;
            when others =>
               raise Constraint_Error;
         end case;
      end loop;

      Step (Map, Start, Pos, Number, Dir);
      Ada.Text_IO.Put (Positive'Image (Map (Start).Pos.X + Pos.X - Size));
      Ada.Text_IO.Put (Positive'Image (Map (Start).Pos.Y + Pos.Y - Size));
      Ada.Text_IO.Put (Dir'Image);
      Ada.Text_IO.New_Line;
      Ada.Integer_Text_IO.Put
        (1000 * (Map (Start).Pos.Y + Pos.Y  - Size) +
         4 * (Map (Start).Pos.X + Pos.X  - Size) +
         Direction'Pos (Dir));
   end;
end Day_22;
