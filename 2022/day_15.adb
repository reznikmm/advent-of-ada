with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers.Doubly_Linked_Lists;

procedure Day_15 is
   type Position is record
      X, Y : Integer;
   end record;

   function Distance (A, B : Position) return Natural is
      (abs (A.X - B.X) + abs (A.Y - B.Y));

   type Pair is record
      Sensor : Position;
      Beacon : Position;
   end record;

   procedure Read_Pair (V : out Pair) is
      Line  : String (1 .. 80);
      Last  : Natural;
      Pos   : Natural;
      Colon : Natural;
   begin
      Ada.Text_IO.Get_Line (Line, Last);
      pragma Assert (Last < Line'Last);
      Colon := Ada.Strings.Fixed.Index (Line, ":");
      Ada.Integer_Text_IO.Get (Line (13 .. Last), V.Sensor.X, Pos);
      Ada.Integer_Text_IO.Get (Line (Pos + 5 .. Colon - 1), V.Sensor.Y, Pos);
      Ada.Integer_Text_IO.Get (Line (Colon + 25 .. Last), V.Beacon.X, Pos);
      Ada.Integer_Text_IO.Get (Line (Pos + 5 .. Last), V.Beacon.Y, Pos);
   end Read_Pair;

   package Pair_Lists is new Ada.Containers.Doubly_Linked_Lists (Pair);

   Pairs : Pair_Lists.List;

   subtype Target is Natural range 0 .. 4000000;

   type Position_Array is array (Positive range <>) of Position;

   function Contour (S : Pair) return Position_Array is
      Dist : constant Positive := Distance (S.Sensor, S.Beacon);
      Result : Position_Array (1 .. 4 * (Dist + 1));
   begin
      for J in 0 .. Dist loop
         Result (4 * J + 1) := (S.Sensor.X + J, S.Sensor.Y - Dist - 1 + J);
         Result (4 * J + 2) := (S.Sensor.X + Dist + 1 - J, S.Sensor.Y + J);
         Result (4 * J + 3) := (S.Sensor.X - J, S.Sensor.Y + Dist + 1 - J);
         Result (4 * J + 4) := (S.Sensor.X - Dist - 1 + J, S.Sensor.Y - J);
      end loop;

      return Result;
   end Contour;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Item : Pair;
      begin
         Read_Pair (Item);
         Pairs.Append (Item);
      end;
   end loop;

   for S of Pairs loop
      for Pos of Contour (S) loop
         if Pos.X in Target and then Pos.Y in Target then
            if (for all X of Pairs =>
                 Distance (X.Sensor, Pos) > Distance (X.Sensor, X.Beacon))
            then
               Ada.Integer_Text_IO.Put (Pos.X);
               Ada.Integer_Text_IO.Put (Pos.Y);
               Ada.Long_Long_Integer_Text_IO.Put
                 (Long_Long_Integer (Pos.Y) +
                      Long_Long_Integer (Pos.X) * 4000000);
               Ada.Text_IO.New_Line;
               return;
            end if;
         end if;
      end loop;
   end loop;
end Day_15;
