--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Numerics.Elementary_Functions;

with Ada.Integer_Text_IO;
with Ada.Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;

procedure AOC.Day_08 is

   type Point is record
      X, Y, Z : Positive;
   end record;

   function Distance (Left, Right : Point) return Float is
     (
        (Ada.Numerics.Elementary_Functions.Sqrt
             (Float (Left.X - Right.X) ** 2 +
              Float (Left.Y - Right.Y) ** 2 +
              Float (Left.Z - Right.Z) ** 2)));

   function Less_X (Left, Right : Point) return Boolean is
     (Left.X < Right.X or else
      (Left.X = Right.X and Left.Y < Right.Y) or else
      (Left.X = Right.X and Left.Y = Right.Y and Left.Z < Right.Z));

   package Point_Sets is new Ada.Containers.Ordered_Sets (Point, Less_X);

   type Circuit is new Natural;

   Last : Circuit := 0;

   package Circuit_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Point,
      Element_Type => Circuit,
      "<"          => Less_X);

   Circuits : Circuit_Maps.Map;

   Points : Point_Sets.Set;

   Total : Long_Long_Integer := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Next : Point;
         Ignore : Character;
      begin
         Ada.Integer_Text_IO.Get (Next.X);
         Ada.Text_IO.Get (Ignore);
         Ada.Integer_Text_IO.Get (Next.Y);
         Ada.Text_IO.Get (Ignore);
         Ada.Integer_Text_IO.Get (Next.Z);
         Points.Insert (Next);
      end;
   end loop;

   for Point of Points loop
      Last := Last + 1;
      Circuits.Insert (Point, Last);
   end loop;

   while Last > 1 loop
      declare
         Min_Dist : Float := Float'Last;
         From, To : Point := Points.First_Element;
      begin
         for Cursor in Points.Iterate loop
            declare
               Left : constant Point := Points (Cursor);
            begin
               for Next in Points.Iterate (Start => Cursor) loop
                  declare
                     Right : constant Point := Points (Next);
                  begin
                     if Circuits (Left) /= Circuits (Right) and then
                       Distance (Left, Right) < Min_Dist
                     then
                        From := Left;
                        To := Right;
                        Min_Dist := Distance (Left, Right);
                     end if;

                     exit when Float (Right.X - Left.X) > Min_Dist;
                  end;
               end loop;
            end;
         end loop;

         declare
            Drop : constant Circuit := Circuits (To);
         begin
            for Item of Circuits when Item = Drop loop
               Item := Circuits (From);
            end loop;
         end;

         Last := Last - 1;

         Ada.Integer_Text_IO.Put (From.X);
         Ada.Text_IO.Put (",");
         Ada.Integer_Text_IO.Put (From.Y);
         Ada.Text_IO.Put (",");
         Ada.Integer_Text_IO.Put (From.Z);
         Ada.Text_IO.Put (" -> ");
         Ada.Integer_Text_IO.Put (To.X);
         Ada.Text_IO.Put (",");
         Ada.Integer_Text_IO.Put (To.Y);
         Ada.Text_IO.Put (",");
         Ada.Integer_Text_IO.Put (To.Z);
         Ada.Text_IO.Put_Line (Last'Image);

         Total := Long_Long_Integer (From.X) * Long_Long_Integer (To.X);
      end;
   end loop;

   Ada.Text_IO.Put_Line (Total'Image);
end AOC.Day_08;
