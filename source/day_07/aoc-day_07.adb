--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Text_IO;

procedure AOC.Day_07 is

   Log : array (1 .. 150) of String (1 .. 150) :=
     [others => [others => '.']];

   Last : Natural := 0;

   Beam : array (1 .. 150) of Long_Long_Integer := [others => 1];

   Total : Long_Long_Integer := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         Last := Last + 1;
         Log (Last) (Line'Range) := Line;
      end;
   end loop;

   for Line of reverse Log (1 .. Last) loop
      for J in Line'Range loop
         case Line (J) is
            when 'S' =>
               Total := Beam (J);
            when '^' =>
               Beam (J) := Beam (J - 1) + Beam (J + 1);
            when others =>
               null;
         end case;
      end loop;
   end loop;

   Ada.Text_IO.Put_Line (Total'Image);
end AOC.Day_07;
