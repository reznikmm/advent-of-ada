--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Text_IO;

procedure AOC.Day_04 is

   Size : constant := 139;
   Grid : array (1 .. Size, 1 .. Size) of Boolean;

   function Has (Row, Column : Natural) return Boolean is
     (Row in Grid'Range (1) and then
      Column in Grid'Range (2) and then
      Grid (Row, Column));

   function Can_Access (Row, Column : Positive) return Boolean;

   ----------------
   -- Can_Access --
   ----------------

   function Can_Access (Row, Column : Positive) return Boolean is
      Count : Natural := 0;
   begin
      for J in Row - 1 .. Row + 1 loop
         for K in Column - 1 .. Column + 1
         when not (Row = J and Column = K)
         loop
            Count := @ + (if Has (J, K) then 1 else 0);
         end loop;
      end loop;

      return Has (Row, Column) and Count < 4;
   end Can_Access;

   Last : Natural := 0;
   Total : Natural := 0;
   Again : Boolean := True;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         Last := Last + 1;
         for J in Line'Range loop
            Grid (Last, J) := Line (J) = '@';
         end loop;
      end;
   end loop;

   while Again loop
      Again := False;
      for J in Grid'Range (1) loop
         for K in Grid'Range (2) loop
            if Has (J, K) and Can_Access (J, K) then
               Total := @ + 1;
               Again := True;
               Grid (J, K) := False;
            end if;
         end loop;
      end loop;
   end loop;

   Ada.Text_IO.Put_Line (Total'Image);
end AOC.Day_04;
