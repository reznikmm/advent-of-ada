--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Text_IO;

procedure AOC.Day_06 is

   Number  : array (1 .. 5000) of Long_Long_Integer := [others => 0];

   procedure Calculate (Line : String; Total : out Long_Long_Integer);

   procedure Calculate (Line : String; Total : out Long_Long_Integer) is
      Op : Character := Line (Line'First);
      Result : Long_Long_Integer := 0;
   begin
      Total := 0;

      for J in Line'Range loop
         if Line (J) /= ' ' then
            Total := Total + Result;
            Op := Line (J);
            Result := (if Op = '+' then 0 else 1);
         end if;

         if Number (J) /= 0 then
            Result :=
              (case Op is
                  when '+'    => Result + Number (J),
                  when '*'    => Result * Number (J),
                  when others => raise Program_Error);
         end if;
      end loop;

      Total := Total + Result;
   end Calculate;

   Total : Long_Long_Integer := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         if Line (1) in '+' | '*' then
            Calculate (Line, Total);
            exit;
         end if;

         for J in Line'Range when Line (J) /= ' ' loop
            Number (J) := 10 * Number (J) + Character'Pos (Line (J)) -
              Character'Pos ('0');
         end loop;
      end;
   end loop;

   Ada.Text_IO.Put_Line (Total'Image);
end AOC.Day_06;
