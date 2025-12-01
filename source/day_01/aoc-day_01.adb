--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure AOC.Day_01 is
   type Number is mod 100;

   --  package Number_IO is new Ada.Text_IO.Modular_IO (Number);

   Current : Number := 50;
   Result : Natural := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Direction : Character;
         Steps : Natural;
      begin
         Ada.Text_IO.Get (Direction);
         Ada.Integer_Text_IO.Get (Steps);
         if Direction = 'L' then
            if Current /= 0 and Current <= Number'Mod (Steps) then
               Result := Result + 1;
            end if;

            Current := Current - Number'Mod (Steps);
         elsif Direction = 'R' then
            if Current /= 0 and
              Number'Last - Current <= Number'Mod (Steps) - 1
            then
               Result := Result + 1;
            end if;

            Current := Current + Number'Mod (Steps);
         else
            raise Constraint_Error with "Wrong dir " & Direction;
         end if;

         Result := Result + Steps / Number'Modulus;
      end;
   end loop;

   Ada.Text_IO.Put_Line (Result'Image);
end AOC.Day_01;
