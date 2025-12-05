--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Maps;
with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure AOC.Day_05 is

   type Ingredient_Id is new Long_Long_Integer;
   package Ingredient_Id_IO is new Ada.Text_IO.Integer_IO (Ingredient_Id);

   type Span is record
      First, Last : Ingredient_Id;
   end record;

   package Span_Lists is new Ada.Containers.Doubly_Linked_Lists (Span);

   Valid_Ids : Span_Lists.List;

   Total : Natural := 0;
begin
   declare
      Space : constant Ada.Strings.Maps.Character_Set :=
        Ada.Strings.Maps.To_Set (' ');
   begin
      while not Ada.Text_IO.End_Of_File loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line;
            From : Positive := Line'First;
            To   : Natural;
            Dash : Positive;
            Next : Span;
         begin
            exit when Line = "";
            while From <= Line'Last loop
               Ada.Strings.Fixed.Find_Token
                 (Line,
                  Space,
                  From   => From,
                  Test   => Ada.Strings.Outside,
                  First  => From,
                  Last   => To);
               Dash := Ada.Strings.Fixed.Index (Line (From .. To), "-");
               Next.First := Ingredient_Id'Value (Line (From .. Dash - 1));
               Next.Last := Ingredient_Id'Value (Line (Dash + 1 .. To));
               Valid_Ids.Append (Next);
               From := To + 1;
            end loop;
         end;
      end loop;
   end;

   while not Ada.Text_IO.End_Of_File loop
      declare
         procedure Skip_New_Line (Text : String) is null;

         Id : Ingredient_Id;
      begin
         Ingredient_Id_IO.Get (Id);
         Skip_New_Line (Ada.Text_IO.Get_Line);

         if (for some Span of Valid_Ids => Id in Span.First .. Span.Last) then
            Total := @ + 1;
         end if;
      end;
   end loop;

   Ada.Text_IO.Put_Line (Total'Image);
end AOC.Day_05;
