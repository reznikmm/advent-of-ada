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
   --  package Ingredient_Id_IO is new Ada.Text_IO.Integer_IO (Ingredient_Id);

   type Span is record
      First, Last : Ingredient_Id;
   end record;

   --  function Is_Empty (Value : Span) return Boolean is
   --    (Value.Last < Value.First);

   function Is_Overlap (Left, Right : Span) return Boolean is
      (Left.First in Right.First .. Right.Last or
         Right.First in Left.First .. Left.Last);

   function "or" (Left, Right : Span) return Span is
     (Ingredient_Id'Min (Left.First, Right.First),
      Ingredient_Id'Max (Left.Last, Right.Last))
        with Pre => Is_Overlap (Left, Right);

   package Span_Lists is new Ada.Containers.Doubly_Linked_Lists (Span);

   procedure Append (List : in out Span_Lists.List; Item : Span);

   procedure Append (List : in out Span_Lists.List; Item : Span) is
      Next   : Span_Lists.List;
      Result : Span_Lists.List := List;
      Again  : Boolean := True;
      Value  : Span := Item;
   begin
      while Again loop
         Again := False;

         for Item of Result loop
            if Is_Overlap (Item, Value) then
               Value := @ or Item;
            else
               Next.Append (Item);
            end if;
         end loop;

         Result.Move (Source => Next);
      end loop;

      List.Move (Source => Result);
      List.Append (Value);
   end Append;

   Valid_Ids : Span_Lists.List;

   Total : Long_Long_Integer := 0;
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
               Append (Valid_Ids, Next);
               From := To + 1;
            end loop;
         end;
      end loop;
   end;

   for Item of Valid_Ids loop
      Total := @ + Long_Long_Integer (Item.Last - Item.First + 1);
   end loop;

   Ada.Text_IO.Put_Line (Total'Image);
end AOC.Day_05;
