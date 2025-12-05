--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Strings.Bounded;
with Ada.Strings.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Text_IO;

procedure AOC.Day_03 is

   type Joltage is range 0 .. 200 * 10**12;

   package String_100 is new Ada.Strings.Bounded.Generic_Bounded_Length (100);

   subtype Battery_Bank is String_100.Bounded_String;

   type Piece is record
      Text   : Battery_Bank;
      Length : Positive;
   end record;

   use type Ada.Containers.Hash_Type;

   function Hash (Key : Piece) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (String_100.To_String (Key.Text)) +
       Ada.Containers.Hash_Type'Mod (Key.Length));

   package Joltage_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Piece,
      Element_Type    => Joltage,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Max_Joltage
     (Memory : in out Joltage_Maps.Map;
      Line   : String;
      Length : Positive) return Joltage;

   function Max_Joltage
     (Memory : in out Joltage_Maps.Map;
      Line   : String;
      Length : Positive) return Joltage
   is
      Current : constant Joltage :=
        Character'Pos (Line (Line'First)) - Character'Pos ('0');
      Tail    : constant String := Line (Line'First + 1 .. Line'Last);

      Key : constant Piece := (String_100.To_Bounded_String (Line), Length);
      Result_Keep : Joltage;
      Result_Skip : Joltage;
      Result      : Joltage;
      Next        : Joltage;
   begin
      if Memory.Contains (Key) then
         return Memory (Key);
      end if;

      Result_Skip :=
        (if Tail /= ""
         then Max_Joltage (Memory, Tail, Length)
         else 0);

      Next :=
        (if Tail /= "" and Length > 1
         then Max_Joltage (Memory, Tail, Length - 1)
         else 0);

      Result_Keep :=
        (if Next > 0
         then Current * 10**(Length - 1) + Next
         elsif Length = 1
         then Current
         else 0);

      Result := Joltage'Max (Result_Keep, Result_Skip);
      Memory.Insert (Key, Result);

      return Result;
   end Max_Joltage;

   Memory : Joltage_Maps.Map;
   Next   : Joltage;
   Total  : Joltage := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         Next := Max_Joltage (Memory, Line, 12);
         Ada.Text_IO.Put_Line (Next'Image);

         Total := Total + Next;
      end;
   end loop;

   Ada.Text_IO.Put_Line (Memory.Length'Image);
   Ada.Text_IO.Put_Line (Total'Image);
end AOC.Day_03;
