--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

procedure AOC.Day_02 is

   type Id is new Long_Long_Integer;

   function Count_Invalids (From, To : Id) return Id'Base;

   function Width (Value : Id) return Positive;
   --  Number of digits in Value

   function Unit (Width, Span : Positive) return Id'Base;
   --  Return "unit" id, like Unit(6,2)=010101, Unit(6,3)=001001
   --  or return 0 if (Width mod Span /= 0).

   function Is_Invalid (Value : Id; Width, Span : Positive) return Boolean is
     (Value = Unit (Width, Span) * (Value mod 10**Span));
   --  Check if Value is invalid, suppose Width(Value)=Width and Span divs Wdth

   function Count_Invalids (From, To : Id) return Id'Base is
      Result     : Id'Base := 0;
      From_Width : constant Positive := Width (From);
      To_Width   : constant Positive := Width (To);
      Next       : Id;
   begin
      for Width in From_Width .. To_Width loop
         for Span in 1 .. Width / 2
           when Width mod Span  = 0
         loop
            for Item in 10**(Span - 1) .. Id'(10**Span - 1) loop
               Next := Unit (Width, Span) * Item;

               if Next in From .. To and then
                 not (for some J in 1 .. Span - 1 =>
                        Is_Invalid (Next, Width, J))
               then
                  Result := Result + Next;
               end if;
            end loop;
         end loop;
      end loop;

      return Result;
   end Count_Invalids;

   function Unit (Width, Span : Positive) return Id'Base is
      Result : Id'Base := 0;
   begin
      if Width mod Span = 0 then
         for J in 1 .. Width / Span loop
            Result := Result * 10**Span + 1;
         end loop;
      end if;

      return Result;
   end Unit;

   function Width (Value : Id) return Positive is
   begin
      for J in 1 .. 10 loop
         if Value < 10**J then
            return J;
         end if;
      end loop;

      raise Program_Error;
   end Width;

   Input : constant String := Ada.Text_IO.Get_Line;
   From  : Positive := Input'First;
   To    : Natural;
   Dash  : Positive;
   Total : Id'Base := 0;
   Comma : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (',');
begin
   while From <= Input'Last loop
      Ada.Strings.Fixed.Find_Token
        (Source => Input,
         Set    => Comma,
         From   => From,
         Test   => Ada.Strings.Outside,
         First  => From,
         Last   => To);

      exit when To < From;

      Dash := Ada.Strings.Fixed.Index (Input (From .. To), "-");

      Total := Total + Count_Invalids
        (From => Id'Value (Input (From .. Dash - 1)),
         To   => Id'Value (Input (Dash + 1 .. To)));

      From := To + 2;
   end loop;

   Ada.Text_IO.Put_Line (Total'Image);
end AOC.Day_02;
