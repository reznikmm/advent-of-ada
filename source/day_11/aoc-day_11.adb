--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

pragma Ada_2022;
--  with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Hash;

procedure AOC.Day_11 is
   pragma Assertion_Policy (Check);

   subtype Device is String (1 .. 3);

   package Device_Lists is new Ada.Containers.Doubly_Linked_Lists (Device);

   package Device_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Device,
      Element_Type    => Device_Lists.List,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Device_Lists."=");

   subtype Number is Long_Long_Integer;

   type Counter is record
      DAC   : Number;
      FFT   : Number;
      Both  : Number;
      None  : Number;
   end record;

   function "+" (Left, Right : Counter) return Counter is
     (Left.DAC   + Right.DAC,
      Left.FFT   + Right.FFT,
      Left.Both  + Right.Both,
      Left.None  + Right.None);

   package Count_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Device,
      Element_Type    => Counter,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   Map   : Device_Maps.Map;
   Total : Counter := (0, 0, 0, 0);
   Step  : Count_Maps.Map := ["svr" => (0, 0, 0, None => 1)];
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         From : Device;
         To   : Device;
         Char : Character := ' ';
         List : Device_Lists.List;
      begin
         Ada.Text_IO.Get (From);
         Ada.Text_IO.Get (Char);
         pragma Assert (Char = ':');

         while not Ada.Text_IO.End_Of_Line loop
            Ada.Text_IO.Get (Char);
            pragma Assert (Char = ' ');
            Ada.Text_IO.Get (To);
            List.Append (To);
         end loop;

         Map.Insert (From, List);
      end;
   end loop;

   while not Step.Is_Empty loop
      declare
         Next : Count_Maps.Map;
      begin
         for Cursor in Step.Iterate loop
            declare
               From  : constant Device := Count_Maps.Key (Cursor);
               Count : Counter;
            begin
               for To of Map (From) loop
                  Count := Count_Maps.Element (Cursor);

                  if To = "dac" then
                     Count :=
                       (DAC  => Count.DAC + Count.None,
                        FFT  => 0,
                        Both => Count.Both + Count.FFT,
                        None => 0);
                  elsif To = "fft" then
                     Count :=
                       (DAC  => 0,
                        FFT  => Count.FFT + Count.None,
                        Both => Count.Both + Count.DAC,
                        None => 0);
                  end if;

                  if To = "out" then
                     Total := Count + Total;
                  elsif Next.Contains (To) then
                     Count := Count + Next (To);
                     Next (To) := Count;
                  else
                     Next.Insert (To, Count);
                  end if;
               end loop;
            end;
         end loop;

         Step.Move (Source => Next);
      end;
   end loop;
   Ada.Text_IO.Put_Line (Total.Both'Image);
end AOC.Day_11;
