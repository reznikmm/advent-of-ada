--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure AOC.Day_10 is

   subtype Counter is Natural range 0 .. 300;

   subtype Counter_Index is Positive range 1 .. 10;

   type Counter_Array is array (Counter_Index) of Counter
     with Component_Size => 16;

   type Counter_Array_Array is array (Positive range <>) of Counter_Array;

   function Read_Button (Value : out Counter_Array) return Boolean is
      Char : Character;
      Index : Natural;
   begin
      Value := [others => 0];
      Ada.Text_IO.Get (Char);
      pragma Assert (Char = ' ');
      Ada.Text_IO.Get (Char);
      if Char = '(' then
         loop
            Ada.Integer_Text_IO.Get (Index);
            Value (Index + 1) := 1;
            Ada.Text_IO.Get (Char);
            exit when Char = ')';
            pragma Assert (Char = ',');
         end loop;

         return True;
      elsif Char = '{' then
         return False;
      else
         raise Program_Error;
      end if;
   end Read_Button;

   procedure Process
     (Result  : out Natural;
      Target  : Counter_Array;
      Buttons : Counter_Array_Array)
   is
      Best : Natural;
      Down : Natural;
      XX   : Counter_Array := [others => 0];
      Copy : Counter_Array := Target;
      Strip : Counter_Array_Array (1 .. Buttons'Length - 1);
   begin
      if Copy = [Copy'Range => 0] then
         Result := 0;
         return;
      elsif Buttons = [] then
         Result := Natural'Last;
         return;
      end if;

      for Button of Buttons loop
         for J in Target'Range when Button (J) /= 0 loop
            XX (J) := @ + 1;
         end loop;
      end loop;

      declare
         Min    : Positive := 1;
         Button : Positive := Positive'Last;
      begin
         for J in XX'Range loop
            if XX (Min) = 0 or (XX (Min) > XX (J) and XX (J) > 0) then
               Min := J;
            end if;
         end loop;

         for J in Buttons'Range loop
            if Buttons (J) (Min) > 0 then
               Button := J;
               exit;
            end if;
         end loop;

         Strip := Buttons (1 .. Button - 1) &
           Buttons (Button + 1 .. Buttons'Last);

         if XX (Min) = 1 then
            Best := Copy (Min);

            if (for all J in Copy'Range =>
                  Copy (J) >= Best * Buttons (Button) (J))
            then
               for J in Copy'Range loop
                  Copy (J) := @ - Best * Buttons (Button) (J);
               end loop;

               Process (Result, Copy, Strip);

               Result := (if @ = Natural'Last then @ else @ + Best);
            else
               Result := Natural'Last;  --
            end if;
         else
            Process (Best, Copy, Strip);

            for Step in reverse 1 .. Target (Min) loop
               Copy := Target;

               if Best > Step and then
                 (for all J in Copy'Range =>
                     Copy (J) >= Step * Buttons (Button) (J))
               then
                  for J in Copy'Range loop
                     Copy (J) := @ - Step * Buttons (Button) (J);
                  end loop;

                  Process (Down, Copy, Strip);
                  Down := (if @ = Natural'Last then @ else @ + Step);

                  Best := Natural'Min (Best, Down);
               end if;
            end loop;

            Result := Best;
         end if;
      end;
   end Process;

   procedure Read_Target (Target : in out Counter_Array) is
      Char : Character;
   begin
      Target := [others => 0];
      for Item of Target loop
         Ada.Integer_Text_IO.Get (Item);
         Ada.Text_IO.Get (Char);
         exit when Char = '}';
         pragma Assert (Char = ',');
      end loop;
   end Read_Target;

   procedure Read_And_Process
     (Total   : in out Natural;
      Width   : Positive;
      Buttons : Counter_Array_Array)
   is
      Down   : Natural;
      Next   : Counter_Array;
      Target : Counter_Array;
   begin
      if Read_Button (Next) then
         Read_And_Process (Total, Width, Buttons & Next);
         return;
      end if;

      Read_Target (Target);
      Process (Down, Target, Buttons);

      Ada.Text_IO.Put_Line (Down'Image);
      Total := @ + Down;
   end Read_And_Process;

   Total : Integer := 0;

begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Last : Natural := 0;
         Char : Character := ' ';
      begin
         while Char /= ']' loop
            Ada.Text_IO.Get (Char);
            Last := Last + 1;
         end loop;
         Read_And_Process (Total, Last - 2, []);
      end;
   end loop;

   Ada.Text_IO.Put_Line (Total'Image);
end AOC.Day_10;
