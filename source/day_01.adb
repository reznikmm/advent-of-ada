with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Bounded;

procedure Day_01 with SPARK_Mode is

   package Bounded_Strings is
     new Ada.Strings.Bounded.Generic_Bounded_Length (5);

   use Bounded_Strings;

   function "+" (Text : String) return Bounded_String is
     (To_Bounded_String (Text));

   Spell : constant array (Character'('1') .. '9') of Bounded_String :=
     [+"one", +"two", +"three", +"four", +"five",
      +"six", +"seven", +"eight", +"nine"];

   function Digit
     (Line  : String;
      From : Positive) return Character
   is
   begin
      if Line (From) in '0' .. '9' then
         return Line (From);
      end if;

      for J in Spell'Range loop
         declare
            To : constant Positive := From + Length (Spell (J)) - 1;
         begin
            if To <= Line'Last and then Line (From .. To) = Spell (J) then
               return J;
            end if;
         end;
      end loop;

      return ' ';
   end Digit;

   Total : Natural := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line  : String (1 .. 60);
         Image : String (1 .. 2) := "  ";
         Next  : Character;
         Last  : Natural;
      begin
         Ada.Text_IO.Get_Line (Line, Last);

         for J in 1 .. Last loop
            Next := Digit (Line (1 .. Last), J);

            if Next in '0' .. '9' then
               if Image (1) = ' ' then
                  Image (1) := Next;
               end if;
               Image (2) := Next;
            end if;
         end loop;

         if Image /= "  " then
            Total := Total + Natural'Value (Image);
         end if;
      end;
   end loop;

   Ada.Integer_Text_IO.Put (Total);
end Day_01;
