--  with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

procedure Day_04 is
   type Number is new Positive;

   function Hash (V : Number) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (V));

   package Number_Sets is new
     Ada.Containers.Hashed_Sets (Number, Hash, "=");

   package Count_Vectors is new Ada.Containers.Vectors
     (Positive, Ada.Containers.Count_Type);

   procedure Parse
     (Line : String;
      Id   : out Positive;
      Win  : out Number_Sets.Set;
      My   : out Number_Sets.Set)
   is
      procedure Read_Numbers
        (From : in out Positive;
         List : out Number_Sets.Set)
      is
         Value : Number;
      begin
         --  Look for part numbers:
         while From + 1 <= Line'Last and then Line (From + 1) /= '|' loop
            declare
               To : Natural;
            begin
               Ada.Strings.Fixed.Find_Token
                 (Line,
                  Ada.Strings.Maps.Constants.Decimal_Digit_Set,
                  Test  => Ada.Strings.Inside,
                  From  => From,
                  First => From,
                  Last  => To);

               exit when To < From;

               Value := Number'Value (Line (From .. To));
               List.Insert (Value);
               From := To + 1;
            end;
         end loop;
      end Read_Numbers;

      From : Natural := Line'First;
      To   : Natural;
   begin
      --  Skip card number:
      Ada.Strings.Fixed.Find_Token
        (Line,
         Ada.Strings.Maps.Constants.Decimal_Digit_Set,
         Test  => Ada.Strings.Inside,
         First => From,
         Last  => To);

      Id := Positive'Value (Line (From .. To));

      From := To + 1;

      Read_Numbers (From, Win);

      From := From + 1;
      Read_Numbers (From, My);
   end Parse;

   Cards  : Count_Vectors.Vector;

   procedure Increment
     (Id    : Positive;
      Value : Ada.Containers.Count_Type := 1)
   is
      use type Ada.Containers.Count_Type;
   begin
      if Cards.Last_Index + 1 = Id then
         Cards.Append (Value);
      else
         Cards (Id) := Cards (Id) + Value;
      end if;
   end Increment;

   Total  : Ada.Containers.Count_Type := 0;
   Max_Id : Natural := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line  : constant String := Ada.Text_IO.Get_Line;
         Id    : Positive;
         Win   : Number_Sets.Set;
         My    : Number_Sets.Set;
         Set   : Number_Sets.Set;
         Count : Ada.Containers.Count_Type := 1;
      begin
         Parse (Line, Id, Win, My);
         Max_Id := Positive'Max (Max_Id, Id);
         Increment (Id);
         Count := Cards (Id);

         Set := Number_Sets.Intersection (Win, My);

         for J in Id + 1 .. Id + Natural (Set.Length) loop
            Increment (J, Count);
         end loop;
      end;
   end loop;

   declare
      use type Ada.Containers.Count_Type;
   begin
      for J of Cards loop
         Total := Total + J;
      end loop;
   end;

   Ada.Text_IO.Put (Total'Image);
end Day_04;
