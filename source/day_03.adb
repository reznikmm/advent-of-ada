with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day_03 is

   type Part_Number is record
      From, To : Positive;  --  span in the line
      Value    : Positive;  --  decimal part number
   end record;

   package Part_Number_Vectors is
     new Ada.Containers.Vectors (Positive, Part_Number);

   type Part_Number_Pair is array (1 .. 2) of Part_Number;

   type Symbol is record
      Char     : Character;  --  '*' for gears
      Position : Positive;   --  pos in the line
      Adjacent : Natural := 0;  --  count of adjacent parts
      Parts    : Part_Number_Pair;  --  first two adjacent parts for '*'
   end record;

   package Symbol_Vectors is
     new Ada.Containers.Vectors (Positive, Symbol);

   type Item_Vectors is record
     Parts  : Part_Number_Vectors.Vector;
     Symbols : Symbol_Vectors.Vector;
   end record;

   procedure Parse_Line (Line : String; Result : out Item_Vectors);
   --  Parse line and find possible part numbers and symbols

   function Is_Adjacent (Part : Part_Number; Pos : Positive) return Boolean is
     (Pos + 1 >= Part.From and Pos - 1 <= Part.To);
   --  Check if Pos(-sition) is adjacent to the part number span

   procedure Find_Gears
     (Symbols : in out Symbol_Vectors.Vector;
      Parts   : Part_Number_Vectors.Vector);
   --  Enumerate symbols adjanced to parts and find '*'. Collect parts for them

   ----------------
   -- Parse_Line --
   ----------------

   procedure Parse_Line (Line : String; Result : out Item_Vectors) is
      From : Natural := Line'First;
   begin
      --  Look for part numbers:
      while From <= Line'Last loop
         declare
            Part : Part_Number;
            To : Natural;
         begin
            Ada.Strings.Fixed.Find_Token
              (Line,
               Ada.Strings.Maps.Constants.Decimal_Digit_Set,
               Test  => Ada.Strings.Inside,
               From  => From,
               First => Part.From,
               Last  => To);

            exit when To < Part.From;

            Part.To := To;
            Part.Value := Positive'Value (Line (Part.From .. Part.To));
            Result.Parts.Append (Part);
            From := Part.To + 1;
         end;
      end loop;

      --  Look for symbols:
      for J in Line'Range loop
         if Line (J) not in '.' | '0' .. '9' then
            Result.Symbols.Append
              (Symbol'(Char     => Line (J),
                       Position => J,
                       Adjacent => 0,
                       Parts    => <>));
         end if;
      end loop;
   end Parse_Line;

   ----------------
   -- Find_Gears --
   ----------------

   procedure Find_Gears
     (Symbols : in out Symbol_Vectors.Vector;
      Parts   : Part_Number_Vectors.Vector) is
   begin
      for Symbol of Symbols loop
         for Part of Parts loop
            if Symbol.Char = '*'
              and then Is_Adjacent (Part, Symbol.Position)
            then
               Symbol.Adjacent := Symbol.Adjacent + 1;

               if Symbol.Adjacent in Symbol.Parts'Range then
                  Symbol.Parts (Symbol.Adjacent) := Part;
               end if;
            end if;
         end loop;
      end loop;
   end Find_Gears;

   procedure Increment_Total
     (Symbols : Symbol_Vectors.Vector;
      Total   : in out Natural) is
   begin
      for Symbol of Symbols loop
         if Symbol.Adjacent = 2 then
            Total := Total + Symbol.Parts (1).Value * Symbol.Parts (2).Value;
         end if;
      end loop;
   end Increment_Total;

   Prev  : Item_Vectors;
   Total : Natural := 0;

begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line  : String (1 .. 200);
         Last  : Natural;
         Next  : Item_Vectors;

      begin
         Ada.Text_IO.Get_Line (Line, Last);
         pragma Assert (Last < Line'Last);
         Parse_Line (Line (1 .. Last), Next);

         Find_Gears (Next.Symbols, Prev.Parts);
         Find_Gears (Prev.Symbols, Next.Parts);
         Find_Gears (Next.Symbols, Next.Parts);

         Increment_Total (Prev.Symbols, Total);
         Prev.Symbols.Move (Source => Next.Symbols);
         Prev.Parts.Move (Source => Next.Parts);
      end;
   end loop;
   Increment_Total (Prev.Symbols, Total);

   Ada.Integer_Text_IO.Put (Total);
end Day_03;
