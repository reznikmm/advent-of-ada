pragma Ada_2022;
pragma Extensions_Allowed (On);  --  Enable 'Reduce which is Ada 2022 actually

with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Day_01 with SPARK_Mode is

   Top_Count : constant := 3;  --  Number of elves to keep in the top list

   type Top_Range is range 1 .. Top_Count;
   --  Range of elves to keep in top list

   subtype Item_Calories is Integer range 0 .. 10 ** 6;
   --  Calories in a single item of meals, snacks, etc

   function To_Integer (Line : String) return Item_Calories
     with Pre => Line'First = 1
       and then Line'Last <= 6
       and then (for all X of Line => X in '0' .. '9');
   --  Cast a String to an integer

   subtype Elf_Calories is Integer range 0 .. Natural'Last / Top_Count;
   --  Calories carried by one Elf

   Sum   : Elf_Calories := 0;  --  Calories per current elf
   Total : Natural := 0;

   Top : array (Top_Range) of Elf_Calories := [others => 0];
   --  Top list (unordered)

   Min : Top_Range := Top'First;  --  Index of min item in Top

   function Min_Index (Left, Right : Top_Range) return Top_Range is
     (if Top (Left) <= Top (Right) then Left else Right);
   --  Select an index with minimal value in Top

   function To_Integer (Line : String) return Item_Calories is
      Result : Item_Calories := 0;
   begin
      for X in Line'Range loop
         pragma Loop_Invariant (Result < 10 ** (X - Line'First));

         Result := Result * 10 +
           Character'Pos (Line (X)) - Character'Pos ('0');
      end loop;

      return Result;
   end To_Integer;

begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : String (1 .. 7);
         Last : Natural;
         Item : Item_Calories;
      begin
         Ada.Text_IO.Get_Line (Line, Last);

         if Last > 6 or else
           (for some X of Line (1 .. Last) => X not in '0' .. '9') or else
           Sum > Elf_Calories'Last - Item_Calories'Last
         then
            Ada.Text_IO.Put_Line ("Unexpected input");
         elsif Last >= Line'First then
            Item := To_Integer (Line (1 .. Last));
            Sum := Sum + Item;
         elsif Top (Min) < Sum then
            Top (Min) := Sum;

            --  Min := [for J in Top_Range => J]'Reduce (Min_Index, 1);
            --  ^^  crash GCC 12
            Min := 1;
            for J in Top_Range loop
               Min := Min_Index (Min, J);
            end loop;

            Sum := 0;
         else
            Sum := 0;
         end if;
      end;
   end loop;

   if Sum > 0 and then Top (Min) < Sum then
      Top (Min) := Sum;
   end if;

   --  Ada.Integer_Text_IO.Put (Top'Reduce ("+", 0));
   --  'Reduce is unsupported by SPARK. Unrolling it:
   for Item of Top loop
      Total := Total + Item;
   end loop;

   Ada.Integer_Text_IO.Put (Total);
end Day_01;
