with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Day_03 with SPARK_Mode is

   Group_Size : constant := 3;  --  Number of elves in each group

   subtype Rucksack_Item is Character
     with Static_Predicate => Rucksack_Item in 'a' .. 'z' | 'A' .. 'Z';

   subtype Priority is Positive range 1 .. 52;

   function Prio (X : Rucksack_Item) return Priority is
     (if X in 'a' .. 'z' then 1 + Character'Pos (X) - Character'Pos ('a')
      else 27 + Character'Pos (X) - Character'Pos ('A'));

   Total : Natural := 0;
   Index : Positive range 1 .. Group_Size := 1;  --  index in a group

   type Flags is array (1 .. Group_Size) of Boolean;
   None : constant Flags := (others => False);
   Full : constant Flags := (others => True);

   Found : array (Character range 'A' .. 'z') of Flags := (others => None);

begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : String (1 .. 50);
         Last : Natural;
      begin
         Ada.Text_IO.Get_Line (Line, Last);

         if (for some C of Line (1 .. Last) => C not in Rucksack_Item) then
            Ada.Text_IO.Put_Line ("Unexpected input");
            return;
         elsif Total >= Natural'Last - Priority'Last then
            Ada.Text_IO.Put_Line ("Result overflow");
            return;
         end if;

         for Item of Line (1 .. Last) loop
            Found (Item) (Index) := True;
         end loop;

         if Index = Group_Size then
            for J in Found'Range loop
               if Found (J) = Full and then
                 J in Rucksack_Item   --  Always True. How to prove in SPARK?
               then
                  Total := Total + Prio (J);

                  exit;
               end if;
            end loop;

            Index := 1;
            Found := (others => None);

         else
            Index := Index + 1;

         end if;
      end;
   end loop;

   Ada.Integer_Text_IO.Put (Total);
end Day_03;
