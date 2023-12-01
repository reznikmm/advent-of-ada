pragma Assertion_Policy (Check);  --  Why alire doesn't turn it on in `dev`?

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

procedure Day_11 is
   subtype Monkey_Index is Natural;
   subtype Worry_Level is Natural;  --  Calculated by Modulus, see below

   package Worry_Level_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Worry_Level);

   type Operation_Kind is ('+', '*');

   type Monkey is record
      Items     : Worry_Level_Lists.List;
      Divider   : Positive;
      Operation : Operation_Kind;
      Operand   : Natural;
      If_True   : Monkey_Index;
      If_False  : Monkey_Index;
      Count     : Natural := 0;
   end record;

   package Monkey_Vectors is new Ada.Containers.Vectors (Monkey_Index, Monkey);

   function More_Count (Left, Right : Monkey) return Boolean is
     (Left.Count > Right.Count);

   package Sorting is new Monkey_Vectors.Generic_Sorting (More_Count);

   procedure Read_Monkey (Value : out Monkey) is

      procedure Expect (Value : String) is
         Next : String (Value'Range);
      begin
         Ada.Text_IO.Get (Next);
         pragma Assert (Next = Value);
      end Expect;

      Index   : Monkey_Index;
      Char    : Character;
   begin
      Expect ("Monkey");
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
         Ignore : Natural;
      begin
         pragma Assert (Line (Line'Last) = ':');
         --  Drop ':' because it causes Get to fail
         Ada.Integer_Text_IO.Get (Line (1 .. Line'Last - 1), Index, Ignore);
      end;
      Expect ("  Starting items:");
      loop
         declare
            Level : Worry_Level;
         begin
            Ada.Integer_Text_IO.Get (Level);
            Value.Items.Append (Level);
            Ada.Text_IO.Get (Char);
            exit when Char /= ',';
         end;
      end loop;
      Expect (" Operation: new = old ");
      Ada.Text_IO.Get (Char);
      Value.Operation :=
        (if Char = '+' then '+' elsif Char = '*' then '*'
         else raise Constraint_Error);

      declare
         Rest : constant String := Ada.Text_IO.Get_Line;
         Last : Natural;
      begin
         if Rest = " old" then
            Value.Operand := 0;  --  means 'old'
         else
            Ada.Integer_Text_IO.Get (Rest, Value.Operand, Last);
            pragma Assert (Last = Rest'Last);
         end if;
      end;

      Expect ("  Test: divisible by");
      Ada.Integer_Text_IO.Get (Value.Divider);
      Expect ("    If true: throw to monkey");
      Ada.Integer_Text_IO.Get (Value.If_True);
      Expect ("    If false: throw to monkey");
      Ada.Integer_Text_IO.Get (Value.If_False);
   end Read_Monkey;

   procedure Read (Monkeys : out Monkey_Vectors.Vector) is
   begin
      while not Ada.Text_IO.End_Of_File loop
         declare
            Item : Monkey;
         begin
            Read_Monkey (Item);
            Monkeys.Append (Item);
         end;
      end loop;
   end Read;

   Monkeys : Monkey_Vectors.Vector;
   Modulus : Positive := 1;
begin
   Read (Monkeys);

   --  Find Modulus that includes every divider
   for Monkey of Monkeys loop
      Modulus := Modulus * Monkey.Divider;
   end loop;

   for J in 1 .. 10_000 loop
      for Monkey of Monkeys loop
         while not Monkey.Items.Is_Empty loop
            declare
               Index : Monkey_Index;
               Item  : Worry_Level := Monkey.Items.First_Element;
               Arg   : constant Positive :=
                 (if Monkey.Operand = 0 then Item else Monkey.Operand);
            begin
               Monkey.Items.Delete_First;
               Monkey.Count := Monkey.Count + 1;

               case Monkey.Operation is
                  when '+' =>
                     Item := (Item + Arg) mod Modulus;
                  when '*' =>
                     Item := Natural
                       ((Long_Long_Integer (Item) * Long_Long_Integer (Arg))
                          mod Long_Long_Integer (Modulus));
               end case;

               --  Item := Item / 3;  --  + (if Item mod 3 = 2 then 1 else 0);

               Index := (if Item mod Monkey.Divider = 0 then Monkey.If_True
                         else Monkey.If_False);

               Monkeys (Index).Items.Append (Item);
            end;
         end loop;
      end loop;

      if J = 1 or else J mod 1000 = 0 then
         for Monkey of Monkeys loop
            Ada.Integer_Text_IO.Put (Monkey.Count);
         end loop;
         Ada.Text_IO.New_Line;
      end if;
   end loop;

   Ada.Text_IO.New_Line;

   for Monkey of Monkeys loop
      Ada.Integer_Text_IO.Put (Monkey.Count);
   end loop;
   Sorting.Sort (Monkeys);

   Ada.Long_Long_Integer_Text_IO.Put
     (Long_Long_Integer (Monkeys (0).Count) *
        Long_Long_Integer (Monkeys (1).Count));
end Day_11;
