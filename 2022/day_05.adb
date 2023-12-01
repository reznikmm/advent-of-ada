with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Formal_Vectors;

procedure Day_05 with SPARK_Mode is
   use type Ada.Containers.Count_Type;

   package Crate_Vectors is new Ada.Containers.Formal_Vectors
     (Positive, Character);

   Max_Stacks : constant := 10;
   Max_Crates : constant := Max_Stacks * 10;

   Stacks : array (1 .. Max_Stacks) of Crate_Vectors.Vector (Max_Crates);

   subtype Stack_Index is Positive range Stacks'Range;
   subtype Crate_Count_Type is Ada.Containers.Count_Type range 0 .. Max_Crates;

   function Crates_Count
     (Last : Stack_Index) return Crate_Count_Type
     with Pre =>
       (for all J in Stacks'Range =>
          Crate_Vectors.Length (Stacks (J)) < Max_Crates / Max_Stacks);

   function Crates_Count
     (Last : Stack_Index) return Crate_Count_Type
   is
      Result : Crate_Count_Type := 0;
   begin
      for J in 1 .. Last loop
         Result := Result + Crate_Vectors.Length (Stacks (J));
      end loop;
      return Result;
   end Crates_Count;

begin
   while not Ada.Text_IO.End_Of_File loop
      pragma Loop_Invariant
        (for all J in Stacks'Range =>
           Crate_Vectors.Length (Stacks (J)) < Max_Crates / Max_Stacks);

      declare
         Line : String (1 .. 4 * Max_Stacks);
         Last : Natural;
      begin
         Ada.Text_IO.Get_Line (Line, Last);

         exit when Last < 1;

         for J in 1 .. (Last + 1) / 4 loop
            pragma Loop_Invariant
              (for all J in Stacks'Range =>
                 Crate_Vectors.Length (Stacks (J)) < Max_Crates / Max_Stacks);

            if Line (J * 4 - 3) = '[' then
               Crate_Vectors.Prepend (Stacks (J), Line (J * 4 - 2));

               if Crate_Vectors.Length (Stacks (J)) =
                 Max_Crates / Max_Stacks
               then
                  return;
               end if;
            end if;
         end loop;
      end;
   end loop;

   pragma Assert
     (for all J in Stacks'Range =>
        Crate_Vectors.Length (Stacks (J)) < Max_Crates / Max_Stacks);

   pragma Assert (Crates_Count (Max_Stacks) <= Max_Crates);

   while not Ada.Text_IO.End_Of_File loop
      declare
         Move_Word : String (1 .. 4);
         From_Word : String (1 .. 5);
         To_Word   : String (1 .. 3);
         Count     : Integer;
         From      : Integer;
         To        : Integer;
      begin
         Ada.Text_IO.Get (Move_Word);
         Ada.Integer_Text_IO.Get (Count);
         Ada.Text_IO.Get (From_Word);
         Ada.Integer_Text_IO.Get (From);
         Ada.Text_IO.Get (To_Word);
         Ada.Integer_Text_IO.Get (To);

         if Move_Word /= "move"
           or else From_Word /= " from"
           or else To_Word /= " to"
           or else From not in Stacks'Range
           or else To not in Stacks'Range
           or else Count not in 1 .. Crate_Vectors.Last_Index (Stacks (From))
           or else From = To
         then
            return;
         end if;

         declare
            Copy   : constant Positive := From;
            Source : Crate_Vectors.Vector renames Stacks (Copy);
            Last   : constant Positive := Crate_Vectors.Last_Index (Source);
            Start  : constant Positive := Last - Count + 1;
         begin
            for Index in Start .. Last loop
               Crate_Vectors.Append
                 (Stacks (To),
                  Crate_Vectors.Element (Source, Index));
            end loop;

            Crate_Vectors.Delete
              (Source, Start, Ada.Containers.Count_Type (Count));
         end;
      end;
   end loop;

   for Stack of Stacks loop
      if not Crate_Vectors.Is_Empty (Stack) then
         Ada.Text_IO.Put (Crate_Vectors.Last_Element (Stack));
      end if;
   end loop;
end Day_05;
