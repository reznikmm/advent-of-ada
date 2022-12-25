with Ada.Containers.Formal_Hashed_Sets;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Day_23 with SPARK_Mode is

   Max_Elfs : constant := 3500;
   Modulus  : constant := 3571;  --  Should be prime more than Max_Elfs

   subtype Coordinate is Integer range -10_000 .. 10_000;

   type Position is record
      X, Y : Coordinate;
   end record;

   type Movement is record
      X, Y : Integer range -1 .. 1;
   end record;

   function "+" (P : Position; M : Movement) return Position is
     (P.X + M.X, P.Y + M.Y);

   Around : constant array (1 .. 8) of Movement :=
     ((0, -1), (0, 1), (-1, 0), (1, 0), (-1, -1), (1, 1), (-1, 1), (1, -1));

   type Movement_Index is mod 4;

   Step : constant array (Movement_Index) of Movement :=
     ((0, -1), (0, 1), (-1, 0), (1, 0));

   type Movement_Check is array (1 .. 3) of Movement;
   Checks : constant array (Movement_Index) of Movement_Check :=
     (((-1, -1), (0, -1), (1, -1)),
      ((-1, 1),  (0, 1),  (1, 1)),
      ((-1, -1), (-1, 0), (-1, 1)),
      ((1, -1),  (1, 0),  (1, 1)));

   function Hash (V : Position) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type'Mod (123 * V.X + V.Y));

   package Position_Sets is new Ada.Containers.Formal_Hashed_Sets
     (Position, Hash, "=");

   subtype Position_Set is Position_Sets.Set (Max_Elfs, Modulus);

   procedure Round
     (Map   : in out Position_Sets.Set;
      Shift : in out Movement_Index)
   is
      use Position_Sets;

      Next     : Position_Set;
      Choices  : Position_Set;
      Conflict : Position_Set;
      Moved    : Boolean;
   begin
      for Pos of Map loop
         if (for some Step of Around => Contains (Map, Pos + Step)) then
            for J in Checks'Range loop
               declare
                  Check : constant Movement_Check := Checks (J + Shift);
               begin
                  if (for all M of Check => not Contains (Map, Pos + M)) then
                     declare
                        Choice : constant Position := Pos + Step (J + Shift);
                     begin
                        if Contains (Choices, Choice) then
                           Include (Conflict, Choice);
                        else
                           Insert (Choices, Choice);
                        end if;

                        exit;
                     end;
                  end if;
               end;
            end loop;
         end if;
      end loop;

      for Pos of Map loop
         Moved := False;

         if (for some Step of Around => Contains (Map, Pos + Step)) then
            for J in Checks'Range loop
               declare
                  Check : constant Movement_Check := Checks (J + Shift);
               begin
                  if (for all M of Check => not Contains (Map, Pos + M)) then
                     declare
                        Choice : constant Position := Pos + Step (J + Shift);
                     begin
                        if not Contains (Conflict, Choice) then
                           Moved := True;
                           Insert (Next, Choice);
                        end if;

                        exit;
                     end;
                  end if;
               end;
            end loop;
         end if;

         if not Moved then
            Insert (Next, Pos);
         end if;
      end loop;

      Move (Map, Source => Next);
      Shift := Shift + 1;
   end Round;

   Map   : Position_Set;
   Row   : Positive := 1;
   Shift : Movement_Index := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : String (1 .. 80);
         Last : Natural;
      begin
         Ada.Text_IO.Get_Line (Line, Last);

         for X in 1 .. Last loop
            if Line (X) = '#' then
               Position_Sets.Insert (Map, (X, Row));
            end if;
         end loop;
         Row := Row + 1;
      end;
   end loop;

   for J in 1 .. 10000 loop
      declare
         Copy  : constant Position_Set := Map;
      begin
         Round (Map, Shift);

         if (for all Item of Copy => Position_Sets.Contains (Map, Item)) then
            Ada.Integer_Text_IO.Put (J);
            exit;
         end if;
      end;
   end loop;
end Day_23;
