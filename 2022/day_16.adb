with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Ada.Strings.Hash;
with Ada.Integer_Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Bounded_Doubly_Linked_Lists;

procedure Day_16 is
   subtype Valve_Index is Positive range 1 .. 16;

   type Valve_Mask is array (Valve_Index) of Boolean with Pack;

   subtype Valve_Name is String (1 .. 2);

   package Valve_Name_Lists is new Ada.Containers.Bounded_Doubly_Linked_Lists
     (Valve_Name);

   type Valve is record
      Name  : String (1 .. 2);
      Rate  : Natural;
      Index : Valve_Index'Base := 0;
      Pass  : Valve_Name_Lists.List (5);
   end record;

   package Valve_Maps is new Ada.Containers.Ordered_Maps
     (Valve_Name, Valve, "<", "=");

   procedure Find_Best
     (Valves  : Valve_Maps.Map;
      Start   : Valve_Name;
      Release : out Integer)
   is
      type Key is record
         You      : Valve_Name;
         Elephant : Valve_Name;
         Open     : Valve_Mask := (others => False);
      end record;

      function Hash (L : Key) return Ada.Containers.Hash_Type is
         Image : String (L.Open'Range);
      begin
         for Index in Image'Range loop
            Image (Index) := (if L.Open (Index) then 'Y' else 'N');
         end loop;

         return Ada.Strings.Hash (Image & L.You & L.Elephant);
      end Hash;

      package Release_Maps is new Ada.Containers.Hashed_Maps
        (Key, Natural, Hash, "=");

      type Worker_State is record
         This     : Key;
         PR       : Natural := 0;  --  Expected pressue release
--         Path    : Valve_Name_Lists.List;
      end record;

      function Hash (L : Worker_State) return Ada.Containers.Hash_Type is
        (Hash (L.This));

      package State_Sets is new Ada.Containers.Hashed_Sets
        (Worker_State, Hash, "=");

      procedure Append
        (Queue : in out State_Sets.Set;
         Found : in out Release_Maps.Map;
         Value : Worker_State)
      is
         Cursor : Release_Maps.Cursor;
         Ok     : Boolean;
      begin
         if Value.This.You > Value.This.Elephant then
            Append
              (Queue,
               Found,
               ((You      => Value.This.Elephant,
                 Elephant => Value.This.You,
                 Open     => Value.This.Open),
                PR => Value.PR));
            return;
         end if;

         Found.Insert (Value.This, Value.PR, Cursor, Ok);

         if Ok then
            Queue.Insert (Value);
         elsif Release_Maps.Element (Cursor) < Value.PR then
            Found (Cursor) := Value.PR;
            Queue.Include (Value);
         end if;

         Release := Integer'Max (Release, Value.PR);
      end Append;

      procedure Update (Item  : Worker_State; Result : out Worker_State) is
      begin
         Result := Item;
         --  Result.Path.Append (Item.Current);

         for Valve of Valves loop
            if Valve.Index > 0 and then Item.This.Open (Valve.Index) then
               Result.PR := Result.PR + Valve.Rate;
            end if;
         end loop;
      end Update;

      Queue : State_Sets.Set;

   begin
      Release := 0;

      Queue.Insert
        (((You      => Start,
           Elephant => Start,
           Open     => (others => False)),
          others  => <>));

      for Time in 1 .. 26 loop
         declare
            use type Ada.Containers.Count_Type;
            Next  : State_Sets.Set;
            Found : Release_Maps.Map;
            Value : Worker_State;
         begin
            for Item of Queue loop
               Update (Item, Value);

               for You of Valves (Item.This.You).Pass loop
                  Value.This.You := You;

                  for Elephant of Valves (Item.This.Elephant).Pass loop
                     Value.This.Elephant := Elephant;
                     Append (Next, Found, Value);
                  end loop;
               end loop;

               if Valves (Item.This.You).Index > 0 and then
                 not Item.This.Open (Valves (Item.This.You).Index)
               then
                  Value.This.Open (Valves (Item.This.You).Index) := True;
                  Value.This.You := Item.This.You;

                  for Elephant of Valves (Item.This.Elephant).Pass loop
                     Value.This.Elephant := Elephant;
                     Append (Next, Found, Value);
                  end loop;

                  Value.This.Open (Valves (Item.This.You).Index) := False;
               end if;

               if Valves (Item.This.Elephant).Index > 0 and then
                 not Item.This.Open (Valves (Item.This.Elephant).Index)
               then
                  Value.This.Open (Valves (Item.This.Elephant).Index) := True;
                  Value.This.Elephant := Item.This.Elephant;

                  for You of Valves (Item.This.You).Pass loop
                     Value.This.You := You;
                     Append (Next, Found, Value);
                  end loop;

                  Value.This.Open (Valves (Item.This.Elephant).Index) := False;
               end if;

               if Item.This.You /= Item.This.Elephant and then
                 Valves (Item.This.You).Index > 0 and then
                 not Item.This.Open (Valves (Item.This.You).Index) and then
                 Valves (Item.This.Elephant).Index > 0 and then
                 not Item.This.Open (Valves (Item.This.Elephant).Index)
               then
                  Value.This.Open (Valves (Item.This.You).Index) := True;
                  Value.This.Open (Valves (Item.This.Elephant).Index) := True;
                  Value.This.You := Item.This.You;
                  Value.This.Elephant := Item.This.Elephant;
                  Append (Next, Found, Value);
               end if;
            end loop;

            Ada.Text_IO.Put
              (Ada.Calendar.Formatting.Image (Ada.Calendar.Clock));
            Ada.Integer_Text_IO.Put (Integer (Next.Length));
            Ada.Text_IO.New_Line;

            if Next.Length > 1_000_000 then
               Queue.Clear;
               for X of Next loop
                  if X.PR > Release * 3 / 4 then
                     Queue.Insert (X);
                  end if;
               end loop;
            else
               Queue.Move (Source => Next);
            end if;
         end;
      end loop;

      for Item of Queue loop
         if Item.PR = Release then
            Ada.Text_IO.Put (Item.This.You);
            Ada.Text_IO.Put (" (");
            Ada.Integer_Text_IO.Put (Item.PR);
            Ada.Text_IO.Put (" ) ");
            for X of Valves loop
               if X.Index > 0 and then Item.This.Open (X.Index) then
                  Ada.Text_IO.Put (X.Name);
                  Ada.Text_IO.Put (" ");
               end if;
            end loop;
            Ada.Text_IO.New_Line;
            --  for X of Item.Path loop
            --     Ada.Text_IO.Put (X);
            --     Ada.Text_IO.Put (",");
            --  end loop;
            Ada.Text_IO.New_Line;
            Ada.Text_IO.New_Line;
         end if;
      end loop;
   end Find_Best;

   Valves  : Valve_Maps.Map;
   Index   : Valve_Index'Base := 0;
   Release : Natural;
--   Start   : Valve_Name;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
         Item : Valve;
         Pos  : Positive;
      begin
         Item.Name := Line (7 .. 8);

         Ada.Integer_Text_IO.Get (Line (24 .. Line'Last), Item.Rate, Pos);
         Pos := Pos + 25;

         while Line (Pos - 1) /= ' ' loop
            Pos := Pos + 1;
         end loop;

         if Item.Rate /= 0 then
            Index := Index + 1;
            Item.Index := Index;
         end if;

         while Pos < Line'Last loop
            Item.Pass.Append (Line (Pos .. Pos + 1));
            Pos := Pos + 4;
         end loop;

         Valves.Insert (Item.Name, Item);

      end;
   end loop;
   Find_Best (Valves, "AA", Release);
   Ada.Integer_Text_IO.Put (Release);
end Day_16;
