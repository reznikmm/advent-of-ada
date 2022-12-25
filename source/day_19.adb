with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Containers.Ordered_Sets;

procedure Day_19 is
   type Item_Kind is (Ore, Clay, Obsidian, Geode);
   type Kind_Array is array (Item_Kind range <>) of Natural;
   type Cost_Matrix is array (Item_Kind) of Kind_Array (Ore .. Obsidian);

   type Blueprint is record
      Id    : Positive;
      Costs : Cost_Matrix := (others => (others => 0));
   end record;

   Digit : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set ("0123456789");

   procedure Read (Result : out Blueprint) is
      Line : constant String := Ada.Text_IO.Get_Line;
      From : Positive := 1;

      procedure Read_Integer (Value : out Natural) is
         To : Natural;
      begin
         Ada.Strings.Fixed.Find_Token
           (Line (From .. Line'Last), Digit, Ada.Strings.Inside, From, To);
         Value := Natural'Value (Line (From .. To));
         From := To + 1;
      end Read_Integer;

   begin
      Read_Integer (Result.Id);
      Read_Integer (Result.Costs (Ore) (Ore));
      Read_Integer (Result.Costs (Clay) (Ore));
      Read_Integer (Result.Costs (Obsidian) (Ore));
      Read_Integer (Result.Costs (Obsidian) (Clay));
      Read_Integer (Result.Costs (Geode) (Ore));
      Read_Integer (Result.Costs (Geode) (Obsidian));
   end Read;

   procedure Calculate
     (Plan : Blueprint; Geodes : out Natural; Time : Positive)
   is
      type Work_Item is record
         Robots : Kind_Array (Item_Kind);
         Items  : Kind_Array (Item_Kind);
      end record;

      function Hash (L : Work_Item) return Ada.Containers.Hash_Type is
         use type Ada.Containers.Hash_Type;
         Result : Ada.Containers.Hash_Type := 0;
      begin
         for X of L.Items loop
            Result := 1013 * Result + Ada.Containers.Hash_Type'Mod (X);
         end loop;

         for X of L.Robots loop
            Result := 1013 * Result + Ada.Containers.Hash_Type'Mod (X);
         end loop;

         return Result;
      end Hash;

      function "<" (L, R : Work_Item) return Boolean is
        (L.Robots < R.Robots or else
           (L.Robots = R.Robots and then L.Items < R.Items));

      package Work_Item_Lists is new Ada.Containers.Ordered_Sets
        (Work_Item, "<", "=");

      Max_Ore : Natural := 0;
      Queue   : Work_Item_Lists.Set;
      Next    : Work_Item_Lists.Set;
   begin
      for X of Plan.Costs loop
         Max_Ore := Natural'Max (Max_Ore, X (Ore));
      end loop;

      Geodes := 0;
      Queue.Insert
        ((Robots => (Ore => 1, others => 0), Items => (others => 0)));

      for Minute in 1 .. Time loop
         for Item of reverse Queue loop
            declare
               procedure Append (Copy : Work_Item) is
                  Cursor : Work_Item_Lists.Cursor;
               begin
                  if Minute = Time then
                     Geodes := Natural'Max (Geodes, Copy.Items (Geode));
                  else
                     Cursor := Next.Ceiling ((Copy.Robots, (others => 0)));
                     while Work_Item_Lists.Has_Element (Cursor) loop
                        declare
                           Item : constant Work_Item := Next (Cursor);
                        begin
                           exit when Item.Robots /= Copy.Robots;

                           if (for all X in Copy.Items'Range =>
                                Copy.Items (X) <= Item.Items (X))
                           then
                              return;
                           elsif (for all X in Copy.Items'Range =>
                                Copy.Items (X) >= Item.Items (X))
                           then
                              Next.Delete (Cursor);
                              Append (Copy);
                              return;
                           end if;

                           Cursor := Work_Item_Lists.Next (Cursor);
                        end;
                     end loop;
                     Next.Include (Copy);
                  end if;
               end Append;

               Copy : Work_Item;
            begin
               for Robot in reverse Plan.Costs'Range loop
                  --  If we need a robot and have enough items
                  if (case Robot is
                        when Ore =>
                          Item.Robots (Ore) < Max_Ore
                          and then
                          (for some X in Clay .. Obsidian =>
                            Item.Robots (X) <
                              Plan.Costs (Item_Kind'Succ (X)) (X)),
                        when Clay =>
                          Item.Robots (Robot) < Plan.Costs (Obsidian) (Robot),
                        when Obsidian =>
                          Item.Robots (Robot) < Plan.Costs (Geode) (Robot),
                        when Geode    => True)
                     and then
                       (for all X in Plan.Costs (Robot)'Range =>
                       Item.Items (X) >= Plan.Costs (Robot) (X))
                  then
                     Copy := Item;

                     for X in Plan.Costs (Robot)'Range loop
                        Copy.Items (X) := Copy.Items (X) -
                          Plan.Costs (Robot) (X);
                     end loop;

                     for X in Copy.Robots'Range loop
                        Copy.Items (X) := Copy.Items (X) + Copy.Robots (X);
                     end loop;

                     Copy.Robots (Robot) := Copy.Robots (Robot) + 1;

                     Append (Copy);
                  end if;
               end loop;

               Copy := Item;

               for X in Copy.Robots'Range loop
                  Copy.Items (X) := Copy.Items (X) + Copy.Robots (X);
               end loop;

               Append (Copy);
            end;
         end loop;

         Queue.Move (Source => Next);
         Ada.Integer_Text_IO.Put (Minute);
         Ada.Integer_Text_IO.Put (Integer (Queue.Length));
         Ada.Integer_Text_IO.Put (Geodes);
         Ada.Text_IO.New_Line;
      end loop;
   end Calculate;

   Total : Natural := 0;
begin
   for J in 1 .. 3 loop
      --  while not Ada.Text_IO.End_Of_File loop
      declare
         Item : Blueprint;
         Count : Natural := 0;
      begin
         Read (Item);
         Calculate (Item, Count, 32);
         Total := Total + Item.Id * Count;
      end;
   end loop;

   Ada.Integer_Text_IO.Put (Total);
end Day_19;
