--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

pragma Ada_2022;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
--  with Ada.Containers.Doubly_Linked_Lists;
--  with Ada.Containers.Vectors;

procedure AOC.Day_12 is

   subtype Shape_Id is Natural range 0 .. 5;

   type Shape_Count is array (Shape_Id) of Natural;

   Size : Shape_Count := [others => 0];

   function "<=" (L, R : Shape_Count) return Boolean is
     (for all J in Shape_Id => L (J) <= R (J));

   type Shape is array (1 .. 3, 1 .. 3) of Boolean;

   type Space is array (Positive range <>, Positive range <>) of Boolean;

   function Fit (Map : Space; Item : Shape; X, Y : Positive) return Boolean is
      (X + 2 in Map'Range (1) and then
       Y + 2 in Map'Range (2) and then
         (for all J in 1 .. 3 =>
           (for all K in 1 .. 3 =>
              not Item (J, K) or not Map (X + J - 1, Y + K - 1))));

   function Put (Map : Space; Item : Shape; X, Y : Positive) return Space;

   function Put (Map : Space; Item : Shape; X, Y : Positive) return Space is
   begin
      return Result : Space := Map do
         for J in 1 .. 3 loop
            for K in 1 .. 3 loop
               Result (X + J - 1, Y + K - 1) := Item (J, K);
            end loop;
         end loop;
      end return;
   end Put;

   type Natural_Array is array (Positive range <>) of Natural range 0 .. 100
     with Component_Size => 8;

   function Hash (Self : Shape) return Ada.Containers.Hash_Type;
   function Hash (Self : Shape_Count) return Ada.Containers.Hash_Type;
   function Hash (Self : Natural_Array) return Ada.Containers.Hash_Type;

   function Hash (Self : Shape) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
      Result : Ada.Containers.Hash_Type := 0;
   begin
      for Item of Self loop
         Result := 2 * @ + Boolean'Pos (Item);
      end loop;

      return Result;
   end Hash;

   function Hash (Self : Shape_Count) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
      Result : Ada.Containers.Hash_Type := 0;
   begin
      for Item of Self loop
         Result := 2 * @ + Ada.Containers.Hash_Type'Mod (Item);
      end loop;

      return Result;
   end Hash;

   function Hash (Self : Natural_Array) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
      Result : Ada.Containers.Hash_Type := 0;
   begin
      for Item of Self loop
         Result := 2 * @ + Ada.Containers.Hash_Type'Mod (Item);
      end loop;

      return Result;
   end Hash;

   package Shape_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Shape,
      Element_Type    => Shape_Id,
      Hash            => Hash,
      Equivalent_Keys => "=");

   package Shape_Count_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Shape_Count,
      Hash                => Hash,
      Equivalent_Elements => "=");

   procedure Read_And_Process
     (Total  : in out Natural;
      Shapes : Shape_Maps.Map);

   procedure Process
     (Shapes : Shape_Maps.Map;
      Width  : Positive;
      Height : Positive;
      Count  : Shape_Count;
      Ok     : out Boolean);

   procedure Process
     (Shapes : Shape_Maps.Map;
      Width  : Positive;
      Height : Positive;
      Count  : Shape_Count;
      Ok     : out Boolean)
   is
      type Mem_Key is record
         H : Natural_Array (1 .. Height);
         V : Natural_Array (1 .. Width);
      end record;

      function Put
        (Key  : Mem_Key;
         Item : Shape;
         X, Y : Positive) return Mem_Key is
      begin
         return Result : Mem_Key := Key do
            for J in 1 .. 3 loop
               for K in 1 .. 3 loop
                  if Item (J, K) then
                     Result.V (X + K - 1) := Height - (Y + J - 1);
                  end if;
                  if Item (K, J) then
                     Result.H (Y + J - 1) := Width - (X + K - 1);
                  end if;
               end loop;
            end loop;
         end return;
      end Put;

      use type Ada.Containers.Hash_Type;

      function Hash (Self : Mem_Key) return Ada.Containers.Hash_Type is
        (Hash (Self.H) + Hash (Self.V));

      type Mem_Node is record
         Good : Shape_Count_Sets.Set;
         Bad  : Shape_Count_Sets.Set;
      end record;

      package Mem_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Mem_Key,
         Element_Type    => Mem_Node,
         Hash            => Hash,
         Equivalent_Keys => "=");

      subtype Space_Map is Space (1 .. Width, 1 .. Height);

      procedure Run
        (Mem     : in out Mem_Maps.Map;
         Map     : Space_Map;
         Horizon : Mem_Key;
         Count   : Shape_Count;
         Ok      : out Boolean)
      is
      begin
         if Count = [Count'Range => 0] then
            Ok := True;
            return;
         elsif Mem.Contains (Horizon) then
            if (for some Item of Mem (Horizon).Good => Count <= Item) then
               Ok := True;
               return;
            elsif (for some Item of Mem (Horizon).Bad => Item <= Count) then
               Ok := False;
               return;
            end if;
         end if;

         All_Shapes :
         for Cursor in Shapes.Iterate
           when Count (Shape_Maps.Element (Cursor)) > 0
         loop
            declare
               Item : constant Shape := Shape_Maps.Key (Cursor);
               Down : Shape_Count := Count;
            begin
               Down  (Shape_Maps.Element (Cursor)) := @ - 1;

               for J in 1 .. Map'Last (1) - 2 loop
                  for K in 1 .. Map'Last (2) - 2
                    when Fit (Map, Item, J, K)
                  loop
                     Run
                       (Mem,
                        Put (Map, Item, J, K),
                        Put (Horizon, Item, J, K),
                        Down,
                        Ok);

                     exit All_Shapes when Ok;
                  end loop;
               end loop;
            end;
         end loop All_Shapes;

         if Mem.Contains (Horizon) then
            if Ok then
               if (for some Item of Mem (Horizon).Good => Item <= Count) then
                  declare
                     Copy : Shape_Count_Sets.Set;
                  begin
                     for Item of Mem (Horizon).Good
                       when not (Item <= Count)
                     loop
                        Copy.Insert (Item);
                     end loop;

                     Mem (Horizon).Good.Move (Source => Copy);
                  end;
               end if;
            else
               if (for some Item of Mem (Horizon).Bad => Count <= Item) then
                  declare
                     Copy : Shape_Count_Sets.Set;
                  begin
                     for Item of Mem (Horizon).Bad
                       when not (Count <= Item)
                     loop
                        Copy.Insert (Item);
                     end loop;

                     Mem (Horizon).Bad.Move (Source => Copy);
                  end;
               end if;
            end if;
         elsif Ok then
            Mem.Insert (Horizon, (Good => [Count], Bad => []));
         else
            Mem.Insert (Horizon, (Good => [], Bad => [Count]));
         end if;
      end Run;

      Mem : Mem_Maps.Map;

      Map : constant Space (1 .. Width, 1 .. Height) :=
        [others => [others => False]];

      Horizon : constant Mem_Key :=
        (H => [others => Height], V => [others => Width]);

      Area    : Integer := Width * Height;

   begin
      for J in Shape_Id loop
         Area := @ - Size (J) * Count (J);
      end loop;

      Ok := Area >= 0;

      --  Run (Mem, Map, Horizon, Count, Ok);
   end Process;

   procedure Read_And_Process
     (Total  : in out Natural;
      Shapes : Shape_Maps.Map)
   is
      Counts : Shape_Count;
      Width  : Positive;
      Height : Positive;
      Char   : Character;
      Text   : String (1 .. 2);
   begin
      Ada.Integer_Text_IO.Get (Width);
      Ada.Text_IO.Get (Char);
      pragma Assert (Char = 'x');
      Ada.Text_IO.Get (Text);

      if Text (2) = ':' then
         Height := Natural'Value (Text (1 .. 1));
      else
         Height := Natural'Value (Text);
         Ada.Text_IO.Get (Char);
         pragma Assert (Char = ':');
      end if;

      for Item of Counts loop
         Ada.Integer_Text_IO.Get (Item);
      end loop;

      declare
         Ok  : Boolean;
      begin
         Process (Shapes, Width, Height, Counts, Ok);
         Ada.Text_IO.Put_Line (Ok'Image);
         if Ok then
            Total := @ + 1;
         end if;
      end;
   end Read_And_Process;

   procedure Rotate_Shapes (Shapes : in out Shape_Maps.Map);

   function Swap (M : Shape) return Shape is
      [for J in 1 .. 3 => [for K in 1 .. 3 => M (J, 4 - K)]];

   function Rotate (M : Shape; J : Positive) return Shape is
     (case J is
         when 1 => M,
         when 2 =>
            [for J in 1 .. 3 => [for K in 1 .. 3 => M (4 - K, J)]],
         when 3 =>
            [for J in 1 .. 3 => [for K in 1 .. 3 => M (4 - J, 4 - K)]],
         when 4 =>
            [for J in 1 .. 3 => [for K in 1 .. 3 => M (K, 4 - J)]],
         when others => Rotate (Swap (M), J - 4));

   procedure Rotate_Shapes (Shapes : in out Shape_Maps.Map) is
      Copy : constant Shape_Maps.Map := Shapes;
   begin
      for Cursor in Copy.Iterate loop
         for J in 2 .. 8 loop
            declare
               Next : constant Shape := Rotate (Shape_Maps.Key (Cursor), J);
            begin
               if not Shapes.Contains (Next) then
                  Shapes.Insert (Next, Shape_Maps.Element (Cursor));
               end if;
            end;
         end loop;
      end loop;
   end Rotate_Shapes;

   Shapes : Shape_Maps.Map;

   Total : Natural := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Id   : Natural;
         Next : Shape;
         Char : Character;
      begin
         --  Ada.Integer_Text_IO.Get (Id);
         Ada.Text_IO.Get (Char);
         Id := Natural'Value ([Char]);
         Ada.Text_IO.Get (Char);
         pragma Assert (Char = ':');
         for Item of Next loop
            Ada.Text_IO.Get (Char);
            pragma Assert (Char in '#' | '.');
            Item := Char = '#';
            Size (Id) := @ + (if Char = '#' then 1 else 0);
         end loop;

         pragma Assert (Id = Natural (Shapes.Length));
         Shapes.Insert (Next, Id);

         exit when Id = Shape_Id'Last;
      end;
   end loop;

   Rotate_Shapes (Shapes);

   while not Ada.Text_IO.End_Of_File loop
      Read_And_Process (Total, Shapes);
   end loop;

   Ada.Text_IO.Put_Line (Shapes.Length'Image);
   Ada.Text_IO.Put_Line (Total'Image);
end AOC.Day_12;
