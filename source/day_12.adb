with Ada.Text_IO;
with Ada.Containers.Formal_Ordered_Sets;

procedure Day_12 with SPARK_Mode is
   Width : constant := 159;
   Height : constant := 41;

   subtype Elevation_Encoding is Character range 'a' .. 'z';
   Map : array (1 .. Height, 1 .. Width) of Elevation_Encoding;

   type Position is record
      X : Positive range 1 .. Width;
      Y : Positive range 1 .. Height;
   end record;

   subtype Item_Weight is Natural range 0 .. Width * Height;

   type Work_Item is record
      Weight : Natural range 0 .. Width * Height;
      Pos    : Position;
   end record;

   Stop : Position := (1, 1);

   subtype Position_Index is Natural range 1 .. 4;

   type Position_Array is array (Position_Index range <>) of Position;

   procedure Find_Shortest_Way
     (From   : Position;
      Length : out Natural)
   is
      W : array (1 .. Height, 1 .. Width) of Item_Weight :=
        (others => (others => Item_Weight'Last));

      function Way (Pos : Position) return Position_Array is
         X      : Positive renames Pos.X;
         Y      : Positive renames Pos.Y;
         Up     : constant Character := Character'Succ (Map (Y, X));
         Result : Position_Array (1 .. 4) := (others => (1, 1));
         Last   : Natural := 0;
      begin
         if X > 1 and then Map (Y, X - 1) <= Up then
            Last := Last + 1;
            Result (Last) := (X - 1, Y);
         end if;
         if X < W'Last (2) and then Map (Y, X + 1) <= Up then
            Last := Last + 1;
            Result (Last) := (X + 1, Y);
         end if;
         if Y > 1 and then Map (Y - 1, X) <= Up then
            Last := Last + 1;
            Result (Last) := (X, Y - 1);
         end if;
         if Y < Height and then Map (Y + 1, X) <= Up then
            Last := Last + 1;
            Result (Last) := (X, Y + 1);
         end if;

         return Result (1 .. Last);
      end Way;

      function Less (L, R : Work_Item) return Boolean is
        (L.Weight < R.Weight or else
           (L.Weight = R.Weight and then L.Pos.X < R.Pos.X) or else
             (L.Weight = R.Weight and then
              L.Pos.X = R.Pos.X and then
              L.Pos.Y < R.Pos.Y));

      package Work_Sets is new Ada.Containers.Formal_Ordered_Sets
        (Work_Item, Less);

      use type Ada.Containers.Count_Type;
      Queue : Work_Sets.Set (Height * Width);
   begin
      W (From.Y, From.X) := 0;
      Work_Sets.Insert (Queue, (0, From));
      Find_Way :
      while not Work_Sets.Is_Empty (Queue) loop
         declare
            Item : constant Work_Item := Work_Sets.First_Element (Queue);
         begin
            Work_Sets.Delete_First (Queue);

            for Next of Way (Item.Pos) loop
               if W (Next.Y, Next.X) > Item.Weight + 1 then
                  if W (Next.Y, Next.X) /= Item_Weight'Last then
                     Work_Sets.Delete
                       (Queue, Item => (W (Next.Y, Next.X), Next));
                  end if;
                  W (Next.Y, Next.X) := Item.Weight + 1;
                  Work_Sets.Insert (Queue, (W (Next.Y, Next.X), Next));

                  exit Find_Way when Stop = (Next.X, Next.Y);
               end if;
            end loop;
         end;
      end loop Find_Way;

      Length := W (Stop.Y, Stop.X);
   end Find_Shortest_Way;

   Result : Natural := Natural'Last;
   Length : Natural;
   Char   : Character;
begin
   for Item of Map loop
      Ada.Text_IO.Get (Char);

      if Char in Elevation_Encoding then
         Item := Char;
      else
         Ada.Text_IO.Put_Line ("Incorrect input");
         Item := 'a';
      end if;
   end loop;

   for J in 1 .. Height loop
      for K in 1 .. Width loop
         if Map (J, K) = 'S' then
            Map (J, K) := 'a';
         elsif Map (J, K) = 'E' then
            Stop := (K, J);
            Map (J, K) := 'z';
         end if;
      end loop;
   end loop;

   for J in 1 .. Height loop
      for K in 1 .. Width loop
         if Map (J, K) = 'a' then
            Find_Shortest_Way ((K, J), Length);
            Result := Natural'Min (Result, Length);
         end if;
      end loop;
   end loop;
   Ada.Text_IO.Put (Result'Image);
end Day_12;
