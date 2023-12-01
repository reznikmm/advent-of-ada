with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Sets;

procedure Day_24 is
   type Position is record
      X, Y : Positive;
   end record;

   function Hash (V : Position) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type'Mod (V.X * 1013 + V.Y));

   type Direction is (Right, Down, Left, Up);

   function Step (P : Position; D : Direction) return Position is
     (case D is
         when Right => (P.X + 1, P.Y),
         when Down  => (P.X, P.Y + 1),
         when Left  => (P.X - 1, P.Y),
         when Up    => (P.X, P.Y - 1));

   type Blizard is record
      Pos : Position;
      Dir : Direction;
   end record;

   package Blizard_Lists is new Ada.Containers.Doubly_Linked_Lists (Blizard);

   package Position_Sets is
     new Ada.Containers.Hashed_Sets (Position, Hash, "=");

   Blizards : Blizard_Lists.List;
   Height   : Natural := 0;
   Width    : Natural := 0;

   function From return Position is (2, 1);
   function Destination return Position is (Width - 1, Height);

   function Blizard_Step (P : Position; D : Direction) return Position is
      Next : constant Position := Step (P, D);
   begin
      if Next.X in 1 | Width or Next.Y in 1 | Height then
         return (case D is
                    when Right => (2, P.Y),
                    when Down  => (P.X, 2),
                    when Left  => (Width - 1, P.Y),
                    when Up    => (P.X, Height - 1));
      else
         return Next;
      end if;
   end Blizard_Step;

   subtype Variant_Index is Positive range 1 .. 5;
   type Position_Array is array (Variant_Index range <>) of Position;

   function Variants (P : Position) return Position_Array is
      Result : Position_Array (Variant_Index) := (P, others => <>);
      Index  : Positive := 1;
   begin
      if P = From then
         return (P, (P.X, P.Y + 1));
      elsif P = Destination then
         return (P, (P.X, P.Y - 1));
      end if;

      for Dir in Direction loop
         declare
            Next : constant Position := Step (P, Dir);
         begin
            if Next in From | Destination or
              (Next.X not in 1 | Width and Next.Y not in 1 | Height)
            then
               Index := Index + 1;
               Result (Index) := Next;
            end if;
         end;
      end loop;

      return Result (1 .. Index);
   end Variants;

   procedure Travel
     (From, To : Position;
      Blizards : in out Blizard_Lists.List;
      Time     : in out Natural)
   is
      Queue : Position_Sets.Set;
   begin
      Queue.Insert (From);

      loop
         declare
            Next : Position_Sets.Set;
            Test : Position_Sets.Set;
         begin
            Time := Time + 1;
            Next.Reserve_Capacity (Queue.Length);
            Test.Reserve_Capacity (Blizards.Length);

            for Blizard of Blizards loop
               Blizard.Pos := Blizard_Step (Blizard.Pos, Blizard.Dir);
               Test.Include (Blizard.Pos);
            end loop;

            for Item of Queue loop
               for V of Variants (Item) loop
                  if V = To then
                     return;
                  elsif not Test.Contains (V) then
                     Next.Include (V);
                  end if;
               end loop;
            end loop;
            Queue.Move (Source => Next);
            Ada.Integer_Text_IO.Put (Time);
            Ada.Integer_Text_IO.Put (Integer (Queue.Length));
            Ada.Text_IO.New_Line;
         end;
      end loop;
   end Travel;

begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         Width := Natural'Max (Width, Line'Length);
         Height := Height + 1;

         if Line (3) /= '#' then
            for X in Line'Range loop
               case Line (X) is
                  when '>' => Blizards.Append (((X, Height), Right));
                  when 'v' => Blizards.Append (((X, Height), Down));
                  when '<' => Blizards.Append (((X, Height), Left));
                  when '^' => Blizards.Append (((X, Height), Up));
                  when others => null;
               end case;
            end loop;
         end if;
      end;
   end loop;

   declare
      Time  : Natural := 0;
   begin
      Travel (From, Destination, Blizards, Time);
      Ada.Text_IO.Put_Line (Time'Image);
      Travel (Destination, From, Blizards, Time);
      Ada.Text_IO.Put_Line (Time'Image);
      Travel (From, Destination, Blizards, Time);
      Ada.Text_IO.Put_Line (Time'Image);
   end;
end Day_24;
