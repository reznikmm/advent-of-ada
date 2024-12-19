with Text_IO;

procedure Solution is

   type Vector is record
      X, Y : Integer;
   end record;

   type Direction is (Up, Down, Left, Right);

   Size   : constant := 141;
   Input  : Text_IO.File_Type;
   From   : Vector;
   To     : Vector;
   Set    : array (1 .. 700) of Vector;
   Last   : Natural := 0;

   Wall : array (1 .. Size, 1 .. Size) of Boolean;

   type Cost_Array is array (Direction) of Natural;

   type Map_Item (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Cost : Cost_Array;
         when False =>
            null;
      end case;
   end record;

   Map  : array (1 .. Size, 1 .. Size) of Map_Item :=
     (others => (others => (Is_Set => False)));

   Step : constant array (Direction) of Vector :=
     ((0, -1), (0, 1), (-1, 0), (1, 0));

   function "+" (Left, Right : Vector) return Vector is
   begin
      return (Left.X + Right.X, Left.Y + Right.Y);
   end "+";

   function "-" (Left, Right : Vector) return Vector is
   begin
      return (Left.X - Right.X, Left.Y - Right.Y);
   end "-";

   function To_Costs (Cost : Natural; Dir : Direction) return Cost_Array is
      Result : Cost_Array;
   begin
      Result (Dir) := Cost;

      case Dir is
         when Up =>
            Result (Left) := Cost + 1000;
            Result (Right) := Cost + 1000;
            Result (Down) := Cost + 2000;
         when Down =>
            Result (Left) := Cost + 1000;
            Result (Right) := Cost + 1000;
            Result (Up) := Cost + 2000;
         when Left =>
            Result (Up) := Cost + 1000;
            Result (Down) := Cost + 1000;
            Result (Right) := Cost + 2000;
         when Right =>
            Result (Up) := Cost + 1000;
            Result (Down) := Cost + 1000;
            Result (Left) := Cost + 2000;
      end case;
      return Result;
   end To_Costs;

   function Min (L, R : Cost_Array) return Cost_Array is
      Result : Cost_Array;
   begin
      for J in Direction loop
         if L (J) < R (J) then
            Result (J) := L (J);
         else
            Result (J) := R (J);
         end if;
      end loop;
      return Result;
   end Min;

   procedure Walk (V : Vector; Cost : Natural; Dir : Direction) is
      Is_Set : constant Boolean := Map (V.Y, V.X).Is_Set;
      Costs  : Cost_Array := To_Costs (Cost, Dir);
   begin
      if V = To then
         if Is_Set then
            if Map (V.Y, V.X).Cost /= Min (Costs, Map (V.Y, V.X).Cost) then
               Costs := Min (Costs, Map (V.Y, V.X).Cost);
               Map (V.Y, V.X) := (True, Costs);
            end if;
         else
            Map (V.Y, V.X) := (True, Costs);
         end if;
         return;
      elsif not Is_Set then
         Map (V.Y, V.X) := (True, Costs);
      end if;

      if Is_Set then
         Costs := Min (Costs, Map (V.Y, V.X).Cost);
      end if;

      for J in Direction loop
         declare
            Next : constant Vector := V + Step (J);
         begin
            if not Wall (Next.Y, Next.X) then
               if not Is_Set or else
                 Map (V.Y, V.X).Cost (J) > Costs (J)
               then
                  Map (V.Y, V.X).Cost (J) := Costs (J);
                  Walk (Next, Costs (J) + 1, J);
               end if;
            end if;
         end;
      end loop;
   end Walk;

   procedure Append_To_Set (V : Vector) is
   begin
      for J in 1 .. Last loop
         if Set (J) = V then
            return;
         end if;
      end loop;

      Last := Last + 1;
      Set (Last) := V;
   end Append_To_Set;

   procedure Go_Back (To : Vector; Dir : Direction) is
      Cost : constant Natural := Map (To.Y, To.X).Cost (Dir);
   begin
      Append_To_Set (To);

      for J in Direction loop
         declare
            Next : constant Vector := To - Step (J);
         begin
            if not Wall (Next.Y, Next.X)
              and then To_Costs
                (Map (Next.Y, Next.X).Cost (J) + 1, J) (Dir) = Cost
            then
               Go_Back (Next, J);
            end if;
         end;
      end loop;
   end Go_Back;

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   for Y in Wall'Range (1) loop
      for X in Wall'Range (2) loop
         declare
            Char : Character;
         begin
            Text_IO.Get (Input, Char);
            Wall (Y, X) := Char = '#';
            if Char = 'S' then
               From := (X, Y);
            elsif Char = 'E' then
               To := (X, Y);
            end if;
         end;
      end loop;
   end loop;

   Walk (From, 0, Right);

   declare
      Min : Integer := Integer'Last;
   begin
      for J in Direction loop
         if Min > Map (To.Y, To.X).Cost (J) then
            Min := Map (To.Y, To.X).Cost (J);
         end if;
      end loop;

      Text_IO.Put_Line (Natural'Image (Min));

      for J in Direction loop
         if Min = Map (To.Y, To.X).Cost (J) then
            Go_Back (To, J);
         end if;
      end loop;

      Text_IO.Put_Line (Natural'Image (Last));
   end;
end Solution;
