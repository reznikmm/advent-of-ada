with Text_IO;

procedure Solution is

   type Vector is record
      X, Y : Integer;
   end record;

   type Direction is (Up, Down, Left, Right);

   Size   : constant := 141;
   Min    : constant := 20;
   Input  : Text_IO.File_Type;
   From   : Vector;
   To     : Vector;
   Result : Natural := 0;

   Wall : array (1 .. Size, 1 .. Size) of Boolean :=
     (others => (others => False));

   Map : array (1 .. Size, 1 .. Size) of Natural;

   Trace : array (1 .. 10_000) of Vector;
   Last  : Natural := 0;

   Step : constant array (Direction) of Vector :=
     ((0, -1), (0, 1), (-1, 0), (1, 0));

   function "+" (Left, Right : Vector) return Vector is
   begin
      return (Left.X + Right.X, Left.Y + Right.Y);
   end "+";

   function "-" (Dir : Direction) return Direction is
      Rev : constant array (Direction) of Direction :=
         (Down, Up, Right, Left);
   begin
      return Rev (Dir);
   end "-";

   function Dist (From, To : Vector) return Natural is
   begin
      return abs (From.X - To.X) + abs (From.Y - To.Y);
   end Dist;

   function Reach
     (From, To : Vector; Limit : Natural) return Natural
   is
      Result : Natural := Natural'Last;
      D1 : Direction := Right;
      D2 : Direction := Up;
   begin
      if From = To then
         return 0;
      elsif Limit < Dist (From, To) then
         return Result;
      end if;

      if From.X < To.X then
         D1 := Right;
      elsif From.X > To.X then
         D1 := Left;
      end if;

      if From.Y < To.Y then
         D2 := Down;
      elsif From.Y > To.Y then
         D2 := Up;
      end if;

      if From.X /= To.X then
         Result := Reach (From + Step (D1), To, Limit - 1);
      end if;

      if Result = Natural'Last then
         if From.Y /= To.Y then
            Result := Reach (From + Step (D2), To, Limit - 1);
         end if;
      end if;

      if Result /= Natural'Last then
         return Result + 1;
      end if;

      for J in Direction loop
         if (From.X = To.X or D1 /= J) and (From.Y = To.Y or D2 /= J) then
            declare
               Next : constant Vector := From + Step (J);
               Cost : constant Natural := Reach (Next, To, Limit - 1);
            begin
               if Cost < Natural'Last and then Result > Cost + 1 then
                  Result := Cost + 1;
               end if;
            end;
         end if;
      end loop;

      return Result;
   end Reach;

   package Integer_IO is new Text_IO.Integer_IO (Integer);

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   for Y in Wall'Range (2) loop
      for X in Wall'Range (2) loop
         declare
            Char : Character;
         begin
            Text_IO.Get (Input, Char);
            Wall (X, Y) := Char = '#';
            if Char = 'S' then
               From := (X, Y);
            elsif Char = 'E' then
               To := (X, Y);
            end if;
         end;
      end loop;
   end loop;

   declare
      Pos  : Vector := From;
      Time : Natural := 0;
      Dir  : Direction;
   begin
      for J in Direction loop
         declare
            Next : constant Vector := Pos + Step (J);
         begin
            Dir := -J;
            exit when not Wall (Next.X, Next.Y);
         end;
      end loop;

      while Pos /= To loop
         Map (Pos.X, Pos.Y) := Time;
         Time := Time + 1;
         Last := Last + 1;
         Trace (Last) := Pos;

         for J in Direction loop
            declare
               Next : constant Vector := Pos + Step (J);
            begin
               if not Wall (Next.X, Next.Y) and J /= Dir then
                  Pos := Next;
                  Dir := -J;
                  exit;
               end if;
            end;
         end loop;
      end loop;

      Map (Pos.X, Pos.Y) := Time;
      Last := Last + 1;
      Trace (Last) := Pos;
   end;

   for S in 1 .. Last loop
      declare
         Start : constant Vector := Trace (S);
      begin
         null;
         for D in S + 1 .. Last loop
            declare
               Way  : Positive;
               Diff : Natural;
               Dest : constant Vector := Trace (D);
               Len  : constant Natural := Dist (Start, Dest);
            begin
               if Len <= Min and then
                 Map (Dest.X, Dest.Y) - Map (Start.X, Start.Y) - Len >= 100
               then
                  Way := Reach (Start, Dest, Min);
                  Diff := Map (Dest.X, Dest.Y) - Map (Start.X, Start.Y);
                  if Diff > Way and then Diff - Way >= 100 then
                     Result := Result + 1;
                  end if;
               end if;
            end;
         end loop;
      end;
   end loop;

   Integer_IO.Put (Result);
end Solution;
