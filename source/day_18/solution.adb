with Text_IO;

procedure Solution is

   type Vector is record
      X, Y : Integer;
   end record;

   type Direction is (Up, Down, Left, Right);

   Size   : constant := 70;
   Input  : Text_IO.File_Type;
   Count  : Natural := 0;
   From   : constant Vector := (0, 0);
   To     : constant Vector := (Size, Size);

   Bad : array
     (0 .. Positive (Size), 0 .. Positive (Size)) of Boolean :=
       (others => (others => False));

   Map : array (Bad'Range (1), Bad'Range (2)) of Natural;

   Step : constant array (Direction) of Vector :=
     ((0, -1), (0, 1), (-1, 0), (1, 0));

   function "+" (Left, Right : Vector) return Vector is
   begin
      return (Left.X + Right.X, Left.Y + Right.Y);
   end "+";

   procedure Walk (V : Vector; Cost : Natural) is
   begin
      Map (V.X, V.Y) := Cost;

      for Dir in Direction loop
         declare
            Next : constant Vector := V + Step (Dir);
         begin
            if Next.X in Bad'Range (1)
              and then Next.Y in Bad'Range (2)
              and then not Bad (Next.X, Next.Y)
              and then Map (Next.X, Next.Y) > Cost + 1
            then
               Walk (Next, Cost + 1);
            end if;
         end;
      end loop;
   end Walk;

   package Integer_IO is new Text_IO.Integer_IO (Integer);

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   while not Text_IO.End_Of_File (Input) loop
      declare
         Skip : Character;
         X, Y : Integer;
      begin
         Integer_IO.Get (Input, X);
         Text_IO.Get (Input, Skip);
         Integer_IO.Get (Input, Y);
         Bad (X, Y) := True;
         Count := Count + 1;
         if Count >= 1024 then
            Map := (others => (others => Natural'Last));
            Walk (From, 0);

            Integer_IO.Put (Map (To.X, To.Y));

            if Map (To.X, To.Y) = Natural'Last then
               Integer_IO.Put (X);
               Integer_IO.Put (Y);
               exit;
            end if;
         end if;
      end;
   end loop;
end Solution;
