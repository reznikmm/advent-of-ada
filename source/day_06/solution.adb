with Text_IO;

procedure Solution is

   Size : constant := 130;

   type Lab_Map is array (1 .. Size, 1 .. Size) of Boolean;

   type Direction is (Up, Right, Down, Left);

   DX : constant array (Direction) of Integer := (0, 1, 0, -1);
   DY : constant array (Direction) of Integer := (-1, 0, 1, 0);

   Map    : Lab_Map;
   Input  : Text_IO.File_Type;
   Result : Natural := 0;
   Init_X : Positive range Map'Range (2);
   Init_Y : Positive range Map'Range (1);

   function On_Map (X, Y : Natural) return Boolean is
   begin
      return X in Map'Range (2) and Y in Map'Range (1);
   end On_Map;

   function Has_Cycle return Boolean is
      Done    : Lab_Map := (others => (others => False));
      On_Trac : Boolean := False;
      X       : Natural;
      Y       : Natural;
      Guard_X : Positive range Map'Range (2) := Init_X;
      Guard_Y : Positive range Map'Range (1) := Init_Y;
      Trac_X  : Positive range Map'Range (2);
      Trac_Y  : Positive range Map'Range (1);
      Result  : Boolean := False;
      Facing  : Direction := Up;
   begin
      Done (Guard_X, Guard_Y) := True;

      loop
         loop
            X := Guard_X + DX (Facing);
            Y := Guard_Y + DY (Facing);

            exit when not On_Map (X, Y) or else Map (X, Y);

            if Facing = Direction'Last then
               Facing := Direction'First;
            else
               Facing := Direction'Succ (Facing);
            end if;
         end loop;

         exit when not On_Map (X, Y);

         Guard_X := X;
         Guard_Y := Y;

         if not Done (X, Y) then
            On_Trac := False;
            Done (X, Y) := True;
         elsif not On_Trac then
            On_Trac := True;
            Trac_X := X;
            Trac_Y := Y;
         elsif X = Trac_X and Y = Trac_Y then
            Result := True;
            exit;
         end if;
      end loop;

      return Result;
   end Has_Cycle;

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   declare
      X : Positive := 1;
      Y : Positive := 1;
   begin
      while not Text_IO.End_Of_File (Input) loop
         declare
            Char  : Character;
         begin
            Text_IO.Get (Input, Char);
            Map (X, Y) := Char /= '#';

            if Char = '^' then
               Init_X := X;
               Init_Y := Y;
            end if;

            if X < Map'Last (2) then
               X := X + 1;
            else
               X := 1;
               Y := Y + 1;
            end if;
         end;
      end loop;
   end;

   for Row in Map'Range (1) loop
      for Column in Map'Range (2) loop
         if Map (Column, Row) and (Init_X /= Column or Init_Y /= Row) then
            Map (Column, Row) := False;

            if Has_Cycle then
               Result := Result + 1;
            end if;

            Map (Column, Row) := True;
         end if;
      end loop;
   end loop;

   Text_IO.Put_Line (Natural'Image (Result));
end Solution;
