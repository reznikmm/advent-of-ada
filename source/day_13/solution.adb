with Text_IO;

procedure Solution is

   type Vector is record
      X, Y : Long_Integer;
   end record;

   Button_A : Vector;
   Button_B : Vector;
   Prize    : Vector;
   Input  : Text_IO.File_Type;
   Result : Long_Integer := 0;

   function "+" (Left, Right : Vector) return Vector is
   begin
      return (Left.X + Right.X, Left.Y + Right.Y);
   end "+";

   function "-" (Left, Right : Vector) return Vector is
   begin
      return (Left.X - Right.X, Left.Y - Right.Y);
   end "-";

   function "*" (Left : Vector; Right : Long_Integer) return Vector is
   begin
      return (Left.X * Right, Left.Y * Right);
   end "*";

   package Integer_IO is new Text_IO.Integer_IO (Long_Integer);

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   while not Text_IO.End_Of_File (Input) loop
      declare
         Line : String (1 .. 80);
         Last : Natural;
         To   : Natural;
      begin
         Text_IO.Get_Line (Input, Line, Last);
         Integer_IO.Get (Line (12 .. Last), Button_A.X, To);
         Integer_IO.Get (Line (To + 4 .. Last), Button_A.Y, To);
         Text_IO.Get_Line (Input, Line, Last);
         Integer_IO.Get (Line (12 .. Last), Button_B.X, To);
         Integer_IO.Get (Line (To + 4 .. Last), Button_B.Y, To);
         Text_IO.Get_Line (Input, Line, Last);
         Integer_IO.Get (Line (10 .. Last), Prize.X, To);
         Integer_IO.Get (Line (To + 5 .. Last), Prize.Y, To);
         Prize := Prize + (10000000000000, 10000000000000);
         if not Text_IO.End_Of_File (Input) then
            Text_IO.Get_Line (Input, Line, Last);
         end if;
      end;

      declare
         A1   : constant Long_Integer :=
           (Prize.X * Button_B.Y - Prize.Y * Button_B.X);
         A2   : constant Long_Integer :=
           (Button_A.X * Button_B.Y - Button_A.Y * Button_B.X);
         A    : constant Long_Integer := A1 / A2;
         B    : constant Long_Integer :=
           (Prize.Y - Button_A.Y * A) / Button_B.Y;
         Cost : constant Long_Integer := 3 * A + B;
         Pos  : constant Vector := Prize - Button_A * A - Button_B * B;
      begin
         if Pos = (0, 0) then
            Result := Result + Cost;
         end if;
      end;
   end loop;

   Integer_IO.Put (Result);
end Solution;
