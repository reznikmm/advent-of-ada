with Text_IO;

procedure Solution is

   package Integer_IO is new Text_IO.Integer_IO (Integer);

   procedure Process_Line
     (Line    : String;
      Enabled : in out Boolean;
      Result  : in out Natural);

   Enabled : Boolean := True;
   Result  : Natural := 0;
   Input   : Text_IO.File_Type;

   procedure Process_Line
     (Line    : String;
      Enabled : in out Boolean;
      Result  : in out Natural)
   is

      L     : Natural := 0;
      R     : Natural := 0;

      Mul  : constant String := "mul(LLL,RRR)";
      Dont : constant String := "don't()";
      Mul_Index  : Positive := Mul'First;
      Dont_Index : Positive := Dont'First;

      procedure Increment is
      begin
         if Enabled then
            Result := Result + L * R;
         end if;
      end Increment;

      procedure Reset is
      begin
         L := 0;
         R := 0;
         Mul_Index := Mul'First;
      end Reset;
   begin
      for J in Line'Range loop
         case Mul (Mul_Index) is
            when 'L' =>
               if Line (J) in '0' .. '9' then
                  L := L * 10 + Character'Pos (Line (J)) - Character'Pos ('0');
                  Mul_Index := Mul_Index + 1;
               elsif Line (J) = ',' then
                  Mul_Index := 9;  --  'R' position
               else
                  Reset;
               end if;
            when 'R' =>
               if Line (J) in '0' .. '9' then
                  R := R * 10 + Character'Pos (Line (J)) - Character'Pos ('0');
                  Mul_Index := Mul_Index + 1;
               elsif Line (J) = ')' then
                  Increment;
                  Reset;
               else
                  Reset;
               end if;
            when others =>
               if Line (J) /= Mul (Mul_Index) then
                  Reset;
               elsif Mul (Mul_Index) = ')' then
                  Increment;
                  Reset;
               else
                  Mul_Index := Mul_Index + 1;
               end if;
         end case;

         if Dont (Dont_Index) = 'n' and Line (J) = '(' then
            Dont_Index := Dont'Last;
         elsif Dont (Dont_Index) /= Line (J) then
            Dont_Index := Dont'First;
         elsif Dont_Index < Dont'Last then
            Dont_Index := Dont_Index + 1;
         else
            Enabled := Line (J - 2) = 'o';
            Dont_Index := Dont'First;
         end if;

      end loop;
   end Process_Line;

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   while not Text_IO.End_Of_File (Input) loop
      declare
         Line : String (1 .. 4000);
         Last : Natural;
      begin
         Text_IO.Get_Line (Input, Line, Last);
         Process_Line (Line (1 .. Last), Enabled, Result);
      end;
   end loop;

   Integer_IO.Put (Result);
end Solution;
