with Text_IO;

procedure Day_01 is
   type Location_Id is new Integer;
   type Location_Array is array (Positive range <>) of Location_Id;

   package Location_IO is new Text_IO.Integer_IO (Location_Id);

   Max : constant := 1000;

   function Min_Index (List : Location_Array) return Positive;
   procedure Move_Min (List : in out Location_Array);

   Left  : Location_Array (1 .. Max);
   Right : Location_Array (1 .. Max);
   Index : Natural := 0;
   Input : Text_IO.File_Type;
   Result : Location_Id := 0;

   function Min_Index (List : Location_Array) return Positive is
      Result : Positive  := List'First;
   begin
      for J in Result + 1 .. List'Last loop
         if List (Result) > List (J) then
            Result := J;
         end if;
      end loop;

      return Result;
   end Min_Index;

   procedure Move_Min (List : in out Location_Array) is
      Min   : constant Positive := Min_Index (List);
      First : constant Location_Id := List (List'First);
   begin
      List (List'First) := List (Min);
      List (Min) := First;
   end Move_Min;

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   while not Text_IO.End_Of_File (Input) loop
      Index := Index + 1;

      Location_IO.Get (Input, Left (Index));
      Location_IO.Get (Input, Right (Index));
   end loop;

   for J in 1 .. Index - 1 loop
      Move_Min (Left (J .. Index));
      Move_Min (Right (J .. Index));
   end loop;

   for J in 1 .. Index loop
      Result := Result + abs (Left (J) - Right (J));
   end loop;

   Location_IO.Put (Result);
end Day_01;
