with Text_IO;

procedure Day_01 is
   type Location_Id is new Integer;

   type Count_Array is array (Location_Id range <>) of Natural;

   package Location_IO is new Text_IO.Integer_IO (Location_Id);

   Max_Id : constant := 99_999;

   Left   : Count_Array (1 .. Max_Id) := (others => 0);
   Right  : Count_Array (1 .. Max_Id) := (others => 0);
   Input  : Text_IO.File_Type;
   Result : Location_Id := 0;

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   while not Text_IO.End_Of_File (Input) loop
      declare
         Id : Location_Id;
      begin
         Location_IO.Get (Input, Id);
         Left (Id) := Left (Id) + 1;

         Location_IO.Get (Input, Id);
         Right (Id) := Right (Id) + 1;
      end;
   end loop;

   for J in Left'Range loop
      Result := Result +
        Location_Id (Left (J)) * Location_Id (Right (J)) * J;
   end loop;

   Location_IO.Put (Result);
end Day_01;
