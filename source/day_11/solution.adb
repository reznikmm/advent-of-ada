with Text_IO;

procedure Solution is

   subtype Stone_Id is Long_Integer range 0 .. Long_Integer'Last;
   package Integer_IO is new Text_IO.Integer_IO (Stone_Id);

   Input  : Text_IO.File_Type;
   Result : Long_Integer := 0;

   Memo   : array (Stone_Id'(0) .. 10, 2 .. 74) of Long_Integer :=
     (others => (others => -1));
   -- Even a tiny cache dramatically speeds up the solution

   function New_Stones
     (Stone  : Stone_Id;
      Count  : Natural) return Long_Integer
   is
      Len : constant Positive := Stone_Id'Image (Stone)'Length - 1;
      Result : Long_Integer;
   begin
      if Count = 0 then
         Result := 0;
      elsif Stone in Memo'Range (1)
        and then Count in Memo'Range (2)
        and then Memo (Stone, Count) /= -1
      then
         return Memo (Stone, Count);  --  cache hit
      elsif Stone = 0 then
         Result := New_Stones (1, Count - 1);
      elsif Len mod 2 = 0 then
         Result := 1
           + New_Stones (Stone / 10 ** (Len / 2), Count - 1)
           + New_Stones (Stone mod 10 ** (Len / 2), Count - 1);
      else
         Result := New_Stones (Stone * 2024, Count - 1);
      end if;

      if Stone in Memo'Range (1) and then Count in Memo'Range (2) then
         Memo (Stone, Count) := Result;
      end if;

      return Result;
   end New_Stones;

begin

   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   while not Text_IO.End_Of_File (Input) loop
      declare
         Stone : Stone_Id;
      begin
         Integer_IO.Get (Input, Stone);
         Result := Result + 1 + New_Stones (Stone, 75);
      end;
   end loop;

   Integer_IO.Put (Result);
end Solution;
