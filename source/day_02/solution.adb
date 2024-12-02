with Text_IO;

procedure Solution is
   type Level is new Integer;
   package Level_IO is new Text_IO.Integer_IO (Level);

   type Level_Array is array (Positive range <>) of Level;

   function Is_Good (List : Level_Array) return Boolean;
   function Is_Mostly_Good (List : Level_Array) return Boolean;
   function Get_Level_Array (Text : String) return Level_Array;

   Input  : Text_IO.File_Type;
   Result : Natural := 0;

   function Get_Level_Array (Text : String) return Level_Array is
      Result : Level_Array (1 .. 20);
      Last   : Natural := 0;
      From   : Positive := Text'First;
   begin
      while From <= Text'Last loop
         Last := Last + 1;
         Level_IO.Get (Text (From .. Text'Last), Result (Last), From);
         From := From + 1;
      end loop;

      return Result (1 .. Last);
   end Get_Level_Array;

   function Is_Good (List : Level_Array) return Boolean is
      Decreasing : constant Boolean := List (1) > List (2);
   begin
      for J in 2 .. List'Last loop
         if abs (List (J - 1) - List (J)) not in 1 .. 3
           or ((List (J - 1) > List (J)) /= Decreasing)
         then
            return False;
         end if;
      end loop;
      return True;
   end Is_Good;

   function Is_Mostly_Good (List : Level_Array) return Boolean is
      Try : Level_Array (1 .. List'Length - 1) := List (2 .. List'Last);
   begin
      for J in Try'Range loop
         if Is_Good (Try) then
            return True;
         end if;

         Try (J) := List (J);
      end loop;

      return Is_Good (Try);
   end Is_Mostly_Good;

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   while not Text_IO.End_Of_File (Input) loop
      declare
         Line : String (1 .. 80);
         Last : Natural;
      begin
         Text_IO.Get_Line (Input, Line, Last);

         if Is_Mostly_Good (Get_Level_Array (Line (1 .. Last))) then
            Result := Result + 1;
         end if;
      end;
   end loop;

   Text_IO.Put_Line (Natural'Image (Result));
end Solution;
