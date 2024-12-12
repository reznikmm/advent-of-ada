with Text_IO;

procedure Solution is

   type Region;
   type Region_Access is access Region;

   type Region is record
      Area      : Natural;
      Perimeter : Positive;
      Kind      : Character;
      Next      : Region_Access;
   end record;

   Size   : constant := 140;
   Line   : array (1 .. Size) of Region_Access;
   List   : Region_Access;
   Input  : Text_IO.File_Type;
   Result : Integer := 0;

   function Get_Region (X : Natural; Char : Character) return Region_Access is
   begin
      if X in Line'Range
        and then Line (X) /= null
        and then Line (X).Kind = Char
      then
         return Line (X);
      else
         return null;
      end if;
   end Get_Region;

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   for Row in 1 .. Size loop
      for X in 1 .. Size loop
         declare
            Char : Character;
            Up   : Region_Access;
            Left : Region_Access;
         begin
            Text_IO.Get (Input, Char);
            Up := Get_Region (X, Char);
            Left := Get_Region (X - 1, Char);

            if Up = Left and Up /= null then
               Up.Area := Up.Area + 1;
            elsif Up /= null and Left /= null then
               Up.Area := Up.Area + Left.Area + 1;
               Up.Perimeter := Up.Perimeter + Left.Perimeter;
               Left.Area := 0;  --  Delete Left
               for J in 1 .. X - 1 loop
                  if Line (J) = Left then
                     Line (J) := Up;
                  end if;
               end loop;
            elsif Up /= null then
               Up.Area := Up.Area + 1;
               Up.Perimeter := Up.Perimeter + 2;
            elsif Left /= null then
               Left.Area := Left.Area + 1;
               Left.Perimeter := Left.Perimeter + 2;
               Line (X) := Left;
            else
               List := new Region'(1, 4, Char, Next => List);
               Line (X) := List;
            end if;
         end;
      end loop;
   end loop;

   while List /= null loop
      Result := Result + List.Area * List.Perimeter;
      Text_IO.Put (List.Kind);
      Text_IO.Put (Integer'Image (List.Area));
      Text_IO.Put (Integer'Image (List.Perimeter));
      Text_IO.Put_Line (Integer'Image (List.Area * List.Perimeter));
      List := List.Next;
   end loop;
   Text_IO.Put_Line (Integer'Image (Result));
end Solution;
