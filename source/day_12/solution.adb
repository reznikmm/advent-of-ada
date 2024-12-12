with Text_IO;

procedure Solution is

   type Region;
   type Region_Access is access Region;

   type Region is record
      Area      : Natural;
      Perimeter : Positive;
      Kind      : Character := ' ';
      Next      : Region_Access;
   end record;

   type Cell is record
      R           : Region_Access;
      Left_Angle  : Boolean;
      Right_Angle : Boolean;
      Upper_Angle : Boolean;
   end record;

   Size   : constant := 140;
   Line   : array (1 .. Size) of Cell;
   List   : Region_Access;
   Input  : Text_IO.File_Type;
   Char   : Character;
   Result : Integer := 0;

   function Get_Region (X : Natural; Char : Character) return Cell is
   begin
      if X in Line'Range
        and then Line (X).R /= null
        and then Line (X).R.Kind = Char
      then
         return Line (X);
      else
         return (null, others => False);
      end if;
   end Get_Region;

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   for Row in 1 .. Size loop
      for X in 1 .. Size loop
         Text_IO.Get (Input, Char);

         declare
            Up   : Cell renames Line (X);
            Left : constant Cell := Get_Region (X - 1, Char);
         begin
            if Up.R = Left.R and Up.R /= null then
               --  The region on the left and above
               Up.R.Area := Up.R.Area + 1;

               if Up.Right_Angle then
                  Up.R.Perimeter := Up.R.Perimeter - 2;
               end if;
               Line (X - 1).Right_Angle := False;
               Up.Left_Angle := False;
               Up.Right_Angle := True;
               Up.Upper_Angle := False;

            elsif Up.R /= null
              and then Up.R.Kind = Char
              and then Left.R /= null
            then
               --  The same as before, but regions are disjoined
               Up.R.Area := Up.R.Area + Left.R.Area + 1;
               Up.R.Perimeter := Up.R.Perimeter + Left.R.Perimeter;
               if Up.Right_Angle then
                  Up.R.Perimeter := Up.R.Perimeter - 2;
               end if;
               Left.R.Area := 0;  --  Delete Left region, remap it to Up
               for J in 1 .. X - 1 loop
                  if Line (J).R = Left.R then
                     Line (J).R := Up.R;
                  end if;
               end loop;
               Line (X - 1).Right_Angle := False;
               Up.Left_Angle := False;
               Up.Right_Angle := True;
               Up.Upper_Angle := False;

            elsif Up.R /= null and then Up.R.Kind = Char then
               --  The region is above
               Up.R.Area := Up.R.Area + 1;
               if not Up.Left_Angle and not Up.Right_Angle then
                  Up.R.Perimeter := Up.R.Perimeter + 4;
               elsif Up.Left_Angle /= Up.Right_Angle then
                  Up.R.Perimeter := Up.R.Perimeter + 2;
               end if;
               Up.Left_Angle := True;
               Up.Right_Angle := True;
               Up.Upper_Angle := False;

            elsif Left.R /= null then
               --  The region on the left above
               Left.R.Area := Left.R.Area + 1;
               if not Left.Upper_Angle then
                  Left.R.Perimeter := Left.R.Perimeter + 2;
               end if;
               Up := Left;
               Line (X - 1).Right_Angle := False;
               Up.Left_Angle := False;
               Up.Right_Angle := True;
               Up.Upper_Angle := True;

            else
               --  A new disjoinded region
               List := new Region'(1, 4, Char, Next => List);
               Up.R := List;
               Up.Left_Angle := True;
               Up.Right_Angle := True;
               Up.Upper_Angle := True;
            end if;
         end;
      end loop;
   end loop;

   while List /= null loop
      Result := Result + List.Area * List.Perimeter;
      List := List.Next;
   end loop;

   Text_IO.Put_Line (Integer'Image (Result));
end Solution;
