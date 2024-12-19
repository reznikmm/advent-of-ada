with Text_IO;

procedure Solution is

   package Towels is
      procedure Initialize (Text : String);
      procedure Reset;
      function Split (Text : String) return Long_Integer;
   private
      type Color is (White, Blue, Black, Red, Green);

      subtype Color_Count is Natural range 0 .. 9;

      function To_Color (C : Character) return Color;

      Index : array
        (Color, Color_Count range 1 .. Color_Count'Last) of Natural :=
          (others => (others => 0));
      Next  : array (1 .. 3000) of Natural := (others => 0);
      Memo  : array (1 .. 40) of Long_Integer;
      Line : String (1 .. 3000);
      Last : Natural;
   end Towels;

   Input : Text_IO.File_Type;
   Result : Long_Integer := 0;

   package body Towels is
      procedure Initialize (Text : String) is
         From : Positive := 1;

         procedure Append (From, To : Positive) is
            First : constant Color := To_Color (Line (From));
            Length : constant Color_Count :=
              Color_Count (To - From + 1);
         begin
            Next (From) := Index (First, Length);
            Index (First, Length) := From;
         end Append;

      begin
         Line (Text'Range) := Text;
         Last := Text'Last;

         for J in Text'Range loop
            if Text (J) = ',' then
               Append (From, J - 1);
            elsif Text (J) = ' ' then
               From := J + 1;
            end if;
         end loop;

         Append (From, Text'Last);
      end Initialize;

      procedure Reset is
      begin
         Memo := (others => Long_Integer'Last);
      end Reset;

      function Split (Text : String) return Long_Integer is
         From   : constant Positive := Text'First;
         Len    : Natural := Text'Length;
         Result : Long_Integer := 0;
      begin
         if Len = 0 then
            return 1;
         elsif Text'Length in Memo'Range
           and then Memo (Text'Length) /= Long_Integer'Last
         then
            return Memo (Text'Length);
         elsif Len > Color_Count'Last then
            Len := Color_Count'Last;
         end if;

         for J in reverse 1 .. Len loop
            declare
               Item : Natural :=
                 Index (To_Color (Text (From)), J);
            begin
               while Item /= 0 loop
                  if Line (Item .. Item + J - 1)
                    = Text (From .. From + J - 1)
                  then
                     Result := Result + Split (Text (From + J .. Text'Last));
                  end if;
                  Item := Next (Item);
               end loop;
            end;
         end loop;

         if Text'Length in Memo'Range then
            Memo (Text'Length) := Result;
         end if;

         return Result;
      end Split;

      function To_Color (C : Character) return Color is
      begin
         case C is
            when 'w' => return White;
            when 'u' => return Blue;
            when 'b' => return Black;
            when 'r' => return Red;
            when 'g' => return Green;
            when others =>
               raise Constraint_Error;
         end case;
      end To_Color;
   end Towels;

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");
   declare
      Line : String (1 .. 3000);
      Last : Natural;
   begin
      Text_IO.Get_Line (Input, Line, Last);
      Towels.Initialize (Line (1 .. Last));
   end;

   while not Text_IO.End_Of_File (Input) loop
      declare
         Line : String (1 .. 80);
         Last : Natural;
      begin
         Text_IO.Get_Line (Input, Line, Last);
         if Last > 0 then
            Towels.Reset;
            Result := Result + Towels.Split (Line (1 .. Last));
         end if;
      end;
   end loop;

   Text_IO.Put (Long_Integer'Image (Result));
end Solution;
