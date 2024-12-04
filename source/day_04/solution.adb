with Text_IO;

procedure Solution is

   Size : constant := 140;

   type String_Array is array (1 .. Size) of String (1 .. Size);

   type Step_On_Map is record
      DX, DY : Integer range -1 .. 1;
   end record;

   MMSS : constant String := "MMSS";

   Steps : constant array (1 .. 4, MMSS'Range) of Step_On_Map :=
     (1 =>                                      --  M_S
        ((-1, -1), (-1, 1), (1, 1), (1, -1)),   --  M S
      2 =>                                      --      M_M
        ((-1, 1), (1, 1), (1, -1), (-1, -1)),   --      S S
      3 =>                                      --  S_M
        ((1, 1), (1, -1), (-1, -1), (-1, 1)),   --  S M
      4 =>                                      --      S_S
        ((1, -1), (-1, -1), (-1, 1), (1, 1)));  --      M M

   Field  : String_Array;
   Input  : Text_IO.File_Type;
   Result : Natural := 0;
   Found  : Boolean;

   function Has
     (Line   : Positive;
      Column : Positive;
      DX     : Integer;
      DY     : Integer;
      Char   : Character) return Boolean is
   begin
      return Line + DY in Field'Range
        and then Column + DX in Field'Range
        and then Field (Line + DY) (Column + DX) = Char;
   end Has;

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   for J in Field'Range loop
      Text_IO.Get (Input, Field (J));
   end loop;

   for Y in Field'Range loop
      for X in Field'Range loop
         if Field (Y) (X) = 'A' then
            for Step in Steps'Range (1) loop
               Found := True;

               for Index in MMSS'Range loop
                  Found := Found and then
                    Has
                      (Line => Y,
                       Column => X,
                       DX     => Steps (Step, Index).DX,
                       DY     => Steps (Step, Index).DY,
                       Char   => MMSS (Index));
               end loop;

               if Found then
                  Result := Result + 1;
               end if;
            end loop;
         end if;
      end loop;
   end loop;

   Text_IO.Put_Line (Natural'Image (Result));
end Solution;
