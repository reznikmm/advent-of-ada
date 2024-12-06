with Text_IO;

procedure Solution is

   type Page is range 1 .. 100;
   type Page_Order is array (Page, Page) of Boolean;
   type Page_Array is array (Positive range <>) of Page;

   package Page_IO is new Text_IO.Integer_IO (Page);

   Order  : Page_Order := (others => (others => False));
   Input  : Text_IO.File_Type;
   Result : Natural := 0;

   function Is_Ordered (List : Page_Array) return Boolean;
   procedure Sort (List : in out Page_Array);

   function Is_Ordered (List : Page_Array) return Boolean is
   begin
      for J in List'Range loop
         for K in J + 1 .. List'Last loop
            if Order (List (K), List (J)) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end Is_Ordered;

   procedure Sort (List : in out Page_Array) is
      procedure Swap (L, R : Positive) is
         Temp : Page := List (L);
      begin
         List (L) := List (R);
         List (R) := Temp;
      end Swap;

      procedure Partition (Pivot_Index : out Positive) is
         Pivot : constant Page := List (List'Last);
         Index : Positive := List'First;
      begin

         for J in List'First .. List'Last - 1 loop
            if Order (Pivot, List (J)) then
               Swap (J, Index);
               Index := Index + 1;
            end if;
         end loop;

         Swap (Index, List'Last);
         Pivot_Index := Index;
      end Partition;

   begin
      if List'Length > 0 then
         declare
            Index : Positive;
         begin
            Partition (Index);
            Sort (List (List'First .. Index - 1));
            Sort (List (Index + 1 .. List'Last));
         end;
      end if;
   end Sort;

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   while not Text_IO.End_Of_File (Input) loop
      declare
         Line  : String (1 .. 6);
         Last  : Natural;
         Left  : Page;
         Right : Page;
      begin
         Text_IO.Get_Line (Input, Line, Last);
         exit when Last = 0;
         Page_IO.Get (Line (1 .. 5), Left, Last);
         Page_IO.Get (Line (Last + 2 .. 5), Right, Last);
         Order (Left, Right) := True;
      end;
   end loop;

   while not Text_IO.End_Of_File (Input) loop
      declare
         Line  : String (1 .. 80);
         Last  : Natural;
      begin
         Text_IO.Get_Line (Input, Line, Last);

         if Last = Line'Last then
            raise Program_Error;
         end if;

         declare
            List : Page_Array (1 .. (Last + 1) / 3);
            From : Natural := Line'First;
         begin
            for J in List'Range loop
               Page_IO.Get (Line (From .. Last), List (J), From);
               From := From + 2;
            end loop;

            if not Is_Ordered (List) then
               Sort (List);
               Result := Result + Positive (List ((List'Last + 1) / 2));
            end if;
         end;
      end;
   end loop;

   Text_IO.Put_Line (Natural'Image (Result));
end Solution;
