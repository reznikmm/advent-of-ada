with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Formal_Ordered_Sets;

procedure Day_07 with SPARK_Mode is

   Disk_Size   : constant := 70_000_000;
   Update_Size : constant := 30_000_000;
   Max_Dirs    : constant := 500;

   subtype File_Size is Natural range 0 .. Disk_Size;

   package Size_Sets is new
     Ada.Containers.Formal_Ordered_Sets (File_Size, "<");

   procedure Traverse
     (Size : out File_Size;
      Set  : in out Size_Sets.Set;
      Fail : out Boolean)
   is
      use type Ada.Containers.Count_Type;
   begin
      Fail := False;
      Size := 0;

      while not Ada.Text_IO.End_Of_File and then not Fail loop
         declare
            Line      : String (1 .. 20);
            Last      : Natural;
            Item_Size : File_Size := 0;

         begin
            Ada.Text_IO.Get_Line (Line, Last);

            exit when Line (1 .. Last) = "$ cd ..";

            if Last > 4 and then Line (1 .. 5) = "$ cd " then
               Traverse (Item_Size, Set, Fail);

            elsif Last > 0 and then Line (1) in '0' .. '9' then
               declare
                  Input  : Integer;
                  Ignore : Natural;
               begin
                  Ada.Integer_Text_IO.Get (Line (1 .. Last), Input, Ignore);

                  if Input in File_Size then
                     Item_Size := Input;
                  else
                     Fail := True;
                  end if;
               end;
            end if;

            if Size <= File_Size'Last - Item_Size then
               Size := Size + Item_Size;
            else
               Fail := True;
            end if;
         end;
      end loop;

      if Size_Sets.Length (Set) < Set.Capacity then
         Size_Sets.Include (Set, Size);
      else
         Fail := True;
      end if;
   end Traverse;

   Need      : Integer;
   Root_Size : File_Size;
   Failed    : Boolean;
   Set       : Size_Sets.Set (Capacity => Max_Dirs);  --  Directories sizes
begin
   Traverse (Root_Size, Set, Failed);

   Need := Update_Size - (Disk_Size - Root_Size);

   if Failed then
      Ada.Text_IO.Put_Line ("Invalid input");
   elsif Need > 0 then
      declare
         Cursor : constant Size_Sets.Cursor := Size_Sets.Ceiling (Set, Need);
      begin
         --  This condition should be always True, because '/' contains all
         --  files. In other words the `Set` contains `Root_Size`.
         if Size_Sets.Has_Element (Set, Cursor) then
            Ada.Integer_Text_IO.Put (Size_Sets.Element (Set, Cursor));
         end if;
      end;
   end if;
end Day_07;
