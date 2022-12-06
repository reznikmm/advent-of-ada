with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Day_06 with SPARK_Mode is
   Prefix : String (1 .. 14) := (others => ' ');
   Index  : Positive := Prefix'Last - 1;
begin
   Ada.Text_IO.Get (Prefix (1 .. Index));

   while not Ada.Text_IO.End_Of_File and then Index < Positive'Last loop
      Ada.Text_IO.Get (Prefix ((Index mod Prefix'Length) + 1));

      Index := Index + 1;

      if (for all J in Prefix'Range =>
           (for all Char of Prefix (J + 1 .. Prefix'Last) =>
               Char /= Prefix (J)))
      then
         Ada.Integer_Text_IO.Put (Index);
         exit;
      end if;
   end loop;
end Day_06;
