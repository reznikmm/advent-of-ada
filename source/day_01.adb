with Ada.Text_IO;

procedure Day_01 with SPARK_Mode is

begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : String (1 .. 7);
         Last : Natural;
      begin
         Ada.Text_IO.Get_Line (Line, Last);
      end;
   end loop;
end Day_01;
