with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Day_10 with SPARK_Mode is
   type Command is (Noop, Addx);
   package Command_IO is new Ada.Text_IO.Enumeration_IO (Command);

   CRT : String (1 .. 40) := (others => ' ');

   procedure Draw (Tick : Natural; X : Integer) is
      Pixel : constant Natural range 0 .. 39 := (Tick - 1) mod 40;
   begin
      --  CRT (Pixel + 1) := (if Pixel in X - 1 .. X + 1 then '#' else '.');
      CRT (Pixel + 1) :=
        (if Pixel + 1 >= X and then Pixel - 1 <= X then '#' else '.');
   end Draw;

   Next : Positive := CRT'Length;
   Tick : Natural := 0;
   X    : Integer := 1;
begin
   while not Ada.Text_IO.End_Of_File and then Tick < 40 * 6 loop
      declare
         Cmd : Command;
         Arg : Integer;
      begin
         Command_IO.Get (Cmd);

         case Cmd is
            when Noop =>
               Tick := Tick + 1;
               Draw (Tick, X);

               Arg := 0;
            when Addx =>
               Ada.Integer_Text_IO.Get (Arg);
               Tick := Tick + 1;
               Draw (Tick, X);
               Tick := Tick + 1;
               Draw (Tick, X);
         end case;

         if Tick >= Next then
            Ada.Text_IO.Put_Line (CRT);
            Next := Next + CRT'Length;
         end if;

         if Arg not in -50 .. 50 then
            Ada.Text_IO.Put_Line ("Invalid argument");
            return;
         elsif X not in Integer'First + 50 .. Integer'Last - 50 then
            Ada.Text_IO.Put_Line ("Should never happen");  --  How to prove?
            return;
         else
            X := X + Arg;
         end if;
      end;
   end loop;
end Day_10;
