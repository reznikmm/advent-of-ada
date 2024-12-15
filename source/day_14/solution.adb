with Text_IO;

procedure Solution is

   type Vector is record
      X, Y : Integer;
   end record;

   type Robot is record
      Position : Vector;
      Velocity : Vector;
   end record;

   List   : array (1 .. 500) of Robot;
   Size_X : constant := 101;
   Size_Y : constant := 103;

   Input  : Text_IO.File_Type;

   Max : Natural := 0;

   function "+" (Left, Right : Vector) return Vector is
   begin
      return ((Left.X + Right.X) mod Size_X, (Left.Y + Right.Y) mod Size_Y);
   end "+";

   function Is_Egg return Boolean is
      Linked : Natural := 0;
   begin
      for J in List'Range loop
         for K in J + 1 .. List'Last loop
            if abs (List (J).Position.X - List (K).Position.X) <= 1
              and then abs (List (J).Position.Y - List (K).Position.Y) <= 1
            then
               Linked := Linked + 1;
            end if;
         end loop;
      end loop;

      if Linked > Max then
         Max := Linked;
         return True;
      else
         return False;
      end if;
   end Is_Egg;

   package Integer_IO is new Text_IO.Integer_IO (Integer);

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   for J in List'Range loop
      declare
         Skip_1   : Character;
         Skip_2   : String (1 .. 2);
         Skip_3   : String (1 .. 3);
         Position : Vector renames List (J).Position;
         Velocity : Vector renames List (J).Velocity;
      begin
         Text_IO.Get (Input, Skip_2);
         Integer_IO.Get (Input, Position.X);
         Text_IO.Get (Input, Skip_1);
         Integer_IO.Get (Input, Position.Y);
         Text_IO.Get (Input, Skip_3);
         Integer_IO.Get (Input, Velocity.X);
         Text_IO.Get (Input, Skip_1);
         Integer_IO.Get (Input, Velocity.Y);
      end;
   end loop;

   for J in 1 .. 1_000_000 loop
      for K in List'Range loop
         List (K).Position := List (K).Position + List (K).Velocity;
      end loop;
      if Is_Egg or else J mod 100_000 = 0 then
         Text_IO.Put ("J=");
         Integer_IO.Put (J);
         Integer_IO.Put (Max);
         Text_IO.New_Line;
         for Y in 0 .. Positive (Size_Y - 1) loop
            for X in 0 .. Positive (Size_X - 1) loop
               declare
                  Count : Natural := 0;
               begin
                  for R in List'Range loop
                     if List (R).Position = (X, Y) then
                        Count := Count + 1;
                     end if;
                  end loop;
                  if Count = 0 then
                     Text_IO.Put (' ');
                  else
                     Text_IO.Put ('x');
                  end if;
               end;
            end loop;
            Text_IO.New_Line;
         end loop;

         exit when Max > 900;
      end if;
   end loop;
end Solution;
