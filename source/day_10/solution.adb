with Text_IO;

procedure Solution is

   Size : constant := 41;

   type Vector is record
      X, Y : Integer;
   end record;

   type Vector_Array is array (Positive range <>) of Vector;

   type Direction is (Up, Right, Down, Left);

   DV : constant array (Direction) of Vector :=
     ((0, 1), (1, 0), (0, -1), (-1, 0));

   subtype Hight is Character range '0' .. '9';

   Map : array (1 .. Size, 1 .. Size) of Hight;

   Result : Natural := 0;

   function "+" (Left, Right : Vector) return Vector is
   begin
      return (Left.X + Right.X, Left.Y + Right.Y);
   end "+";

   function Has (List : Vector_Array; V : Vector) return Boolean is
   begin
      for J in List'Range loop
         if List (J) = V then
            return True;
         end if;
      end loop;

      return False;
   end Has;

   function Can_Go (P : Vector; Dir : Direction) return Boolean is
      P2 : constant Vector := P + DV (Dir);
   begin
      return P2.X in Map'Range (1)
        and then P2.Y in Map'Range (2)
        and then Character'Succ (Map (P.X, P.Y)) = Map (P2.X, P2.Y);
   end Can_Go;

   procedure Walk
     (P     : Vector) is
   begin
      if Map (P.X, P.Y) = '9' then
         Result := Result + 1;
      else
         for Dir in DV'Range loop
            if Can_Go (P, Dir) then
               Walk (P + DV (Dir));
            end if;
         end loop;
      end if;
   end Walk;
begin

   declare
      Input   : Text_IO.File_Type;
      Count_9 : Natural := 0;
   begin
      Text_IO.Open (Input, Text_IO.In_File, "input.txt");
      for Y in 1 .. Size loop
         for X in 1 .. Size loop
            Text_IO.Get (Input, Map (X, Y));
            if Map (X, Y) = '9' then
               Count_9 := Count_9 + 1;
            end if;
         end loop;
      end loop;

      for Y in 1 .. Size loop
         for X in 1 .. Size loop
            if Map (X, Y) = '0' then
               declare
                  --  Heads : Vector_Array (1 .. Count_9);
                  --  Last  : Natural := 0;
               begin
                  Walk ((X, Y));
               end;
            end if;
         end loop;
      end loop;
   end;

   Text_IO.Put_Line (Integer'Image (Result));
end Solution;
