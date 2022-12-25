with Ada.Text_IO;
with Ada.Long_Long_Integer_Text_IO;

with Ada.Containers.Vectors;

procedure Day_20 is
   type Origin_Position is new Positive;
   package Integer_Lists is new Ada.Containers.Vectors
     (Positive, Long_Long_Integer);

   package Position_Lists is new Ada.Containers.Vectors
     (Origin_Position, Positive);

   package Reverse_Lists is new Ada.Containers.Vectors
     (Positive, Origin_Position);

   List   : Integer_Lists.Vector;
   Copy   : Integer_Lists.Vector;
   Index  : Position_Lists.Vector;  --  from origin index to index
   Back   : Reverse_Lists.Vector;   --  from index to origin index
   Length : Positive;

   procedure Swap (Left, Right : Integer) is
      L : constant Positive := ((Left - 1) mod Length) + 1;
      R : constant Positive := ((Right - 1) mod Length) + 1;

      L_Origin : constant Origin_Position := Back (L);
      R_Origin : constant Origin_Position := Back (R);
   begin
      Index (L_Origin) := R;
      Index (R_Origin) := L;
      Back (L) := R_Origin;
      Back (R) := L_Origin;
      List.Swap (L, R);
   end Swap;

begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Item : Long_Long_Integer;
      begin
         Ada.Long_Long_Integer_Text_IO.Get (Item);
         List.Append (Item * 811589153);
         Index.Append (List.Last_Index);
         Back.Append (Index.Last_Index);
      end;
   end loop;

   Copy := List;
   Length := Index.Last_Element;

   for J in 1 .. 10 loop
      for Origin in Index.First_Index .. Index.Last_Index loop
         --  Origin is index of a number to move
         declare
            Position : constant Positive := Index (Origin);
            --  Position is where the number before move
            Offset   : constant Integer :=
              Integer (List (Position) rem Long_Long_Integer (Length - 1));
         begin
            pragma Assert (Copy (Positive (Origin)) = List (Position));
            if Offset > 0 then
               for J in 1 .. Offset loop
                  Swap (Position + J - 1, Position + J);
               end loop;
            elsif Offset < 0 then
               for J in 1 .. -Offset loop
                  Swap (Position - J + 1, Position - J);
               end loop;
            end if;

            --  pragma Assert
            --    (for all X in Index.First_Index .. Index.Last_Index =>
            --       X = Back (Index (X)));
         end;
      end loop;
      Ada.Text_IO.Put_Line (J'Image);
   end loop;

   declare
      Pos : constant Positive := List.Find_Index (0);
      V1  : constant Long_Long_Integer :=
        List.Element ((Pos + 999) mod Length + 1);
      V2  : constant Long_Long_Integer :=
        List.Element ((Pos + 1999) mod Length + 1);
      V3  : constant Long_Long_Integer :=
        List.Element ((Pos + 2999) mod Length + 1);
   begin
      Ada.Long_Long_Integer_Text_IO.Put (V1);
      Ada.Long_Long_Integer_Text_IO.Put (V2);
      Ada.Long_Long_Integer_Text_IO.Put (V3);
      Ada.Long_Long_Integer_Text_IO.Put (V1 + V2 + V3);
   end;
end Day_20;
