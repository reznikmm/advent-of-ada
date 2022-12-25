with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Hashed_Maps;

procedure Day_14 is
   type Ceil_Kind is (Rock, Sand);

   type Position is record
      X, Y : Integer;
   end record;

   function Hash (V : Position) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (V.X + 123 * V.Y));

   procedure Read_Position
     (Line : String; From : out Position; Last : in out Positive) is
   begin
      Ada.Integer_Text_IO.Get (Line (Last .. Line'Last), From.X, Last);
      pragma Assert (Line (Last + 1) = ',');
      Last := Last + 2;
      Ada.Integer_Text_IO.Get (Line (Last .. Line'Last), From.Y, Last);
   end Read_Position;

   package Ceil_Maps is new Ada.Containers.Hashed_Maps
     (Position, Ceil_Kind, Hash, "=");

   Map : Ceil_Maps.Map;
   Max : Integer;
   Count : Natural := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
         Last : Positive := Line'First;
         From : Position;
         To   : Position;
      begin
         Read_Position (Line, From, Last);
         Max := Integer'Max (Max, From.Y);

         while Last < Line'Last loop
            Last := Last + 4;
            Read_Position (Line, To, Last);
            Max := Integer'Max (Max, To.Y);
            if From.X = To.X then
               for Y in
                 Integer'Min (From.Y, To.Y) .. Integer'Max (From.Y, To.Y)
               loop
                  Map.Include ((To.X, Y), Rock);
               end loop;
            elsif From.Y = To.Y then
               for X in
                 Integer'Min (From.X, To.X) .. Integer'Max (From.X, To.X)
               loop
                  Map.Include ((X, To.Y), Rock);
               end loop;
            else
               raise Program_Error;
            end if;

            From := To;
         end loop;
      end;
   end loop;

   loop
      declare
         P     : Position := (500, 0);
         Again : Boolean := True;
      begin
         Count := Count + 1;

         One_Cell_Fall :
         while Again loop
            Again := False;

            for Y in P.Y .. Max + 1 loop
               if Map.Contains ((P.X, Y)) then
                  pragma Assert (Y /= 0);
                  Again := True;

                  if Map.Contains ((P.X - 1, Y)) then
                     if Map.Contains ((P.X + 1, Y)) then
                        Map.Insert ((P.X, Y - 1), Sand);
                        exit One_Cell_Fall;
                     else
                        P := (P.X + 1, Y + 1);
                     end if;
                  else
                     P := (P.X - 1, Y + 1);
                  end if;

                  exit;
               end if;
            end loop;
         end loop One_Cell_Fall;

         if not Again then
            Map.Insert ((P.X, Max + 1), Sand);
         end if;

         exit when Map.Contains ((500, 0));
      end;
   end loop;

   Ada.Integer_Text_IO.Put (Count);
end Day_14;
