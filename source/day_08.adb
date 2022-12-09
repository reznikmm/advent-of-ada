with Ada.Text_IO;

procedure Day_08 is
   M : constant := 99;
   Grid : array (1 .. M) of String (1 .. M);
   Best : Natural := 0;
begin
   for Line of Grid loop
      Ada.Text_IO.Get (Line);
   end loop;

   for J in 2 .. M - 1 loop
      for K in 2 .. M - 1 loop
         declare
            A, B, C, D : Positive;
            Next : Positive;
         begin
            A :=  J - 1;
            for X in reverse 1 .. J - 1 loop
               if Grid (X) (K) >= Grid (J) (K) then
                  A := J - X;
                  exit;
               end if;
            end loop;

            B := M - J;
            for X in J + 1 .. M loop
               if Grid (X) (K) >= Grid (J) (K) then
                  B := X - J;
                  exit;
               end if;
            end loop;

            C := K - 1;
            for Y in reverse 1 .. K - 1 loop
               if Grid (J) (Y) >= Grid (J) (K) then
                  C := K - Y;
                  exit;
               end if;
            end loop;

            D := M - K;
            for Y in K + 1 .. M loop
               if Grid (J) (Y) >= Grid (J) (K) then
                  D := Y - K;
                  exit;
               end if;
            end loop;

            Next := A * B * C * D;

            if Next > Best then
               Best := Next;
            end if;
         end;
      end loop;
   end loop;

   Ada.Text_IO.Put (Integer'Image (Best));
end Day_08;
