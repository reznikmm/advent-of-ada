--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Integer_Text_IO;
with Ada.Text_IO;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;

procedure AOC.Day_09 is

   type Point is record
      X, Y : Positive;
   end record;

   package Map is
      procedure Line_To_X (From : Point; X : Positive);
      procedure Line_To_Y (From : Point; Y : Positive);

      procedure Fill;

      function Area (From, To : Point) return Long_Long_Integer;

   private
      type Span is record
         From : Positive;
         To   : Natural;
      end record;

      function Is_Empty (V : Span) return Boolean is (V.From > V.To);

      function Is_Overlap (L, R : Span) return Boolean is
         (L.From in R.From .. R.To or else R.From in L.From .. L.To);

      function "or" (L, R : Span) return Span is
         (Positive'Min (L.From, R.From), Positive'Max (L.To, R.To))
      with Pre => Is_Overlap (L, R);

      function Before (Left, Right : Span) return Boolean is
        (Left.From < Right.From or else
           (Left.From = Right.From and then Left.To < Right.To));

      package Span_Sets is new Ada.Containers.Ordered_Sets (Span, Before);

      package Span_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type     => Span,   --  Y
         Element_Type => Span_Sets.Set,
         "<"          => Before,
         "="          => Span_Sets."=");

      Data : Span_Maps.Map;
      Done : Boolean := False;
   end Map;

   package body Map is

      function Area (From, To : Point) return Long_Long_Integer is
         New_Y : constant Span := (From.Y, To.Y);
         New_X : constant Span := (From.X, To.X);

         C     : Span_Maps.Cursor :=
           (declare
              Floor : constant Span_Maps.Cursor := Data.Floor (New_Y);
            begin
               (if Span_Maps.Has_Element (Floor) then Floor else Data.First));

         Result : Long_Long_Integer := 0;
      begin
         if From.X > To.X then
            return Area ((To.X, From.Y), (From.X, To.Y));
         elsif From.Y > To.Y then
            return Area ((From.X, To.Y), (To.X, From.Y));
         end if;

         while Span_Maps.Has_Element (C) and then
           Is_Overlap (Span_Maps.Key (C), New_Y)
         loop
            declare
               Key : constant Span := Span_Maps.Key (C);
               DY  : constant Span :=
                 (Positive'Max (Key.From, From.Y),
                  Positive'Min (Key.To, To.Y));
               Set : Span_Sets.Set renames Data (C);
            begin
               for Item of Set when Is_Overlap (Item, New_X) loop
                  declare
                     DX  : constant Span :=
                       (Positive'Max (Item.From, From.X),
                        Positive'Min (Item.To, To.X));
                  begin
                     Result := @ +
                       Long_Long_Integer (DX.To - DX.From + 1) *
                       Long_Long_Integer (DY.To - DY.From + 1);
                  end;
               end loop;
            end;

            Span_Maps.Next (C);
         end loop;

         return Result;
      end Area;

      procedure Fill (New_X : Span; C : Span_Maps.Cursor) is
      begin
         if not Span_Maps.Has_Element (C) then
            return;
         end if;

         declare
            Set   : Span_Sets.Set renames Data (C);
            Prev  : Span_Sets.Cursor := Set.First;
            Next  : Span_Sets.Cursor := Span_Sets.Next (Prev);
            Space : Span;
         begin
            while Span_Sets.Has_Element (Next) loop
               Space := (Set (Prev).To + 1, Set (Next).From - 1);

               if not Is_Empty (Space) and then Is_Overlap (Space, New_X) then
                  Set.Insert (Space);
                  Fill (Space, Span_Maps.Previous (C));
                  Fill (Space, Span_Maps.Next (C));
               end if;

               Prev := Next;
               Span_Sets.Next (Next);
            end loop;
         end;
      end Fill;

      procedure Fill is
         Cursor : constant Span_Maps.Cursor := Span_Maps.Next (Data.First);
         X_Spans : constant Span_Sets.Set := Data (Cursor);

         First   : constant Span := X_Spans.First_Element;

         Second  : constant Span :=
           X_Spans (Span_Sets.Next (X_Spans.First));
      begin
         if Done then
            return;
         else
            Done := True;
         end if;

         pragma Assert (First.From = First.To);
         pragma Assert (Second.From = Second.To);
         Fill ((First.From + 1, Second.To - 1), Cursor);
      end Fill;

      procedure Line_To_X (From : Point; X : Positive) is
         New_Y : constant Span := (From.Y, From.Y);
         New_X : constant Span := (From.X, X);

         C     : Span_Maps.Cursor :=
           (declare
              Floor : constant Span_Maps.Cursor := Data.Floor (New_Y);
            begin
               (if Span_Maps.Has_Element (Floor) then Floor else Data.First));

         Prev_Y : Span;
      begin
         if From.X > X then
            Line_To_X ((X, From.Y), From.X);
            return;
         elsif Span_Maps.Has_Element (C) then
            Prev_Y := Span_Maps.Key (C);
         else
            Data.Insert (New_Y, [New_X]);
            return;
         end if;

         if not Is_Overlap (Prev_Y, New_Y) and then
           Span_Maps.Has_Element (Span_Maps.Next (C))
         then
            C := Span_Maps.Next (C);
            Prev_Y := Span_Maps.Key (C);
         end if;

         if Is_Overlap (Prev_Y, New_Y) then
            if Prev_Y.From < From.Y then
               declare
                  X_Spans : constant Span_Sets.Set := Span_Maps.Element (C);
               begin
                  Data.Delete (C);
                  Data.Insert ((Prev_Y.From, From.Y - 1), X_Spans);
                  Data.Insert ((From.Y, Prev_Y.To), X_Spans);
                  Line_To_X (From, X);
                  return;
               end;
            elsif Prev_Y.To > From.Y then
               declare
                  X_Spans : constant Span_Sets.Set := Span_Maps.Element (C);
               begin
                  Data.Delete (C);
                  Data.Insert ((From.Y, From.Y), X_Spans);
                  Data.Insert ((From.Y + 1, Prev_Y.To), X_Spans);
                  Line_To_X (From, X);
                  return;
               end;
            else
               declare
                  X_Spans : Span_Sets.Set renames Data (C);
               begin
                  pragma Assert (Prev_Y = New_Y);

                  if (for some Item of X_Spans =>
                        Is_Overlap (Item, New_X))
                  then
                     declare
                        Copy : Span_Sets.Set;
                     begin
                        for Item of X_Spans loop
                           if Is_Overlap (Item, New_X) then
                              Copy.Insert (Item or New_X);
                           else
                              Copy.Insert (Item);
                           end if;
                        end loop;
                        X_Spans.Move (Source => Copy);
                     end;
                  else
                     X_Spans.Insert (New_X);
                  end if;
               end;
            end if;
         else
            Data.Insert (New_Y, [New_X]);
         end if;
      end Line_To_X;

      procedure Line_To_Y (From : Point; Y : Positive) is
         New_Y : constant Span := (From.Y, Y);
         New_X : constant Span := (From.X, From.X);

         C     : Span_Maps.Cursor :=
           (declare
              Floor : constant Span_Maps.Cursor := Data.Floor (New_Y);
            begin
               (if Span_Maps.Has_Element (Floor) then Floor else Data.First));

         Prev_Y : Span;
      begin
         if From.Y > Y then
            Line_To_Y ((From.X, Y), From.Y);
            return;
         elsif Span_Maps.Has_Element (C) then
            Prev_Y := Span_Maps.Key (C);
         else
            Data.Insert (New_Y, [New_X]);
            return;
         end if;

         if not Is_Overlap (Prev_Y, New_Y) and then
           Span_Maps.Has_Element (Span_Maps.Next (C))
         then
            C := Span_Maps.Next (C);
            Prev_Y := Span_Maps.Key (C);
         end if;

         if Is_Overlap (Prev_Y, New_Y) then
            if Prev_Y.From < From.Y then
               declare
                  X_Spans : constant Span_Sets.Set := Span_Maps.Element (C);
               begin
                  Data.Delete (C);
                  Data.Insert ((Prev_Y.From, From.Y - 1), X_Spans);
                  Data.Insert ((From.Y, Prev_Y.To), X_Spans);
                  Line_To_Y (From, Y);
                  return;
               end;
            elsif Prev_Y.To > Y then
               declare
                  X_Spans : constant Span_Sets.Set := Span_Maps.Element (C);
               begin
                  Data.Delete (C);
                  Data.Insert ((Prev_Y.From, Y), X_Spans);
                  Data.Insert ((Y + 1, Prev_Y.To), X_Spans);
                  Line_To_Y (From, Y);
                  return;
               end;
            elsif Prev_Y.From > From.Y then
               Line_To_Y (From, Prev_Y.From - 1);
               Line_To_Y ((From.X, Prev_Y.From), Y);
               return;
            elsif Prev_Y.To < Y then
               Line_To_Y (From, Prev_Y.To);
               Line_To_Y ((From.X, Prev_Y.To + 1), Y);
               return;
            else
               declare
                  X_Spans : Span_Sets.Set renames Data (C);
               begin
                  pragma Assert (Prev_Y = New_Y);

                  if (for all Item of X_Spans =>
                         not Is_Overlap (Item, New_X))
                  then
                     X_Spans.Insert (New_X);
                  end if;
               end;
            end if;
         else
            Data.Insert (New_Y, [New_X]);
         end if;
      end Line_To_Y;

   end Map;

   package Point_Lists is new Ada.Containers.Doubly_Linked_Lists (Point);

   List : Point_Lists.List;

   function Area (Left, Right : Point) return Long_Long_Integer is
     ((abs Long_Long_Integer (Left.X - Right.X) + 1) *
      (abs Long_Long_Integer (Left.Y - Right.Y) + 1));

   Total : Long_Long_Integer := 0;

begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Next : Point;
         Ignore : Character;
      begin
         Ada.Integer_Text_IO.Get (Next.X);
         Ada.Text_IO.Get (Ignore);
         Ada.Integer_Text_IO.Get (Next.Y);
         List.Append (Next);
      end;
   end loop;

   declare
      Prev : Point := List.First_Element;
   begin
      for Point of List when Point /= Prev loop
         if Point.X = Prev.X then
            Map.Line_To_Y (Prev, Y => Point.Y);
         else
            Map.Line_To_X (Prev, X => Point.X);
         end if;

         Prev := Point;
      end loop;
   end;

   Map.Fill;

   for J in List.Iterate loop
      declare
         Next : constant Point_Lists.Cursor := List.Iterate.Next (J);
         Val : Long_Long_Integer;
      begin
         if Point_Lists.Has_Element (Next) then
            for K in List.Iterate (Start => Next) loop
               Val := Area (List (J), List (K));
               if Val > Total
                 and then Map.Area (List (J), List (K)) = Val
               then
                  Total := Val;
               end if;
            end loop;
         end if;
      end;
   end loop;

   Ada.Text_IO.Put_Line (Total'Image);
end AOC.Day_09;
