with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Text_IO;
with Ada.Long_Long_Integer_Text_IO;

procedure Day_17 is

   subtype Row_Index is Natural range 1 .. 7;

   type Row is array (Row_Index) of Boolean;
   Empty_Row : constant Row := (Row_Index => False);

   type Shape is array (1 .. 4) of Row;  --  from top to bottom

   subtype Long_Long_Natural is
     Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   package Space is
      type Footprint is private;
      function Save return Footprint;
      function Hash (V : Footprint) return Ada.Containers.Hash_Type;
      procedure Append (Value : Row);
      --  function First_Index return Natural is (1);
      function Last_Index return Long_Long_Natural;
      function Get_Rocks (Y : Long_Long_Natural) return Shape;
      procedure Set_Rocks (Y : Long_Long_Natural; Value : Shape);

   private
      type Footprint is array (Row_Index) of Natural;

   end Space;

   package body Space is
      type Space_Index is mod 4096;
      Last  : Long_Long_Natural := 0;
      Space : array (Space_Index) of Row;

      function Hash (V : Footprint) return Ada.Containers.Hash_Type is
         use type Ada.Containers.Hash_Type;
         Result : Ada.Containers.Hash_Type := 0;
      begin
         for X of V loop
            Result := Result * 127 + Ada.Containers.Hash_Type'Mod (X);
         end loop;

         return Result;
      end Hash;

      function Save return Footprint is
         V   : Footprint;
         Top : constant Space_Index := Space_Index'Mod (Last);
      begin
         for J in V'Range loop
            V (J) := 0;
            for K in Space_Index loop
               exit when Space (Top - K) (J);
               V (J) := V (J) + 1;
            end loop;
         end loop;

         return V;
      end Save;

      procedure Append (Value : Row) is
      begin
         Space (0) := Value;
      end Append;

      function Last_Index return Long_Long_Natural is (Last);

      function Get_Rocks (Y : Long_Long_Natural) return Shape is
         Result : Shape;
         Index  : Long_Long_Natural := Y;
      begin
         for Row of reverse Result loop
            Row := (if Index <= Last then Space (Space_Index'Mod (Index))
                    else Empty_Row);

            Index := Index + 1;
         end loop;

         return Result;
      end Get_Rocks;

      procedure Set_Rocks
        (Y     : Long_Long_Natural;
         Value : Shape)
      is
         Index  : Long_Long_Natural := Y;
      begin
         for Row of reverse Value loop
            exit when Row = Empty_Row;

            if Index <= Last then
               Space (Space_Index'Mod (Index)) :=
                 Space (Space_Index'Mod (Index)) or Row;
            else
               Last := Last + 1;
               Space (Space_Index'Mod (Last)) := Row;
               Space (Space_Index'Mod (Last + 7)) := Empty_Row;
            end if;

            Index := Index + 1;
         end loop;
      end Set_Rocks;

   end Space;

   subtype Direction is Character
     with Static_Predicate => Direction in '<' | '>';

   function Intersect (L, R : Shape) return Boolean is
     (for some J in Shape'Range => (L (J) and R (J)) /= Empty_Row);

   function Shift (Item : Shape; To : Direction) return Shape is
      Result : Shape;
   begin
      for Y in Item'Range loop
         declare
            R : Row renames Item (Y);
         begin
            if To = '<' then
               Result (Y) (1 .. 6) := R (2 .. 7);
               Result (Y) (7) := False;
            else
               Result (Y) (2 .. 7) := R (1 .. 6);
               Result (Y) (1) := False;
            end if;
         end;
      end loop;

      return Result;
   end Shift;

   function Can_Shift (Item : Shape; To : Direction) return Boolean is
     (for all Row of Item =>
         not Row (if To = '<' then Row'First else Row'Last));

   type Shape_Index is mod 5;
   type Shape_Array is array (Shape_Index) of Shape;

   package Shape_Forms is
      o : constant Boolean := False;
      X : constant Boolean := True;

      F1 : constant Shape :=
        ((o, o, o, o, o, o, o),
         (o, o, o, o, o, o, o),
         (o, o, o, o, o, o, o),
         (o, o, X, X, X, X, o));

      F2 : constant Shape :=
        ((o, o, o, o, o, o, o),
         (o, o, o, X, o, o, o),
         (o, o, X, X, X, o, o),
         (o, o, o, X, o, o, o));

      F3 : constant Shape :=
        ((o, o, o, o, o, o, o),
         (o, o, o, o, X, o, o),
         (o, o, o, o, X, o, o),
         (o, o, X, X, X, o, o));

      F4 : constant Shape :=
        ((o, o, X, o, o, o, o),
         (o, o, X, o, o, o, o),
         (o, o, X, o, o, o, o),
         (o, o, X, o, o, o, o));

      F5 : constant Shape :=
        ((o, o, o, o, o, o, o),
         (o, o, o, o, o, o, o),
         (o, o, X, X, o, o, o),
         (o, o, X, X, o, o, o));

      Forms : constant Shape_Array := (F1, F2, F3, F4, F5);

   end Shape_Forms;

   Shapes : Shape_Array renames Shape_Forms.Forms;
   Next_Shape : Shape_Index := Shapes'First;

   Jets : constant String := Ada.Text_IO.Get_Line;
   Jet_Index : Positive range Jets'Range := Jets'First;

   function Next_Jet return Direction is
      Result : constant Direction := Jets (Jet_Index);
   begin
      Jet_Index := (if Jet_Index = Jets'Last then 1 else Jet_Index + 1);

      return Result;
   end Next_Jet;

   procedure Run (Count : Long_Long_Integer) is
   begin
      for J in 1 .. Count loop
         declare
            Next : Shape := Shapes (Next_Shape);
            Y    : Long_Long_Integer := Space.Last_Index + 4;
         begin
            Next_Shape := Next_Shape + 1;

            loop
               declare
                  Dir     : constant Direction := Next_Jet;
                  Shifted : constant Shape := Shift (Next, Dir);
               begin
                  if Can_Shift (Next, Dir) and then
                    not Intersect (Shifted, Space.Get_Rocks (Y))
                  then
                     Next := Shifted;
                  end if;

                  exit when Intersect (Next, Space.Get_Rocks (Y - 1));

                  Y := Y - 1;
               end;
            end loop;

            Space.Set_Rocks (Y, Next);
         end;
      end loop;
   end Run;

   type Key is record
      FP  : Space.Footprint;
      Jet : Positive;
   end record;

   function Hash (V : Key) return Ada.Containers.Hash_Type is
     (Ada.Containers."+"
        (Space.Hash (V.FP), Ada.Containers.Hash_Type'Mod (V.Jet)));

   type Info_Record is record
      Shapes : Long_Long_Integer;
      Height : Long_Long_Integer;
   end record;

   package Footprint_Maps is new Ada.Containers.Hashed_Maps
     (Key, Info_Record, Hash, "=");

   Seen   : Footprint_Maps.Map;
   Cursor : Footprint_Maps.Cursor;
   Info   : Info_Record := (0, 0);
   Ok     : Boolean := True;
begin
   Space.Append ((Row_Index => True));  --  Append a floor

   while Ok loop
      Run (Shapes'Length);

      Info :=
        (Shapes => Info.Shapes + Shapes'Length,
         Height => Space.Last_Index);

      Seen.Insert ((Space.Save, Jet_Index), Info, Cursor, Ok);
   end loop;

   declare
      Prev   : constant Info_Record := Footprint_Maps.Element (Cursor);
      Period : constant Long_Long_Integer := Info.Shapes - Prev.Shapes;
      To_Go  : constant Long_Long_Integer := 1000000000000 - Info.Shapes;
      Cycles : constant Long_Long_Integer := To_Go / Period;
      Left   : constant Long_Long_Integer := To_Go rem Period;
   begin
      Ada.Long_Long_Integer_Text_IO.Put (Period);
      Run (Left);
      Ada.Long_Long_Integer_Text_IO.Put
        (Cycles * (Info.Height - Prev.Height) + Space.Last_Index);
   end;
end Day_17;
