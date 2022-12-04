with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Day_04 with SPARK_Mode is
   type Section_Assignment is record
      From, To : Positive;
   end record;

   function Is_Empty (Left : Section_Assignment) return Boolean is
     (Left.From > Left.To);

   function Overlap (Left, Right : Section_Assignment) return Boolean is
     (Left.From in Right.From .. Right.To
        or else Left.To in Right.From .. Right.To);

   type Assignment_Pair is array (1 .. 2) of Section_Assignment;

   procedure Read (Pair : out Assignment_Pair; Ok : out Boolean)
     with Post =>
       (not Ok or else (for all Item of Pair => not Is_Empty (Item)));

   procedure Read (Pair : out Assignment_Pair; Ok : out Boolean) is
      From_1 : Integer;
      Dash_1 : Character;
      To_1   : Integer;
      Comma  : Character;
      From_2 : Integer;
      Dash_2 : Character;
      To_2   : Integer;
   begin
      Ada.Integer_Text_IO.Get (From_1);
      Ada.Text_IO.Get (Dash_1);
      Ada.Integer_Text_IO.Get (To_1);
      Ada.Text_IO.Get (Comma);
      Ada.Integer_Text_IO.Get (From_2);
      Ada.Text_IO.Get (Dash_2);
      Ada.Integer_Text_IO.Get (To_2);

      Ok := Dash_1 = '-'
        and then Comma = ','
        and then Dash_2 = '-'
        and then From_1 > 0
        and then To_1 > 0
        and then From_2 > 0
        and then To_2 > 0;

      if Ok then
         Pair := ((From_1, To_1), (From_2, To_2));
         if Is_Empty (Pair (1)) or else Is_Empty (Pair (2)) then
            Ok := False;
         end if;
      else
         Pair := ((1, 1), (2, 2));
      end if;
   end Read;

   Total : Natural := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Pair  : Assignment_Pair;
         Ok    : Boolean;
      begin
         Read (Pair, Ok);

         if not Ok or else Total = Natural'Last then
            Ada.Text_IO.Put_Line ("Unexpected input or overflow");
            return;
         end if;

         if Overlap (Pair (1), Pair (2)) or else
           Overlap (Pair (2), Pair (1))
         then
            Total := Total + 1;
         end if;
      end;
   end loop;

   Ada.Integer_Text_IO.Put (Total);
end Day_04;
