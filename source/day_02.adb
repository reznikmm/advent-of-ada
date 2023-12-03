with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

procedure Day_02 is
   type Color is (Red, Green, Blue);
   type Cube_Set is array (Color) of Natural;
   type Set_Count is new Natural range 0 .. 10;
   type Cube_Set_Array is array (Set_Count range <>) of Cube_Set;

   type Game_Sample (Length : Set_Count := 0) is record
      Id : Positive;
      Set : Cube_Set_Array (1 .. Length);
   end record;

   package Color_IO is new Ada.Text_IO.Enumeration_IO (Color);

   ---------------
   -- Read_Game --
   ---------------

   procedure Read_Game
     (Text : String;
      Game : out Game_Sample)
   is

      procedure Read_Set
        (From : in out Natural;
         Set  : out Cube_Set)
      is
         To : Natural;
      begin
         Set := [others => 0];

         loop
            declare
               Count : Natural;
               Next  : Color;
            begin
               Ada.Integer_Text_IO.Get (Text (From .. Text'Last), Count, To);
               From := To + 1;
               Color_IO.Get (Text (From .. Text'Last), Next, To);
               From := To + 1;
               Set (Next) := Count;
               exit when From > Text'Last or else Text (From) /= ',';
               From := From + 1;  -- skip ','
            end;
         end loop;
      end Read_Set;

      From : Natural;
      To   : Natural;
      Id   : Natural;
      Set  : Cube_Set_Array (Set_Count'Range);
      Last : Set_Count := 0;
   begin
      Ada.Strings.Fixed.Find_Token
        (Source => Text,
         Set    => Ada.Strings.Maps.Constants.Decimal_Digit_Set,
         Test   => Ada.Strings.Inside,
         First  => From,
         Last   => To);

      Id := Natural'Value (Text (From .. To));
      From := To + 2;

      while From < Text'Last loop
         Last := Last + 1;
         Read_Set (From, Set (Last));
         From := From + 1; --  skip ';' if any
      end loop;

      Game :=
        (Length => Last,
         Id     => Id,
         Set    => Set (1 .. Last));
   end Read_Game;

   --  function Match (Game : Game_Sample) return Boolean is
   --  begin
   --     for Set of Game.Set loop
   --        if Set (Red) > 12 or Set (Green) > 13 or Set (Blue) > 14 then
   --           return False;
   --        end if;
   --     end loop;
   --
   --     return True;
   --  end Match;

   function Power (Set : Cube_Set) return Natural is
     (Set'Reduce ("*", 1));

   function Power (Game : Game_Sample) return Natural is
      Min : Cube_Set := [others => 0];
   begin
      for Set of Game.Set loop
         for C in Color loop
            Min (C) := Natural'Max (Min (C), Set (C));
         end loop;
      end loop;

      return Power (Min);
   end Power;

   Total : Natural := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line  : String (1 .. 200);
         Game  : Game_Sample;
         Last  : Natural;
      begin
         Ada.Text_IO.Get_Line (Line, Last);
         pragma Assert (Last < Line'Last);
         if Last > 0 then
            Read_Game (Line (1 .. Last), Game);
            Total := Total + Power (Game);
         end if;
      end;
   end loop;

   Ada.Integer_Text_IO.Put (Total);
end Day_02;
