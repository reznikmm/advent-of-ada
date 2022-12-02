with Ada.Text_IO;

procedure Day_02 with SPARK_Mode is

   type Hand_Shape is (Rock, Paper, Scissors);

   subtype Hand_Shape_Encoding is Character range 'A' .. 'C';

   To_Shape : constant array (Hand_Shape_Encoding) of Hand_Shape :=
     ('A' => Rock, 'B' => Paper, 'C' => Scissors);

   type Round_Result is (Lose, Draw, Win);

   subtype Round_Result_Encoding is Character range 'X' .. 'Z';

   To_Round_Result : constant array (Round_Result_Encoding) of Round_Result :=
     ('X' => Lose, 'Y' => Draw, 'Z' => Win);

   Round : constant array (Hand_Shape, Round_Result) of Hand_Shape :=
     (Rock     => (Draw => Rock, Win  => Paper, Lose => Scissors),
      Paper    => (Lose => Rock, Draw => Paper, Win  => Scissors),
      Scissors => (Win  => Rock, Lose => Paper, Draw => Scissors));
   --  Answer for given offer and round result

   Shape_Score : constant array (Hand_Shape) of Positive :=
     (Rock => 1, Paper => 2, Scissors => 3);

   Round_Score   : constant array (Round_Result) of Natural :=
     (Lose => 0, Draw => 3, Win => 6);

   Total : Natural := 0;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Offer_Encoded  : Character;
         Space          : Character;
         Result_Encoded : Character;
         Offer          : Hand_Shape;  --  opponent's hand shape
         Answer         : Hand_Shape;  --  your hand shape
         Result         : Round_Result;  --  Round result
      begin
         Ada.Text_IO.Get (Offer_Encoded);
         Ada.Text_IO.Get (Space);
         Ada.Text_IO.Get (Result_Encoded);

         if Offer_Encoded not in Hand_Shape_Encoding
           or else Space /= ' '
           or else Result_Encoded not in Round_Result_Encoding
           or else Total > Natural'Last - 9  --  To avoid possible overflows
         then
            Ada.Text_IO.Put_Line ("Unexpected input");
            return;
         end if;

         Offer  := To_Shape (Offer_Encoded);
         Result := To_Round_Result (Result_Encoded);
         Answer := Round (Offer, Result);
         Total  := Total + Round_Score (Result) + Shape_Score (Answer);
      end;
   end loop;

   Ada.Text_IO.Put_Line (Total'Image);
end Day_02;
