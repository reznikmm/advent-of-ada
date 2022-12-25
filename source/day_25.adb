with Ada.Text_IO;

procedure Day_25 is
   subtype SNAFU_Index is Positive range 1 .. 23;
   subtype SNAFU is String (SNAFU_Index);

   procedure Add (Left : in out SNAFU; Right : String) is
      function Next (V : Character) return Character is
        (case V is
            when '=' => '-', when '-' => '0', when '0' => '1', when '1' => '2',
            when others => raise Constraint_Error);
      function Prev (V : Character) return Character is
        (case V is
            when '-' => '=', when '0' => '-', when '1' => '0', when '2' => '1',
            when others => raise Constraint_Error);

      procedure Sum
        (L     : in out Character;
         R     : Character;
         Carry : in out Character)
      is
         --    \  2    1    0    -1    -2
         --   2  -1+  -2+   2     1     0
         --   1  -2+   2    1     0    -1
         --   0   2    1    0    -1    -2
         --  -1   1    0   -1    -2     2-
         --  -2   0   -1   -2    2-     1-

         --  2+2+1 = 0+   -2-2-1= 0-
      begin
         if Carry = '1' then
            if R /= '2' then
               Carry := '0';
               Sum (L, Next (R), Carry);
            elsif L = '2' then
               L := '0'; Carry := '1';
            else
               declare
                  Next_L : constant Character := Next (L);
               begin
                  L := R;
                  Carry := '0';
                  Sum (L, Next_L, Carry);
               end;
            end if;
         elsif Carry = '-' then
            if R /= '=' then
               Carry := '0';
               Sum (L, Prev (R), Carry);
            elsif L = '=' then
               L := '0'; Carry := '-';
            else
               declare
                  Prev_L : constant Character := Prev (L);
               begin
                  L := R;
                  Carry := '0';
                  Sum (L, Prev_L, Carry);
               end;
            end if;
         elsif L = '2' then
            case R is
               when '2' => L := '-'; Carry := '1';
               when '1' => L := '='; Carry := '1';
               when '0' => L := '2';
               when '-' => L := '1';
               when '=' => L := '0';
               when others => raise Constraint_Error;
            end case;
         elsif L = '1' then
            case R is
               when '2' => L := '='; Carry := '1';
               when others => L := Next (R);
            end case;
         elsif L = '0' then
            L := R;
         elsif L = '-' then
            case R is
               when '=' => L := '2'; Carry := '-';
               when others => L := Prev (R);
            end case;
         elsif L = '=' then
            case R is
               when '2' => L := '0';
               when '1' => L := '-';
               when '0' => L := '=';
               when '-' => L := '2'; Carry := '-';
               when '=' => L := '1'; Carry := '-';
               when others => raise Constraint_Error;
            end case;
         else
            raise Constraint_Error;
         end if;
      end Sum;
      Carry : Character := '0';
      Index : Natural := Right'Last;
   begin
      for L of reverse Left loop
         if Index in Right'Range then
            Sum (L, Right (Index), Carry);
            Index := Index - 1;
         elsif Carry /= '0' then
            Sum (L, '0', Carry);
         else
            exit;
         end if;
      end loop;
   end Add;

   Result : SNAFU := (others => '0');
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         Add (Result, Line);
      end;
   end loop;

   Ada.Text_IO.Put_Line (Result);
end Day_25;
