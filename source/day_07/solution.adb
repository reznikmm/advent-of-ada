with Text_IO;

procedure Solution is

   subtype Long is Long_Integer;

   Input  : Text_IO.File_Type;
   Result : Long := 0;

   type Long_Array is array (Positive range <>) of Long;

   package Long_IO is new Text_IO.Integer_IO (Long);

   function Log (Value : Long) return Positive is
      Width : constant Natural := Long'Width;
   begin
      for J in 1 .. Width - 2 loop
         if 10 ** J > Value then
            return J;
         end if;
      end loop;

      raise Constraint_Error;
   end Log;

   function Check
     (Expect : Long;
      Args   : Long_Array) return Boolean
   is
      Right : Long renames Args (Args'Last);
   begin
      if Args'Length = 1 then
         return Expect = Right;
      end if;

      if Expect >= Right
        and then Check
          (Expect - Right, Args (Args'First .. Args'Last - 1))
      then
         return True;  --  '+' works
      elsif Expect mod Right = 0
        and then Check
          (Expect / Right, Args (Args'First .. Args'Last - 1))
      then
         return True;  --  '*' works
      else
         declare  --  check ||
            J : constant Natural := Log (Right);
         begin
            if Expect mod 10 ** J = Right then
               return Check
                 (Expect / 10 ** J, Args (Args'First .. Args'Last - 1));
            end if;
         end;
      end if;

      return False;
   end Check;

begin
   Text_IO.Open (Input, Text_IO.In_File, "input.txt");

   while not Text_IO.End_Of_File (Input) loop
      declare
         Line   : String (1 .. 80);
         Last   : Natural;
         To     : Natural;
         Expect : Long;
         Args   : Long_Array (1 .. 20);
         Count  : Natural := 0;
      begin
         Text_IO.Get_Line (Input, Line, Last);
         if Last = Line'Last then
            raise Program_Error;
         end if;

         for J in 2 .. Last loop
            if Line (J) = ':' then
               Long_IO.Get (Line (1 .. J - 1), Expect, To);
               To := J + 1;
               exit;
            end if;
         end loop;

         while To <= Last loop
            Count := Count + 1;
            Long_IO.Get (Line (To .. Last), Args (Count), To);
            To := To + 1;
         end loop;

         if Check (Expect, Args (1 .. Count)) then
            Result := Result + Expect;
         end if;
      end;
   end loop;

   Long_IO.Put (Result);
end Solution;
