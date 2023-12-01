with Ada.Long_Long_Integer_Text_IO;
with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
procedure Day_21 is
   subtype Monkey_Name is String (1 .. 4);

   package Value_Maps is new Ada.Containers.Ordered_Maps
     (Monkey_Name, Long_Long_Integer, "<");

   type Operation is record
      Char  : Character;
      Left  : Monkey_Name;
      Right : Monkey_Name;
   end record;

   package Operation_Maps is new Ada.Containers.Ordered_Maps
     (Monkey_Name, Operation, "<");

   Ops    : Operation_Maps.Map;
   Values : Value_Maps.Map;

   procedure Find (Name : Monkey_Name; Result : out Long_Long_Integer) is
   begin
      if Values.Contains (Name) then
         Result := Values (Name);
      else
         declare
            Op : constant Operation := Ops (Name);
            Left, Right : Long_Long_Integer;
         begin
            Find (Op.Left, Left);
            Find (Op.Right, Right);
            case Op.Char is
               when '+' => Result := Left + Right;
               when '-' => Result := Left - Right;
               when '*' => Result := Left * Right;
               when '/' => Result := Left / Right;
               when others => raise Constraint_Error;
            end case;
            Values.Insert (Name, Result);
         end;
      end if;
   end Find;

   function Has_Humn (Name : Monkey_Name) return Boolean is
   begin
      if Name = "humn" then
         return True;
      elsif Values.Contains (Name) then
         return False;
      else
         declare
            Op : constant Operation := Ops (Name);
         begin
            return Has_Humn (Op.Left) or else Has_Humn (Op.Right);
         end;
      end if;
   end Has_Humn;

   procedure Down (Name : Monkey_Name; Result : in out Long_Long_Integer) is
      Op          : Operation;
      Left, Right : Long_Long_Integer;
   begin
      if Name = "humn" then
         return;
      end if;

      Op := Ops (Name);

      if Has_Humn (Op.Left) then
         Find (Op.Right, Right);

         case Op.Char is
            when '+' => Left := Result - Right;
            when '-' => Left := Result + Right;
            when '*' => Left := Result / Right;
            when '/' => Left := Result * Right;
            when others => raise Constraint_Error;
         end case;

         Result := Left;
         Down (Op.Left, Result);
      else
         Find (Op.Left, Left);

         case Op.Char is
            when '+' => Right := Result - Left;
            when '-' => Right := Left - Result;
            when '*' => Right := Result / Left;
            when '/' => Right := Left / Result;
            when others => raise Constraint_Error;
         end case;

         Result := Right;
         Down (Op.Right, Result);
      end if;
   end Down;

   Result : Long_Long_Integer;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
         Name : constant String := Line (1 .. 4);
         Op   : Operation;
      begin
         if Line (7) in '0' .. '9' then
            Values.Insert
              (Name, Long_Long_Integer'Value (Line (7 .. Line'Last)));
         else
            Op.Left := Line (7 .. 10);
            Op.Char := Line (12);
            Op.Right := Line (14 .. 17);
            Ops.Insert (Name, Op);
         end if;
      end;
   end loop;

   declare
      Op : constant Operation := Ops ("root");
   begin
      if Has_Humn (Op.Left) then
         Find (Op.Right, Result);
         Down (Op.Left, Result);
      else
         Find (Op.Left, Result);
         Down (Op.Right, Result);
      end if;
      Ada.Long_Long_Integer_Text_IO.Put (Result);
   end;
end Day_21;
