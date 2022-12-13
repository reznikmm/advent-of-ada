with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Indefinite_Vectors;

procedure Day_13 is

   type Parser_State_Kind is (Start_List, End_List, Number, EOF);

   type Parser_State (Kind : Parser_State_Kind := Number) is record
      case Kind is
         when Number =>
            Number : Integer;
            Nested : Natural;
         when others =>
            null;
      end case;
   end record;

   procedure Next
     (Text  : String;
      Pos   : in out Positive;
      State : out Parser_State) is
   begin
      if Pos > Text'Last then
         State := (Kind => EOF);
      else
         case Text (Pos) is
            when '[' =>
               State := (Kind => Start_List);
               Pos := Pos + 1;
            when ']' =>
               State := (Kind => End_List);
               Pos := Pos + 1;
            when '0' .. '9' =>
               State := (Kind => Number, Number => 0, Nested => 0);
               Ada.Integer_Text_IO.Get
                 (Text (Pos .. Text'Last), State.Number, Pos);
               Pos := Pos + 1;
            when ',' =>
               Pos := Pos + 1;
               Next (Text, Pos, State);
            when others =>
               raise Program_Error;
         end case;
      end if;
   end Next;

   function Less (L, R : String) return Boolean is
      L_State : Parser_State;
      R_State : Parser_State;
      L_Pos   : Positive := L'First;
      R_Pos   : Positive := R'First;
   begin
      Next (L, L_Pos, L_State);
      Next (R, R_Pos, R_State);
      pragma Assert
        (L_State.Kind = Start_List and then R_State.Kind = Start_List);

      loop
         case L_State.Kind is
            when Start_List =>
               case R_State.Kind is
                  when Start_List =>
                     Next (L, L_Pos, L_State);
                     Next (R, R_Pos, R_State);
                  when Number =>  --  [list] with number
                     Next (L, L_Pos, L_State);
                     R_State.Nested := R_State.Nested + 1;
                  when End_List =>
                     return False;
                  when EOF =>
                     raise Program_Error;
               end case;
            when Number =>
               case R_State.Kind is
                  when Start_List =>
                     Next (R, R_Pos, R_State);
                     L_State.Nested := L_State.Nested + 1;
                  when Number =>
                     if L_State.Number /= R_State.Number then
                        return L_State.Number < R_State.Number;
                     else
                        if L_State.Nested > 0 then
                           for J in 1 .. L_State.Nested loop
                              Next (R, R_Pos, R_State);

                              if R_State.Kind /= End_List then
                                 return True;
                              end if;
                           end loop;
                        elsif R_State.Nested > 0 then
                           for J in 1 .. R_State.Nested loop
                              Next (L, L_Pos, L_State);

                              if L_State.Kind /= End_List then
                                 return False;
                              end if;
                           end loop;
                        end if;

                        Next (L, L_Pos, L_State);
                        Next (R, R_Pos, R_State);
                     end if;
                  when End_List =>
                     pragma Assert (L_State.Nested = 0);
                     return False;
                  when EOF =>
                     raise Program_Error;
               end case;
            when End_List =>
               case R_State.Kind is
                  when Start_List =>
                     return True;
                  when Number =>  --  [list] with number
                     pragma Assert (L_State.Nested = 0);
                     return True;
                  when End_List =>
                     Next (L, L_Pos, L_State);
                     Next (R, R_Pos, R_State);
                  when EOF =>
                     raise Program_Error;
               end case;
            when EOF =>
               case R_State.Kind is
                  when EOF =>
                     return False;
                  when others =>
                     raise Program_Error;
               end case;
         end case;
      end loop;
   end Less;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, String, "=");

   package Sorting is new String_Vectors.Generic_Sorting (Less);

   Divider_1 : constant String := "[[2]]";
   Divider_2 : constant String := "[[6]]";

   Vector : String_Vectors.Vector;
begin
   loop
      declare
         Left : constant String := Ada.Text_IO.Get_Line;
         Right : constant String := Ada.Text_IO.Get_Line;
      begin
         Vector.Append (Left);
         Vector.Append (Right);
         exit when Ada.Text_IO.End_Of_File;
         Ada.Text_IO.Put_Line (Ada.Text_IO.Get_Line);
      end;
   end loop;
   Vector.Append (Divider_1);
   Vector.Append (Divider_2);
   Sorting.Sort (Vector);

   Ada.Integer_Text_IO.Put (Vector.Find_Index (Divider_1));
   Ada.Integer_Text_IO.Put (Vector.Find_Index (Divider_2));
   Ada.Integer_Text_IO.Put
     (Vector.Find_Index (Divider_1) * Vector.Find_Index (Divider_2));
end Day_13;
