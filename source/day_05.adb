with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;

procedure Day_05 is
   type Id is range 0 .. 10_000_000_000;

   package Id_IO is new Ada.Text_IO.Integer_IO (Id);

   procedure Skip_Line;

   type Id_Span is array (1 .. 2) of Id;
   --  All ids between the first and second element

   package Id_Span_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Id_Span);

   type Id_Span_Array is array (Positive range <>) of Id_Span;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line is
      Ignore : String := Ada.Text_IO.Get_Line;
   begin
      pragma Assert (Ignore = "");
   end Skip_Line;

   package Id_Maps is
      type Id_Map is tagged private;

      procedure Read (Self : in out Id_Map);
      function Get (Self : Id_Map; Span : Id_Span) return Id_Span_Array;

   private
      package Span_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type     => Id_Span,
         Element_Type => Id'Base,
         "<"          => "<",
         "="          => "=");

      type Id_Map is tagged record
         Map : Span_Maps.Map;
      end record;
   end Id_Maps;

   package body Id_Maps is

      function To_Array (List : Id_Span_Lists.List) return Id_Span_Array;

      --------------
      -- To_Array --
      --------------

      function To_Array (List : Id_Span_Lists.List) return Id_Span_Array is
         Result : Id_Span_Array (1 .. Natural (List.Length));
         Cursor : Id_Span_Lists.Cursor := List.First;
      begin
         for Item of Result loop
            Item := Id_Span_Lists.Element (Cursor);
            Id_Span_Lists.Next (Cursor);
         end loop;

         return Result;
      end To_Array;

      ---------
      -- Get --
      ---------

      function Get (Self : Id_Map; Span : Id_Span) return Id_Span_Array is
         List   : Id_Span_Lists.List;
         X      : Id_Span := Span;
      begin
         while X (1) <= X (2) loop
            declare
               Item   : Id_Span;
               First  : constant Span_Maps.Cursor :=
                 Self.Map.Floor ([X (1), Id'Last]);
            begin
               if Span_Maps.Has_Element (First) then
                  Item := Span_Maps.Key (First);

                  if X (1) >= Item (1) and X (1) <= Item (2) then
                     List.Append
                       ([1 => X (1) + Span_Maps.Element (First),
                         2 => Id'Min (Item (2), X (2))
                                + Span_Maps.Element (First)]);

                     X (1) := Id'Min (Item (2), X (2)) + 1;

                  else
                     declare
                        Next : constant Span_Maps.Cursor :=
                          Span_Maps.Next (First);
                     begin
                        if Span_Maps.Has_Element (Next) then
                           Item := Span_Maps.Key (Next);

                           if X (2) >= Item (1) and X (2) <= Item (2) then
                              List.Append
                                ([1 => X (1),
                                  2 => Id'Min (Item (1) - 1, X (2))]);

                              X (1) := Id'Min (Item (1) - 1, X (2)) + 1;
                           else
                              List.Append (X);
                              X (1) := X (2) + 1;
                           end if;
                        else
                           List.Append (X);
                           X (1) := X (2) + 1;
                        end if;
                     end;
                  end if;
               else
                  Item := Self.Map.First_Key;

                  if X (2) >= Item (1) and X (2) <= Item (2) then
                     List.Append
                       ([1 => X (1),
                         2 => Id'Min (Item (1) - 1, X (2))]);

                     X (1) := Id'Min (Item (1) - 1, X (2)) + 1;
                  else
                     List.Append (X);
                     X (1) := X (2) + 1;
                  end if;
               end if;
            end;
         end loop;

         return To_Array (List);
      end Get;

      procedure Read (Self : in out Id_Map) is
         Ignore : String := Ada.Text_IO.Get_Line;
      begin
         while not Ada.Text_IO.End_Of_Line loop
            declare
               Destination : Id;
               Source      : Id;
               Length      : Id;
            begin
               Id_IO.Get (Destination);
               Id_IO.Get (Source);
               Id_IO.Get (Length);

               Self.Map.Insert
                 (Key      => [Source, Source + Length - 1],
                  New_Item => Destination - Source);

               Skip_Line;
            end;
         end loop;

         if not Ada.Text_IO.End_Of_File then
            Skip_Line;
         end if;
      end Read;
   end Id_Maps;

   type Map_Kind is
     (Seed_To_Soil,
      Soil_To_Fertilizer,
      Fertilizer_To_Water,
      Water_To_Light,
      Light_To_Temperature,
      Temperature_To_Humidity,
      Humidity_To_Location);

   function Find_Min_Location
     (Span : Id_Span;
      Kind : Map_Kind := Seed_To_Soil) return Id;

   Maps : array (Map_Kind) of Id_Maps.Id_Map;

   function Find_Min_Location
     (Span : Id_Span;
      Kind : Map_Kind := Seed_To_Soil) return Id
   is
      List : constant Id_Span_Array := Maps (Kind).Get (Span);
      Ids  : array (List'Range) of Id;
      Min  : Id := Id'Last;
   begin
      for J in List'Range loop
         Ids (J) :=
           (if Kind = Map_Kind'Last then List (J) (1)
            else Find_Min_Location (List (J), Map_Kind'Succ (Kind)));
      end loop;

      for Next of Ids loop
         Min := Id'Min (Min, Next);
      end loop;

      return Min;
   end Find_Min_Location;

   Seeds : Id_Span_Lists.List;
   Min   : Id := Id'Last;
begin
   declare
      Ignore : String (1 .. 6);
   begin
      Ada.Text_IO.Get (Ignore);
   end;

   while not Ada.Text_IO.End_Of_Line loop
      declare
         Next : Id;
         Length : Id;
      begin
         Id_IO.Get (Next);
         Id_IO.Get (Length);
         Seeds.Append ([Next, Next + Length - 1]);
      end;
   end loop;

   Skip_Line;
   Skip_Line;

   for Map of Maps loop
      Map.Read;
   end loop;

   for Span of Seeds loop
      declare
         Next : constant Id := Find_Min_Location (Span);
      begin
         Min := Id'Min (Min, Next);
      end;
   end loop;

   Id_IO.Put (Min);
end Day_05;
