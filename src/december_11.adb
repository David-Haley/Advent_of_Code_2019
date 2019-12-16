with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Intercode_09; use Intercode_09;
with Ada.Containers.Ordered_Maps;

procedure December_11 is

   type Coordinates is record
      X, Y : Integer := 0;
   end record; -- Coordinates;

   subtype Colours is Program_Store_Elements range 0 .. 1;

   White : constant Colours := 1;
   Black : constant Colours := 0;

   type Path_Elements is record
      Coordinate : Coordinates;
      Colour :  Colours := Black;
   end record; -- Path_Elements

   function "<" (Left, Right : Coordinates) return Boolean is

   begin -- "<"
      return Left.Y < Right.Y or else
        (Left.Y = Right.Y and Left.X < Right.X);
   end "<";

   function "=" (Left, Right : Coordinates) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Path_Maps is new
     Ada.Containers.Ordered_Maps (Coordinates, Path_Elements);
   use Path_Maps;

      type Directions is (Up, Left, Down, Right);

   procedure Paint (Path_Map : out Path_Maps.Map;
                    Start_Colour : Colours := Black) is

      type Directions is (Up, Left, Down, Right);

      subtype Turns is Program_Store_Elements range 0 .. 1;

      Left_Turn : constant Colours := 0;
      Right_Turn : constant Colours := 1;

      Turn : Turns;
      Path_Element : Path_Elements;
      Current_Direction : Directions := Up;
      Brain : Processor;
      Path_Cursor : Path_Maps.Cursor;
      Paint_Cycles : Positive := 1;

   begin -- Paint
      Path_Map := Path_Maps.Empty_Map;
      Brain.Load_Program ("December_11.txt");
      Brain.Run_Program;
      Path_Element.Colour := Start_Colour;
      loop -- until tasking error on input
         begin -- Input exception block
            Brain.Send_Input (Path_Element.Colour);
         exception
            when Tasking_Error =>
               exit;
         end; -- Input exception block
         Brain.Receive_Output (Path_Element.Colour);
         Include (Path_Map, Path_Element.Coordinate, Path_Element);
         -- over writes any element with the same coordinates
         Brain.Receive_Output (Turn);
         -- order is Up, Left, Down, Right
         case Turn is
            when Left_Turn =>
               if Current_Direction = Directions'Last then
                  Current_Direction := Directions'First;
               else
                  Current_Direction := Directions'Succ (Current_Direction);
               end if; -- Current_Direction = Directions'Last
            when Right_Turn =>
               if Current_Direction = Directions'First then
                  Current_Direction := Directions'Last;
               else
                  Current_Direction := Directions'Pred (Current_Direction);
               end if; -- Current_Direction = Directions'Last
         end case; -- Turn
         case Current_Direction is
            when Up =>
               Path_Element.Coordinate.Y := Path_Element.Coordinate.Y + 1;
            when Down =>
               Path_Element.Coordinate.Y := Path_Element.Coordinate.Y - 1;
            when Left =>
               Path_Element.Coordinate.X := Path_Element.Coordinate.X - 1;
            when Right =>
               Path_Element.Coordinate.X := Path_Element.Coordinate.X + 1;
         end case; -- Current_Direction
         Path_Cursor := Find (Path_Map, Path_Element.Coordinate);
         if Path_Cursor = No_Element then
            Path_Element.Colour := Black;
         else
            Path_Element.Colour := Element (Path_Cursor).Colour;
         end if; -- Path_Cursor = No_Element
         Paint_Cycles := Paint_Cycles + 1;
      end loop; -- until tasking error on input;
   end Paint;

   procedure Display (Path_Map : in Path_Maps.Map) is

      Search_Coordinate : Coordinates;
      Path_Cursor : Path_Maps.Cursor;
      X_Max, Y_Max : Integer := Integer'First;
      X_Min, Y_Min : Integer := Integer'Last;

   begin -- Display
      for I in Iterate (Path_Map) loop
         Search_Coordinate := Key (I);
         if Search_Coordinate.X < X_Min then
            X_Min := Search_Coordinate.X;
         end if; -- Search_Coordinate.X < X_Min;
         if Search_Coordinate.X > X_Max then
            X_Max := Search_Coordinate.X;
         end if; -- Search_Coordinate.X > X_Max
         if Search_Coordinate.Y < Y_Min then
            Y_Min := Search_Coordinate.Y;
         end if; -- Search_Coordinate.Y < Y_Min;
         if Search_Coordinate.Y > Y_Max then
            Y_Max := Search_Coordinate.Y;
         end if; -- Search_Coordinate.Y > Y_Max
      end loop; -- I in Iterate (Path_Map)
      for Y in reverse Integer range Y_Min .. Y_Max loop
         Search_Coordinate.Y := Y;
         for X in Integer range X_Min .. X_Max loop
            Search_Coordinate.X := X;
            Path_Cursor := Find (Path_Map, Search_Coordinate);
            if Path_Cursor = Path_Maps.No_Element then
               Put (' '); -- default to black
            else
               if Path_Map (Path_Cursor).Colour = White then
                  Put ('*');
               else
                  Put (' ');
               end if; -- Path_Map (Path_Cursor).Colour = White
            end if; -- Path_Cursor = Path_Maps.No_Element
         end loop; -- X in Integer range X_Min .. Y_Max
         New_Line;
      end loop; -- in reverse Integer range Y_Min .. Y_Max
   end Display;

   Path_Map : Path_Maps.Map;

begin -- December_11
   Paint (Path_Map);
   Put_Line ("Painted Panels:" & Count_Type'Image (Length (Path_Map)));
   Path_Map := Path_Maps.Empty_Map;
   Paint (Path_Map, White);
   Put_Line ("Part Two");
   Display (Path_Map);
end December_11;
