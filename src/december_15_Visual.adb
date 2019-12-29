with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with NT_Console;
with Intercode_09; use Intercode_09;

procedure December_15_Visual is

   type Coordinates is record
      X, Y : Integer := 0;
   end record; -- Coordinates;

   subtype Droid_Responses is Program_Store_Elements range 0 .. 2;

   Wall : constant Droid_Responses := 0;
   Clear : constant Droid_Responses := 1;
   Oxygen : constant Droid_Responses := 2;

   type Path_Elements is record
      Droid_Response : Droid_Responses := Wall;
      Bread_Crumb : Boolean := False;
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

   type Step_Elements is record
      Coordinate : Coordinates;
      Droid_Response : Droid_Responses;
   end record; -- Step_Elements

   package Step_histories is new
     Ada.Containers.Vectors (Positive, Step_Elements);
   use Step_Histories;

   procedure Find_Limits (Path_Map : in Path_Maps.Map;
                          X_Min, X_Max, Y_Min, Y_Max : out Integer) is

      Search_Coordinate : Coordinates;

   begin -- Find_Limits
      X_Max := Integer'First;
      Y_Max := Integer'First;
      X_Min := Integer'Last;
      Y_Min := Integer'Last;
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
   end Find_Limits;

   procedure Display (Step_history : in Step_histories.Vector;
                      X_Min, X_Max, Y_Min, Y_Max : in Integer) is

      package Screen is new NT_Console (X_Max - X_Min + 1, Y_Max - Y_Min + 2);
      use Screen;

   begin -- Display
      Clear_Screen;
      Goto_XY (-X_Min, -Y_min);
      -- Implicit assumption that both X_min and Y_min are negative;
      Put ('+');
      for I in Iterate (Step_History) loop
         Goto_XY (Step_history (I).Coordinate.X - X_Min,
                  Step_history (I).Coordinate.Y - Y_Min);
         case Step_history (I).Droid_Response is
         when Wall =>
            Put ('#');
         when Clear =>
            Put ('.');
         when Oxygen =>
            Put ('O');
         end case; -- Step_history (I).Droid_Response
         delay 0.1;
      end loop; -- I in Iterate (Step_History)
      Goto_XY (0, Y_Max - Y_Min);
   end Display;

   Brain : Processor;

   procedure Explore (Coordinate : in Coordinates;
                      Path_Map : in out Path_Maps.Map;
                     Step_history : in out Step_histories.Vector) is

      subtype Directions is Program_Store_Elements range 1 .. 4;
      North : constant Directions := 1;
      South : constant Directions := 2;
      West : constant Directions := 3;
      East : constant Directions := 4;

      procedure Forward (Coordinate : in Coordinates;
                         Direction : in Directions;
                         New_Coordinate : out Coordinates) is

      begin -- Forward
         case Direction is
            when North =>
               New_Coordinate.X := Coordinate.X;
               New_Coordinate.Y := Coordinate.Y - 1;
            when South =>
               New_Coordinate.X := Coordinate.X;
               New_Coordinate.Y := Coordinate.Y + 1;
            when West =>
               New_Coordinate.X := Coordinate.X - 1;
               New_Coordinate.Y := Coordinate.Y;
            when East =>
               New_Coordinate.X := Coordinate.X + 1;
               New_Coordinate.Y := Coordinate.Y;
         end case; -- Current_Direction
      end Forward;

      function Back_Track (Direction : in Directions) return Directions is

      begin -- Back_Track
         case Direction is
            when North =>
               return South;
            when South =>
               return North;
            when West =>
               return East;
            when East =>
               return West;
         end case; -- Current_Direction
      end Back_Track;

      New_Coordinate : Coordinates;
      Path_Element : Path_Elements;
      Path_Cursor : Path_Maps.Cursor;
      Back_Track_Response : Program_Store_Elements;
      Step_Element : Step_Elements;

   begin -- Explore
      Path_Map (Coordinate).Bread_Crumb := True;
      for Direction in Directions loop
         Forward (Coordinate, Direction, New_Coordinate);
         Path_Cursor := Find (Path_Map, New_Coordinate);
         if Path_Cursor = Path_Maps.No_Element or else
           not Path_Map (Path_Cursor).Bread_Crumb then
            Brain.Send_Input (Direction);
            Brain.Receive_Output (Path_Element.Droid_Response);
            Step_Element.Coordinate := New_Coordinate;
            Step_Element.Droid_Response := Path_Element.Droid_Response;
            Append (Step_history, Step_Element);
            Path_Element.Bread_Crumb := False;
            if Path_Element.Droid_Response = Wall then
               include (Path_Map, New_Coordinate, Path_Element);
            else
               Path_Cursor := Find (Path_Map, New_Coordinate);
               If Path_Cursor = Path_Maps.No_Element then
                  include (Path_Map, New_Coordinate, Path_Element);
                  Explore (New_Coordinate, Path_Map, Step_history);
               else
                  Explore (New_Coordinate, Path_Map, Step_history);
               end if; -- Path_Cursor = No_Element
               Brain.Send_Input (Back_Track (Direction));
               Brain.Receive_Output (Back_Track_Response);
               Assert (Back_Track_Response =
                         Path_Map (Coordinate).Droid_Response,
                       "Bad Bactrack");
            end if; -- Path_Element.Droid_Response = Wall
         end if; -- Path_Cursor = No_Element or else ...
      end loop; -- Direction : in Directions
      Path_Map (Coordinate).Bread_Crumb := False;
   end Explore;

   Path_Element : Path_Elements := (Droid_Response => Clear,
                                   Bread_Crumb => True);
   Coordinate : constant Coordinates := (0, 0);
   Path_Map : Path_Maps.Map;
   X_Min, X_Max, Y_Min, Y_Max : Integer;
   Step_history : Step_histories.Vector;

begin -- December_15_Visual
   Path_Map := Path_Maps.Empty_Map;
   Brain.Load_Program ("December_15.txt");
   Brain.Run_Program;
   Include (Path_Map, Coordinate, Path_Element);
   Step_history := Step_histories.Empty_Vector;
   Explore (Coordinate, Path_Map, Step_History);
   Find_Limits (Path_Map, X_Min, X_Max, Y_Min, Y_Max);
   Display (Step_history, X_Min, X_Max, Y_Min, Y_Max);
   Brain.Send_Input (0);
   -- illegal direction causes the repair droid program to halt and hence
   -- exit
end December_15_Visual;
