with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Intercode_09; use Intercode_09;

procedure December_15 is

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

   procedure Display (Path_Map : in Path_Maps.Map;
                      Droid : in Coordinates := (0, 0)) is

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
      for Y in Integer range Y_Min .. Y_Max loop
         Search_Coordinate.Y := Y;
         for X in Integer range X_Min .. X_Max loop
            Search_Coordinate.X := X;
            Path_Cursor := Find (Path_Map, Search_Coordinate);
            if X = 0 and Y = 0 then
               Put ('+');
            elsif Path_Cursor = Path_Maps.No_Element then
               -- default to unknown
               If Droid.X = X and Droid.y = Y then
                  Put ('U');
               else
                  Put ('u');
               end if; -- Droid.X = X and Droid.y = Y
            elsif Droid.X = X and Droid.y = Y then
               if Path_Map (Path_Cursor).Droid_Response /= Clear then
                  Put ('D');
               else
                  Put ('d');
               end if; -- Path_Map (Path_Cursor).Droid_Response /= Clear
            else
               case Path_Map (Path_Cursor).Droid_Response is
               when Clear =>
                  Put ('.');
               when Wall =>
                  Put ('#');
               when Oxygen =>
                  Put ('O');
               end case; -- Path_Map (Path_Cursor).Droid_Response = Clear
            end if; -- X = 0 and Y = 0
         end loop; -- X in Integer range X_Min .. Y_Max
         New_Line;
      end loop; -- Y in Integer range Y_Min .. Y_Max
   end Display;

   Brain : Processor;

   procedure Explore (Coordinate : in Coordinates;
                      Distance : in Natural;
                      Shortest_Distance : in out Natural;
                      Path_Map : in out Path_Maps.Map) is

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

   begin -- Explore
      Path_Map (Coordinate).Bread_Crumb := True;
      for Direction in Directions loop
         Forward (Coordinate, Direction, New_Coordinate);
         Path_Cursor := Find (Path_Map, New_Coordinate);
         if Path_Cursor = No_Element or else
           not Path_Map (Path_Cursor).Bread_Crumb then
            Brain.Send_Input (Direction);
            Brain.Receive_Output (Path_Element.Droid_Response);
            Path_Element.Bread_Crumb := False;
            if Path_Element.Droid_Response = Wall then
               include (Path_Map, New_Coordinate, Path_Element);
            else
               if  Path_Element.Droid_Response = Oxygen and then
                 Distance + 1 < Shortest_Distance then
                  Shortest_Distance := Distance + 1;
               end if;
               Path_Cursor := Find (Path_Map, New_Coordinate);
               If Path_Cursor = No_Element then
                  include (Path_Map, New_Coordinate, Path_Element);
                  Explore (New_Coordinate, Distance + 1, Shortest_Distance,
                           Path_Map);
               else
                  Explore (New_Coordinate, Distance + 1, Shortest_Distance,
                           Path_Map);
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

   procedure Oxygen_Fill (Path_Map : in Path_Maps.Map;
                          Fill_Time : out Natural) is

      procedure Find_Oxygen (Path_Map : in out Path_Maps.Map;
                            Oxygen_Coordinate : out Coordinates) is

      begin -- Find_Oxygen
         for I in Iterate (Path_Map) loop
            if Path_Map (I).Droid_Response = Oxygen then
               Path_Map (I).Bread_Crumb := True;
               Oxygen_Coordinate := Key (I);
            else
               Path_Map (I).Bread_Crumb := False;
            end if; -- Path_Map (I).Droid_Response = Oxygen
         end loop; -- I in Iterate (Path_Map)
      end Find_Oxygen;

      type Queue_Element is record
         Coordinate : Coordinates;
         Fill_Time : Natural; -- Distance from oxigen is a proxy for time
      end record; -- Queue_Element

      package OQI is new
        Ada.Containers.Synchronized_Queue_Interfaces (Queue_Element);

      package Oxygen_Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (OQI);

      Oxygen_Queue : Oxygen_Queues.Queue;
      Oxygen_Coordinate : Coordinates;
      Filled, Test : Queue_Element;
      Current_Map : Path_Maps.Map := Copy (Path_Map);

      -- uses Bread_Crumb in Path_Element to indicate the presence of Oxygen.

   begin -- Oxygen_Fill
      Find_Oxygen (Current_Map, Oxygen_Coordinate);
      Oxygen_Queue.Enqueue ((Oxygen_Coordinate, 0));
      while Oxygen_Queue.Current_Use > 0 loop
         Oxygen_Queue.Dequeue (Filled);
         Current_Map (Filled.Coordinate).Bread_Crumb := True;
         -- Test cells adjacent to the recently filled element
         Test.Coordinate.X := Filled.Coordinate.X;
         Test.Coordinate.Y := Filled.Coordinate.Y - 1;
         Test.Fill_Time := Filled.Fill_Time + 1;
         if Current_Map (Test.Coordinate).Droid_Response /= Wall and then
           not Current_Map (Test.Coordinate).Bread_Crumb then
            Oxygen_Queue.Enqueue (Test);
         end if; -- Current_Map (Test.Coordinate).Droid_Response /= Wall ...
         Test.Coordinate.Y := Filled.Coordinate.Y + 1;
         if Current_Map (Test.Coordinate).Droid_Response /= Wall and then
           not Current_Map (Test.Coordinate).Bread_Crumb then
            Oxygen_Queue.Enqueue (Test);
         end if; -- Current_Map (Test.Coordinate).Droid_Response /= Wall  ...
         Test.Coordinate.Y := Filled.Coordinate.Y;
         Test.Coordinate.X := Filled.Coordinate.X - 1;
         if Current_Map (Test.Coordinate).Droid_Response /= Wall and then
           not Current_Map (Test.Coordinate).Bread_Crumb then
            Oxygen_Queue.Enqueue (Test);
         end if; -- Current_Map (Test.Coordinate).Droid_Response /= Wall ...
         Test.Coordinate.X := Filled.Coordinate.X + 1;
         if Current_Map (Test.Coordinate).Droid_Response /= Wall and then
           not Current_Map (Test.Coordinate).Bread_Crumb then
            Oxygen_Queue.Enqueue (Test);
         end if; -- Current_Map (Test.Coordinate).Droid_Response /= Wall ...
      end loop; --  Oxygen_Queue.Current_Use > 0
      Fill_Time := Filled.Fill_Time;
   end Oxygen_Fill;

   Start_Element : constant Path_Elements := (Droid_Response => Clear,
                                              Bread_Crumb => True);
   Start_Coordinate : constant Coordinates := (X => 0, Y => 0);
   Shortest_Distance : Natural := Natural'Last;
   -- Starting conditions for Explore
   Fill_Time : Natural;
   Path_Map : Path_Maps.Map;

begin -- December_15
   Path_Map := Path_Maps.Empty_Map;
   Brain.Load_Program ("December_15.txt");
   Brain.Run_Program;
   Include (Path_Map, Start_Coordinate, Start_Element);
   Explore (Start_Coordinate, 0, Shortest_Distance, Path_Map);
   Display (Path_Map);
   Put_Line ("Shortest distance to Oxygen:" &
               Natural'Image (Shortest_Distance));
   Oxygen_Fill (Path_Map, Fill_Time);
   Put_Line ("Time to fill with Oxygen:" & Natural'Image (Fill_Time));
   Brain.Send_Input (0);
   -- illegal direction causes the repair droid program to halt and hence
   -- exit
end December_15;
