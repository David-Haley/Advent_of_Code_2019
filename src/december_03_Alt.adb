with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure December_03_Alt is

   type Directions is (Up, Down, Left, Right);

   package Direction_IO is new Ada.Text_IO.Enumeration_IO (Directions);
   use Direction_IO;

   package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);
   use Natural_IO;

   type Segments is record
      Direction : Directions;
      Length : Natural;
   end record; -- Segment

   package Wires is new Ada.Containers.Vectors (Natural, Segments);
   use Wires;

   procedure Read_Input (Wire_1, Wire_2 : out Wires.Vector) is

      Input_File : File_Type;
      Dir, Comma : Character;
      Segment : Segments;
      First_Wire : Boolean := True;

   begin -- Read_Input
      Put_Line ("Reading first wire ");
      Open (Input_File, In_File, "December_03.txt");
      Wire_1 := Wires.Empty_Vector;
      Wire_2 := Wires.Empty_Vector;
      while not End_Of_File (Input_File) loop
         Get (Input_File, Dir);
         case Dir is
            when 'U' =>
               Segment.Direction := Up;
            when 'D' =>
               Segment.Direction := Down;
            when 'L' =>
               Segment.Direction := Left;
            when 'R' =>
               Segment.Direction := Right;
            when others =>
               Assert (False, "Expected 'U' | 'D' | 'L' | 'R' found '" &
                         Dir & "'");
         end case; -- Dir
         Get (Input_File, Segment.Length);
         if First_Wire then
            Append (Wire_1, Segment);
         else
            Append (Wire_2, Segment);
         end if; -- First_Wire
         if End_Of_Line (Input_File) then
            if First_Wire then
               Put_Line ("Reading second wire ");
            end if; -- First_Wire
            First_Wire := False;
            Skip_Line (Input_File);
         else
            Get (Input_File, Comma);
            Assert (Comma = ',', "Expected ',' found '" & Comma & "'");
         end if; -- End_of_Line (Input_File)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Find_Intersections (Wire_1, Wire_2 : in Wires.Vector;
                                Distance, Steps : out Natural) is

      type Coordinates is record
         X, Y : Integer := 0;
      end record; -- Coordinates;

      type Wire_Elements is record
         Coordinate : Coordinates;
         Step_Count : Natural := 0;
      end record; -- Wire_Element

      function "<" (Left, Right : Coordinates) return Boolean is

      begin -- "<"
         return Left.X < Right.X or else
           (Left.X = Right.X and Left.Y < Right.Y);
      end "<";

      function "=" (Left, Right : Coordinates) return Boolean is

      begin -- "="
         return Left.X = Right.X and Left.Y = Right.Y;
      end "=";

      package Wire_Maps is new
        Ada.Containers.Ordered_Maps (Coordinates, Wire_Elements);
      use Wire_Maps;

      Procedure Trace_Wire (Wire : in Wires.Vector;
                            Wire_Map : out Wire_Maps.Map) is

         Wire_Element : Wire_Elements;

      begin -- Trace_Wire
         -- wire at the origin is excluded
         Clear (Wire_Map);
         for I in Iterate (Wire) loop
            for Length in Natural range 1 .. Wire (I).Length loop
               case Wire (I).Direction is
               when Up =>
                   Wire_Element.Coordinate.Y := Wire_Element.Coordinate.Y + 1;
               when Down =>
                  Wire_Element.Coordinate.Y := Wire_Element.Coordinate.Y - 1;
               when Left =>
                  Wire_Element.Coordinate.X := Wire_Element.Coordinate.X - 1;
               when Right =>
                  Wire_Element.Coordinate.X := Wire_Element.Coordinate.X + 1;
               end case; -- Wire (I).Direction
               Wire_Element.Step_Count := Wire_Element.Step_Count + 1;
               if not Contains (Wire_Map, Wire_Element.Coordinate) then
                  Include (Wire_Map, Wire_Element.Coordinate, Wire_Element);
                  -- Only include first instance of a wire passing through a
                  -- coordinate, that is nearest to the start of the wire.
               end if; --not Contains (Wire_Map, Wire_Element.Coordinate)
            end loop; -- Length in Natural range 1 .. Wire (I).Length
         end loop; -- I in Iterate (Wire)
      end Trace_Wire;

      Wire_1_Map, Wire_2_Map : Wire_Maps.Map;
      Intersection_Steps : Natural;

   begin -- Find_Intersections
      Distance := Natural'Last;
      Steps := Natural'Last;
      Trace_Wire (Wire_1, Wire_1_Map);
      Trace_Wire (Wire_2, Wire_2_Map);
      for I in Iterate (Wire_1_Map) loop
         if Contains (Wire_2_Map, Wire_1_Map (I).Coordinate) then
            -- is intersection
            if Distance > abs (Wire_1_Map (I).Coordinate.X) +
            abs (Wire_1_Map (I).Coordinate.Y) then
               Distance := abs (Wire_1_Map (I).Coordinate.X) +
               abs (Wire_1_Map (I).Coordinate.Y);
            end if; -- smaller distance
            Intersection_Steps := Wire_1_Map (I).Step_Count +
              Wire_2_Map (Find (Wire_2_Map,
                          Wire_1_Map (I).Coordinate)).Step_Count;
            if Steps > Intersection_Steps then
               Steps := Intersection_Steps;
            end if; -- Steps > Intersection_Steps
         end if; -- Contains (Wire_2_Map, Wire_1_Map (I).Coordinate)
      end loop; -- I in Iterate (Wire_1_Map)
   end Find_Intersections;

   Wire_1, Wire_2 : Wires.Vector;
   Distance, Steps : Natural;

begin -- December_03_Alt
   Read_Input (Wire_1, Wire_2);
   Put_Line ("Finding Intersections");
   Find_Intersections (Wire_1, Wire_2, Distance, Steps);
   Put_Line ("Distance:" & Natural'Image (Distance));
   Put_Line ("Combined Steps:" & Natural'Image (Steps));
end December_03_Alt;
