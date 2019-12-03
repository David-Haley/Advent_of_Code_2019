with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_03 is

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
         X, Y : Integer;
      end record; -- Coordinates;

      package Wire_Elements is new
        Ada.Containers.Vectors (Natural, Coordinates);
      use Wire_Elements;

      Procedure Trace_Wire (Wire : in Wires.Vector;
                            Wire_Element : in out Wire_Elements.Vector) is

         Coordinate : Coordinates := (X => 0, Y => 0);

      begin -- Trace_Wire
         -- wire at the origin is excluded
         for I in Iterate (Wire) loop
            for Length in Natural range 1 .. Wire (I).Length loop
               case Wire (I).Direction is
               when Up =>
                  Coordinate.Y := Coordinate.Y + 1;
               when Down =>
                  Coordinate.Y := Coordinate.Y - 1;
               when Left =>
                  Coordinate.X := Coordinate.X - 1;
               when Right =>
                  Coordinate.X := Coordinate.X + 1;
               end case; -- Wire (I).Direction
               Append (Wire_Element, Coordinate);
            end loop; -- Length in Natural range 1 .. Wire (I).Length
         end loop; -- I in Iterate (Wire)
      end Trace_Wire;

      Wire_1_Element, Wire_2_Element : Wire_Elements.Vector
        := Wire_Elements.Empty_Vector;

      Steps_1, Steps_2 : Natural;

   begin -- Find_Intersections
      Distance := Natural'Last;
      Steps := Natural'Last;
      Trace_Wire (Wire_1, Wire_1_Element);
      Trace_Wire (Wire_2, Wire_2_Element);
      for I in Iterate (Wire_1_Element) loop
         if Contains (Wire_2_element, Wire_1_Element (I)) then
            -- is intersection
            if Distance >
            abs (Wire_1_Element (I).X) + abs (Wire_1_Element (I).Y) then
               Distance := abs (Wire_1_Element (I).X) +
               abs (Wire_1_Element (I).Y);
            end if; -- smaller distance
            Steps_1 := To_Index (I) + 1;
            Steps_2 := Find_Index (Wire_2_Element, Wire_1_Element (I)) + 1;
            if Steps > Steps_1 + Steps_2 then
               Steps := Steps_1 + Steps_2;
            end if; -- Steps > Steps_1 + Steps_2
         end if; -- Contains (Wire_2_element, Wire_1_Element (I))
      end loop; -- I in Iterate (Wire_1_Element)
   end Find_Intersections;

   Wire_1, Wire_2 : Wires.Vector;
   Distance, Steps : Natural;

begin -- December_03
   Read_Input (Wire_1, Wire_2);
   Put_Line ("Finding Intersections");
   Find_Intersections (Wire_1, Wire_2, Distance, Steps);
   Put_Line ("Distance:" & Natural'Image (Distance));
   Put_Line ("Combined Steps:" & Natural'Image (Steps));
end December_03;
