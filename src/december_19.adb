with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Intercode_09; use Intercode_09;

procedure December_19 is

   Input_File_Name : constant String := "December_19.txt";

   subtype Coordinate_Elements is Program_Store_Elements range 0 .. 49;

   type Coordinates is record
      X, Y : Coordinate_Elements := 0;
   end record; -- Coordinates;

   subtype Spaces is Program_Store_Elements range 0 .. 1;
   Empty : constant Spaces := 0;
   Beam : constant Spaces := 1;

   function "<" (Left, Right : Coordinates) return Boolean is

   begin -- "<"
      return Left.Y < Right.Y or else
        (Left.Y = Right.Y and Left.X < Right.X);
   end "<";

   function "=" (Left, Right : Coordinates) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Space_Maps is new
     Ada.Containers.Ordered_Maps (Coordinates, Spaces);
   use Space_Maps;

   function Test_Beam (X, Y : in Program_Store_Elements) return Spaces is

      Drone_Control : Processor;
      Space : Spaces;

   begin -- Test_Beam
      Drone_Control.Load_Program (Input_File_Name);
      Drone_Control.Run_Program;
      Drone_Control.Send_Input (X);
      Drone_Control.Send_Input (Y);
      Drone_Control.Receive_Output (Space);
      return Space;
   end Test_Beam;

   procedure Map_Beam (Space_Map : out Space_Maps.Map) is

      Current : Coordinates;

   begin -- Map_Beam
      Space_Map := Space_Maps.Empty_Map;
      for Y in Coordinate_Elements loop
         Current.Y := Y;
         for X in Coordinate_Elements loop
            Current.X := X;
            Include (Space_Map, Current, Test_Beam (X, Y));
         end loop; -- X in Coordinate_Elements
      end loop; -- Coordinates.Y := Y;
   end Map_Beam;

   procedure Put (Space_Map : in Space_Maps.Map) is

      Current : Coordinates;
      Cursor : Space_Maps.Cursor;

   begin -- Put
      for Y in Coordinate_Elements loop
         Current.Y := Y;
         for X in Coordinate_Elements loop
            Current.X := X;
            Cursor := Find (Space_Map, Current);
            if Space_Map (Cursor) = Beam then
               Put ('#');
            else
               Put ('.');
            end if; -- Space_Map (Cursor) = Beam
         end loop; -- X in Coordinate_Elements
         New_Line;
      end loop; -- Coordinates.Y := Y;
   end Put;

   procedure Search (Space_Map : in Space_Maps.Map) is

      Side : constant Program_Store_Elements := 100;

      procedure Find_Width (Space_Map : in Space_Maps.Map;
                             Last_Left, Last_Right, Ys :
                             out Program_Store_Elements) is

         Xs : Program_Store_Elements;
         Current : Coordinates;
         Cursor : Space_Maps.Cursor;
         First_Found : Boolean := False;
         Space : Spaces := Beam;

      begin -- Find_Width
         for Y in reverse Coordinate_Elements loop
            Current.Y := Y;
            for X in Coordinate_Elements loop
               Current.X := X;
               Cursor := Find (Space_Map, Current);
               if Space_Map (Cursor) = Beam then
                  Ys := Y;
                  Last_Left := X;
                  First_Found := True;
               end if; -- Space_Map (Cursor) = Beam
               exit when First_Found;
            end loop; -- X in Coordinate_Elements
            exit when First_Found;
         end loop; -- Y in reverse Coordinate_Elements
         Xs := Last_Left + 1;
         Last_Right := Last_Left;
         while Space = Beam loop
            Space := Test_Beam (Xs, Ys);
            if Space = Beam then
               Last_Right := Xs;
            end if; -- Space = Beam
            Xs := Xs + 1;
         end loop; -- Space = Beam
      end  Find_Width;

      procedure Find_Width (X0, Ys : in Program_Store_Elements;
                            Last_Left,
                            Last_Right : out Program_Store_Elements) is

         Xs : Program_Store_Elements;
         Cursor : Space_Maps.Cursor;
         Space : Spaces;

      begin -- Find_Width
         Xs := X0 + 1;
         Space := Beam;
         Last_Right := X0;
         while Space = Beam loop
            Space := Test_Beam (Xs, Ys);
            if Space = Beam then
               Last_Right := Xs;
            end if; -- Space = Beam
            Xs := Xs + 1;
         end loop; -- Space = Beam
         Xs := X0 - 1;
         Space := Beam;
         Last_Left := X0;
         while Space = Beam loop
            Space := Test_Beam (Xs, Ys);
            if Space = Beam then
               Last_Left := Xs;
            end if; -- Space = Beam
            Xs := Xs - 1;
         end loop; -- Space = Beam
      end Find_Width;

      Last_Left, Last_Right, Ys : Program_Store_Elements;

   begin -- Search
      Find_Width (Space_Map, Last_Left, Last_Right, Ys);
      loop -- Find squar in beam
         Ys := Ys + 1;
         Find_Width ((Last_Left + Last_Right) / 2, Ys, Last_Left, Last_Right);
         exit when Last_Right - Last_Left + 1 > Side and then
         -- Top Left
           Beam = Test_Beam (Last_Right - Side + 1, Ys + Side - 1) and then
         -- Bottom Left
           Beam= Test_Beam (Last_Right, Ys + Side - 1);
         -- Bottom Right
      end loop; -- Find squar in beam
      Put_Line ("Part Two:" & Program_Store_Elements'Image
                (10000 * (Last_Right - Side + 1) + Ys));
   end Search;

   Space_Map : Space_Maps.Map;
   Count : Natural := 0;

begin -- December_19
   Map_Beam (Space_Map);
   for I in Iterate (Space_Map) loop
      if Space_Map (I) = Beam then
         Count := Count + 1;
      end if; -- Space_Map (I) = Block
   end loop; -- I in Iterate (Space_Map)
   Put (Space_Map);
   Put_Line ("In Beam:" & Natural'Image (Count));
   Search (Space_Map);
end December_19;
