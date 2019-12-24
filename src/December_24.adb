with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Interfaces; use Interfaces;

procedure December_24 is

   Bug : constant Character := '#';
   No_Bug : constant Character := '.';

   subtype Coordinates is Natural range 1 .. 5;
   type Grids is array (Coordinates, Coordinates) of Boolean;

   procedure Read_Grid (Grid : out Grids) is

      Input_File : File_Type;

      subtype Cells is Character with Static_Predicate => Cells in Bug | No_Bug;
      Cell : Cells;

   begin -- Read_Grid
      Open (Input_File, In_File, "December_24.txt");
      for Y in Coordinates loop
         for X in Coordinates loop
            Get (Input_File, Cell);
            if Cell = Bug then
               Grid (X, Y) := True;
            else
               Grid (X, Y) := False;
            end if; -- Cell = Bug;
         end loop; --X in Coordinates
         Skip_Line (Input_File);
      end loop; --Y in Coordinates
      Close (Input_File);
   end Read_Grid;

   procedure Display (Grid : in Grids) is

   begin -- Display
      for Y in Coordinates loop
         for X in Coordinates loop
            if Grid (X, Y) then
               Put (Bug);
            else
               Put (No_Bug);
            end if; -- Grid (X, Y)
         end loop; --X in Coordinates
         New_Line;
      end loop; --Y in Coordinates
   end Display;

   procedure Solve_Part_One is

      subtype Biodiversities is Unsigned_32 range 0 ..
        2 ** (Coordinates'Last ** 2);

      package Histories is new Ada.Containers.Ordered_Maps (Biodiversities,
                                                            Grids);
      use Histories;

      procedure Update_Grid (Current_Grid : in Grids;
                             Next_Grid : out Grids) is

         subtype Neighbour_Counts is Natural range 0 .. 4;

         function Bug_Neighbours (Current_Grid : in Grids;
                                  X, Y : in Coordinates)
                               return Neighbour_Counts is

            Count : Neighbour_Counts := 0;

         begin -- Bug_Neighbours
            if X - 1 in Coordinates and then Current_Grid (X - 1, Y) then
               Count := Count + 1;
            end if; -- X - 1 in Coordinates and then Current_Grid (X - 1, Y)
            if X + 1 in Coordinates and then Current_Grid (X + 1, Y) then
               Count := Count + 1;
            end if; -- X + 1 in Coordinates and then Current_Grid (X + 1, Y)
            if Y - 1 in Coordinates and then Current_Grid (X, Y - 1) then
               Count := Count + 1;
            end if; -- Y - 1 in Coordinates and then Current_Grid (X, Y - 1)
            if Y + 1 in Coordinates and then Current_Grid (X, Y + 1) then
               Count := Count + 1;
            end if; -- Y + 1 in Coordinates and then Current_Grid (X, Y + 1)
            return Count;
         end Bug_Neighbours;

      begin -- Update_Grid
         for Y in Coordinates loop
            for X in Coordinates loop
               if Current_Grid (X, Y) then
                  Next_Grid (X, Y) := Bug_Neighbours (Current_Grid, X, Y) = 1;
               else
                  Next_Grid (X, Y) := Bug_Neighbours (Current_Grid, X, Y) = 1 or
                    Bug_Neighbours (Current_Grid, X, Y) = 2;
               end if; -- Current_Grid (X, Y)
            end loop; -- X in Coordinates
         end loop; -- Y in Coordinates
      end Update_Grid;

      function Calculate_Biodiversity (Grid : in Grids) return Biodiversities is

         Count : Biodiversities := 0;

      begin -- Calculate_Biodiversity
         for Y in reverse Coordinates loop
            for X in reverse Coordinates loop
               Count := Shift_Left (Count, 1);
               if Grid (X, Y) then
                  Count := Count + 1;
               end if; -- Grid (X, Y)
            end loop; --X in Coordinates
         end loop; --Y in Coordinates
         Return Count;
      end Calculate_Biodiversity;

      Current_Grid, Next_Grid : Grids;
      History : Histories.Map := Histories.Empty_Map;
      Current_Biodiversity : Biodiversities;
      Repeated : Boolean := False;

   begin -- Solve_Part_One
      Read_Grid (Current_Grid);
      Current_Biodiversity := Calculate_Biodiversity (Current_Grid);
      Include (History, Current_Biodiversity, Current_Grid);
      while not Repeated loop
         Update_Grid (Current_Grid, Next_Grid);
         Current_Grid := Next_Grid;
         Current_Biodiversity := Calculate_Biodiversity (Current_Grid);
         Repeated := Contains (History, Current_Biodiversity);
         Include (History, Current_Biodiversity, Current_Grid);
      end loop;
      Display (Current_Grid);
      Put_Line ("Current biodiversity:" &
                  Unsigned_32'Image (Current_Biodiversity));
   end Solve_Part_One;

   procedure Solve_Part_Two is

      subtype Levels is Integer range -201 .. 201;
      -- Assumed to be sufficient for 200 iterations, because propagation away
      -- from level 0 should only be one level per iteration at most.
      type Recursive_Grids is array (Levels) of Grids;

      procedure Update_Grid (Current_Grid : in Recursive_Grids;
                             Next_Grid : out Recursive_Grids) is

         subtype Neighbour_Counts is Natural range 0 .. 8;

         function Bug_Neighbours (Current_Grid : in Recursive_Grids;
                                  X, Y : in Coordinates;
                                  Z : in Levels)
                                  return Neighbour_Counts is
            -- Not valid when X = 3 and Y = 3

            Count : Neighbour_Counts := 0;

         begin -- Bug_Neighbours
            Assert (X /= 3 or Y /= 3, "Attempt to count for centre cell");
            if X = 1 and Current_Grid (Z - 1) (2, 3) then
               -- outside (Z - 1) left
               Count := Count + 1;
            end if; -- X = 1 and Current_Grid (Z - 1) (2, 3)
            if X = 5 and Current_Grid (Z - 1) (4, 3) then
               -- outside (Z - 1) right
               Count := Count + 1;
            end if; -- X = 5 and Current_Grid (Z - 1) (4, 3)
            if Y = 1 and Current_Grid (Z - 1) (3, 2) then
               -- outside (Z - 1) top
               Count := Count + 1;
            end if; -- Y = 1 and Current_Grid (Z - 1) (3, 2)
            if Y = 5 and Current_Grid (Z - 1) (3, 4) then
               -- outside (Z - 1) bottom
               Count := Count + 1;
            end if; -- Y = 5 and Current_Grid (Z - 1) (3, 4)
            if X = 2 and Y = 3 then
               -- inside (Z + 1) left
               for Yi in Coordinates loop
                  if Current_Grid (Z + 1) (1, Yi) then
                     Count := Count + 1;
                  end if; -- Current_Grid (Z + 1) (1, Yi)
               end loop; -- Yi in Coordinates
            end if; -- X = 2 and Y = 3
            if X = 4 and Y = 3 then
               -- inside (Z + 1) right
               for Yi in Coordinates loop
                  if Current_Grid (Z + 1) (5, Yi) then
                     Count := Count + 1;
                  end if; -- Current_Grid (Z + 1) (1, Yi)
               end loop; -- Yi in Coordinates
            end if; -- X = 4 and Y = 3
            if X = 3 and Y = 2 then
               -- inside (Z + 1) top
               for Xi in Coordinates loop
                  if Current_Grid (Z + 1) (Xi, 1) then
                     Count := Count + 1;
                  end if; -- Current_Grid (Z + 1) (Xi, 1)
               end loop; -- Xi in Coordinates
            end if; -- X = 3 and Y = 2
            if X = 3 and (Y = 4) then
               -- inside (Z + 1) bottom
               for Xi in Coordinates loop
                  if Current_Grid (Z + 1) (Xi, 5) then
                     Count := Count + 1;
                  end if; -- Current_Grid (Z + 1) (Xi, 5)
               end loop; -- Xi in Coordinates
            end if; -- X = 3 and (Y = 4)
            if X - 1 in Coordinates and then Current_Grid (Z) (X - 1, Y) then
               Count := Count + 1;
            end if; -- X - 1 in Coordinates and then ...
            if X + 1 in Coordinates and then Current_Grid (Z) (X + 1, Y) then
               Count := Count + 1;
            end if; -- X + 1 in Coordinates and then ...
            if Y - 1 in Coordinates and then Current_Grid (Z) (X, Y - 1) then
               Count := Count + 1;
            end if; -- Y - 1 in Coordinates and then ...
            if Y + 1 in Coordinates and then Current_Grid (Z) (X, Y + 1) then
               Count := Count + 1;
            end if; -- Y + 1 in Coordinates and then ...
            return Count;
         end Bug_Neighbours;

      begin -- Update_Grid
         for Z in Levels range Levels'First + 1 .. Levels'Last - 1 loop
            -- Levels'First and Levels'Last are read but not written.
            for Y in Coordinates loop
               for X in Coordinates loop
                  if X /= 3 or Y /= 3 then
                     if Current_Grid (Z) (X, Y) then
                        Next_Grid (Z) (X, Y) :=
                          Bug_Neighbours (Current_Grid, X, Y, Z) = 1;
                     else
                        Next_Grid (Z) (X, Y) :=
                          Bug_Neighbours (Current_Grid, X, Y, Z) = 1 or
                          Bug_Neighbours (Current_Grid, X, Y, Z) = 2;
                     end if; -- Current_Grid (Z) (X, Y)
                  end if; -- X /= 3 or Y /= 3
               end loop; -- X in Coordinates
            end loop; -- Y in Coordinates
         end loop; -- Z in Levels range Levels'First + 1 .. Levels'Last - 1
      end Update_Grid;

      function Count_Bugs (Current_Grid : in Recursive_Grids) return Natural is

         Count : Natural := 0;

      begin -- Count_Bugs
         for Z in Levels loop
            for Y in Coordinates loop
               for X in Coordinates loop
                  if Current_Grid (Z) (X, Y) then
                     Count := Count + 1;
                  end if; -- Current_Grid (Z) (X, Y)
               end loop; -- X in Coordinates
            end loop; -- Y in Coordinates
         end loop; -- Z in Levels
         return Count;
      end Count_Bugs;

      Current_Grid, Next_Grid : Recursive_Grids :=
        (others => (others => (others => False)));

   begin -- Solve_Part_Two
      Read_Grid (Current_Grid (0));
      for I in Positive range 1 .. 200 loop
         Update_Grid (Current_Grid, Next_Grid);
         Current_Grid := Next_Grid;
      end loop; -- I in Positive range 1 to 10
      Put_Line ("Part two bug count:" &
                  Natural'Image (Count_Bugs (Current_Grid)));
   end Solve_Part_Two;

begin -- December_18
   Solve_Part_One;
   Solve_Part_Two;
end December_24;
