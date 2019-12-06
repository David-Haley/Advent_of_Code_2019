with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

procedure December_06 is

   String_Length : constant Positive := 3;
   subtype Orbital_Objects is String (1 .. String_Length);
   type Satellite_Pairs is record
      Orbits_Around, Satellite : Orbital_Objects;
   end record; -- Satellite_Pairs

   function "<" (Left, Right : Satellite_Pairs) return Boolean is

   begin -- "<"
      return Left.Satellite < Right.Satellite;
   end "<";

   function "=" (Left, Right : Satellite_Pairs) return Boolean is

   begin -- "="
      return Left.Satellite = Right.Satellite;
   end "=";

   package Orbit_Maps is new
     Ada.Containers.Ordered_Maps (Key_Type => Orbital_Objects,
                                  Element_Type => Satellite_Pairs);
   use Orbit_Maps;

   package Transfer_Lists is new Ada.Containers.Vectors (Natural,
                                                         Orbital_Objects);
   use Transfer_Lists;

   procedure Read_Input (Orbit_Map : out Orbit_Maps.Map) is

      Input_File : File_Type;
      Right_Bracket : Character;
      Current_Pair : Satellite_Pairs;

   begin -- Read_Input
      Put_Line ("Reading Orbits ");
      Open (Input_File, In_File, "December_06.txt");
      Clear (Orbit_Map);
      while not End_Of_File (Input_File) loop
         Get (Input_File, Current_Pair.Orbits_Around);
         Get (Input_File, Right_Bracket);
         Assert (Right_Bracket = ')', "Expected ) and found '" &
                   Right_Bracket & "'");
         Get (Input_File, Current_Pair.Satellite);
         Insert (Orbit_Map, Current_Pair.Satellite, Current_Pair);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Find_Centre (Orbit_Map : in Orbit_Maps.Map)
                         return Orbital_Objects is

      Found : Natural := 0;
      Result : Orbital_Objects;

   begin -- Find_Centre
      for I in Iterate (Orbit_Map) loop
         if not Contains (Orbit_Map, Orbit_Map (I).Orbits_Around) then
            Result := Orbit_Map (I).Orbits_Around;
            Found := Found + 1;
         end if; -- not Contains (Orbit_Map, Orbit_Map (I).Orbits_Around)
      end loop; -- I in Iterate (Orbit_Map)
      Assert (Found = 1, "No Unique Centre, Found:" & Natural'Image (Found));
      return Result;
   end Find_Centre;

   function Count_Orbits (Orbit_Map : in Orbit_Maps.Map;
                         Centre : in Orbital_Objects)
                          return Natural is

      Result : Natural := 0;
      Orbits_This : Orbital_Objects;

   begin -- Count_Orbits
      for I in Iterate (Orbit_Map) loop
         Orbits_This := Orbit_Map (I).Orbits_Around;
         loop -- trace to Centre
            Result := Result + 1;
            exit when Orbits_This = Centre;
            Orbits_This := Element (Orbit_Map, Orbits_This).Orbits_Around;
         end loop; -- trace to Centre
      end loop; -- I in Iterate (Orbit_Map)
      return Result;
   end Count_Orbits;

   procedure Build_Transfer_List (Orbit_Map : in Orbit_Maps.Map;
                                  Start_At, Centre : in Orbital_Objects;
                                  Transfer_List : out Transfer_Lists.Vector) is

      Orbits_This : Orbital_Objects;

   begin -- Build_Transfer_List
      Clear (Transfer_List);
      Orbits_This := Element (Orbit_Map, Start_At).Orbits_Around;
      loop -- trace to Centre
         Append (Transfer_List, Orbits_This);
         exit when Orbits_This = Centre;
         Orbits_This := Element (Orbit_Map, Orbits_This).Orbits_Around;
      end loop; -- trace to Centre
   end Build_Transfer_List;

   function Transfer_Count (Start_List, End_List : in Transfer_Lists.Vector)
                            return Natural is

   begin -- Transfer_Count
      for I in Iterate (Start_List) loop
         for J in Iterate (End_List) loop
            if Element (I) = Element (J) then
               -- Common Element found
               return To_Index (I) + To_Index (J);
               -- Index represents transfers start of list
            end if; -- Element (I) = Element (J)
         end loop; -- J in Iterate (End_List)
      end loop; -- I in Iterate (Start_List)
      return Natural'Last;
      -- will only be reached if there is no common element
   end Transfer_Count;

   Orbit_Map : Orbit_Maps.Map;
   Universe_Centre : Orbital_Objects;
   You_List, Santa_List : Transfer_Lists.Vector;

begin -- December_06
   Read_Input (Orbit_Map);
   Universe_Centre := Find_Centre (Orbit_Map);
   Put_Line ("Centre of Universe: " & Universe_Centre);
   -- We were given the cemtre; however this verifies that the data was read
   -- correctly.
   Put_Line ("Total Orbits:" &
               Natural'Image (Count_Orbits (Orbit_Map, Universe_Centre)));
   Build_Transfer_List (Orbit_Map, "YOU", Universe_Centre, You_List);
   Build_Transfer_List (Orbit_Map, "SAN", Universe_Centre, Santa_List);
   Put_Line ("Transfer Count:" &
               Natural'Image (Transfer_Count (You_List, Santa_List)));
end December_06;
