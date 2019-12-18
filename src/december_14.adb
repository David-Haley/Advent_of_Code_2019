with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure December_14 is

   Ore : constant Unbounded_String := To_Unbounded_String ("ORE");
   Fuel : constant Unbounded_String := To_Unbounded_String ("FUEL");

   subtype Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   subtype Long_Positive is Long_Long_Integer range 1 .. Long_Long_Integer'Last;

   type Reagents is record
      Name : Unbounded_String;
      Quantity : Long_Positive;
   end record; -- Reagents

   package Input_Reagents is new Ada.Containers.Vectors (Positive, Reagents);
   use Input_Reagents;

   type Reaction_Elements is record
      Product : Reagents;
      Input_List :  Input_Reagents.Vector;
   end record; --  Reaction_Elements

   package Reactions is new Ada.Containers.Ordered_Maps (Unbounded_String,
                                                         Reaction_Elements);
   use Reactions;

   type Pool_Elements is record
      Required, Made : Long_Natural := 0;
      Modularity : Long_Positive := 1;
   end record; -- Pool_Elements

   package Reagent_Pools is new Ada.Containers.Ordered_Maps (Unbounded_String,
                                                             Pool_Elements);
   use Reagent_Pools;

   package Long_Positive_IO is new Ada.Text_IO.Integer_IO (Long_Positive);
   use Long_Positive_IO;

   procedure Read_Input (Reaction : out Reactions.map) is

      Input_File : File_Type;
      Current : Reagents;
      Reaction_Element : Reaction_Elements;
      End_of_Equation : Boolean;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Delimiter_Set : Character_Set := To_Set (" ,");

   begin -- Read_Input;
      Reaction := Reactions.Empty_Map;
      Open (Input_File, In_File, "December_14.txt");
      while not End_Of_File (Input_File) loop
         Reaction_Element.Input_List := Input_Reagents.Empty_Vector;
         End_of_Equation := False;
         Get_Line (Input_File, Text);
         Start_At := 1;
         while not End_of_Equation loop
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Current.Quantity := Long_Positive'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Find_Token (Text, Delimiter_Set, Start_At, Outside, First, Last);
            Current.Name := Unbounded_Slice (Text, First, Last);
            Append (Reaction_Element.Input_List, Current);
            Start_At := Last + 1;
            End_of_Equation := Element (Text, Start_At) /= ',';
            if End_of_Equation then
               Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                           Last);
               Reaction_Element.Product.Quantity :=
                 Long_Positive'Value (Slice (Text, First, Last));
               Start_At := Last + 1;
               Find_Token (Text, Delimiter_Set, Start_At, Outside, First, Last);
               Reaction_Element.Product.Name :=
                 Unbounded_Slice (Text, First, Last);
               Include (Reaction, Reaction_Element.Product.Name,
                        Reaction_Element);
            end if; -- End_of_Equation
         end loop; -- not End_of_Equation
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Create (Reaction : in Reactions.Map;
                     Reagent_Pool : out Reagent_Pools.Map) is

      Pool_Element : Pool_Elements;

   begin -- Create
      Reagent_Pool := Reagent_Pools.Empty_Map;
      Include (Reagent_Pool, Ore, Pool_Element);
      for I in iterate (Reaction) loop
         Pool_Element.Modularity := Reaction (I).Product.Quantity;
         Include (Reagent_Pool, Key (I), Pool_Element);
      end loop; -- I in iterate (Reaction)
   end Create;

   function Ceiling (Required : Long_Natural; Modularity : Long_Positive)
                       return Long_Natural is

      Result : Long_Natural := Required / Modularity;

   begin -- Ceiling
      if Required > Result * Modularity then
         Result := Result + 1;
      end if; -- Required > Result * Modularity
      return Result;
   end Ceiling;

   procedure Manufacture (Reaction : in Reactions.Map;
                          Reagent_Pool : in out Reagent_Pools.Map;
                          To_Make : in Unbounded_String;
                          Units : in Long_Positive) is

      Pool_C : Reagent_Pools.Cursor;
      Reaction_C : Reactions.Cursor;
      Product_Factor : Long_Positive;

   begin -- Manufacture
      Pool_C := Find (Reagent_Pool, To_Make);
      if To_Make /= Ore then
         Reaction_C := Find (Reaction, To_Make);
         if Units > Reagent_Pool (Pool_C).Made - Reagent_Pool (Pool_C).Required
         then
            -- more product to make
            Product_Factor :=
              Ceiling (Reagent_Pool (Pool_C).Required + Units -
                           Reagent_Pool (Pool_C).Made,
                       Reagent_Pool (Pool_C).Modularity);
            -- Indicated as made before search for inputs
            Reagent_Pool (Pool_C).Required :=
              Reagent_Pool (Pool_C).Required + Units;
            Reagent_Pool (Pool_C).Made := Reagent_Pool (Pool_C).Made +
              Reagent_Pool (Pool_C).Modularity * Product_Factor;
            -- Recursive call to manufacture the reagents required for "To_Make"
            For I in Iterate (Reaction (Reaction_C).Input_List) loop
               Manufacture (Reaction, Reagent_Pool, Element (I).Name,
                            Element (I).Quantity * Product_Factor);
            end loop; -- I in Iterate (Reaction (Reaction_C).Input_List)
         else
            -- no more product to make use existing surplace in reagent pool
            Reagent_Pool (Pool_C).Required :=
              Reagent_Pool (Pool_C).Required + Units;
         end if; --  Units > Reagent_Pool (Pool_C).Made ...
      else
         Reagent_Pool (Pool_C).Required :=
           Reagent_Pool (Pool_C).Required + Units;
      end if; -- To_Make /= Ore
   end Manufacture;

   Reaction : Reactions.Map;
   Reagent_Pool : Reagent_Pools.Map;
   Pool_C : Reagent_Pools.Cursor;
   Part_Two_Ore : constant Long_Positive := 1000000000000;
   Fuel_Estimate, Ore_Estimate, Required_Ore : Long_Positive;
   Part_Two_Fuel : Long_Positive := 1;

begin -- December_14
   Read_Input (Reaction);
   Create (Reaction, Reagent_Pool);
   Manufacture (Reaction, Reagent_Pool, Fuel, 1);
   -- Make one unit of fuel
   Pool_C := Find (Reagent_Pool, Ore);
   Put_Line ("Ore Required for one unit of fuel:" &
               Long_Natural'Image (Reagent_Pool (Pool_C).Required));
   Ore_Estimate := Reagent_Pool (Pool_C).Required;
   Fuel_Estimate := Part_Two_Ore / Ore_Estimate;
   -- First estimate, lots of wastage of intermediate reagents;
   Create (Reaction, Reagent_Pool);
   Manufacture (Reaction, Reagent_Pool, Fuel, Fuel_Estimate);
   Pool_C := Find (Reagent_Pool, Ore);
   Ore_Estimate := Reagent_Pool (Pool_C).Required;
   Fuel_Estimate := Part_Two_Ore * Fuel_Estimate / Ore_Estimate;
   -- Second estimate is allows a better estimate of Ore / Fuel for a large
   -- large quantity of fuel, that is a lower percentage wastage of intermediate
   -- reagents; The fuel estimate is guaranteed to below the actual value but
   -- close enough to allow a single step approach in reasonable time.
   -- For my input about 1400 iterations or a fraction of a second.
   Required_Ore := Ore_Estimate;
   while Part_Two_Ore > Required_Ore loop
      Create (Reaction, Reagent_Pool);
      Manufacture (Reaction, Reagent_Pool, Fuel, Fuel_Estimate);
      Pool_C := Find (Reagent_Pool, Ore);
      Required_Ore := Reagent_Pool (Pool_C).Required;
      if Part_Two_Ore > Required_Ore then
         Part_Two_Fuel := Fuel_Estimate;
      end if; -- Part_Two_Ore > Required_Ore
      Fuel_Estimate := Fuel_Estimate + 1;
   end loop; -- Part_Two_Ore > Required_Ore
   Put_Line ("Part two fuel made:" & Long_Natural'Image (Part_Two_Fuel));
end December_14;
