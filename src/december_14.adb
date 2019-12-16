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

   type Reagents is record
      Name : Unbounded_String;
      Quantity : Positive;
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
      Name : Unbounded_String := Null_Unbounded_String;
      Required, Made : Natural := 0;
      Modularity : Positive := 1;
   end record; -- Pool_Elements

   package Reagent_Pools is new Ada.Containers.Ordered_Maps (Unbounded_String,
                                                             Pool_Elements);
   use Reagent_Pools;

   package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);
   use Positive_IO;

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
            Current.Quantity := Positive'Value (Slice (Text, First, Last));
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
                 Positive'Value (Slice (Text, First, Last));
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
      Pool_Element.Name := Ore;
      Include (Reagent_Pool, Ore, Pool_Element);
      for I in iterate (Reaction) loop
         Pool_Element.Name := Key (I);
         Pool_Element.Modularity := Reaction (I).Product.Quantity;
         Include (Reagent_Pool, Key (I), Pool_Element);
      end loop; -- I in iterate (Reaction)
   end Create;

   function Ceiling (Required : Natural; Modularity : Positive)
                       return Natural is

      Result : Natural := Required / Modularity;

   begin -- Ceiling
      if Required > Result * Modularity then
         Result := Result + 1;
      end if; -- Required > Result * Modularity
      return Result;
   end Ceiling;

   procedure Manufacture (Reaction : in Reactions.Map;
                          Reagent_Pool : in out Reagent_Pools.Map;
                          To_Make : in Unbounded_String;
                          Units : in Positive) is

      Pool_C : Reagent_Pools.Cursor;
      Reaction_C : Reactions.Cursor;
      Product_Factor : Positive;

   begin -- Manufacture
      if To_Make /= Ore then
         Pool_C := Find (Reagent_Pool, To_Make);
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
            For I in Iterate (Reaction (Reaction_C).Input_List) loop
               Manufacture (Reaction, Reagent_Pool, Element (I).Name,
                            Element (I).Quantity * Product_Factor);
            end loop; -- I in Iterate (Reaction (Reaction_C).Input_List)
         else
            -- no more product to make use reagent pool
            Reagent_Pool (Pool_C).Required :=
              Reagent_Pool (Pool_C).Required + Units;
         end if; --  Units > Reagent_Pool (Pool_C).Made ...
      else
         Pool_C := Find (Reagent_Pool, To_Make);
         Reagent_Pool (Pool_C).Required :=
           Reagent_Pool (Pool_C).Required + Units;
      end if; -- To_Make /= Ore
   end Manufacture;

   Reaction : Reactions.Map;
   Reagent_Pool : Reagent_Pools.Map;
   Pool_C : Reagent_Pools.Cursor;
   Saved_Fuel : Positive;
   Least_Ore : Positive := Positive'Last;

begin -- December_14
   Read_Input (Reaction);
   Create (Reaction, Reagent_Pool);
   Manufacture (Reaction, Reagent_Pool, Fuel, 1);
   -- Make one unit of fuel
   Pool_C := Find (Reagent_Pool, Ore);
   Put_Line ("Ore Required:" & Natural'Image (Reagent_Pool (Pool_C).Required));
   for I in Positive range 1 .. 1000 loop
      Clear (Reagent_Pool);
      Create (Reaction, Reagent_Pool);
      Manufacture (Reaction, Reagent_Pool, Fuel, I);
      Pool_C := Find (Reagent_Pool, Ore);
      if Least_Ore > Reagent_Pool (Pool_C).Required / I then
         Least_Ore := Reagent_Pool (Pool_C).Required / I;
         Saved_Fuel := I;
      elsif Least_Ore = Reagent_Pool (Pool_C).Required / I then
         Put_Line ("Equal:" & Positive'Image (I));
      end if;
      Put_Line (Positive'Image (Least_Ore));
   end loop; -- I in Positive 1 .. 100
end December_14;
