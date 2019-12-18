with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_16 is

   type Code_Digits is range 0 .. 9;
   type Multipliers is range -1 .. 1;

   function "*" (Left : Code_Digits; Right : Multipliers) return Integer is

   begin --"*"
      return Integer (Left) * Integer (Right);
   end "*";

   package Code_Stores is new Ada.Containers.Vectors (Positive, Code_Digits);
   use Code_Stores;

   package Multiplier_Vectors is new Ada.Containers.Vectors (Positive,
                                                             Multipliers);
   use Multiplier_Vectors;

   procedure Read_Input (Code_Store : out Code_Stores.Vector;
                         Repeats : in Positive := 1) is

      Input_File : File_Type;
      String_1 : String (1 .. 1);

   begin -- Read_Input
      Code_Store := Code_Stores.Empty_Vector;
      Open (Input_File, In_File, "December_16.txt");
      --        Open (Input_File, In_File, "Example_16b.txt");
      for Copies in Positive range 1 .. Repeats loop
         Reset (Input_File);
         while not End_Of_File (Input_File) loop
            Get (Input_File, String_1);
            Append (Code_Store, Code_Digits'Value (String_1));
         end loop; -- not End_Of_File (Input_File)
      end loop; -- Copies in Positive range 1 .. Repeats
      Close (Input_File);
   end Read_Input;

   procedure Put (Multiplier_Vector : out Multiplier_Vectors.Vector) is

   begin -- Put
      for I in Iterate (Multiplier_Vector) loop
         Put (Multipliers'Image (Multiplier_Vector (I)));
      end loop; -- I in Iterate (Multiplier_Vector)
      New_Line;
   end Put;

   procedure Put (Code_Store : Code_Stores.Vector;
                  Offset : Natural := 0;
                  Eight_Digits : in Boolean := True) is

      Start : Positive := 1 + Offset;
      Last : Positive;

   begin -- Put
      if Eight_Digits then
         Last := Start + 7;
         for I in Positive range Start .. Last loop
            Put (Code_Digits'Image (Code_Store (I)));
         end loop; -- I in Positive range Start .. Last
      else
         for I in Iterate (Code_Store) loop
            Put (Code_Digits'Image (Code_Store (I)));
         end loop; -- I in Iterate (Code_Store)
      end if; -- I in Iterate (Code_Store)
      New_Line;
   end Put;

   function Offset (Code_Store : in Code_Stores.Vector) return Natural is

      Offset_Length : constant Positive := 7;
      Result : Natural := 0;

   begin -- Offset
      for I in Positive range 1 .. Offset_Length loop
         Result := Result * 10;
         Result := Result + Natural (Element (Code_Store, I));
      end loop; -- I in Positive range 1 .. Offset_Length
      return Result;
   end Offset;

   procedure Build_Mult (Multiplier_Vector : out Multiplier_Vectors.Vector;
                          Length : in Positive; Output_Digit : in Positive) is

   begin -- Build_Mult
      Multiplier_Vector := Multiplier_Vectors.Empty_Vector;
      for I in Natural range 0 .. Length loop
         case (I / Output_Digit) mod 4 is
            when 0 =>
               Append (Multiplier_Vector, 0);
            when 1 =>
               Append (Multiplier_Vector, 1);
            when 2 =>
               Append (Multiplier_Vector, 0);
            when 3 =>
               Append (Multiplier_Vector, -1);
            when others =>
               Assert (False, "Broken Compiler");
         end case; -- (I / Output_Digit) mod 4
      end loop; -- for I in Natural range 0 .. Length
      Delete_First (Multiplier_Vector);
   end Build_Mult;

   procedure Phase (Code : in out Code_Stores.Vector) is

      function Product (Code : in Code_Stores.Vector;
                        Multiplier_Vector : in Multiplier_Vectors.Vector)
                        return Code_Digits is

         Sum : Integer := 0;

      begin -- Product
         for I in Positive range 1 .. Positive (Length (Code)) loop
            Sum := Sum + Code (I) * Multiplier_Vector (I);
         end loop; -- I in Positive range 1 .. Positive (Length (Code))
         return Code_Digits (abs (Sum) mod Integer (Code_Digits'Last + 1));
      end Product;

      Current_MV : Multiplier_Vectors.Vector;
      Code_Length : Constant Positive := Positive (Length (Code));
      Temp : Code_Stores.Vector := Copy (Code);

   begin -- Phase
      Clear (Code);
      for I in Positive range 1 .. Code_Length loop
         Build_Mult (Current_MV, Code_Length, I);
         Append (Code, Product (Temp, Current_Mv));
      end loop; -- I in Positive range 2 .. Code_Length
   end Phase;

   Code_Store : Code_Stores.Vector;
   Current, Next : Multiplier_Vectors.Vector;
   Saved_Offset : Natural;

begin -- December_16
   Read_Input (Code_Store);
   for I in Positive range 1 .. 100 loop
      Phase (Code_Store);
   end loop; -- I in Positive range 1 .. 4;
   Put ("Part One");
   Put (Code_Store);
   Clear (Code_Store);
   Read_Input (Code_Store, 10000);
   Saved_Offset := Offset (Code_Store);
   for I in Positive range 1 .. 100 loop
      Put (Positive'Image (I));
      if I mod 20 = 0 then
         New_Line;
      end if; -- I mod 20 = 0
      Phase (Code_Store);
   end loop; -- I in Positive range 1 .. 100
   Put ("Part Two");
   Put (Code_Store, Saved_Offset);
end December_16;
