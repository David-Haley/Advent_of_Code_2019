with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_16 is

   type Code_Digits is mod 10;
   package Code_Stores is new Ada.Containers.Vectors (Positive, Code_Digits);
   use Code_Stores;

   Phases : constant Positive := 100;
   Result_Length : constant Positive := 8;

   package Code_Digit_IO is new Ada.Text_IO.Modular_IO (Code_Digits);
   use Code_Digit_IO;

   procedure Read_Input (Code_Store : out Code_Stores.Vector;
                         Repeats : in Positive := 1) is

      Input_File : File_Type;
      Digit_In : Code_Digits;

   begin -- Read_Input
      Code_Store := Code_Stores.Empty_Vector;
--        Open (Input_File, In_File, "Example_16e.txt");
      Open (Input_File, In_File, "December_16.txt");
      for Copies in Positive range 1 .. Repeats loop
         Reset (Input_File);
         while not End_Of_File (Input_File) loop
            Get (Input_File, Digit_In, 1);
            Append (Code_Store, Digit_In);
         end loop; -- not End_Of_File (Input_File)
      end loop; -- Copies in Positive range 1 .. Repeats
      Close (Input_File);
   end Read_Input;

   procedure Put (Code_Store : Code_Stores.Vector) is

   begin -- Put
      for I in Positive range 1 .. Result_Length loop
         Put (Code_Store (I), 1);
      end loop; -- I in Positive range 1 .. Result_Length
      New_Line;
   end Put;

   function Offset (Code_Store : in Code_Stores.Vector) return Count_Type is

      Offset_Length : constant Positive := 7;
      Result : Count_Type := 0;

   begin -- Offset
      for I in Positive range 1 .. Offset_Length loop
         Result := Result * 10 + Count_Type (Element (Code_Store, I));
      end loop; -- I in Positive range 1 .. Offset_Length
      return Result;
   end Offset;

   procedure Part_One (Code : in out Code_Stores.Vector) is

      type Multipliers is range -1 .. 1;

      package Multiplier_Vectors is new Ada.Containers.Vectors (Positive,
                                                                Multipliers);
      use Multiplier_Vectors;

      function "*" (Left : Code_Digits; Right : Multipliers) return Integer is

      begin --"*"
         case Right is
         when -1 =>
            return -Integer (Left);
            -- N.B. the position of the negative sign is critical! The negation
            -- has to be applied to Left after it is converted to type integer.
            -- Thus Integer (-Left) applies the mod operator to -Left before
            -- the type conversion.
         when 0 =>
            return 0;
         when 1 =>
            return Integer (Left);
         end case; -- Right
      end "*";

      procedure Build_Mult (Length : in Positive; Output_Digit : in Positive;
                            Multiplier_Vector : out Multiplier_Vectors.Vector)
      is

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

      function Product (Code : in Code_Stores.Vector;
                        Multiplier_Vector : in Multiplier_Vectors.Vector)
                        return Code_Digits is

         Sum : Integer := 0;

      begin -- Product
         for I in Positive range 1 .. Positive (Length (Code)) loop
            Sum := Sum + Code (I) * Multiplier_Vector (I);
         end loop; -- I in Positive range 1 .. Positive (Length (Code))
         return Code_Digits'Mod (abs (Sum));
      end Product;

      Current_MV : Multiplier_Vectors.Vector;
      Code_Length : Constant Positive := Last_Index (Code);
      Temp : Code_Stores.Vector := Copy (Code);

   begin -- Part_One
      for P in Positive range 1 .. Phases loop
         Clear (Code);
         for Digit in Positive range 1 .. Code_Length loop
            Build_Mult (Code_Length, Digit, Current_MV);
            Append (Code, Product (Temp, Current_Mv));
         end loop; -- Digit in Positive range 1 .. Code_Length
         Temp := Copy (Code);
      end loop; -- P in Positive range 1 .. Phases
   end Part_One;

   procedure Part_Two (Code : in out Code_Stores.Vector) is
      -- This solution is not entirely generic! It will only work wnere the
      -- offset only devides the length once. This means that the only
      -- multipliers are 0 and 1, that is, no second 0 or -1.
      -- 1,1,1,1,1,1 ...
      -- 0,1,1,1,1,1 ...
      -- 0,0,1,1,1,1 ...
      -- 0,0,0,1,1,1 ...
      -- 0,0,0,0,1,1 ...
      -- 0,0,0,0,0,1 ...

      Sum : Code_Digits;

   begin  -- Part_Two
      for I in Positive range 1 .. Phases loop
         Sum := 0;
         for Digit in reverse Positive range 1 .. Last_Index (Code) loop
            -- Note reverse iteration is used so that each digit can be
            -- calculated and then stored after the previous value has been
            -- summed. This avoids the need to copy a large data
            -- structure for the start of the next phase.
            Sum := Sum + Code (Digit);
            Code (Digit) := Sum;
         end loop; -- Digit in reverse Positive range 1 .. Last_Index (Code)
      end loop; -- I in Positive range 1 .. Phases
   end Part_Two;

   Code_Store : Code_Stores.Vector;

begin -- December_16
   Read_Input (Code_Store);
   Part_One (Code_Store);
   Put ("Part One: ");
   Put (Code_Store);
   Read_Input (Code_Store, 10000);
   Delete_First (Code_Store, Offset (Code_Store));
   -- Delete the offest number of digits, to be skipped. These play no part in
   -- calculating the part two result.
   Part_Two (Code_Store);
   Put ("Part Two: ");
   Put (Code_Store);
end December_16;
