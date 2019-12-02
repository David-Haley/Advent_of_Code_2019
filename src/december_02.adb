with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_02 is

   Package Program_Stores is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Natural);
   use Program_Stores;

   package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);
   use Natural_IO;

   procedure Read_Input (Program_Store : in out Program_Stores.Vector) is

      Input_File : File_Type;
      Comma : Character;
      Operand : Natural;

   begin -- Read_Input
      Clear (Program_Store);
      Open (Input_File, In_File, "December_02.txt");
      while not End_Of_File (Input_File) loop
         Get (Input_File, Operand);
         Append (Program_Store, Operand);
         if not End_Of_File (Input_File) then
            Get (Input_File, Comma);
            Assert (Comma = ',', "Expected ',' found '" & Comma & "'");
         end if; -- not End_Of_File (Input_File)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Dump (Program_Store : in Program_Stores.Vector) is

   begin -- Dump
      for I in Iterate (Program_Store) loop
         Put (Program_Store (I), 0);
         Put (',');
      end loop; -- I in Iterate (Program_Store)
      New_Line;
   end Dump;

   procedure Run_Code (Program_Store : in out Program_Stores.Vector) is

      Program_Counter : Natural := 0;
      Operand_1, Operand_2, Result : Natural;

   Run : Boolean := True;

   begin -- Run_Code
      while Run loop
         Operand_1 := Program_Counter + 1;
         Operand_2 := Program_Counter + 2;
         Result := Program_Counter + 3;
         case Program_Store (Program_Counter) is
            when 1 =>
               Program_Store (Program_Store (Result)) :=
                                Program_Store (Program_Store (Operand_1)) +
                                  Program_Store (Program_Store (Operand_2));
            when 2 =>
               Program_Store (Program_Store (Result)) :=
                                Program_Store (Program_Store (Operand_1)) *
                                  Program_Store (Program_Store (Operand_2));
            when 99 =>
               Run := False;
            when others =>
               Assert (False, "Invalid Op Code (" &
                         Positive'Image (Program_Counter) & ") =>" &
                         Natural'Image (Program_Store (Program_Counter)));
         end case; -- Program_Store (Program_Counter)
         Program_Counter := Program_Counter + 4;
      end loop; -- Run
   end Run_Code;

   Program_Store : Program_Stores.Vector := Program_Stores.Empty_Vector;
   Noun_Address : constant Natural := 1;
   Verb_Address : constant Natural := 2;
   Part_Two_0_Value : constant Natural := 19690720;
   Part_Two_Answer : Natural;

begin -- December_02
   Read_Input (Program_Store);
   Dump (Program_Store);
   Program_Store (Noun_Address) := 12;
   Program_Store (Verb_Address) := 2;
   Run_Code (Program_Store);
   Dump (Program_Store);
   Put_Line ("Program_Store (0)" & Natural'Image (Program_Store (0)));
   Put_Line ("Part Two");
   For Noun in Natural range 0 .. 99 loop
      For Verb in Natural range 0 .. 99 loop
         Part_Two_Answer := 100 * Noun + Verb;
         Read_Input (Program_Store);
         Program_Store (Noun_Address) := Noun;
         Program_Store (Verb_Address) := Verb;
         Run_Code (Program_Store);
         exit when Program_Store (0) = Part_Two_0_Value;
      end loop; -- Verb in Natural range 0 .. 99
      exit when Program_Store (0) = Part_Two_0_Value;
   end loop; -- Noun in Natural range 0 .. 99
   Dump (Program_Store);
   Put_Line ("100 * Noun + Verb:" & Natural'Image (Part_Two_Answer));
end December_02;
