with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_05 is

   subtype Program_Store_Elements is Integer;

   package Program_Store_IO is new
     Ada.Text_IO.Integer_IO (Program_Store_Elements);
   use Program_Store_IO;

   subtype Addresses is Natural;
   subtype Instruction_Lengths is Addresses range 2 .. 4;

   Package Program_Stores is new
     Ada.Containers.Vectors (Index_Type => Addresses,
                             Element_Type => Program_Store_Elements);
   use Program_Stores;

   subtype Address_Modes is Natural range 0 .. 1;

   procedure Read_Input (Program_Store : in out Program_Stores.Vector) is

      Input_File : File_Type;
      Comma : Character;
      Operand : Program_Store_Elements;

   begin -- Read_Input
      Clear (Program_Store);
      Open (Input_File, In_File, "December_05.txt");
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

      Op_Code_Mod : constant Program_Store_Elements := 100;
      Mode_Mod : constant Program_Store_Elements := 10;

      function Read_Operand (Address : in Addresses;
                             Address_Mode : in Address_Modes)
                             return Program_Store_Elements is
      begin -- Read_Operand
         if Address_Mode = 0 then
            return Program_Store (Program_Store (Address));
            -- mode 0 direct addressing
         else
            return Program_Store (Address);
            -- mode 1 immediste
         end if; -- Address_Mode = 0
      end Read_Operand;

      Program_Counter : Addresses := 0;
      Jump_Target : Addresses;
      Instruction_Length : Instruction_Lengths;
      Operand_1, Operand_2, Result : Addresses;
      Operand_1_Mode, Operand_2_Mode : Address_Modes;
      Input_Data : Program_Store_Elements;

      Run : Boolean := True;
      Jump : Boolean;

   begin -- Run_Code
      Put_Line ("Running");
      while Run loop
         Jump := False;
         Operand_1 := Program_Counter + 1;
         Operand_1_Mode :=
           (Program_Store (Program_Counter) / Op_Code_Mod) mod Mode_Mod;
         Operand_2 := Program_Counter + 2;
         Operand_2_Mode :=
           (Program_Store (Program_Counter) / Op_Code_Mod / Mode_Mod)
         mod Mode_Mod;
         Result := Program_Counter + 3;
         Instruction_Length := 4; -- default instruction length
         case Program_Store (Program_Counter) mod Op_Code_Mod is
            when 1 =>
               Program_Store (Program_Store (Result)) :=
                 Read_Operand (Operand_1, Operand_1_Mode) +
                 Read_Operand (Operand_2, Operand_2_Mode);
            when 2 =>
               Program_Store (Program_Store (Result)) :=
                 Read_Operand (Operand_1, Operand_1_Mode) *
                 Read_Operand (Operand_2, Operand_2_Mode);
            when 3 =>
               Instruction_Length := 2;
               Result := Program_Counter + 1;
               Put ("Input Required:");
               Get (Input_Data);
               Program_Store (Program_Store (Result)) := Input_Data;
            when 4 =>
               Instruction_Length := 2;
               Put ("Output: ");
               Put (Read_Operand (Operand_1, Operand_1_Mode), 0);
               New_Line;
            when 5 =>
               if Read_Operand (Operand_1, Operand_1_Mode) /= 0 then
                  Jump := True;
                  Jump_Target := Read_Operand (Operand_2, Operand_2_Mode);
               else
                  Instruction_Length := 3;
               end if; -- Read_Operand (Operand_1, Operand_1_Mode) /= 0
            when 6 =>
               if Read_Operand (Operand_1, Operand_1_Mode) = 0 then
                  Jump := True;
                  Jump_Target := Read_Operand (Operand_2, Operand_2_Mode);
               else
                  Instruction_Length := 3;
               end if; -- Read_Operand (Operand_1, Operand_1_Mode) = 0
            when 7 =>
               if Read_Operand (Operand_1, Operand_1_Mode) <
                 Read_Operand (Operand_2, Operand_2_Mode) then
                  Program_Store (Program_Store (Result)) := 1;
               else
                  Program_Store (Program_Store (Result)) := 0;
               end if; -- less than test
            when 8 =>
               if Read_Operand (Operand_1, Operand_1_Mode) =
                 Read_Operand (Operand_2, Operand_2_Mode) then
                  Program_Store (Program_Store (Result)) := 1;
               else
                  Program_Store (Program_Store (Result)) := 0;
               end if; -- equal test
            when 99 =>
               Run := False;
               Put_Line ("Halted");
            when others =>
               Assert (False, "Invalid Op Code (" &
                         Positive'Image (Program_Counter) & ") =>" &
                         Natural'Image (Program_Store (Program_Counter)));
         end case; -- Program_Store (Program_Counter)
         if Jump then
            Program_Counter := Jump_Target;
         else
            Program_Counter := Program_Counter + Instruction_Length;
         end if; -- Jump
      end loop; -- Run
   end Run_Code;

   Program_Store : Program_Stores.Vector := Program_Stores.Empty_Vector;

begin -- December_05
   Read_Input (Program_Store);
   Dump (Program_Store);
   Run_Code (Program_Store);
end December_05;
