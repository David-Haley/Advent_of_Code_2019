with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_07 is

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

   subtype Address_Modes is Integer range 0 .. 1;

   subtype Phases_1 is Program_Store_Elements range 0 .. 4;
   subtype Phases_2 is Program_Store_Elements range 5 .. 9;

   procedure Read_Input (Program_Store : in out Program_Stores.Vector) is

      Input_File : File_Type;
      Comma : Character;
      Operand : Program_Store_Elements;

   begin -- Read_Input
      Clear (Program_Store);
      Open (Input_File, In_File, "December_07.txt");
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

   procedure Run_Code_1 (Program_Store_Archive : in Program_Stores.Vector;
                         Phase : in Phases_1;
                         Amp_Input : in Program_Store_Elements;
                         Amp_Output: out Program_Store_Elements) is

      Op_Code_Mod : constant Program_Store_Elements := 100;
      Mode_Mod : constant Program_Store_Elements := 10;

      Program_Store : Program_Stores.Vector := Copy (Program_Store_Archive);

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
      Input_Counter : Natural := 0;

   begin -- Run_Code_1
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
               case Input_Counter is
                  when 0 =>
                     Input_Data := Phase;
                  when 1 =>
                     Input_Data := Amp_Input;
                  when others =>
                     Assert (False, "Too many input instructions processed");
               end case; -- Input_Counter
               Program_Store (Program_Store (Result)) :=
                 Input_Data;
               Input_Counter := Input_Counter + 1;
            when 4 =>
               Instruction_Length := 2;
               Amp_Output := Read_Operand (Operand_1, Operand_1_Mode);
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
               Assert (Input_Counter = 2, "Failed to read input twice");
            when others =>
               Assert (False, "Invalid Op Code (" &
                         Positive'Image (Program_Counter) & ") =>" &
                         Program_Store_Elements'Image
                         (Program_Store (Program_Counter)));
         end case; -- Program_Store (Program_Counter)
         if Jump then
            Program_Counter := Jump_Target;
         else
            Program_Counter := Program_Counter + Instruction_Length;
         end if; -- Jump
      end loop; -- Run
   end Run_Code_1;

   procedure Find_Solution_1 (Program_Store_Archive : in Program_Stores.Vector;
                              Thrust : out Program_Store_Elements) is

      A_to_B, B_to_C, C_to_D, D_to_E, E_Out : Program_Store_Elements;

   begin -- Find_Solution_1
      Thrust := Program_Store_Elements'First;
      for A in Phases_1 loop
         Run_Code_1 (Program_Store_Archive, A, 0, A_to_B);
         for B in Phases_1 loop
            if A /= B then
               Run_Code_1 (Program_Store_Archive, B, A_to_B, B_to_C);
               for C in Phases_1 loop
                  if A /= C and B /= C then
                     Run_Code_1 (Program_Store_Archive, C, B_to_C, C_to_D);
                     for D in Phases_1 loop
                        if A /= D and B /= D and C /= D then
                           Run_Code_1 (Program_Store_Archive, D, C_to_D,
                                       D_to_E);
                           for E in Phases_1 loop
                              if A /= E and B /= E and C /= E and D /= E then
                                 Run_Code_1 (Program_Store_Archive, E, D_to_E,
                                             E_Out);
                                 if E_Out > Thrust then
                                    Thrust := E_Out;
                                 end if; -- E_Out > Thrust
                              end if; -- A /= E and B /= E and C /= E and D /= E
                           end loop; -- E in Phases_1
                        end if; -- A /= D and B /= D and C /= D
                     end loop; -- D in Phases_1
                  end if; -- A /= C and B /= C
               end loop; -- C in Phases_1
            end if; -- A /= B
         end loop; -- B in Phases_1
      end loop; -- A in Phases_1
   end Find_Solution_1;

   procedure Run_Code_2 (Program_Store_Archive : in Program_Stores.Vector;
                         Phase_A, Phase_B, Phase_C, Phase_D, Phase_E
                         : in Phases_2;
                         Thrust : out Program_Store_Elements) is

      -- Runs seperate tasks for each amplifier. The output of each amplier is
      -- transferred to the next by calling the appropriate entry, thus ensuring
      -- that the transfer is synchronous with the virtual machine execution.
      -- Amplifiers A to D are similar; however amplifier E has asignificant
      -- differences. Task execution is initiated by the initial value being
      -- sent to Amp_E by calling Run_Code_2 calling its entry. Execution is
      -- sustained by Amp_E sending its output (feedback) to Amp_A. Amp_E
      -- transfers its final output to Run_Code_2. Note Run_Code_2 will not
      -- return until all amplifiers terminate. This solution produces the
      -- correct resuts; however it is not particularly elegant, the tasks could
      -- be generic instantiations and it may be possible to avoid the exception
      -- handeler in Amp_E which does nothing other than allowing Amp_E to
      -- terminate normally whewn it attemps to send output to Amp_A after Amp_A
      -- has run to completion.

      Op_Code_Mod : constant Program_Store_Elements := 100;
      Mode_Mod : constant Program_Store_Elements := 10;

      task Amp_A is
         entry Get_Input (Amp_Input : in Program_Store_Elements);
      end Amp_A;

      task Amp_B is
         entry Get_Input (Amp_Input : in Program_Store_Elements);
      end Amp_B;

      task Amp_C is
         entry Get_Input (Amp_Input : in Program_Store_Elements);
      end Amp_C;

      task Amp_D is
         entry Get_Input (Amp_Input : in Program_Store_Elements);
      end Amp_D;

      task Amp_E is
         entry Get_Input (Amp_Input : in Program_Store_Elements);
         entry Final_Result (Thrust : out Program_Store_Elements);
      end Amp_E;

      task body Amp_A is

         Program_Store : Program_Stores.Vector := Copy (Program_Store_Archive);

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
         Run, Phase_Input : Boolean := True;
         Jump : Boolean;

      begin -- Amp_A
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
               if Phase_Input then
                  Input_Data := Phase_A;
                  Phase_Input := False;
               else
                  accept Get_Input (Amp_Input : in Program_Store_Elements) do
                     Input_Data := Amp_Input;
                  end Get_Input;
               end if; -- Phase_Input
               Program_Store (Program_Store (Result)) := Input_Data;
            when 4 =>
               Instruction_Length := 2;
               Amp_B.Get_Input (Read_Operand (Operand_1, Operand_1_Mode));
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
            when others =>
               Assert (False, "Invalid Op Code (" &
                         Positive'Image (Program_Counter) & ") =>" &
                         Program_Store_Elements'Image
                         (Program_Store (Program_Counter)));
            end case; -- Program_Store (Program_Counter)
            if Jump then
               Program_Counter := Jump_Target;
            else
               Program_Counter := Program_Counter + Instruction_Length;
            end if; -- Jump
         end loop; -- Run
      end Amp_A;

      task body Amp_B is

         Program_Store : Program_Stores.Vector := Copy (Program_Store_Archive);

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
         Run, Phase_input : Boolean := True;
         Jump : Boolean;

      begin -- Amp_B
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
               if Phase_input then
                  Input_Data := Phase_B;
                  Phase_input := False;
               else
                  accept Get_Input (Amp_Input : in Program_Store_Elements) do
                     Input_Data := Amp_Input;
                  end Get_Input;
               end if; -- Phase_input
               Program_Store (Program_Store (Result)) := Input_Data;
            when 4 =>
               Instruction_Length := 2;
               Amp_C.Get_Input (Read_Operand (Operand_1, Operand_1_Mode));
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
            when others =>
               Assert (False, "Invalid Op Code (" &
                         Positive'Image (Program_Counter) & ") =>" &
                         Program_Store_Elements'Image
                         (Program_Store (Program_Counter)));
            end case; -- Program_Store (Program_Counter)
            if Jump then
               Program_Counter := Jump_Target;
            else
               Program_Counter := Program_Counter + Instruction_Length;
            end if; -- Jump
         end loop; -- Run
      end Amp_B;

      task body Amp_C is

         Program_Store : Program_Stores.Vector := Copy (Program_Store_Archive);

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
         Run, Phase_Input : Boolean := True;
         Jump : Boolean;

      begin -- Amp_C
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
               if Phase_Input then
                  Input_Data := Phase_C;
                  Phase_Input := False;
               else
                  accept Get_Input (Amp_Input : in Program_Store_Elements) do
                     Input_Data := Amp_Input;
                  end Get_Input;
               end if; -- Phase_Input
               Program_Store (Program_Store (Result)) := Input_Data;
            when 4 =>
               Instruction_Length := 2;
               Amp_D.Get_Input (Read_Operand (Operand_1, Operand_1_Mode));
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
            when others =>
               Assert (False, "Invalid Op Code (" &
                         Positive'Image (Program_Counter) & ") =>" &
                         Program_Store_Elements'Image
                         (Program_Store (Program_Counter)));
            end case; -- Program_Store (Program_Counter)
            if Jump then
               Program_Counter := Jump_Target;
            else
               Program_Counter := Program_Counter + Instruction_Length;
            end if; -- Jump
         end loop; -- Run
      end Amp_C;

      task body Amp_D is

         Program_Store : Program_Stores.Vector := Copy (Program_Store_Archive);

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
         Run, Phase_Input : Boolean := True;
         Jump : Boolean;

      begin -- Amp_D
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
               if Phase_Input then
                  Input_Data := Phase_D;
                  Phase_Input := False;
               else
                  accept Get_Input (Amp_Input : in Program_Store_Elements) do
                     Input_Data := Amp_Input;
                  end Get_Input;
               end if; -- Phase_Input
               Program_Store (Program_Store (Result)) := Input_Data;
            when 4 =>
               Instruction_Length := 2;
               Amp_E.Get_Input (Read_Operand (Operand_1, Operand_1_Mode));
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
                  Jump_Target :=Read_Operand (Operand_2, Operand_2_Mode);
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
            when others =>
               Assert (False, "Invalid Op Code (" &
                         Positive'Image (Program_Counter) & ") =>" &
                         Program_Store_Elements'Image
                         (Program_Store (Program_Counter)));
            end case; -- Program_Store (Program_Counter)
            if Jump then
               Program_Counter := Jump_Target;
            else
               Program_Counter := Program_Counter + Instruction_Length;
            end if; -- Jump
         end loop; -- Run
      end Amp_D;

      task body Amp_E is

         Program_Store : Program_Stores.Vector := Copy (Program_Store_Archive);

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
         Run, Phase_Input, A_Running : Boolean := True;
         Jump : Boolean;
         E_Out : Program_Store_Elements;

      begin -- Amp_E
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
               if Phase_Input then
                  Input_Data := Phase_E;
                  Phase_Input := False;
               else
                  accept Get_Input (Amp_Input : in Program_Store_Elements) do
                     Input_Data := Amp_Input;
                  end Get_Input;
               end if; -- Phase_Input
               Program_Store (Program_Store (Result)) := Input_Data;
            when 4 =>
               Instruction_Length := 2;
               begin -- Handles exception when Amp_A has run to completion
                  Amp_A.Get_Input (Read_Operand (Operand_1, Operand_1_Mode));
               exception
                  when Tasking_Error =>
                     null; -- Exception is ignored!!!!
               end; -- Handles exception when Amp_A has run to completion
               E_Out := Read_Operand (Operand_1, Operand_1_Mode);
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
            when others =>
               Assert (False, "Invalid Op Code (" &
                         Positive'Image (Program_Counter) & ") =>" &
                         Program_Store_Elements'Image
                         (Program_Store (Program_Counter)));
            end case; -- Program_Store (Program_Counter)
            if Jump then
               Program_Counter := Jump_Target;
            else
               Program_Counter := Program_Counter + Instruction_Length;
            end if; -- Jump
         end loop; -- Ru
         accept Final_Result (Thrust : out Program_Store_Elements) do
            Thrust := E_Out;
         end Final_Result;
      end Amp_E;

   begin -- Run_Code_2
      Amp_A.Get_Input (0); -- Initial input to Amp_A
      Amp_E.Final_Result (Thrust);
   end Run_Code_2;

   procedure Find_Solution_2 (Program_Store_Archive : in Program_Stores.Vector;
                              Max_Thrust : out Program_Store_Elements) is

      Thrust : Program_Store_Elements;

   begin -- Find_Solution_2
      Max_Thrust := Program_Store_Elements'First;
      for A in Phases_2 loop
         for B in Phases_2 loop
            if A /= B then
               for C in Phases_2 loop
                  if A /= C and B /= C then
                     for D in Phases_2 loop
                        if A /= D and B /= D and C /= D then
                           for E in Phases_2 loop
                              if A /= E and B /= E and C /= E and D /= E then
                                 Run_Code_2 (Program_Store_Archive,
                                             A, B, C, D, E, Thrust);
                                 if Thrust > Max_Thrust then
                                    Max_Thrust := Thrust;
                                 end if; -- E_Out > Thrust
                              end if; -- A /= E and B /= E and C /= E and D /= E
                           end loop; -- E in Phases_2
                        end if; -- A /= D and B /= D and C /= D
                     end loop; -- D in Phases_2
                  end if; -- A /= C and B /= C
               end loop; -- C in Phases_2
            end if; -- A /= B
         end loop; -- B in Phases_2
      end loop; -- A in Phases_2
   end Find_Solution_2;

   Program_Store, Program_Store_Archive : Program_Stores.Vector
     := Program_Stores.Empty_Vector;
   Thrust : Program_Store_Elements;

begin -- December_07
   Read_Input (Program_Store_Archive);
   Find_Solution_1 (Program_Store_Archive, Thrust);
   Put ("Thrust: ");
   Put (Thrust, 0);
   New_Line;
   Find_Solution_2 (Program_Store_Archive, Thrust);
   Put ("Thrust with feedback: ");
   Put (Thrust, 0);
   New_Line;
end December_07;
