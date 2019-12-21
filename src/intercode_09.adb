with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

package body Intercode_09 is

   package Program_Store_IO is new
     Ada.Text_IO.Integer_IO (Program_Store_Elements);
   use Program_Store_IO;

   subtype Instruction_Lengths is Addresses range 2 .. 4;

   Package Program_Stores is new
     Ada.Containers.Vectors (Index_Type => Addresses,
                             Element_Type => Program_Store_Elements);
   use Program_Stores;

   procedure Read_Input (Code_File_Name : in String;
                         Program_Store : out Program_Stores.Vector) is

      Input_File : File_Type;
      Comma : Character;
      Operand : Program_Store_Elements;

   begin -- Read_Input
      Program_Store := Program_Stores.Empty_Vector;
      Open (Input_File, In_File, Code_File_Name);
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

   procedure Dump (Program_Store : in Program_Stores.Vector;
                   Trace_File : in File_Type) is

      package Address_IO is new Ada.Text_IO.Integer_IO (Addresses);

   begin -- Dump
      New_Line (Trace_File);
      for I in Iterate (Program_Store) loop
         Address_IO.Put (Trace_File, To_Index (I), 4);
         Put (Trace_File, ':');
         Put (Trace_File, Program_Store (I), 6);
         New_Line (Trace_File);
      end loop; -- I in Iterate (Program_Store)
   end Dump;

   task body Processor is

      Op_Code_Mod : constant Program_Store_Elements := 100;
      Mode_Mod : constant Program_Store_Elements := 10;

      subtype Address_Modes is Program_Store_Elements range 0 .. 2;
      Direct : constant Address_Modes := 0; -- mode 0 direct addressing
      Immediate : constant Address_Modes := 1; -- mode 1 immediste
      Relative : constant Address_Modes := 2; -- mode 2 relative

      Program_Store : Program_Stores.Vector;
      Base : Addresses := 0;
      Trace_File : File_Type;

      function Read_Operand (Address : in Addresses;
                             Address_Mode : in Address_Modes;
                             Trace : in Boolean)
                             return Program_Store_Elements is
         -- Note Program_Store and Base are global variables WRT Read_Operand.
         -- Checks if the address is within the current Program_Store, if not
         -- it extends the program store initalizing the extension to zero and
         -- reads the requsite location.

         Effective_Address : Addresses;

      begin -- Read_Operand
         case Address_Mode is
         when Direct =>
            if Addresses (Element (Program_Store, Address)) >
              Last_Index (Program_Store) then
               if Trace then
                  Put (Trace_File," Extension <" &
                         Addresses'Image (Addresses (Element (Program_Store,
                         Address)) - Last_Index (Program_Store)) & "> ");
               end if; -- Trace
               Append (Program_Store, 0,
                       Count_Type (Addresses (Element (Program_Store, Address))
                         - Last_Index (Program_Store)));
            end if; --  Addresses (Element (Program_Store, Address)) > ...
            Effective_Address := Addresses (Element (Program_Store, Address));
         when Immediate =>
            Effective_Address := Address;
         when Relative =>
            if Addresses (Element (Program_Store, Address) +
                            Program_Store_Elements (Base)) >
              Last_Index (Program_Store) then
               if Trace then
                  Put (Trace_File, " Extension <" &
                         Addresses'Image (
                         Addresses (Element (Program_Store, Address) +
                             Program_Store_Elements (Base)) -
                           Last_Index (Program_Store)) & "> ");
               end if; -- Trace
               Append (Program_Store, 0,
                       Count_Type (Addresses (Element (Program_Store, Address) +
                           Program_Store_Elements (Base)) -
                           Last_Index (Program_Store)));
            end if; -- Addresses (Element (Program_Store, Address)) + ...
            Effective_Address :=
              Addresses (Element (Program_Store, Address) +
                           Program_Store_Elements (Base));
         end case; -- Address_Mode
         if Trace then
            Put (Trace_File, " Read (" & Addresses'Image (Effective_Address) &
                   "):" & Program_Store_Elements'Image
                   (Program_Store (Effective_Address)));
         end if; -- Trace
         return Program_Store (Effective_Address);
      end Read_Operand;

      Procedure Write (Address : in Addresses;
                       Address_Mode : in Address_Modes;
                       Data : in Program_Store_Elements;
                       Trace : in Boolean) is

         -- Note Program_Store and Base are global variables WRT Write.
         -- Checks if the address is within the current Program_Store, if not
         -- it extends the program store initalizing the extension to zero and
         -- writes to the requsite location.

         Effective_Address : Addresses;

      begin -- Write
         case Address_Mode is
         when Direct =>
            if Addresses (Element (Program_Store, Address)) >
              Last_Index (Program_Store) then
               if Trace then
                  Put (Trace_File, " Extension <" &
                         Addresses'Image (Addresses (Element (Program_Store,
                         Address)) - Last_Index (Program_Store)) & "> ");
               end if; -- Trace
               Append (Program_Store, 0,
                       Count_Type (Addresses (Element (Program_Store, Address))
                         - Last_Index (Program_Store)));
            end if; --  Addresses (Element (Program_Store, Address)) > ...
            Effective_Address := Addresses (Element (Program_Store, Address));
         when Immediate =>
            Assert (False, "Invalid addressing mode for memory write");
         when Relative =>
            if Addresses (Element (Program_Store, Address) +
                            Program_Store_Elements (Base)) >
              Last_Index (Program_Store) then
               if Trace then
                  Put (Trace_File, " Extension <" &
                         Addresses'Image (
                         Addresses (Element (Program_Store, Address) +
                             Program_Store_Elements (Base)) -
                           Last_Index (Program_Store)) & "> ");
               end if; -- Trace
               Append (Program_Store, 0,
                       Count_Type (Addresses (Element (Program_Store, Address) +
                           Program_Store_Elements (Base)) -
                           Last_Index (Program_Store)));
            end if; -- Addresses (Element (Program_Store, Address)) + ...
            Effective_Address :=
              Addresses (Element (Program_Store, Address) +
                             Program_Store_Elements (Base));
         end case; -- Address_Mode
         if Trace then
            Put (Trace_File, " Write (" & Addresses'Image (Effective_Address) &
                   "):" & Program_Store_Elements'Image (Data));
         end if; -- Trace
         Program_Store (Effective_Address) := Data;
      end Write;

      Program_Counter : Addresses := 0;
      Jump_Target : Addresses;
      Instruction_Length : Instruction_Lengths;
      Operand_1, Operand_2, Result : Addresses;
      Operand_1_Mode, Operand_2_Mode, Result_Mode : Address_Modes;
      Input_Data : Program_Store_Elements;

      Run, Loaded, Trace : Boolean := False;
      Jump : Boolean;

   begin -- Processor
      while not Loaded or not Run loop
         Select
            accept Load_Program (Code_File_Name : in String) do
               Read_Input (Code_File_Name, Program_Store);
               Loaded := True;
            end Load_Program;
         or
            accept Patch (Location : in Addresses;
                          Value : in Program_Store_Elements) do
               if Loaded then
                  Program_Store (Location) := Value;
               end if; -- Loaded
               -- Do nothing if the program is not loaded
            end Patch;
         or
            accept Run_Program do
               Run := True;
            end Run_Program;
         or
            accept Trace_On (Trace_Name : String := "Trace.txt") do
               Trace := True;
               Create (Trace_File, Out_File, Trace_Name);
            end Trace_On;
         end Select;
      end loop; -- not Loaded or not Run
      if Trace then
         Dump (Program_Store, Trace_File);
         Put_Line (Trace_File, "Processor Started");
      end if; -- Trace
      while Run loop
         if Trace then
            Put (Trace_File, "(" & Addresses'Image (Program_Counter) & "):" &
                   Program_Store_Elements'Image
                   (Program_Store (Program_Counter)));
            case Program_Store (Program_Counter) mod Op_Code_Mod is
            when 1 =>
               Put (Trace_File, " add ");
            when 2 =>
               Put (Trace_File, " multiply ");
            when 3 =>
               Put (Trace_File, " input ");
            when 4 =>
               Put (Trace_File, " output ");
            when 5 =>
               Put (Trace_File, " jump /= 0 ");
            when 6 =>
               Put (Trace_File, " jump = 0 ");
            when 7 =>
               Put (Trace_File, " test < ");
            when 8 =>
               Put (Trace_File, " test = ");
            when 9 =>
               Put (Trace_File, " adjust base ");
            when 99 =>
               Put (Trace_File, " halt ");
            when others =>
               Put (Trace_File, " Unknown Op Code ");
            end case; -- Program_Store (Program_Counter)
         end if; -- Trace
         Jump := False;
         Operand_1 := Program_Counter + 1;
         Operand_1_Mode :=
           (Program_Store (Program_Counter) / Op_Code_Mod) mod Mode_Mod;
         Operand_2 := Program_Counter + 2;
         Operand_2_Mode :=
           (Program_Store (Program_Counter) / Op_Code_Mod / Mode_Mod)
         mod Mode_Mod;
         Result := Program_Counter + 3;
         Result_Mode :=
           (Program_Store (Program_Counter) / Op_Code_Mod / Mode_Mod/ Mode_Mod)
         mod Mode_Mod;
         Instruction_Length := 4; -- default instruction length
         case Program_Store (Program_Counter) mod Op_Code_Mod is
            when 1 =>
               Write (Result, Result_Mode,
                      Read_Operand (Operand_1, Operand_1_Mode, Trace) +
                        Read_Operand (Operand_2, Operand_2_Mode, Trace), Trace);
            when 2 =>
               Write (Result, Result_Mode,
                      Read_Operand (Operand_1, Operand_1_Mode, Trace) *
                        Read_Operand (Operand_2, Operand_2_Mode, Trace), Trace);
            when 3 =>
               Instruction_Length := 2;
               accept Send_Input (Data_In : in Program_Store_Elements) do
                  Input_Data := Data_in;
               end Send_Input;
               Write (Operand_1, Operand_1_Mode, Input_Data, Trace);
            when 4 =>
               Instruction_Length := 2;
               accept Receive_Output (Data_Out : out Program_Store_Elements) do
                  Data_Out := Read_Operand (Operand_1, Operand_1_Mode, Trace);
               end Receive_Output;
            when 5 =>
               if Read_Operand (Operand_1, Operand_1_Mode, Trace) /= 0 then
                  Jump := True;
                  Jump_Target :=
                    Addresses (Read_Operand (Operand_2, Operand_2_Mode, Trace));
               else
                  Instruction_Length := 3;
               end if; -- Read_Operand (Operand_1, Operand_1_Mode, Trace) /= 0
            when 6 =>
               if Read_Operand (Operand_1, Operand_1_Mode, Trace) = 0 then
                  Jump := True;
                  Jump_Target :=
                    Addresses (Read_Operand (Operand_2, Operand_2_Mode, Trace));
               else
                  Instruction_Length := 3;
               end if; -- Read_Operand (Operand_1, Operand_1_Mode, Trace) = 0
            when 7 =>
               if Read_Operand (Operand_1, Operand_1_Mode, Trace) <
                 Read_Operand (Operand_2, Operand_2_Mode, Trace) then
                  Write (Result, Result_Mode, 1, Trace);
               else
                  Write (Result, Result_Mode, 0, Trace);
               end if; -- less than test
            when 8 =>
               if Read_Operand (Operand_1, Operand_1_Mode, Trace) =
                 Read_Operand (Operand_2, Operand_2_Mode, Trace) then
                  Write (Result, Result_Mode, 1, Trace);
               else
                  Write (Result, Result_Mode, 0, Trace);
               end if; -- equal test
            when 9 =>
               Instruction_Length := 2;
               Base := Addresses (Program_Store_Elements (Base) +
                                    Read_Operand (Operand_1, Operand_1_Mode,
                                      Trace));
            when 99 =>
               Run := False;
               if Trace then
                  Put_Line (Trace_File, " ** Halted **");
                  Close (Trace_File);
               end if; -- Trace
            when others =>
               Assert (False, "Invalid Op Code");
         end case; -- Program_Store (Program_Counter)
         if Jump then
            Program_Counter := Jump_Target;
         else
            Program_Counter := Program_Counter + Instruction_Length;
         end if; -- Jump
         if Trace then
            New_Line (Trace_File);
         end if; -- Trace
      end loop; -- Run
   exception
      when others =>
         if Trace then
            Dump (Program_Store, Trace_File);
            Put_Line (Trace_File, "Program Counter:" &
                        Addresses'Image (Program_Counter));
            Put_Line (Trace_File, "Base:" & Addresses'Image (Base));
         else
            Dump (Program_Store, Standard_Output);
            Put_Line (Standard_Output, "Program Counter:"
                      & Addresses'Image (Program_Counter));
            Put_Line (Standard_Output,"Base:" & Addresses'Image (Base));
         end if; -- Trace
         raise;
   end Processor;

end intercode_09;
