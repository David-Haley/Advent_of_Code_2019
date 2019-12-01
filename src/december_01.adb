with Ada.Text_IO; use  Ada.Text_IO;

procedure December_01 is

   package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);
   use Natural_IO;

   function Fuel_Equation (Mass : in Natural) return Natural is

      Result : Integer;

   begin -- Fuel_Equation
      Result := Mass / 3 - 2;
      if Result < 0 then
         Result := 0;
      end if; -- result < 0
      return Natural (Result);
   end Fuel_Equation;

   Module_Mass, Fuel, Fuel_For_Fuel, Incremental_Fuel_For_Fuel : Natural;
   Input_File : File_Type;

begin -- December_01
   Open (Input_File, In_File, "December_01.txt");
   Fuel := 0;
   while not End_Of_File (Input_File) loop
      Get (Input_File, Module_Mass);
      Skip_Line (Input_File);
      Fuel := Fuel + Fuel_Equation (Module_Mass);
   end loop; -- not End_Of_File (Input_File)
   Put ("Required Fuel: ");
   Put (Fuel, 0);
   New_Line;
   Put_Line ("Part 2, Fuel required for the fuel load");
   Reset (Input_File);
   while not End_Of_File (Input_File) loop
      Get (Input_File, Module_Mass);
      Skip_Line (Input_File);
      Incremental_Fuel_For_Fuel := Fuel_Equation (Fuel_Equation (Module_Mass));
      Fuel_For_Fuel := 0;
      while Incremental_Fuel_For_Fuel > 0 loop
         Fuel_For_Fuel := Fuel_For_Fuel + Incremental_Fuel_For_Fuel;
         Incremental_Fuel_For_Fuel := Fuel_Equation (Incremental_Fuel_For_Fuel);
      end loop; -- Incremental_Fuel_For_Fuel > 0
      Fuel := Fuel + Fuel_For_Fuel;
   end loop; -- not End_Of_File (Input_File)
   Put ("Required Fuel: ");
   Put (Fuel, 0);
   Close (Input_File);
end December_01;
