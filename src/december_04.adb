with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;

procedure December_04 is

   package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);
   use Natural_IO;

   Number_OF_Digits : constant Natural := 6;
   Base : constant Natural := 10;

   procedure Read_Input (Lower_Limit, Upper_Limit : out Natural) is

      Input_File : File_Type;
      Dash : Character;

   begin -- Read_Input
      Open (Input_File, In_File, "December_04.txt");
      Get (Input_File, Lower_Limit);
      Get (Input_File, Dash);
      Assert (Dash = '-', "Expected '-' and found '" & Dash & "'");
      Get (Input_File, Upper_Limit);
      Close (Input_File);
   end Read_Input;

   function Test_Code (Pass_Code : in Natural) return Boolean is

      Increasing : Boolean := True;
      Repeated : Boolean := False;
      Number : Natural := Pass_Code;
      Previous_Digit : Natural;

   begin -- Test_Code
      Previous_Digit := Number mod Base;
      Number := Number / Base;
      -- Digits scanned right to left, that is, starting with LSD
      for I in 2 .. Number_OF_Digits loop
         Increasing := Increasing and Number mod Base <= Previous_Digit;
         Repeated := Repeated or Number mod Base = Previous_Digit;
         Previous_Digit := Number mod Base;
         Number := Number / Base;
      end loop; -- I in 2 .. Number_OF_Digits
      return Increasing and Repeated;
   end Test_Code;

   function Test_Run_Length (Pass_Code : in Natural;
                             Required_Run_Length : Natural := 2)
                             return Boolean is

      Number : Natural := Pass_Code;
      Previous_Digit, Run_Length : Natural;
      Result : Boolean := False;

   begin -- Test_Run_Length
      Previous_Digit := Number mod Base;
      Number := Number / Base;
      Run_Length := 0;
      -- Digits scanned right to left, that is, starting with LSD
      for I in 2 .. Number_OF_Digits loop
         if Number mod Base = Previous_Digit then
            if Run_Length = 0 then
               Run_Length := 2;
            else
               Run_Length := Run_Length + 1;
            end if; -- Run_Length = 0
         elsif Run_Length > 0 then
            -- run of two or more digits and a mis-match between digits
            Result := Result or Run_Length = Required_Run_Length;
            -- contains at least one run of length equal to Required_Run_Length
            Run_Length := 0;
         end if; -- Number mod Base = Previous_Digit
         Previous_Digit := Number mod Base;
         Number := Number / Base;
      end loop; -- I in 2 .. Number_OF_Digits
      return Result or Run_Length = Required_Run_Length;
      -- The or condition is required to cover cases where the repeated pair of
      -- digits is the first two digits in the Pass_Code, that is, the last two
      -- digits tested. Comment based on Required_Run_Length equal to two.
   end Test_Run_Length;

   Lower_Limit, Upper_Limit : Natural;
   Pass_Code_Count, Part_Two_Pass_Code_Count : Natural := 0;

begin -- December_04
   Read_Input (Lower_Limit, Upper_Limit);
   for Pass_Code in Natural range Lower_Limit .. Upper_Limit loop
      if Test_Code (Pass_Code) then
         Pass_Code_Count := Pass_Code_Count + 1;
         if Test_Run_Length (Pass_Code) then
            Part_Two_Pass_Code_Count := Part_Two_Pass_Code_Count + 1;
         end if; -- Test_Run_Length (Pass_Code)
      end if; -- Test_Code (Pass_Code)
   end loop; -- Pass_Code in Natural range Lower_Limit .. Upper_Limit
   Put_Line ("Pass Code Count:" & Natural'Image (Pass_Code_Count));
   Put_Line ("Part Two Pass Code Count:" &
               Natural'Image (Part_Two_Pass_Code_Count));
end December_04;
