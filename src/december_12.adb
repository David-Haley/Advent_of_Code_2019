with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_12 is

   subtype Velocities is Integer;
   subtype Positions is Integer;

   subtype Axies is Character range 'x' .. 'z';

   type Position_Arrays is array (Axies) of Positions;
   type Velocity_Arrays is array (Axies) of Velocities;

   type Moons is record
      Position : Position_Arrays;
      Velocity : Velocity_Arrays := (0, 0, 0);
   end record; -- Moons

   package Moon_Lists is new Ada.Containers.Vectors (Positive, Moons);
   use Moon_Lists;

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   use Int_IO;

   procedure Read_Input (Moon_List : out Moon_Lists.Vector) is

      Input_File : File_Type;
      Ch : Character;
      EOL : Boolean;
      Moon : Moons;

   begin -- Read_Input
      Moon_List := Moon_Lists.Empty_Vector;
      Open (Input_File, In_File, "December_12.txt");
      while not End_Of_File (Input_File) loop
         Get (Input_File, Ch);
         Assert (Ch = '<', "Expected '<' and found '" & Ch & "'");
         for Axis in Axies loop
            Get (Input_File, Ch);
            Assert (Ch = Axis, "Expected '" & Axis & "' and found '" & Ch &
                      "'");
            Get (Input_File, Ch);
            Assert (Ch = '=', "Expected '=' and found '" & Ch & "'");
            Get (Input_File, Moon.Position (Axis));
            if Axis /= Axies'Last then
               Get (Input_File, Ch);
               Assert (Ch = ',', "Expected ',' and found '" & Ch & "'");
               Look_Ahead (Input_File, Ch, EOL);
               while Ch = ' ' and not EOL loop
                  Get (Input_File, Ch);
                  Look_Ahead (Input_File, Ch, EOL);
               end loop; -- Ch = ' ' and not EOL
            end if; -- Axis /= Axies'Last
         end loop; -- Axis in Axies
         Get (Input_File, Ch);
         Assert (Ch = '>', "Expected '>' and found '" & Ch & "'");
         Append (Moon_List, Moon);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Simulate (Moon_List : in out Moon_Lists.Vector;
                       Cycles : in Positive := 1) is

   begin -- Simulate
      -- apply gravity
      for Cycle in Positive range 1 .. Cycles loop
         for M1 in Positive range 1 .. Positive (Length (Moon_List)) loop
            for M2 in Positive range M1 .. Positive (Length (Moon_List)) loop
               for Axis in Axies loop
                  if Moon_List (M1).Position (Axis) >
                    Moon_List (M2).Position (Axis) then
                     Moon_List (M1).Velocity (Axis) :=
                       Moon_List (M1).Velocity (Axis) - 1;
                     Moon_List (M2).Velocity (Axis) :=
                       Moon_List (M2).Velocity (Axis) + 1;
                  elsif Moon_List (M1).Position (Axis) <
                    Moon_List (M2).Position (Axis) then
                     Moon_List (M1).Velocity (Axis) :=
                       Moon_List (M1).Velocity (Axis) + 1;
                     Moon_List (M2).Velocity (Axis) :=
                       Moon_List (M2).Velocity (Axis) - 1;
                  end if; -- Moon_List (M1).Position (Axis) ...
               end loop; -- Axis in Axies
            end loop; --  M2 in Positive range M1 ...
         end loop; -- M1 in Positive range 1 .. Positive (Length (Moon_List))
         -- apply velocity
         for M in Positive range 1 .. Positive (Length (Moon_List)) loop
            for Axis in Axies loop
               Moon_List (M).Position (Axis) := Moon_List (M).Position (Axis)
                 + Moon_List (M).Velocity (Axis);
            end loop; -- Axis in Axies
         end loop; -- M1 in Positive range 1 .. Positive (Length (Moon_List))
      end loop; -- Cycle in Positive range 1 .. Cycles
   end Simulate;

   function Energy (Moon_List : in Moon_Lists.Vector) return Natural is

      Result : Natural := 0;
      Potential, Kinetic : Natural;

   begin -- Energy
      for M in Iterate (Moon_List) loop
         Potential := 0;
         Kinetic := 0;
         for Axis in Axies loop
            Potential := Potential + abs (Moon_List (M).Position (Axis));
            Kinetic := Kinetic + abs (Moon_List (M).Velocity (Axis));
         end loop; -- Axis in Axies
         Result := Result + Potential * Kinetic;
      end loop; -- M in Iterate (Moon_List)
      return Result;
   end Energy;

   procedure Put (Moon_List :  Moon_Lists.Vector) is

   begin -- Put
      for M in Iterate (Moon_List) loop
         Put ("Position ");
         for Axis in Axies loop
            Put (Axis);
            Put (Moon_List (M).Position (Axis), 0);
         end loop; -- Axis in Axies
         Put (" Velocity ");
         for Axis in Axies loop
            Put (Axis);
            Put (Moon_List (M).Velocity (Axis), 0);
         end loop; -- Axis in Axies
         New_Line;
      end loop; -- M in Iterate (Moon_List)
      Put_Line ("Moon Energy:" & Natural'Image (Energy (Moon_List)));
   end Put;

   function GCD (A, B : Long_Long_Integer) return Long_Long_Integer is
      M : Long_Long_Integer := A;
      N : Long_Long_Integer := B;
      T : Long_Long_Integer;
   begin -- LCM
      while N /= 0 loop
         T := M;
         M := N;
         N := T mod N;
      end loop; -- N /= 0
      return M;
   end GCD;

   function LCM (A, B : Long_Long_Integer) return Long_Long_Integer is
   begin -- LCM
      if A = 0 or B = 0 then
         return 0;
      end if; -- A = 0 or B = 0
      return abs (A) * (abs (B) / Gcd (A, B));
   end LCM;

   function Solution_2 (Moon_List : in Moon_Lists.Vector)
                        return Long_Long_Integer is

      function Find_Cycle (Moon_List : in Moon_Lists.Vector;
                           Axis : in Axies) return Long_Long_Integer is

         subtype Moon_Indices is
           Positive range 1 .. Positive (Length (Moon_List));

         type Moon_States is record
            Position, Velocity : integer;
         end record; -- Moon_States

         type State_Elements is array (Moon_Indices) of Moon_States;

         procedure Copy_State (Moon_List : in Moon_Lists.Vector;
                               Axis : in Axies;
                               State_Element : out State_Elements) is

         begin -- Copy_State
            for M in Moon_Indices loop
               State_Element (M).Position := Moon_List (M).Position (Axis);
               State_Element (M).Velocity := Moon_List (M).Velocity (Axis);
            end loop; -- M in Moon_Indices
         end Copy_State;

         Same_State : Boolean := False;
         First, Current : State_Elements;
         Cycle : Natural := 0;
         ML : Moon_Lists.Vector := Copy (Moon_List);

      begin -- Find_Cycle
         Copy_State (ML, Axis, First);
         while not Same_State loop
            Simulate (ML, 1);
            Cycle := Cycle + 1;
            Copy_State (ML, Axis, Current);
            Same_State := First = Current;
         end loop; -- not Same_State
         return Long_Long_Integer (Cycle);
      end Find_Cycle;

      Period : array (Axies) of Long_Long_Integer;

   begin -- Solution_2
      for Axis in Axies loop
         Period (Axis) := Find_Cycle (Moon_List, Axis);
         Put_Line (Axis & " Axis Cycle Length" &
                     Long_Long_Integer'Image (Period (Axis)));
      end loop; -- Axis in Axies
      Return LCM (LCM (Period ('x'), Period ('y')), Period ('z'));
   end Solution_2;

   Moon_List : Moon_Lists.Vector;

begin -- December_12
   Read_Input (Moon_List);
   Simulate (Moon_List, 1000);
   Put (Moon_List);
   Read_Input (Moon_List);
   Put_Line ("Cycles:" & Long_Long_Integer'Image (Solution_2 (Moon_List)));
end December_12;
