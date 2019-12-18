with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Intercode_09; use Intercode_09;

procedure December_17 is

   Input_File_Name : constant String := "December_17.txt";

   procedure Find_Size (X_Size, Y_Size : out Natural) is

      Input_File : File_Type;
      ASCII_Out : Program_Store_Elements;
      Finished : Boolean := False;
      ASCII_System : Processor;
      X_Count : Natural;


   begin -- Find_Size
      X_Size := 0;
      ASCII_System.Load_Program (Input_File_Name);
      ASCII_System.Run_Program;
      while not Finished loop
         ASCII_System.Receive_Output (ASCII_Out);
         Finished := Character'Val (ASCII_Out) = LF;
         if not Finished then
            X_Size := X_Size + 1;
         end if; -- not Finished
      end loop; -- not End_Of_Line (Input_File)
      Finished := False;
      Y_Size := 1;
      X_Count := 0;
      while not Finished loop
         begin -- Output Exception
            ASCII_System.Receive_Output (ASCII_Out);
         exception
            when Tasking_Error =>
               Finished := True;
         end; -- Output Exception
         if Character'Val (ASCII_Out) = LF then
            if X_Count > 0 then
               Y_Size := Y_Size + 1;
            end if; -- X_Count > 0
            X_Count := 0;
         else
            X_Count := X_Count + 1;
         end if; -- Character'Val (ASCII_Out) = LF
      end loop; -- not Finished
      if X_Count >= X_Size then
         Y_Size := Y_Size + 1;
      end if; -- X_Count >= X_Size
   end Find_Size;

   procedure Solve (X_Size, Y_Size : in Natural) is

      Scaffold : constant Character := '#';

      subtype X_Coordinates is Natural range 0 .. X_Size - 1;
      subtype Y_Coordinates is Natural range 0 .. Y_Size - 1;

      type Scaffold_Arrays is array (X_Coordinates, Y_Coordinates) of Character;

      procedure Read_Input (Scaffold_Array : out Scaffold_Arrays) is

         Input_File : File_Type;
         ASCII_System : Processor;
         ASCII_Out : Program_Store_Elements;
         Finished : Boolean := False;

      begin -- Read_Input;
         ASCII_System.Load_Program (Input_File_Name);
         ASCII_System.Run_Program;
         for Y in Y_Coordinates loop
            for X in X_Coordinates loop
               ASCII_System.Receive_Output (ASCII_Out);
               Scaffold_Array (X, Y) := Character'Val (ASCII_Out);
            end loop; -- X in X_Coordinates
            ASCII_System.Receive_Output (ASCII_Out); -- Skip_Line
            Assert (Character'Val (ASCII_Out) = LF,
                    "No end of line");
         end loop; -- Y in Y_Coordinates
         -- skip blank line outputs to allow task to terminate
         while not Finished loop
            begin -- Output Exception
               ASCII_System.Receive_Output (ASCII_Out); -- Skip_Line
               Assert (Character'Val (ASCII_Out) = LF,
                       "No end of line skipping blank lines");
            exception
               when Tasking_Error =>
                  Finished := True;
            end; -- Output Exception
         end loop; -- not Finished
      end Read_Input;

      procedure Find_Path (Scaffold_Array : in Scaffold_Arrays;
                           Path : Unbounded_String) is

         type Directions is (Up, Right, Down, Left);
         type Turns is (Left, Right);

         procedure Which_Turn (Scaffold_Array : in Scaffold_Arrays;
                               Current_Direction : in Directions;
                               X : in X_Coordinates;
                               Y : in Y_Coordinates;
                               Turn : out Turns;
                               No_Turn : out Boolean) is

         begin -- Which_Turn
            No_Turn := False;
            case Current_Direction is
               when Up =>
                  if X - 1 in X_Coordinates and then
                    Scaffold_Array (X - 1, Y) = Scaffold then
                     Turn := Left;
                  elsif X + 1 in X_Coordinates and then
                    Scaffold_Array (X + 1, Y) = Scaffold then
                     Turn := Right;
                  else
                     No_Turn := True;
                  end if; -- X - 1 in X_Coordinates and
               when Right =>
                  if Y - 1 in Y_Coordinates and then
                    Scaffold_Array (X, Y - 1) = Scaffold then
                     Turn := Left;
                  elsif Y + 1 in Y_Coordinates and then
                    Scaffold_Array (X, Y + 1) = Scaffold then
                     Turn := Right;
                  else
                     No_Turn := True;
                  end if; -- Y - 1 in Y_Coordinates
               when Down =>
                  if X - 1 in X_Coordinates and then
                    Scaffold_Array (X - 1, Y) = Scaffold then
                     Turn := Right;
                  elsif X + 1 in X_Coordinates and then
                    Scaffold_Array (X + 1, Y) = Scaffold then
                     Turn := Left;
                  else
                     No_Turn := True;
                  end if; -- X - 1 in X_Coordinates
               when Left =>
                  if Y - 1 in Y_Coordinates and then
                    Scaffold_Array (X, Y - 1) = Scaffold then
                     Turn := Right;
                  elsif Y + 1 in Y_Coordinates and then
                    Scaffold_Array (X, Y + 1) = Scaffold then
                     Turn := Left;
                  else
                     No_Turn := True;
                  end if; -- Y - 1 in Y_Coordinates
            end case; -- Current_Direction
         end Which_Turn;

         procedure Find_Robot (Scaffold_Array : in Scaffold_Arrays;
                               Robot_X : out X_Coordinates;
                               Robot_Y : out Y_Coordinates;
                               Robot_Dir : out Directions) is

            Direction_Table : constant array (Directions) of Character :=
              (Up => '^', Right => '>', Down => 'v', Left => '<');

            -- Asume only one robot

         begin -- Find_Robot
            for Y in Y_Coordinates loop
               for X in X_Coordinates loop
                  for D in Directions loop
                     if Direction_Table (D) = Scaffold_Array (X, Y) then
                        Robot_X := X;
                        Robot_Y := Y;
                        Robot_Dir := D;
                     end if; -- Direction_Table (D) = Scaffold_Array (X, Y)
                  end loop; -- D in Directions
               end loop; -- X in X_Coordinates
            end loop; -- Y in Y_Coordinates
         end Find_Robot;

         function Steps (Scaffold_Array : in Scaffold_Arrays;
                         Robot_X : in X_Coordinates;
                         Robot_Y : in Y_Coordinates;
                         Current_Direction : in Directions) return Natural is

            Count : Natural := 0;

         begin -- Steps
            case Current_Direction is
            when Up =>
               while Robot_Y - Count - 1 in Y_Coordinates and then
                 Scaffold_Array (Robot_X, Robot_Y - Count - 1) = Scaffold loop
                  Count := Count + 1;
               end loop; -- continue up
            when Right =>
               while Robot_X + Count + 1 in X_Coordinates and then
                 Scaffold_Array (Robot_X + Count + 1, Robot_Y) = Scaffold loop
                  Count := Count + 1;
               end loop; -- continue up
            when Down =>
               while Robot_Y + Count + 1 in Y_Coordinates and then
                 Scaffold_Array (Robot_X, Robot_Y + Count + 1) = Scaffold loop
                  Count := Count + 1;
               end loop; -- continue down
            when Left =>
               while Robot_X - Count - 1 in X_Coordinates and then
                 Scaffold_Array (Robot_X - Count - 1, Robot_Y) = Scaffold loop
                  Count := Count + 1;
               end loop; -- continue up
            end case; -- Current_Direction
            Return Count;
         end Steps;

         procedure Move (Current_Direction : in Directions;
                         Step : in Natural;
                         X : in out X_Coordinates; Y : in out Y_Coordinates) is

         begin -- Move
            case Current_Direction is
            when Up => Y :=
                 Y - Step;
            when Right =>
               X := X + Step;
            when Down =>
               Y := Y + Step;
            when Left =>
               X := X - Step;
            end case; -- Current_Direction
         end Move;

         Robot_X :  X_Coordinates;
         Robot_Y :  Y_Coordinates;
         Robot_Dir : Directions;
         Next_Turn : Turns;
         Next_Steps : Natural;
         No_Turn : Boolean;
         Turn_Table : constant array (Directions, Turns) of Directions :=
           (Up => (Left => Left, Right => Right),
            Right => (Left => Up, Right => Down),
            Down => (Left => Right, Right => Left),
            Left => (Left => Down, Right => Up));

      begin -- Find_Path
         Find_Robot (Scaffold_Array, Robot_X, Robot_Y, Robot_Dir);
         loop -- until end found
            Which_Turn (Scaffold_Array, Robot_Dir, Robot_X, Robot_Y, Next_Turn,
                        No_Turn);
            exit when No_Turn;
            Robot_Dir := Turn_Table (Robot_Dir, Next_Turn);
            Next_Steps := Steps (Scaffold_Array, Robot_X, Robot_Y, Robot_Dir);
            Move (Robot_Dir, Next_Steps, Robot_X, Robot_Y);
            Put_Line (X_Coordinates'Image (Robot_X) &
                     Y_Coordinates'Image (Robot_Y) & ' ' &
                     Directions'Image (Robot_Dir) & ' ' &
                     Turns'Image (Next_Turn) & ' ' & Boolean'Image (No_Turn) &
                        Natural'Image (Next_Steps));
         end loop; -- until end found
      end Find_Path;

      Scaffold_Array : Scaffold_Arrays;
      Sum : Natural := 0;
      Path : Unbounded_String;

   begin --Solve
      Read_Input (Scaffold_Array);
      for Y in Y_Coordinates range 1 .. Y_Coordinates'Last - 1 loop
         for X in X_Coordinates range 1 .. X_Coordinates'Last - 1 loop
            if Scaffold_Array (X, Y) = Scaffold and
              Scaffold_Array (X - 1, Y) = Scaffold and
              Scaffold_Array (X + 1, Y) = Scaffold and
              Scaffold_Array (X, Y - 1) = Scaffold and
              Scaffold_Array (X, Y + 1) = Scaffold then
               Sum := Sum + X * Y;
            end if; -- is an intersection
            end loop; -- X in X_Coordinates
         end loop; -- Y in Y_Coordinates
      Put_Line ("Sum:" & Natural'Image (Sum));
      Find_Path (Scaffold_Array, Path);
   end Solve;

   X_Size, Y_Size : Natural;

begin -- December_17
   Find_Size (X_Size, Y_Size);
   Put_Line ("Limits" & Natural'Image (X_Size) & Natural'Image (Y_Size));
   Solve (X_Size, Y_Size);
end December_17;
