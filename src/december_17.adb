with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with NT_Console;
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

      subtype Function_Indices is Character range 'A' .. 'C';
      type Robot_Functions is array (Function_Indices) of Unbounded_String;
      type Robot_Programs is record
         Main : Unbounded_String;
         Robot_Function : Robot_Functions;
      end record; -- Robot_Programs

      procedure Read_Input (Scaffold_Array : out Scaffold_Arrays) is

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
                           Path : out Unbounded_String) is

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

         Turn_Table : constant array (Directions, Turns) of Directions :=
           (Up => (Left => Left, Right => Right),
            Right => (Left => Up, Right => Down),
            Down => (Left => Right, Right => Left),
            Left => (Left => Down, Right => Up));

         Robot_X :  X_Coordinates;
         Robot_Y :  Y_Coordinates;
         Robot_Dir : Directions;
         Next_Turn : Turns;
         Next_Steps : Natural;
         No_Turn : Boolean;
         Number : Unbounded_String;

      begin -- Find_Path
         Path := Null_Unbounded_String;
         Find_Robot (Scaffold_Array, Robot_X, Robot_Y, Robot_Dir);
         loop -- until end found
            Which_Turn (Scaffold_Array, Robot_Dir, Robot_X, Robot_Y, Next_Turn,
                        No_Turn);
            exit when No_Turn;
            Robot_Dir := Turn_Table (Robot_Dir, Next_Turn);
            Next_Steps := Steps (Scaffold_Array, Robot_X, Robot_Y, Robot_Dir);
            Move (Robot_Dir, Next_Steps, Robot_X, Robot_Y);
            if Length (Path) > 0 then
               Path := Path & ",";
            end if; -- Length (Path) > 0
            case Next_Turn is
            when Left =>
               Path := Path & "L,";
            when Right =>
               Path := Path & "R,";
            end case; -- Next_Turn
            Number := To_Unbounded_String (Natural'Image (Next_Steps));
            Trim (Number, Left);
            Path := Path & Number;
         end loop; -- until end found
      end Find_Path;

      procedure Split (Path : in Unbounded_String;
                       Robot_Program : out Robot_Programs) is

         Main_Set : constant Character_Set := To_Set ("ABC");
         Function_Set : constant Character_Set := To_Set ("LR");
         Delimiter : constant Character := ',';

         Command_Length : constant Positive := 20;

         Start_At, Function_Start, Function_End : Positive;
         First, Last : Natural;
         One_Character : String (1 .. 1);

      begin -- Split
         Robot_Program.Main := Path;
         for F in Function_Indices loop
            Start_At := 1;
            Find_Token (Robot_Program.Main, Function_Set, Start_At, Inside,
                        First, Last);
            Last := First + Command_Length - 1;
            if Index (Robot_Program.Main, Main_Set, First) < Last and
              Index (Robot_Program.Main, Main_Set, First) /= 0 then
               Last := Index (Robot_Program.Main, Main_Set, First) - 1;
            end if; -- Index (Robot_Program.Main, Main_Set, First) < Last ...
            Function_Start := First;
            Function_End := Index (Robot_Program.Main, Decimal_Digit_Set, Last,
                                   Inside, Backward);
            Robot_Program.Robot_Function (F) :=
              Unbounded_Slice (Robot_Program.Main, Function_Start,
                               Function_End);
            Start_At := Function_Start;
            One_Character (1) := F;
            loop -- remove instance Robot_Program.Robot_Function (F) ...
               First := Index (Robot_Program.Main,
                              To_String (Robot_Program.Robot_Function (F)),
                              Start_At);
               exit when First = 0;
               Delete (Robot_Program.Main, First,
                       First + Length (Robot_Program.Robot_Function (F)) - 1);
               Insert (Robot_Program.Main, First, One_Character);
            end loop; -- remove instance Robot_Program.Robot_Function (F) ...
        end loop; -- F in Function_Indices
      end Split;

      procedure Warn_Robots (Robot_Program : in Robot_Programs) is

         ASCII_System : Processor;

         task Display is
         end Display;

         task body Display is

            package Screen is new NT_Console (X_Size, Y_Size + 2);
            use Screen;

            Finished : Boolean := False;
            Data : Program_Store_Elements;
            Previous_Data : Program_Store_Elements := 0;

         begin -- Display
            Clear_Screen;
            Goto_XY (0, 0);
            while not Finished loop
               begin -- Input exception block
                  ASCII_System.Receive_Output (Data);
               exception
                  when Tasking_Error =>
                     Finished := True;
                  when others =>
                     raise;
               end; -- Input exception block
               if not Finished then
                  if Data > 255 then
                     Goto_XY (0, Y_Size + 1);
                     Put_Line ("Part two Dust:" & Program_Store_Elements'Image (Data));
                  elsif Data = Program_Store_Elements (Character'Pos (LF)) then
                     New_Line;
                     if Previous_Data = Data then
                        -- two consecutive LFs indicates end of display
                        Goto_XY (0, 0);
                     end if; -- Previous_Data = Data
                  else
                     Put (Character'Val (Data));
                  end if; -- Data > 255
               end if; -- not Finished
               Previous_Data := Data;
            end loop; -- not Finished
         end Display;

         procedure Send_Command (Command : in String) is

         begin -- Send_Command
            for I in Positive range 1 .. Command'Length loop
               ASCII_System.Send_Input
                 (Program_Store_Elements (Character'Pos (Command (I))));
            end loop; -- I Positive in range 1 .. Command'Length
            ASCII_System.Send_Input
              (Program_Store_Elements (Character'Pos (LF)));
         end Send_Command;

      begin -- Warn_Robots
         ASCII_System.Load_Program (Input_File_Name);
         ASCII_System.Patch (0, 2);
         ASCII_System.Run_Program;
         Send_Command (To_String (Robot_Program.Main));
         for F in Function_Indices loop
            Send_Command (To_String (Robot_Program.Robot_Function (F)));
         end loop;
         Send_Command ("y");
      end Warn_Robots;

      Scaffold_Array : Scaffold_Arrays;
      Sum : Natural := 0;
      Path : Unbounded_String;
      Robot_Program : Robot_Programs;

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
      Find_Path (Scaffold_Array, Path);
      Split (Path, Robot_Program);
      Warn_Robots (Robot_Program);
      Put_Line ("Part one Sum:" & Natural'Image (Sum));
   end Solve;

   X_Size, Y_Size : Natural;

begin -- December_17
   Find_Size (X_Size, Y_Size);
   Solve (X_Size, Y_Size);
end December_17;
