with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Intercode_09; use Intercode_09;
with NT_Console;

procedure December_13 is

   Input_File_Name : constant String := "December_13.txt";

   type Coordinates is record
      X, Y : Program_Store_Elements := 0;
   end record; -- Coordinates;

   subtype Tiles is Program_Store_Elements range 0 .. 4;
   Empty : constant Tiles := 0;
   Wall : constant Tiles := 1;
   Block : constant Tiles := 2;
   Paddle : constant Tiles := 3;
   Ball : constant Tiles := 4;

   function "<" (Left, Right : Coordinates) return Boolean is

   begin -- "<"
      return Left.Y < Right.Y or else
        (Left.Y = Right.Y and Left.X < Right.X);
   end "<";

   function "=" (Left, Right : Coordinates) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Screen_Maps is new
     Ada.Containers.Ordered_Maps (Coordinates, Tiles);
   use Screen_Maps;

   package Screen is new NT_Console (80, 80);
   use Screen;

   procedure Read_Input (Screen_Map : out Screen_Maps.Map) is

      Input_File : File_Type;
      Finished : Boolean := False;
      Current : Coordinates;
      Tile : Tiles;
      Arcade_Cabinet : Processor;

   begin -- Read_Input
      Arcade_Cabinet.Load_Program (Input_File_Name);
--        Arcade_Cabinet.Trace_On;
      Arcade_Cabinet.Run_Program;
      Screen_Map := Screen_Maps.Empty_Map;
      while not Finished loop
         begin -- Output Exception
            Arcade_Cabinet.Receive_Output (Current.X);
         exception
            when Tasking_Error =>
               Finished := True;
         end; -- Output Exception
         If not Finished then
            Arcade_Cabinet.Receive_Output (Current.Y);
            Arcade_Cabinet.Receive_Output (Tile);
            Include (Screen_Map, Current, Tile);
         end if; -- not Finished
      end loop; -- not Finished
   end Read_Input;

   procedure Play_Game is

      Arcade_Cabinet : Processor;

      task Joystick is
         entry Start_Reading;
      end Joystick;

      task body Joystick is

         Finished : Boolean := False;
         Ch : Character := 's';
         Position : Program_Store_Elements;

      begin --
         accept Start_Reading  do
            null;
         end Start_Reading;
         while not Finished loop
            if Key_Available then
               Ch := Get_Key;
            else
            end if; -- Key_Available
            case Ch is
            when 'a' | 'A' =>
               Position := -1; -- Left
            when 's' | 'S' =>
               Position := 0; -- Neutral
            when 'd' | 'D' =>
               Position := 1; -- Right
            when others =>
               null;
            end case; -- Ch
            begin -- Input exception block
               Arcade_Cabinet.Send_Input (Position);
            exception
               when Tasking_Error =>
                  Finished := True;
               when others =>
                  raise;
            end; -- Input exception block
         end loop; -- not Finished
      end Joystick;

      Input_File : File_Type;
      Finished : Boolean := False;
      X, Y, Score : Program_Store_Elements;
      Tile : Tiles;

   begin -- Play_Game
      Arcade_Cabinet.Load_Program (Input_File_Name);
--        Arcade_Cabinet.Trace_On ("Trace_13_2.txt");
      Arcade_Cabinet.Patch (0, 2);
      Arcade_Cabinet.Run_Program;
      Clear_Screen;
      Joystick.Start_Reading;
      while not Finished loop
         begin -- Output Exception
            Arcade_Cabinet.Receive_Output (X);
         exception
            when Tasking_Error =>
               Finished := True;
               when others =>
                  raise;
         end; -- Output Exception
         Arcade_Cabinet.Receive_Output (Y);
         If not Finished then
            if X >= 0 then
               Arcade_Cabinet.Receive_Output (Tile);
               Goto_XY (X_Pos (X), Y_Pos (Y));
               case Tile is
               when Empty =>
                  Set_Background (Black);
                  Put (' ');
               when Wall =>
                  Set_Background (Brown);
                  Put (Program_Store_Elements'Image (Y ));
               when Block =>
                  Set_Background (Yellow);
                  Put (' ');
               when Paddle =>
                  Set_Background (White);
                  Put (' ');
               when Ball =>
                  Set_Background (Red);
                  Put (' ');
                  delay 0.4;
               end case; -- Tile
            else
               Arcade_Cabinet.Receive_Output (Score);
               Goto_XY (10, 0);
               Set_Background (Brown);
               Put (Program_Store_Elements'Image (Score));
            end if; -- not Finished
         end if; -- not Finished
      end loop; -- not Finished
   exception
      when others =>
      Goto_XY (0, 30);
      Put_Line ("Score:" & Program_Store_Elements'Image (Score));
   end Play_Game;

   Screen_Map : Screen_Maps.Map;
   Count : Natural := 0;

begin -- December_13
   Read_Input (Screen_Map);
   for I in Iterate (Screen_Map) loop
      if Screen_Map (I) = Block then
         Count := Count + 1;
      end if; -- Screen_Map (I) = Block
   end loop; -- I in Iterate (Screen_Map)
   Put_Line ("Block Tiles:" & Natural'Image (Count));
   Play_Game;
end December_13;
