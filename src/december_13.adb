with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Intercode_09; use Intercode_09;
with NT_Console;

procedure December_13 is

   Input_File_Name : constant String := "December_13.txt";

   package Console_IO is new Ada.Text_IO.Integer_IO (Program_Store_Elements);
   use Console_IO;

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

   package Screen is new NT_Console;
   use Screen;

   procedure Read_Input (Screen_Map : out Screen_Maps.Map) is

      Input_File : File_Type;
      Finished : Boolean := False;
      Current : Coordinates;
      Tile : Tiles;
      Arcade_Cabinet : Processor;

   begin -- Read_Input
      Arcade_Cabinet.Load_Program (Input_File_Name);
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

      type Auto_Elements is record
         X, Y, Tile : Program_Store_Elements;
      end record; -- Auto_Elements

      package AQI is new
        Ada.Containers.Synchronized_Queue_Interfaces (Auto_Elements);
      package Auto_Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (AQI);

      Auto_Queue : Auto_Queues.Queue;

      task Auto_Play is
      end Auto_Play;

      task body Auto_Play is

         Left : constant Program_Store_Elements := -1;
         Neutral : constant Program_Store_Elements := 0;
         Right : constant Program_Store_Elements := 1;
         Finished : Boolean := False;
         Position, Paddle_X, Ball_X : Program_Store_Elements;
         Auto_Element : Auto_Elements;

      begin -- Auto_Play
         while not Finished loop
            delay 0.05;
            -- delay to make auto play viewable;
            while Auto_Queue.Current_Use > 0 loop
               Auto_Queue.Dequeue (Auto_Element);
               case Auto_Element.Tile is
                  when Ball =>
                     Ball_X := Auto_Element.X;
                  when Paddle =>
                     Paddle_X := Auto_Element.X;
                  when others =>
                     null;
               end case; -- Auto_Element.Tile
            end loop; -- Auto_Queue.Current_Use > 0
            if Ball_X > Paddle_X then
               Position := Right;
            elsif Ball_X < Paddle_X then
               Position := Left;
            else
               Position := Neutral;
            end if; -- Ball_X > Paddle_X
            begin -- Input exception block
               Arcade_Cabinet.Send_Input (Position);
            exception
               when Tasking_Error =>
                  Finished := True;
               when others =>
                  raise;
            end; -- Input exception block
         end loop; -- not Finished
      end Auto_Play;

      Input_File : File_Type;
      Finished : Boolean := False;
      X, Y, Score : Program_Store_Elements;
      Tile : Tiles;
      Saved_Score : Program_Store_Elements := 0;
      Auto_Element : Auto_Elements;

   begin -- Play_Game
      Arcade_Cabinet.Load_Program (Input_File_Name);
      Arcade_Cabinet.Patch (0, 2);
      Arcade_Cabinet.Run_Program;
      Clear_Screen;
      Set_Cursor (False);
      while not Finished loop
         begin -- Output Exception
            Arcade_Cabinet.Receive_Output (X);
            Auto_Element.X := X;
         exception
            when Tasking_Error =>
               Finished := True;
               when others =>
                  raise;
         end; -- Output Exception
         Arcade_Cabinet.Receive_Output (Y);
         Auto_Element.Y := Y;
         If not Finished then
            if X >= 0 then
               Arcade_Cabinet.Receive_Output (Tile);
               Auto_Element.Tile := Tile;
               Auto_Queue.Enqueue (Auto_Element);
               Goto_XY (X_Pos (X), Y_Pos (Y));
               case Tile is
               when Empty =>
                  Set_Background (Black);
               when Wall =>
                  Set_Background (Brown);
               when Block =>
                  Set_Background (Yellow);
               when Paddle =>
                  Set_Background (White);
               when Ball =>
                  Set_Background (Red);
               end case; -- Tile
               Put (' ');
            else
               Arcade_Cabinet.Receive_Output (Score);
               Goto_XY (1, 0);
               Set_Background (Brown);
               Put (Program_Store_Elements'Image (Score));
               if Saved_Score < Score then
                  Saved_Score := Score;
               end if; -- Saved_Score < Score
            end if; -- not Finished
         end if; -- not Finished
      end loop; -- not Finished
   exception
      when others =>
         Set_Background (Black);
         Set_Cursor (True);
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
   Play_Game;
   Goto_XY (0, 21);
   Put_Line ("Part one block tiles:" & Natural'Image (Count));
   Goto_XY (0, 22); -- Place command prompt outside of game area
end December_13;
