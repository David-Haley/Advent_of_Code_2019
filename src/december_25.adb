with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Intercode_09; use Intercode_09;
with NT_Console;

procedure December_25 is

   Input_File_Name : constant String := "December_25.txt";

   Droid_Brain : Processor;

   package Screen is new NT_Console (80, 40);
   use Screen;

      task Display is
         entry Start_Reading;
      end Display;

   task body Display is

      Finished : Boolean := False;
      Data : Program_Store_Elements;

   begin -- Display
      accept Start_Reading do
         null;
      end Start_Reading;
      Clear_Screen;
      Goto_XY (0, 0);
      while not Finished loop
         begin -- Input exception block
            Droid_Brain.Receive_Output (Data);
         exception
            when Tasking_Error =>
               Finished := True;
            when others =>
               raise;
         end; -- Input exception block
         if not Finished then
            if Data > 255 then
               New_Line;
               Put_Line ("Damage:" & Program_Store_Elements'Image (Data));
            elsif Data = Program_Store_Elements (Character'Pos (LF)) then
               New_Line;
            else
               Put (Character'Val (Data));
            end if; -- Data > 255
         end if; -- not Finished
      end loop; -- not Finished
   end Display;

   procedure Send_Command (Command : in String) is

   begin -- Send_Command
      for I in Positive range 1 .. Command'Length loop
         Droid_Brain.Send_Input
           (Program_Store_Elements (Character'Pos (Command (I))));
      end loop; -- I Positive in range 1 .. Command'Length
      Droid_Brain.Send_Input (Program_Store_Elements (Character'Pos (LF)));
   end Send_Command;

   function Get_Item_Name return String is

      Text : Unbounded_String;

   begin -- Get_Item_Name
      Put ("Item Name: ");
      Get_Line (Text);
      return To_String (Text);
   end Get_Item_Name;

begin -- December_25
   Droid_Brain.Load_Program (Input_File_Name);
--     Droid_Brain.Trace_On ("Trace_25.txt");
   Droid_Brain.Run_Program;
   Display.Start_Reading;
   loop -- process user input
      case Get_Key is
      when 'a' | 'A' =>
         Send_Command ("west");
         Put ("West");
      when 'w' | 'W' =>
         Send_Command ("north");
         Put ("North");
      when 's' | 'S' =>
         Send_Command ("south");
         Put ("South");
      when 'd' | 'D' =>
         Send_Command ("east");
         Put ("East");
      when 'g' | 'G' =>
         Put ("Take ");
         Send_Command ("take " & Get_Item_Name);
      when 'p' | 'P' =>
         Put ("Drop ");
         Send_Command ("drop " & Get_Item_Name);
      when 'i' | 'I' =>
         Send_Command ("inv");
      when 'x' | 'X' =>
         exit;
      when others =>
         Bleep;
      end case; -- Get_Key
   end loop; -- process uset input
end December_25;
