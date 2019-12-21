with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Assertions; use Ada.Assertions;
with Intercode_09; use Intercode_09;

procedure December_21 is

   Input_File_Name : constant String := "December_21.txt";

   procedure Run_Springbot (Part_Two : in Boolean := False) is

      Springbot_Brain : Processor;

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
         while not Finished loop
            begin -- Input exception block
               Springbot_Brain.Receive_Output (Data);
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

      Input_File : File_Type;
      Ch : Character;

   begin -- Run_Springbot
      Springbot_Brain.Load_Program (Input_File_Name);
--        Springbot_Brain.Trace_On ("Trace_31_1.txt");
      Springbot_Brain.Run_Program;
      Display.Start_Reading;
      if Part_Two then
         Open (Input_File, In_File, "My_Input_21_2.txt");
      else
         Open (Input_File, In_File, "My_Input_21_1.txt");
      end if; -- Part_Two
      while Line (Input_File) <= 15 and not End_Of_File (Input_File) loop
         while not End_Of_Line (Input_File) loop
            Get (Input_File, Ch);
            Springbot_Brain.Send_Input (Program_Store_Elements
                                        (Character'Pos (Ch)));
            Put (CH);
         end loop; -- while not End_Of_Line (Input_File)
         New_Line;
         Skip_Line (Input_File);
         Springbot_Brain.Send_Input (Program_Store_Elements
                                     (Character'Pos (LF)));
      end loop; -- Line (Input_File) <= 15 and not End_Of_File (Input)
      if Part_Two then
         Springbot_Brain.Send_Input
           (Program_Store_Elements (Character'Pos ('R')));
         Springbot_Brain.Send_Input
           (Program_Store_Elements (Character'Pos ('U')));
         Springbot_Brain.Send_Input
           (Program_Store_Elements (Character'Pos ('N')));
         Put_Line ("RUN");
      else
         Springbot_Brain.Send_Input
           (Program_Store_Elements (Character'Pos ('W')));
         Springbot_Brain.Send_Input
           (Program_Store_Elements (Character'Pos ('A')));
         Springbot_Brain.Send_Input
           (Program_Store_Elements (Character'Pos ('L')));
         Springbot_Brain.Send_Input
           (Program_Store_Elements (Character'Pos ('K')));
         Put_Line ("WALK");
      end if; -- Part_Two
      Springbot_Brain.Send_Input (Program_Store_Elements (Character'Pos (LF)));
      Close (Input_File);
   end Run_Springbot;

begin -- December_21
   Run_Springbot;
   Run_Springbot (True);
end December_21;
