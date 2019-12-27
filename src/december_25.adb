with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Directories; use Ada.Directories;
with Interfaces; use Interfaces;
with Intercode_09; use Intercode_09;
with NT_Console;

procedure December_25 is

   Input_File_Name : constant String := "December_25.txt";

   User_Input_Flag : constant Unbounded_String := To_Unbounded_String (">");

   Droid_Brain : Processor;

   package Screen is new NT_Console (80, 40);
   use Screen;

   package LQI is new
     Ada.Containers.Synchronized_Queue_Interfaces (Unbounded_String);

   package Logger_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (LQI);

   Logger_Queue : Logger_Queues.Queue;

   task Display is
      entry Start_Reading;
   end Display;

   task Logger is
   end Logger;


   task body Display is

      Flag : constant String := "<";
      Finished : Boolean := False;
      Data : Program_Store_Elements;
      Logger_Output : Unbounded_String := To_Unbounded_String (Flag);

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
            if Data = Program_Store_Elements (Character'Pos (LF)) then
               Logger_Queue.Enqueue (Logger_Output);
               Logger_Output := To_Unbounded_String (Flag);
               New_Line;
            else
               Append (Logger_Output, Character'Val (Data));
               Put (Character'Val (Data));
            end if; -- Data > 255
         end if; -- not Finished
      end loop; -- not Finished
   end Display;

   task body Logger is

      Log_File : File_Type;
      Log_Entry : Unbounded_String;

   begin -- Logger
      Create (Log_File, Out_File, "December_25_Log.txt");
      loop -- forever
         Logger_Queue.Dequeue (Log_Entry);
         Put_Line (Log_File, Log_Entry);
         Flush (Log_File);
      end loop; -- forever
   end; -- Logger

   procedure Send_Command (Command : in String) is

      Logger_Output : Unbounded_String := User_Input_Flag;

   begin -- Send_Command
      for I in Positive range 1 .. Command'Length loop
         Droid_Brain.Send_Input
           (Program_Store_Elements (Character'Pos (Command (I))));
      end loop; -- I Positive in range 1 .. Command'Length
      Droid_Brain.Send_Input (Program_Store_Elements (Character'Pos (LF)));
      -- Order is important here the command can only be sent to the logger
      -- queue after the game requests input allowing this thread to run. This
      -- ensures that the command and game output appear in the log in the
      -- correct orded.
      Append (Logger_Output, Command);
      Logger_Queue.Enqueue (Logger_Output);
   end Send_Command;

   function Get_Item_Name return String is

      Text : Unbounded_String;

   begin -- Get_Item_Name
      Put ("Item Name: ");
      Get_Line (Text);
      return To_String (Text);
   end Get_Item_Name;

   procedure Replay is

      Replay_File : File_Type;
      Replay_File_Name, Command : Unbounded_String;
      Text : Unbounded_String;
      Start : Natural;

   begin -- Replay
      Put ("Replay file name: ");
      Get_Line (Replay_File_Name);
      if Length (Replay_File_Name) > 0 and then
        Exists (To_String (Replay_File_Name)) then
         Open (Replay_File, In_File, To_String (Replay_File_Name));
         while not End_Of_File (Replay_File) loop
            Get_Line (Replay_File, Text);
            Start := Index (Text, To_String (User_Input_Flag));
            if Start = 1 then
               Delete (Text, Start, Length (User_Input_Flag) - Start + 1);
               Send_Command (To_String (Text));
            end if; -- Start = 1
         end loop; -- not End_Of_File (Replay_File)
         Close (Replay_File);
      else
         Put_Line ("Replay file """ & Replay_File_Name & """ not found");
         Bleep;
      end if; -- Length (Replay_File_Name) > 0 and then ...
   end Replay;

   Procedure Test_Permutations is

      -- Assumptions: The Inventory file contains at least one line for each
      -- inventory entry. No additional leading or trailing characters
      -- permitted.

      Selector_Mask : constant Unsigned_8 := 2#00000001#;
      subtype Inventory_Indices is Natural range 0 .. 8;

      Inventory_File : File_Type;
      Inventory_File_Name, Command : Unbounded_String;
      Inventory : array (Inventory_Indices) of Unbounded_String;
      Previous_Selector : Unsigned_8 := 2#00000000#;


   begin -- Test_Permutations
      Put ("Inventory file name: ");
      Get_Line (Inventory_File_Name);
      if Length (Inventory_File_Name) > 0 and then
        Exists (To_String (Inventory_File_Name)) then
         Open (Inventory_File, In_File, To_String (Inventory_File_Name));
         for I in Inventory_Indices loop
            Get_Line (Inventory_File, Inventory (I));
         end loop; -- I in Inventory_Indices
         Close (Inventory_File);
         for Selector in Unsigned_8 loop
            -- take items dropped on the previous cycle that are needed now
            for I in Inventory_Indices loop
               -- drop itemes for this cycle
               if (((Previous_Selector xor Selector) and Previous_Selector) and
                     Shift_Left (Selector_Mask, I)) > 0 then
                  Send_Command ("take " & To_String (Inventory (I)));
               end if; -- (((Previous_Selector xor I) and Previous_Selector) and
               -- additional items to drop
               if (((Previous_Selector xor Selector) and Selector) and
                     Shift_Left (Selector_Mask, I)) > 0 then
                  Send_Command ("drop " & To_String (Inventory (I)));
               end if; -- (Selector and Shift_Left (Selector_Mask, I)) > 0
            end loop; -- I in Inventory_Indices
            Previous_Selector := Selector;
            Send_Command ("south");
         end loop; -- Selector in Unsigned_8
      else
         Put_Line ("Inventory file """ & Inventory_File_Name & """ not found");
         Bleep;
      end if; -- Length (Rep
   end Test_Permutations;

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
      when 'r' | 'R' =>
         Replay;
      when 't' | 'T' =>
         Test_Permutations;
      when 'x' | 'X' =>
         abort Droid_Brain, Display, Logger;
         exit;
      when others =>
         Bleep;
      end case; -- Get_Key
   end loop; -- process uset input
exception
   when Tasking_Error =>
      abort Droid_Brain, Display, Logger;
end December_25;
