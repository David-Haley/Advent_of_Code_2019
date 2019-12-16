with Ada.Text_IO; use  Ada.Text_IO;
with Intercode_09; use Intercode_09;

procedure December_09_Alt is

   CPU1, CPU2 : Processor;
   Data_Out : Program_Store_Elements;

begin -- December_09_Alt
   CPU1.Load_Program ("December_09.txt");
   CPU1.Run_Program;
   CPU1.Send_Input (1);
   CPU1.Receive_Output (Data_Out);
   Put_Line ("Output:" & Program_Store_Elements'Image (Data_Out));
   CPU2.Load_Program ("December_09.txt");
   CPU2.Run_Program;
   CPU2.Send_Input (2);
   CPU2.Receive_Output (Data_Out);
   Put_Line ("Part Two Output:" & Program_Store_Elements'Image (Data_Out));
end December_09_Alt;
