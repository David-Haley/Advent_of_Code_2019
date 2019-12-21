package Intercode_09 is
   -- Intercode processor as per day 9 AOC 2019

   subtype Addresses is Natural;
   subtype Program_Store_Elements is Long_Long_Integer;

   task type Processor is
      entry Load_Program (Code_File_Name : String);
      entry Patch (Location : in Addresses; Value : in Program_Store_Elements);
      entry Trace_On (Trace_Name : String := "Trace.txt");
      entry Run_Program;
      entry Send_Input (Data_In : in Program_Store_Elements);
      entry Receive_Output (Data_Out : out Program_Store_Elements);
   end; -- Processor

end Intercode_09;
