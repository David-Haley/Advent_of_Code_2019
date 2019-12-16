package Intercode_09 is
   -- Intercode processor as per day 9 AOC 2019

   subtype Program_Store_Elements is Long_Long_Integer;

   task type Processor is
      entry Load_Program (Code_File_Name : String);
      entry Trace_On;
      entry Run_Program;
      entry Send_Input (Data_In : in Program_Store_Elements);
      entry Receive_Output (Data_Out : out Program_Store_Elements);
   end; -- Processor

end Intercode_09;
