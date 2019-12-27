with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_22 is

   type Operations is (Stack, Cut, Deal);

   subtype Parameters is Integer;

   type Shuffle_Ops is record
      Operation : Operations;
      N : Parameters := 0;
   end record; -- Shuffle_Ops

   package Shuffle_Lists is new Ada.Containers.Vectors (Positive, Shuffle_Ops);
   use Shuffle_Lists;

   subtype Long_Integer is Long_Long_Integer;
   subtype Long_Natural is Long_Integer range 0 .. Long_Integer'Last;
   subtype Long_Positive is Long_Natural range 0 .. Long_Natural'Last;

   procedure Read_Input (Shuffle_List : out Shuffle_Lists.Vector) is

      New_Stack : constant String := "deal into new stack";
      Cut_N_Cards : constant String := "cut";
      Deal_Inc : constant String := "deal with increment";
      Integer_Set : constant Character_Set := To_Set ("-0123456789");
      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Shuffle_Op : Shuffle_Ops;

   begin -- Read_Input
      Open (Input_File, In_File, "December_22.txt");
--        Open (Input_File, In_File, "Example_22.txt");
      Shuffle_List := Shuffle_Lists.Empty_Vector;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         if Index (Text, New_Stack) > 0 then
            Shuffle_Op.Operation := Stack;
         elsif Index (Text, Cut_N_Cards) > 0 then
            Shuffle_Op.Operation := Cut;
            Start_At := Cut_N_Cards'Length;
            Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
            Shuffle_Op.N := Integer'Value (Slice (Text, First, Last));
         elsif Index (Text, Deal_Inc) > 0 then
            Shuffle_Op.Operation := Deal;
            Start_At := Deal_Inc'Length;
            Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
            Shuffle_Op.N := Integer'Value (Slice (Text, First, Last));
            Assert (Shuffle_Op.N > 0, "Invalid parameter");
         else
            Assert (False, "Unknown shuffle");
         end if; -- Index (Text, New_Stack) > 0
         Append (Shuffle_List, Shuffle_Op);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Shuffle (Shuffle_List : in Shuffle_Lists.Vector) is

      Number_Of_Cards : constant Natural := 10007;
      type Card_Numbers is mod Number_Of_Cards;
      type Decks is array (Card_Numbers) of Card_Numbers;

      procedure New_Deck (Deck : out Decks) is

      begin -- New_Deck
         for I in Card_Numbers loop
            Deck (I) := I;
         end loop; -- I in Card_Numbers
      end New_Deck;

      Deck, Temporary : Decks;
      Deck_Pointer : Card_Numbers;
      Card_2019 : Card_Numbers;
      Last_I : constant Integer := Integer (Card_Numbers'Last);
      Last_Card : constant Card_Numbers := Card_Numbers'Last;
      N : Integer;

   begin -- Shuffle;
      New_Deck (Temporary);
      Deck := Temporary; -- returns unshuffled deck if no operations applied
      for S in Iterate (Shuffle_List) loop
         N := Shuffle_List (S).N;
         Case Shuffle_List (S).Operation is
         when Stack =>
            for I in Card_Numbers loop
               Deck (Card_Numbers'Last - I) := Temporary (I);
            end loop; -- I in Card_Numbers
         when Cut =>
            If N > 0 then
               Deck (Card_Numbers (Last_I - N + 1) .. Last_Card) :=
                 Temporary (0 .. Card_Numbers (N - 1));
               Deck (0 .. Card_Numbers (Last_I - N)) :=
                 Temporary (Card_Numbers (N) .. Last_Card);
            else
               N := abs (N);
               Deck (0 .. Card_Numbers (N - 1)) :=
                 Temporary (Card_Numbers (Last_I - N + 1) .. Last_Card);
               Deck (Card_Numbers (N) .. Card_Numbers'Last) :=
                 Temporary (0 .. Card_Numbers (Last_I - N));
            end if; -- N > 0
         when Deal =>
            Deck_Pointer := 0;
            for I in Card_Numbers loop
               Deck (Deck_Pointer) := Temporary (I);
               Deck_Pointer := Deck_Pointer + Card_Numbers (N);
            end loop; -- I in Card_Numbers
         end case; -- Shuffle_List (S).Operation
         Temporary := Deck; -- Read for next operation
      end loop; -- S in Iterate (Shuffle_List)
      for I in Card_Numbers loop
         if Deck (I) = 2019 then
            Card_2019 := I;
         end if; -- Deck (I) = 2019
      end loop; -- I in Card_Numbers
      Put_Line ("Card 2019 position:" & Card_Numbers'Image (Card_2019));
   end Shuffle;

   procedure Unshuffle (Shuffle_List : in Shuffle_Lists.Vector;
                        Final_Position : in Long_Natural;
                        Cards : in Long_Positive;
                        Applied : in Long_Positive := 101741582076661;
                        Initial_Position : out Long_Natural) is

      Last_Card : constant Long_Natural := Cards - 1;

      function Un_Stack (Position : in Long_Natural) return Long_Natural is

      begin -- Un_Stack
         return Last_Card - Position;
      end Un_Stack;

      function Un_Cut (Position : in Long_Natural;
                       N_In : in Long_Integer) return Long_Natural is

         N : Long_Integer := N_In;

      begin -- Un_Cut
         if N > 0 then
            if Cards - N <= Position then
               return Position - Cards + N;
            else
               return Position + N;
            end if; -- Cards - N < Position
         else
            N := abs (N);
            if Position < N then
               return Position + Cards - N;
            else
               return Position - N;
            end if; -- Position < N
         end if; -- N > 0
      end Un_Cut;

      function Un_Deal (Position : in Long_Natural;
                        N : in Long_Positive) return Long_Natural is

         M : Long_Natural := 0;

      begin -- Un_Deal
         while (Position + M * Cards) mod N /= 0 loop
            M := M + 1;
         end loop; -- (Position + M * Cards) mod N /= 0
         return (Position + M * Cards) / N;
      end Un_Deal;

      Position : Long_Natural := Final_Position;

   begin -- Unshuffle
      for I in Long_Positive range 1 .. Applied loop
         for S in reverse Iterate (Shuffle_List) loop
            case Shuffle_List (S).Operation is
            when Stack =>
               Position := Un_Stack (Position);
            when Cut =>
               Position := Un_Cut (Position, Long_Integer (Shuffle_List (S).N));
            when Deal =>
               Position := Un_Deal (Position,
                                    Long_Positive (Shuffle_List (S).N));
            end case; --  Shuffle_List (S).Operation
         end loop; -- S in Reverse_Iterate (Shuffle_List)
      end loop; -- I in Long_Positive range 1 .. Applied
      Initial_Position := Position;
   end Unshuffle;

   Shuffle_List : Shuffle_Lists.Vector;
   Position : Long_Natural;
   I : Long_Positive := 1;

begin -- December_22
   Read_Input (Shuffle_List);
   Shuffle (Shuffle_List);
   Unshuffle (Shuffle_List, 2020, 119315717514047, 101741582076661, Position);
   Put_Line ("Part two card in position 2020:" & Long_Natural'Image (Position));
end December_22;
