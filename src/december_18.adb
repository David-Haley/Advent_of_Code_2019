with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;

procedure December_18 is

   Input_File : File_Type;
   X_Limit, Y_Limit : Positive;

   procedure Find_Limits (X_Limit, Y_Limit : out Positive) is

      Text : Unbounded_String;

   begin -- Find_Limits
      while not End_Of_File (Input_File) loop
         Y_Limit := Positive (Line (Input_File));
         Get_Line (Input_File, Text);
         X_Limit := Length (Text);
      end loop; -- not End_Of_File (Input_File)
   end Find_Limits;

   procedure Find_Solution (Input_File : in File_Type;
                            X_Limit, Y_Limit : in Positive) is

      subtype X_Coordinates is Positive range 1 .. X_Limit;
      subtype Y_Coordinates is Positive range 1 .. Y_Limit;
      type Coordinates is record
         X : X_Coordinates;
         Y : Y_Coordinates;
      end record; -- Coordinates

      Path : constant Character := '.';
      Wall : constant Character := '#';
      Start_Ch : constant Character := Character'Pred ('a');
      -- this is substituted for the '@' character to make the robot symbol
      -- contiguous with the keys. this allows iteration over the robot and
      -- keys. -Iteration over a subtype with a static predicate is not allowed.
      subtype Duct_Characters is Character
        with Static_Predicate => Duct_Characters in Path | Wall |
          'A' .. 'Z' | Start_Ch .. 'z';
      subtype Targets is Character range Start_Ch .. 'z';
      subtype Doors is Duct_Characters range 'A' .. 'Z';
      subtype Door_Keys is Character range 'a' .. 'z';

      package Key_Sets is new Ada.Containers.Ordered_Sets (Door_Keys);
      use Key_Sets;

      type Duct_Elements is record
         Duct_Character : Duct_Characters;
         Bread_Crumb : Boolean := False;
      end record; -- Duct_Elements
      type Ducts is array (X_Coordinates, Y_Coordinates) of Duct_Elements;

      type Target_Arrays is array (Targets) of Coordinates;

      type Results is record
         Distance : Natural;
         Required_Keys : Key_Sets.Set;
      end record; -- Results

      type Result_Arrays is array (Targets, Door_Keys) of Results;

      package Caches is new
        Ada.Containers.Ordered_Maps (Unbounded_String, Natural);
      use Caches;

      subtype Quadrents is Natural range 0 .. 3;
      type Robots is record
         Target : Target_Arrays;
         Required : Key_Sets.Set;
         Result : Result_Arrays;
      end record; -- Robots
      type Robot_Arrays is array (Quadrents) of Robots;
      type Robot_Positions is array (Quadrents) of Targets;

      procedure Read_Ducts (Input_File : in File_Type;
                            Duct : out Ducts;
                            Target_Array : out Target_Arrays;
                            Required_Keys : out Key_Sets.Set) is

         Ch : Character;

      begin -- Read_Ducts
         Required_Keys := Key_Sets.Empty_Set;
         for Y in Y_Coordinates loop
            for X in X_Coordinates loop
               Get (Input_File, Ch);
               if Ch = '@' then
                  Ch := Start_Ch;
                  -- replace "@" with '`'
                  Target_Array (Ch).X := X;
                  Target_Array (Ch).Y := Y;
               elsif Ch in Door_Keys then
                  Include (Required_Keys, Ch);
                  Target_Array (Ch).X := X;
                  Target_Array (Ch).Y := Y;
               end if; -- Ch = '@'
               Duct (X, Y).Duct_Character := Ch;
            end loop; -- X in X_Coordinates
            Skip_Line (Input_File);
         end loop; -- Y in Y_Coordinates
      end Read_Ducts;

      procedure Part_Two_Mod (Duct : in out Ducts;
                              Robot_Array : out Robot_Arrays) is

         procedure Find_Keys (Duct : in Ducts;
                              Q : in Quadrents;
                              Xl, Xh : in X_Coordinates;
                              Yl, Yh : in Y_Coordinates;
                              Robot : in out Robot_Arrays) is
         begin -- Find_Keys
            for Y in Y_Coordinates range Yl .. Yh loop
               for X in X_Coordinates range Xl .. Xh loop
                  if Duct (X, Y).Duct_Character in Targets then
                     Robot (Q).Target (Duct (X, Y).Duct_Character).X := X;
                     Robot (Q).Target (Duct (X, Y).Duct_Character).Y := Y;
                  end if; -- Duct (X, Y).Duct_Character in Targets
                  if Duct (X, Y).Duct_Character in Door_Keys then
                     Include (Robot (Q).Required, Duct (X, Y).Duct_Character);
                  end if; -- Duct (X, Y).Duct_Character in Door_Keys
               end loop; -- X in X_Coordinates range Xl .. Xh
            end loop; -- Y in Y_Coordinates range Yl .. Yh
         end Find_Keys;

         Centre_X : constant X_Coordinates :=
           (X_Coordinates'First + X_Coordinates'Last) / 2;
         Centre_Y : constant Y_Coordinates :=
           (Y_Coordinates'First + Y_Coordinates'Last) / 2;

      begin -- Part_Two_Mod
         Duct (Centre_X - 1, Centre_Y).Duct_Character := Wall;
         Duct (Centre_X, Centre_Y - 1).Duct_Character := Wall;
         Duct (Centre_X, Centre_Y).Duct_Character := Wall;
         Duct (Centre_X, Centre_Y + 1).Duct_Character := Wall;
         Duct (Centre_X + 1, Centre_Y).Duct_Character := Wall;
         Duct (Centre_X - 1, Centre_Y + 1).Duct_Character := Start_Ch;
         Duct (Centre_X + 1, Centre_Y + 1).Duct_Character := Start_Ch;
         Duct (Centre_X + 1, Centre_Y - 1).Duct_Character := Start_Ch;
         Duct (Centre_X - 1, Centre_Y - 1).Duct_Character := Start_Ch;
         Find_Keys (Duct, 0, X_Coordinates'First, Centre_X,
                    Y_Coordinates'First, Centre_Y, Robot_Array);
         Find_Keys (Duct, 1, Centre_X, X_Coordinates'Last,
                    Y_Coordinates'First, Centre_Y, Robot_Array);
         Find_Keys (Duct, 2, Centre_X, X_Coordinates'Last,
                    Centre_Y, Y_Coordinates'Last, Robot_Array);
         Find_Keys (Duct, 3, X_Coordinates'First, Centre_X,
                    Centre_Y, Y_Coordinates'Last, Robot_Array);
      end Part_Two_Mod;

      procedure Put (Duct : in Ducts) is

      begin -- Put
         for Y in Y_Coordinates loop
            for X in X_Coordinates loop
               Put (Duct (X, Y).Duct_Character);
            end loop; -- X in X_Coordinates
            New_Line;
         end loop; -- Y in Y_Coordinates
      end Put;

      procedure Search (Duct_In : in Ducts;
                        Target_Array : in Target_Arrays;
                        Start : in Targets;
                        To_Find : in Door_Keys;
                        Keys_Held : in Key_Sets.Set;
                        Potential_Keys : out Key_Sets.Set;
                        Distance : out Natural) is

         Type Search_Element is record
            X : X_Coordinates;
            Y : Y_Coordinates;
            Distance : Natural;
         end record; -- Search_Element

         package SQI is new
           Ada.Containers.Synchronized_Queue_Interfaces (Search_Element);

         package Queues is new
           Ada.Containers.Unbounded_Synchronized_Queues (SQI);

         function Continue (Duct : in Ducts;
                            Test_X : in X_Coordinates;
                            Test_Y : in Y_Coordinates;
                            Test_D : in Natural;
                            Keys_Held : in Key_Sets.Set) return Boolean is

         begin -- Continue
            return (Duct (Test_X, Test_Y).Duct_Character = Path or
                      Duct (Test_X, Test_Y).Duct_Character in Targets or
                      (Duct (Test_X, Test_Y).Duct_Character in Doors and then
                       Contains (Keys_Held, To_Lower
                         (Duct (Test_X, Test_Y).Duct_Character)))) and
              not Duct (Test_X, Test_Y).Bread_Crumb;
         end Continue;

         Duct : Ducts := Duct_In;
         Queue : Queues.Queue;
         Check_This : Search_Element;
         X : X_Coordinates;
         Y : Y_Coordinates;
         D : Natural;

      begin -- Search
         Distance := Natural'Last;
         Potential_Keys := Key_Sets.Empty_Set;
         Queue.Enqueue ((Target_Array (Start).X, Target_Array (Start).Y, 0));
         while Queue.Current_Use > 0 loop
            Queue.Dequeue (Check_This);
            X := Check_This.X;
            Y := Check_This.Y;
            D := Check_This.Distance;
            if X = Target_Array (To_Find).X and
              Y = Target_Array (To_Find).Y then
               Distance := D;
               exit;
            end if; -- Found
            if Duct (X, Y).Duct_Character in Doors then
               -- N.B. this does not mean that the door is in the route between
               -- Start and To_Find, just that it is at a lesser distance from
               -- Start than To_Find.
               Include (Potential_Keys, To_Lower (Duct (X, Y).Duct_Character));
            end if; -- Duct (X, Y).Duct_Character in Door_Keys and then ...
            if Continue (Duct, X - 1, Y, D + 1, Keys_Held)
            then
               Queue.Enqueue ((X - 1, Y, D + 1));
               Duct (X - 1, Y).Bread_Crumb := True;
            end if; -- Continue ...
            if Continue (Duct, X + 1, Y, D + 1, Keys_Held)
            then
               Queue.Enqueue ((X + 1, Y, D + 1));
               Duct (X + 1, Y).Bread_Crumb := True;
            end if; -- Continue ...
            if Continue (Duct, X, Y - 1, D + 1, Keys_Held)
            then
               Queue.Enqueue ((X, Y - 1, D + 1));
               Duct (X, Y - 1).Bread_Crumb := True;
            end if; -- Continue ...
            if Continue (Duct, X, Y + 1, D + 1, Keys_Held)
            then
               Queue.Enqueue ((X, Y + 1, D + 1));
               Duct (X, Y + 1).Bread_Crumb := True;
            end if; -- Continue ...
         end loop; -- Queue.Current_Use > 0
      end Search;

      procedure Build_Results (Duct : in Ducts;
                               Required_Keys, Quadrent_Keys : in Key_Sets.Set;
                               Target_Array : in Target_Arrays;
                               Result_Array : out Result_Arrays) is

         Test_Keys, Junk: Key_Sets.Set;
         Test_Distance : Natural;

      begin -- Build_Results
         for Start in Targets loop
            if Start = Start_Ch or else Contains (Quadrent_Keys, Start) then
               for To_Find in Door_Keys range
                 Character'Succ (Start) .. Door_Keys'Last loop
                  if Contains (Quadrent_Keys, To_Find) then
                     Search (Duct, Target_Array, Start, To_Find, Required_Keys,
                             Result_Array (Start, To_Find).Required_Keys,
                             Result_Array (Start, To_Find).Distance);
                     Assert (Result_Array (Start, To_Find).Distance <
                               Natural'Last, "Not found with all keys");
                     -- Eliminate keys that are not on the shortest path by
                     -- testing if the minimum distance increases if a
                     -- particular key is not held
                     for K in Door_Keys loop
                        if Contains
                          (Result_Array (Start, To_Find).Required_Keys, K) then
                           Test_Keys :=
                             Copy (Result_Array (Start, To_Find).Required_Keys);
                           Delete (Test_Keys, K);
                           Search (Duct, Target_Array, Start, To_Find,
                                   Test_Keys, Junk, Test_Distance);
                           if Test_Distance =
                             Result_Array (Start, To_Find).Distance then
                              -- key was not required
                              Delete
                                (Result_Array (Start, To_Find).Required_Keys,
                                 K);
                           end if; -- Test_Distance = ...
                        end if; -- Contains ...
                     end loop; -- K in Door_Keys
                     -- Create the reverse lookup
                     if Start /= Start_Ch then
                        Result_Array (To_Find, Start) :=
                          Result_Array (Start, To_Find);
                     end if; -- Start /= Start_Ch
                  end if; -- Contains (Required_Keys, To_Find)
               end loop; -- To_Find in Door_Keys range ...
            end if; -- Start = Start_Ch or else Contains (Required_Keys, Start)
         end loop; -- Start in Targets loop
         -- Remove the entrance from the required key list, this is necessary
         -- because the reversal of entrance and exit will make what was once an
         -- exit now an entrance.
         for Start in Door_Keys loop
            for To_Find in Door_Keys loop
               if Start /= To_Find and then Contains
                 (Result_Array (Start, To_Find).Required_Keys, Start) then
                  Delete (Result_Array (Start, To_Find).Required_Keys, Start);
               end if; -- Start /= To_Find and then Contains ..
            end loop; -- To_Find in Door_Keys
         end loop; -- To_Find in Door_Keys
      end Build_Results;

      Hits : Natural := 0;

      procedure Concatinated_Search (Start : in Targets;
                                     Result_Array : in Result_Arrays;
                                     Keys_Held, Required_Keys : in Key_Sets.Set;
                                     Distance : in Natural;
                                     Saved_Distance : in out Natural;
                                     Cache : in out Caches.Map;
                                     Saved_N : in out Natural) is

         Test_Keys, Remaining_Keys : Key_Sets.Set;
         Test_String : Unbounded_String;
         Next_Distance : Natural;

      begin -- Concatinated_Search
         if Keys_Held = Required_Keys then
            if Distance < Saved_Distance then
               Saved_Distance := Distance;
            end if; -- Distance < Saved_Distance
            if Distance < Saved_N then
               Saved_N := Distance;
            end if; -- Distance < Saved_N
         else
            for K in Iterate (Required_Keys - Keys_Held) loop
               Test_Keys := Copy (Keys_Held);
               Include (Test_Keys, Element (K));
               Remaining_Keys := Required_Keys - Test_Keys;
               if Is_Subset (Result_Array (Start, Element (K)).Required_Keys,
                             Test_Keys)  then
                  -- Tests that sufficient keys are held to get from the
                  -- current position to entrance position for the next level
                  -- search. This test should ensure that a failed search will
                  -- terminate here.
                  Next_Distance := Distance +
                    Result_Array (Start, Element (K)).Distance;
                  Test_String := Null_Unbounded_String;
                  Test_String := Test_String & Element (K);
                  for Rc in Iterate (Remaining_Keys) loop
                     Test_String := Test_String & Element (Rc);
                  end loop; -- Rc in Iterate (Remaining_Keys)
                  if Contains (Cache, Test_String) then
                     Hits := Hits + 1;
                     -- The call below is guaranteed to terminate search and
                     -- save results, since the Keys_Held = Required_Keys.
                     Concatinated_Search (Element (K),
                                          Result_Array,
                                          Required_Keys,
                                          Required_Keys,
                                          Next_Distance +
                                            Cache (Test_String),
                                          Saved_Distance,
                                          Cache,
                                          Saved_N);
                  else
                     Saved_N := Natural'Last;
                     Concatinated_Search (Element (K),
                                          Result_Array,
                                          Test_Keys,
                                          Required_Keys,
                                          Next_Distance,
                                          Saved_Distance,
                                          Cache,
                                          Saved_N);
                     if Saved_N /= Natural'Last then
                        Include (Cache, Test_String,
                                 Saved_N - Next_Distance);
                     else
                        Assert (False, "Search failed with correct key set");
                     end if; -- Saved_N /= Natural'Last
                  end if; -- Contains (Cache, Test_String)
               end if; -- Is_Subset (Result_Array (Start, ...
            end loop; -- K in Iterate (Required_Keys - Keys_Held)
         end if; -- Keys_Held = Required_Keys
      end Concatinated_Search;

      procedure Part_Two_Search (Robot_Position_In : in Robot_Positions;
                                 Robot : in Robot_Arrays;
                                 Keys_Held, Required_Keys : in Key_Sets.Set;
                                 Distance : in Natural;
                                 Saved_Distance : in out Natural;
                                 Cache : in out Caches.Map;
                                 Saved_N : in out Natural) is

         function Select_Robot (Robot : in Robot_Arrays;
                                Target : in Targets) return Quadrents is

            Result : Quadrents;

         begin -- Select_Robot
            for Q in Quadrents loop
               if Contains (Robot (Q).Required, Target) then
                  Result := Q;
               end if; -- Contains (Robot (Q).Required, Target)
            end loop; -- Q in Quadrents
            return Result;
         end Select_Robot;

         Test_Keys, Remaining_Keys : Key_Sets.Set;
         Test_String : Unbounded_String;
         Next_Distance : Natural;
         Robot_Position : Robot_Positions;
         Q : Quadrents;

      begin -- Part_Two_Search
         if Keys_Held = Required_Keys then
            if Distance < Saved_Distance then
               Saved_Distance := Distance;
            end if; -- Distance < Saved_Distance
            if Distance < Saved_N then
               Saved_N := Distance;
            end if; -- Distance < Saved_N
         else
            for K in Iterate (Required_Keys - Keys_Held) loop
               Q := Select_Robot (Robot, Element (K));
               Test_Keys := Copy (Keys_Held);
               Include (Test_Keys, Element (K));
               Remaining_Keys := Required_Keys - Test_Keys;
               if Is_Subset (Robot (Q).Result (Robot_Position_In(Q),
                             Element (K)).Required_Keys, Test_Keys)  then
                  -- Tests that sufficient keys are held to get from the
                  -- current position to entrance position for the next level
                  -- search. This test should ensure that a failed search will
                  -- terminate here.
                  Next_Distance := Distance +
                    Robot (Q).Result (Robot_Position_In (Q),
                                      Element (K)).Distance;
                  Robot_Position := Robot_Position_In;
                  Robot_Position (Q) := Element (K);
                  Test_String := Null_Unbounded_String;
                  for R in Quadrents loop
                     Test_String := Test_String & Robot_Position_In (R);
                  end loop; -- R in Quadrents
                  Test_String := Test_String & Element (K);
                  for Rc in Iterate (Remaining_Keys) loop
                     Test_String := Test_String & Element (Rc);
                  end loop; -- Rc in Iterate (Remaining_Keys)
                  if Contains (Cache, Test_String) then
                     Hits := Hits + 1;
                     Part_Two_Search (Robot_Position,
                                      Robot,
                                      Required_Keys,
                                      Required_Keys,
                                      Next_Distance +
                                        Cache (Test_String),
                                      Saved_Distance,
                                      Cache,
                                      Saved_N);
                  else
                     Saved_N := Natural'Last;
                     Part_Two_Search (Robot_Position,
                                      Robot,
                                      Test_Keys,
                                      Required_Keys,
                                      Next_Distance,
                                      Saved_Distance,
                                      Cache,
                                      Saved_N);
                     if Saved_N /= Natural'Last then
                        Include (Cache, Test_String,
                                 Saved_N - Next_Distance);
                     else
                        Assert (False, "Search failed with correct key set");
                     end if; -- Saved_N /= Natural'Last
                  end if; -- Contains (Cache, Test_String)
               end if; --  Is_Subset (Robot (Q).Result (Robot_Position (Q) ...
            end loop; -- K in Iterate (Required_Keys - Keys_Held)
         end if; -- Keys_Held = Required_Keys
      end Part_Two_Search;

      Duct : Ducts;
      Target_Array : Target_Arrays;
      Required_Keys : Key_Sets.Set;
      Result_Array : Result_Arrays;
      Saved_Distance, Saved_N : Natural := Natural'Last;
      Cache : Caches.Map := Caches.Empty_Map;
      Robot : Robot_Arrays;

   begin -- Find_Solution
      Read_Ducts (Input_File, Duct, Target_Array, Required_Keys);
      Put (Duct);
      Build_Results (Duct, Required_Keys, Required_Keys, Target_Array,
                     Result_Array);
      Concatinated_Search (Start_Ch,
                           Result_Array,
                           Key_Sets.Empty_Set,
                           Required_Keys,
                           0,
                           Saved_Distance,
                           Cache,
                           Saved_N);
      Put_Line ("Shortest distance:" & Natural'Image (Saved_Distance));
      Put_Line ("Cache " & Count_Type'Image (Length (Cache)) &
                  " Hits:" & Natural'Image (Hits));
      Part_Two_Mod (Duct, Robot);
      Put (Duct);
      for Q in Quadrents loop
         Build_Results (Duct, Required_Keys,  Robot (Q).Required,
                        Robot (Q).Target, Robot (Q).Result);
      end loop; -- Q in Quadrents
      Cache := Caches.Empty_Map;
      Saved_Distance := Natural'Last;
      Saved_N := Natural'Last;
      Part_Two_Search ((Start_Ch, Start_Ch, Start_Ch, Start_Ch),
                       Robot,
                       Key_Sets.Empty_Set,
                       Required_Keys,
                       0,
                       Saved_Distance,
                       Cache,
                       Saved_N);
      Put_Line ("Part two distance:" & Natural'Image (Saved_Distance));
      Put_Line ("Cache " & Count_Type'Image (Length (Cache)) &
                  " Hits:" & Natural'Image (Hits));
   end Find_Solution;

begin -- December_18
   Open (Input_File, In_File, "December_18.txt");
   Find_Limits (X_Limit, Y_Limit);
   Reset (Input_File);
   Find_Solution (Input_File, X_Limit, Y_Limit);
   Close (Input_File);
end December_18;
