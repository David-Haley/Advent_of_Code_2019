-- It took an inordinatly long time to get part two. I had a solution which
-- could solve the examples and another persons imput but not mine. It would
-- appear that the solution to my imput involves decending through a path used
-- during the ascent. This precludes the simple mark the elements that have been
-- searched and don't go there again approach. After the success of the part two
-- solution I applied it to part one which results in simpler code and much
-- faster execution. The solution is to apply a BFS to the concatination of the
-- connections between portals. Since most portals are only connected to one
-- other there are very few paths to check. The use of a priority queue is
-- essential because the lengths of the connectionss between portals vary and
-- the shortest has to be found first. The solution presented here can solve
-- both parts in under 3s!

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Unbounded_Priority_Queues;

procedure December_20 is

   Path : constant Character := '.';
   Wall : constant Character := '#';
   Void : constant Character := ' ';
   subtype Duct_Characters is Character with Static_Predicate =>
     Duct_Characters in Path | Wall | Void | 'A' .. 'Z';
   subtype Portal_Characters is Character range 'A' .. 'Z';

   procedure Find_Limits (Input_File : in File_Type;
                          X_Limit, Y_Limit : out Positive) is

      Text : Unbounded_String;

   begin -- Find_Limits
      X_Limit := 1;
      while not End_Of_File (Input_File) loop
         Y_Limit := Positive (Line (Input_File));
         Get_Line (Input_File, Text);
         if X_Limit < Length (Text) then
            X_Limit := Length (Text);
         end if; -- X_Limit > Length (Text)
      end loop; -- not End_Of_File (Input_File)
   end Find_Limits;

   procedure Find_Solution (Input_File : in File_Type;
                            X_Limit, Y_Limit : in Positive) is

      subtype X_Coordinates is Positive range 1 .. X_Limit;
      subtype Y_Coordinates is Positive range 1 .. Y_Limit;

      subtype Teleport_Names is String (1 .. 2);
      Type Portal_IDs is (Inside, Outside);

      Start_Name : constant Teleport_Names := "AA";
      End_Name : constant Teleport_Names := "ZZ";

      type Duct_Elements is record
         Duct_Character : Duct_Characters;
         Bread_Crumb : Boolean := False;
         Is_Portal : Boolean := False;
         Teleport_Name : Teleport_Names := "  ";
      end record; -- Duct_Elements

      type Ducts is array (X_Coordinates, Y_Coordinates) of Duct_Elements;

      type Portal_Coordinates is record
         X : X_Coordinates := 1;
         Y : Y_Coordinates := 1;
      end record; --_Coordinates

      Type Portals is array (Portal_IDs) of Portal_Coordinates;

      package Teleport_Maps is new
        Ada.Containers.Ordered_Maps (Teleport_Names, Portals);
      use Teleport_Maps;

      type Connection_Keys is record
         Teleport_Name : Teleport_Names;
         Portal_ID : Portal_IDs;
      end record; -- Conection_Keys

      type Connections is record
         Teleport_Name : Teleport_Names;
         Portal_ID : Portal_IDs;
         Distance : Natural;
      end record; -- Connections

      package Connection_Lists is new
        Ada.Containers.Vectors (Natural, Connections);
      use Connection_Lists;

      function "<" (Left, Right : in Connection_Keys) return Boolean is

      begin -- "<"
         if Left.Teleport_Name < Right.Teleport_Name then
            return True;
         elsif Left.Teleport_Name = Right.Teleport_Name then
            return Left.Portal_ID < Right.Portal_ID;
         else
            return False;
         end if; -- Left.Teleport_Name < Right.Teleport_Name
      end "<";

      function "=" (Left, Right : in Connection_Lists.Vector) return Boolean is

      begin -- "="
         return Connection_Lists."=" (Left, Right);
      end "=";

      package Connection_Maps is new
        Ada.Containers.Ordered_Maps (Connection_Keys, Connection_Lists.Vector);
      use Connection_Maps;

      procedure Read_Ducts (Input_File : in File_Type; Duct : out Ducts) is

         procedure Deadend (Duct : in out Ducts) is

            Count : Natural;
            All_Blocked : Boolean;

         begin -- Deadend
            loop -- more to block
               All_Blocked := True;
               for Y in Y_Coordinates range
                 Y_Coordinates'First + 1 .. Y_Coordinates'Last - 1 loop
                  for X in X_Coordinates range
                    X_Coordinates'First + 1 .. X_Coordinates'Last - 1 loop
                     if Duct (X, Y).Duct_Character = Path then
                        Count := 0;
                        if Duct (X - 1, Y).Duct_Character = Wall then
                           Count := Count + 1;
                        end if; --  Duct (X - 1, Y).Duct_Character = Wall
                        if Duct (X + 1, Y).Duct_Character = Wall then
                           Count := Count + 1;
                        end if; -- Duct (X + 1, Y).Duct_Character = Wall
                        if Duct (X, Y - 1).Duct_Character = Wall then
                           Count := Count + 1;
                        end if; -- Duct (X, Y - 1).Duct_Character = Wall
                        if Duct (X, Y + 1).Duct_Character = Wall then
                           Count := Count + 1;
                        end if; -- Duct (X, Y + 1).Duct_Character = Wall
                        if Count = 3 then
                           Duct (X, Y).Duct_Character := Wall;
                           All_Blocked := False;
                        end if; -- Count = 3
                     end if; -- Duct (X, Y).Duct_Character = Path
                  end loop; -- X in X_Coordinates range ...
               end loop; -- Y in Y_Coordinates range ...
               exit when All_Blocked;
            end loop; -- more to block
         end Deadend;

      begin -- Read_Ducts
         for Y in Y_Coordinates loop
            for X in X_Coordinates loop
               Get (Input_File, Duct (X, Y).Duct_Character);
            end loop; -- X in X_Coordinates
            Skip_Line (Input_File);
         end loop; -- Y in Y_Coordinates
         Deadend (Duct);
      end Read_Ducts;

      procedure Put (Duct : in Ducts) is

      begin -- Put
         for Y in Y_Coordinates loop
            for X in X_Coordinates loop
               Put (Duct (X, Y).Duct_Character);
            end loop; -- X in X_Coordinates
            New_Line;
         end loop; -- Y in Y_Coordinates
      end Put;

      procedure Build_Teleport_Map (Duct : in out Ducts;
                                    Teleport_Map : out Teleport_Maps.Map) is

         procedure Correct (Teleport_Map : in out Teleport_Maps.Map) is
            -- Identifies all the outside Portals and swaps the order if
            -- incorrect.

            Temp : Portal_Coordinates;
            X_Min : X_Coordinates := X_Coordinates'Last;
            X_Max : X_Coordinates := X_Coordinates'First;
            Y_Min : Y_Coordinates := Y_Coordinates'Last;
            Y_Max : Y_Coordinates := Y_Coordinates'First;

         begin -- Correct
            -- Find the coordinate limits of all the outside portals
            for I in Iterate (Teleport_Map) loop
               for P in Portal_IDs loop
                  if Teleport_Map (I) (P).X /= 1 and
                    Teleport_Map (I) (P).X < X_Min then
                     X_Min := Teleport_Map (I) (P).X;
                  end if; -- Smaller but not default
                  if Teleport_Map (I) (P).X > X_Max then
                     X_Max := Teleport_Map (I) (P).X;
                  end if; -- Teleport_Map (I) (P).X > X_Max
                  if Teleport_Map (I) (P).Y /= 1 and
                    Teleport_Map (I) (P).Y < Y_Min then
                     Y_Min := Teleport_Map (I) (P).Y;
                  end if; -- Smaller but not default
                  if Teleport_Map (I) (P).Y > Y_Max then
                     Y_Max := Teleport_Map (I) (P).Y;
                  end if; -- Teleport_Map (I) (P).Y > Y_Max
               end loop; -- P in_IDs
            end loop; -- I in Iterate (Teleport_Map)
            for I in Iterate (Teleport_Map) loop
               if Teleport_Map (I) (Inside).X = X_Min or
                 Teleport_Map (I) (Inside).X = X_Max or
                 Teleport_Map (I) (Inside).Y = Y_Min or
                 Teleport_Map (I) (Inside).Y = Y_Max then
                  Temp := Teleport_Map (I) (Inside);
                  Teleport_Map (I) (Inside) :=
                    Teleport_Map (I) (Outside);
                  Teleport_Map (I) (Outside) := Temp;
               end if; -- Swap Inside to Outside is rewuired
            end loop; -- I in Iterate (Teleport_Map)
         end Correct;

         Portal_Coordinate : Portal_Coordinates;
         Teleport_Name : Teleport_Names;
         Teleport_Cursor : Teleport_Maps.Cursor;
         Portal : Portals;
         Teleport_Found : Boolean;

      begin -- Build_Teleport_Map
         Teleport_Map := Teleport_Maps.Empty_Map;
         -- The construct of the for loops guarantees that X + 1, X - 1, Y + 1
         -- and Y - 1 are all within the Duct array bounds.
         -- it is assumes that all teleport names either read left to right or
         -- down.
         for Y in Y_Coordinates range
           Y_Coordinates'First + 1 .. Y_Coordinates'Last - 1 loop
            for X in X_Coordinates range
              X_Coordinates'First + 1 .. X_Coordinates'Last - 1 loop
               Teleport_Found := False;
               if Duct (X, Y).Duct_Character in Portal_Characters then
                  if Duct (X - 1, Y).Duct_Character in Portal_Characters and
                    Duct (X + 1, Y).Duct_Character = Path then
                     -- Left lable
                     Teleport_Name := (Duct (X - 1, Y).Duct_Character,
                                       Duct (X, Y).Duct_Character);
                     Duct (X + 1, Y).Is_Portal := True;
                     Duct (X + 1, Y).Teleport_Name := Teleport_Name;
                     Portal_Coordinate := (X + 1, Y);
                     Teleport_Found := True;
                  elsif Duct (X + 1, Y).Duct_Character in Portal_Characters and
                    Duct (X - 1, Y).Duct_Character = Path then
                     -- Right label
                     Teleport_Name := (Duct (X, Y).Duct_Character,
                                       Duct (X + 1, Y).Duct_Character);
                     Duct (X - 1, Y).Is_Portal := True;
                     Duct (X - 1, Y).Teleport_Name := Teleport_Name;
                     Portal_Coordinate := (X - 1, Y);
                     Teleport_Found := True;
                  elsif Duct (X, Y - 1).Duct_Character in Portal_Characters and
                    Duct (X, Y + 1).Duct_Character = Path then
                     -- Top label
                     Teleport_Name := (Duct (X, Y - 1).Duct_Character,
                                       Duct (X, Y).Duct_Character);
                     Duct (X, Y + 1).Is_Portal := True;
                        Duct (X, Y + 1).Teleport_Name := Teleport_Name;
                        Portal_Coordinate := (X, Y + 1);
                        Teleport_Found := True;
                  elsif Duct (X, Y + 1).Duct_Character in Portal_Characters
                    and Duct (X, Y - 1).Duct_Character = Path then
                        -- Bottom label
                        Teleport_Name := (Duct (X, Y).Duct_Character,
                                          Duct (X, Y + 1).Duct_Character);
                        Duct (X, Y - 1).Is_Portal := True;
                           Duct (X, Y - 1).Teleport_Name := Teleport_Name;
                           Portal_Coordinate := (X, Y - 1);
                           Teleport_Found := True;
                        end if; -- label orientation
                     end if; -- Duct (X, Y) in_Characters
                     if Teleport_Found then
                        Teleport_Cursor := Find (Teleport_Map, Teleport_Name);
                        if Teleport_Maps.No_Element = Teleport_Cursor then
                           Portal (Inside) := Portal_Coordinate;
                           Include (Teleport_Map, Teleport_Name, Portal);
                        else
                           Teleport_Map (Teleport_Cursor) (Outside) :=
                             Portal_Coordinate;
                        end if; -- Teleport_Maps.No_Element = Teleport_Cursor
                        --_ID may be wrong now, first occurance goes in
                        ---Inside and second occurance goes in outside.
                     end if; -- Teleport_Found
                  end loop; -- X in X_Coordinates range ...
               end loop; -- Y in Y_Coordinates range
               Correct (Teleport_Map);
      end Build_Teleport_Map;

      procedure Find_Connections (Duct_In : in Ducts;
                                  Teleport_Map : in Teleport_Maps.Map;
                                  Connection_Map : out Connection_Maps.Map) is

         type Queue_Elements is record
            X : X_Coordinates;
            Y : Y_Coordinates;
            Distance : Natural;
         end record; -- Queue_Elements

         package QI is new
           Ada.Containers.Synchronized_Queue_Interfaces (Queue_Elements);

         package Queues is new
           Ada.Containers.Unbounded_Synchronized_Queues (QI);

         Duct : Ducts;
         Queue : Queues.Queue;
         Queue_Element : Queue_Elements;
         X : X_Coordinates;
         Y : Y_Coordinates;
         Distance : Natural;
         Tc : Teleport_Maps.Cursor;
         Current_Key : Connection_Keys;
         Current : Connections;

      begin -- Find_Connections
         Connection_Map := Connection_Maps.Empty_Map;
         for T in Iterate (Teleport_Map) loop
            for P in Portal_IDs loop
               if not (Key (T) = Start_Name or Key (T) = End_Name) or
                 P = Outside then
                  Current_Key.Teleport_Name := Key (T);
                  Current_Key.Portal_ID := P;
                  Duct := Duct_In;
                  -- Get a clean copy of the Duct and hence Bread_Crumbs at the
                  -- start of each search.
                  Queue.Enqueue ((Teleport_Map (T) (P).X,
                                 Teleport_Map (T) (P).Y, 0));
                  while Queue.Current_Use > 0 loop
                     Queue.Dequeue (Queue_Element);
                     X := Queue_Element.X;
                     Y := Queue_Element.Y;
                     Distance :=  Queue_Element.Distance;
                     Duct (X, Y).Bread_Crumb := True;
                     if Duct (X, Y).Is_Portal and
                       not (X = Teleport_Map (T) (P).X
                            and Y = Teleport_Map (T) (P).Y) then
                        Current.Teleport_Name := Duct (X, Y).Teleport_Name;
                        Tc := Find (Teleport_Map, Duct (X, Y).Teleport_Name);
                        if Teleport_Map (Tc) (Inside).X = X and
                          Teleport_Map (Tc) (Inside).Y = Y then
                           Current.Portal_ID := Inside;
                        elsif Teleport_Map (Tc) (Outside).X = X and
                          Teleport_Map (Tc) (Outside).Y = Y then
                           Current.Portal_ID := Outside;
                        else
                           Assert (False,"No Portal_ID Match");
                        end if; -- Teleport_Map (Tc) (Inside).X = X and ...
                        Current.Distance := Distance;
                        if not Contains (Connection_Map, Current_Key) then
                           Include (Connection_Map, Current_Key,
                             Connection_Lists.Empty_Vector);
                        end if; -- not Contains (Connection_Map, Current_Key)
                        Append (Connection_Map (Current_Key), Current);
                     end if; -- Duct (X, Y).Is_Portal add --
                     if Duct (X - 1, Y).Duct_Character = Path and then
                       not Duct (X - 1, Y).Bread_Crumb then
                        Queue.Enqueue ((X - 1, Y, Distance + 1));
                     end if; -- search left
                     if Duct (X, Y - 1).Duct_Character = Path and then
                       not Duct (X, Y - 1).Bread_Crumb then
                        Queue.Enqueue ((X, Y - 1, Distance + 1));
                     end if; -- search up
                     if Duct (X + 1, Y).Duct_Character = Path and then
                       not Duct (X + 1, Y).Bread_Crumb then
                        Queue.Enqueue ((X + 1, Y, Distance + 1));
                     end if; -- search right
                     if Duct (X, Y + 1).Duct_Character = Path and then
                       not Duct (X, Y + 1).Bread_Crumb then
                        Queue.Enqueue ((X, Y + 1, Distance + 1));
                     end if; -- search down
                  end loop; -- Current_Use (Queue) > 0
               end if; --  not (Key (T) = Start_Name or Key (T) = End_Name) or
            end loop; -- P in Portal_IDs
         end loop; -- T in Iterate (Teleport_Maps)
      end Find_Connections;

      procedure Part_One (Start, To_Find : in Teleport_Names;
                          Connection_Map : in Connection_Maps.Map;
                          Distance : out Natural) is

         type Queue_Elements is record
            Teleport_Name : Teleport_Names;
            Portal_ID : Portal_IDs;
            Distance : Natural;
         end record; -- Queue_Elements

         package QI is new
           Ada.Containers.Synchronized_Queue_Interfaces (Queue_Elements);

         function Get_Priority (Query_Element : in Queue_Elements)
                                return Natural is

         begin -- Get_Priority
            return Query_Element.Distance;
         end Get_Priority;

         function Before (Left, Right : in Natural) return Boolean is

         begin -- Before
            return Left < Right;
         end Before;

         package Queues is new Ada.Containers.Unbounded_Priority_Queues
           (Queue_Interfaces => QI,
            Queue_Priority => Natural,
            Get_Priority => Get_Priority,
            Before => Before);

         Current : Queue_Elements := (Start, Outside, 0);
         Queue : Queues.Queue;
         Connection_Key : Connection_Keys;

      begin -- Part_One
         Queue.Enqueue (Current);
         while Queue.Current_Use > 0 loop
            Queue.Dequeue (Current);
            if Current.Teleport_Name = To_Find then
               Distance := Current.Distance;
               exit;
            end if; -- Current.Teleport_Name = To_Find and Current.Level = 0
            Connection_Key.Teleport_Name := Current.Teleport_Name;
            Connection_Key.Portal_ID := Current.Portal_ID;
            for I in Iterate (Connection_Map (Connection_Key)) loop
               if Connection_Map (Connection_Key) (I).Portal_ID = Inside then
                  Queue.Enqueue
                    ((Connection_Map (Connection_Key) (I).Teleport_Name,
                     Outside,
                     Connection_Map (Connection_Key) (I).Distance + 1
                     -- + 1 for teleport
                     + Current.Distance));
               elsif Connection_Map (Connection_Key) (I).Portal_ID = Outside and
                 not (Connection_Map (Connection_Key) (I).Teleport_Name
                      = Start or
                        Connection_Map (Connection_Key) (I).Teleport_Name
                      = To_Find) then
                  Queue.Enqueue
                    ((Connection_Map (Connection_Key) (I).Teleport_Name,
                     Inside,
                     Connection_Map (Connection_Key) (I).Distance + 1
                     -- + 1 for teleport
                     + Current.Distance));
               elsif Connection_Map (Connection_Key) (I).Teleport_Name
                 = To_Find then
                  Queue.Enqueue
                    ((Connection_Map (Connection_Key) (I).Teleport_Name,
                     Connection_Map (Connection_Key) (I).Portal_ID,
                     Connection_Map (Connection_Key) (I).Distance +
                       Current.Distance));
               end if; -- Continue Search
            end loop; -- I in Iterate (Connection_Map (Connection_Key))
         end loop; -- Queue.Current_Use > 0
      end Part_One;

      procedure Part_Two (Start, To_Find : in Teleport_Names;
                           Connection_Map : in Connection_Maps.Map;
                           Distance : out Natural) is

         subtype Levels is Natural;

         type Queue_Elements is record
            Teleport_Name : Teleport_Names;
            Portal_ID : Portal_IDs;
            Level : Levels;
            Distance : Natural;
         end record; -- Queue_Elements

         package QI is new
           Ada.Containers.Synchronized_Queue_Interfaces (Queue_Elements);

         function Get_Priority (Query_Element : in Queue_Elements)
                                return Natural is

         begin -- Get_Priority
            return Query_Element.Distance;
         end Get_Priority;

         function Before (Left, Right : in Natural) return Boolean is

         begin -- Before
            return Left < Right;
         end Before;

         package Queues is new Ada.Containers.Unbounded_Priority_Queues
           (Queue_Interfaces => QI,
            Queue_Priority => Natural,
            Get_Priority => Get_Priority,
            Before => Before);

         Current : Queue_Elements := (Start, Outside, 0, 0);
         Queue : Queues.Queue;
         Connection_Key : Connection_Keys;

      begin -- Part_Two
         Queue.Enqueue (Current);
         while Queue.Current_Use > 0 loop
            Queue.Dequeue (Current);
            if Current.Teleport_Name = To_Find and Current.Level = 0 then
               Distance := Current.Distance;
               exit;
            end if; -- Current.Teleport_Name = To_Find and Current.Level = 0
            Connection_Key.Teleport_Name := Current.Teleport_Name;
            Connection_Key.Portal_ID := Current.Portal_ID;
            for I in Iterate (Connection_Map (Connection_Key)) loop
               if Connection_Map (Connection_Key) (I).Portal_ID = Inside then
                  Queue.Enqueue
                    ((Connection_Map (Connection_Key) (I).Teleport_Name,
                     Outside,
                     Current.Level + 1,
                     Connection_Map (Connection_Key) (I).Distance + 1
                     -- + 1 for teleport
                     + Current.Distance));
               elsif Current.Level > 0 and
                 Connection_Map (Connection_Key) (I).Portal_ID = Outside and
                 not (Connection_Map (Connection_Key) (I).Teleport_Name
                      = Start or
                        Connection_Map (Connection_Key) (I).Teleport_Name
                      = To_Find) then
                  Queue.Enqueue
                    ((Connection_Map (Connection_Key) (I).Teleport_Name,
                     Inside,
                     Current.Level - 1,
                     Connection_Map (Connection_Key) (I).Distance + 1
                     -- + 1 for teleport
                     + Current.Distance));
               elsif Current.Level = 0 and
                 Connection_Map (Connection_Key) (I).Teleport_Name
                 = To_Find then
                  Queue.Enqueue
                    ((Connection_Map (Connection_Key) (I).Teleport_Name,
                     Connection_Map (Connection_Key) (I).Portal_ID,
                     Current.Level,
                     Connection_Map (Connection_Key) (I).Distance +
                       Current.Distance));
               end if; -- Continue Search
            end loop; -- I in Iterate (Connection_Map (Connection_Key))
         end loop; -- Queue.Current_Use > 0
      end Part_Two;

      Duct : Ducts;
      Teleport_Map : Teleport_Maps.Map;
      Connection_Map : Connection_Maps.Map;
      Distance : Natural;

   begin -- Find_Solution
      Read_Ducts (Input_File, Duct);
      Put (Duct);
      Build_Teleport_Map (Duct, Teleport_Map);
      Find_Connections (Duct, Teleport_Map, Connection_Map);
      Part_One (Start_Name, End_Name, Connection_Map, Distance);
      Put_Line ("Part One Distance:" & Natural'Image (Distance));
      Part_Two (Start_Name, End_Name, Connection_Map, Distance);
      Put_Line ("Part Two Distance:" & Natural'Image (Distance));
   end Find_Solution;

   Input_File : File_Type;
   X_Limit, Y_Limit : Positive := 1;

begin -- December_20
   Open (Input_File, In_File, "December_20.txt");
   Find_Limits (Input_File, X_Limit, Y_Limit);
   Reset (Input_File);
   Find_Solution (Input_File, X_Limit, Y_Limit);
   Close (Input_File);
end December_20;
