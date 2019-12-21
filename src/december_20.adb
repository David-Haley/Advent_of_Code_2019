with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

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
      subtype Portal_IDs is Boolean;

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
      end record; -- Portal_Coordinates

      type Teleports is array (Portal_IDs) of Portal_Coordinates;

      package Teleport_Maps is new
        Ada.Containers.Ordered_Maps (Teleport_Names, Teleports);
      use Teleport_Maps;

      type Distances is array (Portal_Characters, Portal_Characters)
        of Positive;

      package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);
      use Positive_IO;

      procedure Read_Ducts (Input_File : in File_Type; Duct : out Ducts) is

      begin -- Read_Ducts
         for Y in Y_Coordinates loop
            for X in X_Coordinates loop
               Get (Input_File, Duct (X, Y).Duct_Character);
            end loop; --X in X_Coordinates
            Skip_Line (Input_File);
         end loop; -- Y in Y_Coordinates
      end Read_Ducts;

      procedure Build_Teleport_Map (Duct : in out Ducts;
                                    Teleport_Map : in out Teleport_Maps.Map) is

         Portal_Coordinate : Portal_Coordinates;
         Teleport_Name : Teleport_Names;
         Teleport_Cursor : Teleport_Maps.Cursor;
         Portal_ID : Portal_IDs;
         Teleport : Teleports;
         Teleport_Found : Boolean;

      begin -- Build_Teleport_Map
         Teleport_Map := Teleport_Maps.Empty_Map;
         -- The construct of the for loops guarantees that X + 1, X - 1, Y + 1
         -- and Y - 1 are all within the Duct attay bounds.
         -- it is assumes that all teleport names either read left to reigt or
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
                  elsif Duct (X, Y + 1).Duct_Character in Portal_Characters and
                    Duct (X, Y - 1).Duct_Character = Path then
                     -- Bottom label
                     Teleport_Name := (Duct (X, Y).Duct_Character,
                                       Duct (X, Y + 1).Duct_Character);
                     Duct (X, Y - 1).Is_Portal := True;
                     Duct (X, Y - 1).Teleport_Name := Teleport_Name;
                     Portal_Coordinate := (X, Y - 1);
                     Teleport_Found := True;
                  end if; -- label orientation
               end if; -- Duct (X, Y) in Portal_Characters
               if Teleport_Found then
                  Teleport_Cursor := Find (Teleport_Map, Teleport_Name);
                  Portal_ID := Teleport_Maps.No_Element /= Teleport_Cursor;
                  -- Portal_ID is True when the second portal is found
                  if Portal_ID then
                     Teleport_Map (Teleport_Cursor) (Portal_ID) :=
                       Portal_Coordinate;
                  else
                     Teleport (Portal_ID) := Portal_Coordinate;
                     Include (Teleport_Map, Teleport_Name, Teleport);
                  end if; -- Portal_ID
               end if; -- Teleport_Found
            end loop; -- X in X_Coordinates range ...
         end loop; -- Y in Y_Coordinates range
      end Build_Teleport_Map;

      procedure Find_Target (Start, To_Find : Portal_Coordinates;
                             Duct : in out Ducts;
                             Teleport_Map : in Teleport_Maps.Map;
                             Distance : out Natural) is

         -- Search assume that there is a full boundary of Wall characters thus
         -- ensuring that X + 1, X - 1, Y + 1 and Y - 1 will always be within
         -- the bounds of Duct

         type Queue_Elements is record
            X : X_Coordinates;
            Y : Y_Coordinates;
            Distance : Natural;
         end record; -- Queue_Elements

         package QI is new
           Ada.Containers.Synchronized_Queue_Interfaces (Queue_Elements);

         package Queues is new
           Ada.Containers.Unbounded_Synchronized_Queues (QI);
         -- The bound placed on the queue is the absolute worst case, that
         -- is, every Path element of Duct is being examined. Note if the
         -- queue bound is too small this will not raise an exception,
         -- Enqueue will block and the search will fail in the blocked state,
         -- that is, no progress.

         procedure Do_Teleport (Teleport_Map : in Teleport_Maps.Map;
                                X_In : in X_Coordinates;
                                Y_In : in Y_Coordinates;
                                To_Find : in Portal_Coordinates;
                                X_Out : out X_Coordinates;
                                Y_Out : out Y_Coordinates;
                                Distance : in out Natural) is

            Teleport_Cursor : Teleport_Maps.Cursor;
            Portal_ID : Portal_IDs;

         begin -- Do_Teleport
            if X_In = To_Find.X and Y_In = To_Find.Y then
               -- This is the end of search, not a portal
               X_Out := X_In;
               Y_Out := Y_In;
               Distance := Distance + 1;
               -- matches the non portal case
            else
               Teleport_Cursor := Find (Teleport_Map,
                                        Duct (X_In, Y_In).Teleport_Name);
               Portal_ID := X_In = Teleport_Map (Teleport_Cursor) (False).X and
                 Y_In = Teleport_Map (Teleport_Cursor) (False).Y;
               X_Out := Teleport_Map (Teleport_Cursor) (Portal_ID).X;
               Y_Out := Teleport_Map (Teleport_Cursor) (Portal_ID).Y;
               Assert (Duct (X_In, Y_In).Teleport_Name =
                         Duct (X_Out, Y_Out).Teleport_Name and
                         X_In /= X_Out and Y_In /= Y_Out,
                       Duct (X_In, Y_In).Teleport_Name &" Failed Teleport " &
                         Duct (X_Out, Y_Out).Teleport_Name);
               Distance := Distance + 2;
               -- has to be + 1 in the non teleport case, hence + 2
            end if; -- X_In = To_Find.X and Y_In = To_Find.Y
         end Do_Teleport;

         Queue : Queues.Queue;
         Queue_Element : Queue_Elements;
         X : X_Coordinates;
         Y : Y_Coordinates;

      begin -- Find_Target
         Queue_Element := (X => Start.X, Y=> Start.Y, Distance => 0);
         Queue.Enqueue (Queue_Element);
         Duct (Start.X, Start.Y).Bread_Crumb := True;
         while Queue.Current_Use > 0 loop
            Queue.Dequeue (Queue_Element);
            X := Queue_Element.X;
            Y := Queue_Element.Y;
            Distance :=  Queue_Element.Distance;
            exit when X = To_Find.X and Y = To_Find.Y;
            if Duct (X - 1, Y).Duct_Character = Path and then
              not Duct (X - 1, Y).Bread_Crumb then
               if Duct (X - 1, Y).Is_Portal then
                  Duct (X - 1, Y).Bread_Crumb := True;
                  Do_Teleport (Teleport_Map, X - 1, Y, To_Find, X, Y, Distance);
                  Queue.Enqueue ((X, Y, Distance));
                  Duct (X, Y).Bread_Crumb := True;
               else
                  Queue.Enqueue ((X - 1, Y, Distance + 1));
                  Duct (X - 1, Y).Bread_Crumb := True;
               end if; -- Duct (X - 1, Y).Is_Portal
            end if; -- search left
            if Duct (X, Y - 1).Duct_Character = Path and then
              not Duct (X, Y - 1).Bread_Crumb then
               if Duct (X, Y - 1).Is_Portal then
                  Duct (X, Y - 1).Bread_Crumb := True;
                  Do_Teleport (Teleport_Map, X, Y - 1, To_Find, X, Y, Distance);
                  Queue.Enqueue ((X, Y, Distance));
                  Duct (X, Y).Bread_Crumb := True;
               else
                  Queue.Enqueue ((X, Y - 1, Distance + 1));
                  Duct (X, Y - 1).Bread_Crumb := True;
               end if; -- Duct (X, Y - 1).Is_Portal
            end if; -- search up
            if Duct (X + 1, Y).Duct_Character = Path and then
              not Duct (X + 1, Y).Bread_Crumb then
               if Duct (X + 1, Y).Is_Portal then
                  Duct (X + 1, Y).Bread_Crumb := True;
                  Do_Teleport (Teleport_Map, X + 1, Y, To_Find, X, Y, Distance);
                  Queue.Enqueue ((X, Y, Distance));
                  Duct (X, Y).Bread_Crumb := True;
               else
                  Queue.Enqueue ((X + 1, Y, Distance + 1));
                  Duct (X + 1, Y).Bread_Crumb := True;
               end if; -- Duct (X + 1, Y).Is_Portal
            end if; -- search right
            if Duct (X, Y + 1).Duct_Character = Path and then
              not Duct (X, Y + 1).Bread_Crumb then
               if Duct (X, Y + 1).Is_Portal then
                  Duct (X, Y + 1).Bread_Crumb := True;
                  Do_Teleport (Teleport_Map, X, Y + 1, To_Find, X, Y, Distance);
                  Queue.Enqueue ((X, Y, Distance));
                  Duct (X, Y).Bread_Crumb := True;
               else
                  Queue.Enqueue ((X, Y + 1, Distance + 1));
                  Duct (X, Y + 1).Bread_Crumb := True;
               end if; -- Duct (X, Y + 1).Is_Portal then
            end if; -- search down
         end loop; -- Current_Use (Queue) > 0
         -- clean up, reset Bread_Crumb
         for X in X_Coordinates loop
            for Y in Y_Coordinates loop
               Duct (X, Y).Bread_Crumb := False;
            end loop; -- Y in Y_Coordinates
         end loop; -- X in X_Coordinates
      end Find_Target;

      Duct : Ducts;
      Start, To_Find : Portal_Coordinates;
      Teleport_Map : Teleport_Maps.Map;
      Distance : Natural;

   begin -- Find_Solution
      Read_Ducts (Input_File, Duct);
      Build_Teleport_Map (Duct, Teleport_Map);
      Start := Element (Teleport_Map, "AA") (False);
      To_Find := Element (Teleport_Map, "ZZ") (False);
      Find_Target (Start, To_Find, Duct, Teleport_Map, Distance);
      Put_Line ("Part One Distance:" & Natural'Image (Distance));
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
