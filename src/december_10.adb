with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with  Ada.Containers.Vectors;

procedure December_10 is

   Asteroid : constant Character := '#';
   Empty : constant Character := '.';
   Input_File_Name : constant String := "December_10.txt";

   type Quadrents is (Q1, Q2, Q3, Q4);

   type Gradients is record
      A, B : Integer;
      Quadrent : Quadrents := Q1;
   end record; -- Gradients

   type My_Real is digits (15);

   package Gradient_Lists is new Ada.Containers.Vectors (Natural, Gradients);
   use Gradient_Lists;

   function "<" (Left, Right : Gradients) return Boolean is

   begin -- "<"
      if Left.Quadrent < Right.Quadrent then
         return True;
      elsif Left.Quadrent > Right.Quadrent then
         return False;
      else
         -- both Left and Right in the same quadrent
         case Left.Quadrent is
            when Q1 =>
               if Left.A /= 0 and Right.A /= 0 then
                  return My_Real (abs (Left.B)) / My_Real (Left.A) >
                    My_Real (abs (Right.B)) / My_Real (Right.A);
               else
                  return Left.A = 0;
               end if; -- Left.A /= 0 and Right.A /= 0
            when Q2 =>
               if Left.A /= 0 and Right.A /= 0 then
                  return My_Real (Left.B) / My_Real (Left.A) <
                    My_Real (Right.B) / My_Real (Right.A);
               else
                  return not (Left.A = 0);
               end if; -- Left.A /= 0 and Right.A /= 0
            when Q3 =>
               if Left.A /= 0 and Right.A /= 0 then
                  return My_Real (Left.B) / My_Real (abs (Left.A)) >
                    My_Real (Right.B) / My_Real (abs (Right.A));
               else
                  return Left.A = 0;
               end if; -- Left.A /= 0 and Right.A /= 0
            when Q4 =>
               if Left.A /= 0 and Right.A /= 0 then
                  return My_Real (abs (Left.B)) / My_Real (abs (Left.A)) <
                    My_Real (abs (Right.B)) / My_Real (abs (Right.A));
               else
                  return not (Left.A = 0);
               end if; -- Left.A /= 0 and Right.A /= 0
         end case; -- Left.Quadrent
      end if; -- Left.Quadrent < Right.Quadrent
   end "<";

   package Gradient_Sorts is new Gradient_Lists.Generic_Sorting;
   use Gradient_Sorts;

   procedure Find_Size (X_Size, Y_Size : out Natural) is

      Input_File : File_Type;
      Ch : Character;
      Finished : Boolean := False;

   begin -- Find_Size
      X_Size := 0;
      Y_Size := 0;
      Open (Input_File, In_File, Input_File_Name);
      while not End_Of_Line (Input_File) loop
         Get (Input_File, Ch);
         Assert (Ch = Asteroid or Ch = Empty,
                 "Unexpected Character '" & Ch & "'");
         X_Size := X_Size + 1;
      end loop; -- not End_Of_Line (Input_File)
      while not End_Of_File (Input_File) and not Finished loop
         Y_Size := Y_Size + 1;
         Skip_Line (Input_File);
         if End_Of_Line (Input_File) then
            Finished := True;
         else
            Get (Input_File, Ch);
            Assert (Ch = Asteroid or Ch = Empty,
                    "Unexpected Character '" & Ch & "'");
            -- check first character is valid
         end if; -- End_Of_Line (Input_File)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Find_Size;

   function Used (Used_List : in out Gradient_Lists.Vector;
                  A, B : in Integer; Quadrent : in Quadrents := Q1)
                  return Boolean is
      -- Assumes that it is only being used in one quadrent at a time.
      -- That is the Used_List is empty at the search of each quadrent.

      Result : Boolean := False;
      Gradient : Gradients := (A, B, Quadrent);
      Ratio : Integer;

   begin -- Used
      for I in Iterate (Used_List) loop
         if Used_List (I).A /= 0 and Used_List (I).B /= 0  then
            Ratio := A / Used_List (I).A;
            Result := Result or
              (A = Ratio * Used_List (I).A and B = Ratio * Used_List (I).B);
         elsif (A = 0) and Used_List (I).A = 0 then
            Result := True;
         elsif (B = 0) and Used_List (I).B = 0 then
            Result := True;
         end if; -- Used_List (I).A /= 0 and Used_List (I).B /= 0
      end loop; -- I in Iterate (Used_List)
      if not Result then
         Append (Used_List, Gradient);
      end if; -- not Result
      Return Result;
   end Used;

   procedure Solve (X_Size, Y_Size : in Natural;
                   Max_Count, X_Max, Y_Max, Part_Two : out Natural) is

      subtype X_Coordinates is Natural range 0 .. X_Size - 1;
      subtype Y_Coordinates is Natural range 0 .. Y_Size - 1;

      type Asteroid_Arrays is array (X_Coordinates, Y_Coordinates) of Boolean;

      procedure Read_Input (Asteroid_Array : out Asteroid_Arrays) is

         Input_File : File_Type;
         Ch : Character;

      begin -- Read_Input;
         Open (Input_File, In_File, Input_File_Name);
         for Y in Y_Coordinates loop
            for X in X_Coordinates loop
               Get (Input_File, Ch);
               Assert (Ch = Asteroid or Ch = Empty,
                       "Unexpected Character '" & Ch & "'");
               Asteroid_Array (X, Y) := CH = Asteroid;
            end loop; -- X in X_Coordinates
            Skip_Line (Input_File);
         end loop; -- Y in Y_Coordinates
         Close (Input_File);
      end Read_Input;

      procedure Build_Gradient (X0 : in X_Coordinates; Y0 : Y_Coordinates;
                                Gradient_List : Out Gradient_Lists.Vector) is

         Used_List : Gradient_Lists.Vector := Empty_Vector;
         Junk : Boolean;

      begin -- Build_Gradient
         Clear (Gradient_List);
         Clear (Used_List);
         -- First Quadrent with A positive and B negative
         for A in Integer range 0 .. X_Coordinates'Last - X0 loop
            for B in reverse Integer range Y_Coordinates'First - Y0 .. 0 loop
               if A /= 0 or B /= 0 then
                  Junk := Used (Used_List, A, B, Q1);
               end if; -- A /= 0 or B /= 0
            end loop; -- B
         end loop; -- A
         Sort (Used_List);
         Gradient_List := Gradient_List & Used_List;
         Clear (Used_List);
         -- Second Quadrent with A positive and B positive
         for A in Integer range 0 .. X_Coordinates'Last - X0 loop
            for B in Integer range 0 .. Y_Coordinates'Last - Y0 loop
               if B /= 0 then
                  Junk := Used (Used_List, A, B, Q2);
               end if; -- A /= 0 or B /= 0
            end loop; -- B
         end loop; -- A
         Sort (Used_List);
         Gradient_List := Gradient_List & Used_List;
         Clear (Used_List);
         -- Third Quadrent with A and B positive
         for A in reverse Integer range X_Coordinates'First - X0 .. 0 loop
            for B in Integer range 0 .. Y_Coordinates'Last - Y0 loop
               if A /=0 and B /= 0 then
                  Junk := Used (Used_List, A, B, Q3);
               end if; -- A /= 0 or B /= 0
            end loop; -- B
         end loop; -- A
         Sort (Used_List);
         Gradient_List := Gradient_List & Used_List;
         Clear (Used_List);
         -- Fourth Quadrent with A and B both negative
         for A in reverse Integer range X_Coordinates'First - X0 .. 0 loop
            for B in reverse Integer range Y_Coordinates'First - Y0 .. 0 loop
               if A /= 0 then
                  Junk := Used (Used_List, A, B, Q4);
               end if; -- A /= 0 or B /= 0
            end loop; -- B
         end loop; -- A
         Sort (Used_List);
         Gradient_List := Gradient_List & Used_List;
      end Build_Gradient;

      function Count_Visible (Gradient_List : in Gradient_Lists.Vector;
                              Asteroid_Array : in Asteroid_Arrays;
                              X0 : in X_Coordinates;
                              Y0 : in Y_Coordinates) return Natural is

         p : Positive;
         Count : Natural := 0;
         Xp, Yp : Integer;
         Finished : Boolean;

      begin -- Count_Visible
         for I in Iterate (Gradient_List) loop
            P := 1;
            loop -- Until Finished
               Xp := (Gradient_List (I).A * P) + X0;
               Yp := (Gradient_List (I).B * P) + Y0;
               Finished := Xp > X_Coordinates'Last or Xp < X_Coordinates'First
                 or Yp > Y_Coordinates'Last or Yp < Y_Coordinates'First;
               if not Finished and then Asteroid_Array (Xp, Yp) then
                  Count := Count + 1;
                  Finished := True;
               end if; -- not Finished and then Asteroid_Array (Xp, Yp)
               exit when Finished;
               P := P + 1;
            end loop; -- Until Finished
         end loop; -- I in Iterate (Gradient_List)
         return Count;
      end Count_Visible;

      function Zap_Asteroids (Gradient_List : in Gradient_Lists.Vector;
                                 Asteroid_Array : in out Asteroid_Arrays;
                                 X0 : in X_Coordinates; Y0 : in Y_Coordinates;
                                 To_Zap : in Natural := 200) return Natural is

         p : Positive;
         Zapped : Natural := 0;
         Xp, Yp : Integer;
         X_Zap : X_Coordinates;
         Y_Zap : Y_Coordinates;
         Finished : Boolean;

      begin -- Zap_Asteroids
         while Zapped < To_Zap loop
            for I in Iterate (Gradient_List) loop
               P := 1;
               loop -- Until Finished
                  Xp := (Gradient_List (I).A * P) + X0;
                  Yp := (Gradient_List (I).B * P) + Y0;
                  Finished := Xp > X_Coordinates'Last or
                    Xp < X_Coordinates'First or
                    Yp > Y_Coordinates'Last or Yp < Y_Coordinates'First;
                  if not Finished and then Asteroid_Array (Xp, Yp) then
                     Asteroid_Array (Xp, Yp) := False;
                     Zapped := Zapped + 1;
                     X_Zap := Xp;
                     Y_Zap := Yp;
                     Finished := True;
                  end if; -- not Finished and then Asteroid_Array (Xp, Yp)
                  exit when Finished;
                  P := P + 1;
               end loop; -- Until Finished
               exit when Zapped >= To_Zap;
               -- prematuerly exit for loop when quota reached
            end loop; -- I in Iterate (Gradient_List)
         end loop; -- Zapped < To_Zap
         return 100 * X_Zap + Y_Zap;
      end Zap_Asteroids;

      Asteroid_Array : Asteroid_Arrays;
      Visible : Natural;
      Gradient_List : Gradient_Lists.Vector := Empty_Vector;

   begin -- Solve
      Max_Count := 0;
      Read_Input (Asteroid_Array);
      for Y in Y_Coordinates loop
         for X in X_Coordinates loop
            if Asteroid_Array (X, Y) then
               Clear (Gradient_List);
                 Build_Gradient (X, Y, Gradient_List);
               Visible := Count_Visible (Gradient_List, Asteroid_Array, X, Y);
               --- Visible := Count_Visible (Asteroid_Array, X, Y);
               if Visible > Max_Count then
                  Max_Count := Visible;
                  X_Max := X;
                  Y_Max := Y;
               end if; -- Visible > Max_Count
            end if; -- Asteroid_Array (X, Y)
         end loop; -- X in X_Coordinates
      end loop; -- Y in Y_Coordinates
      Clear (Gradient_List);
      Build_Gradient (X_Max, Y_Max, Gradient_List);
      Part_Two := Zap_Asteroids (Gradient_List, Asteroid_Array, X_Max, Y_Max);
   end Solve;

   X_Size, Y_Size, Max_Count, X_max, Y_Max, Part_Two : Natural;

begin -- December_10
   Find_Size (X_Size, Y_Size);
   Put_Line ("Limits" & Natural'Image (X_Size) & Natural'Image (Y_Size));
   Solve (X_Size, Y_Size, Max_Count, X_max, Y_Max, Part_Two);
   Put_Line ("Visible asteroids at (" & Natural'Image (X_Max) & "," &
               Natural'Image (Y_Max) & ")" & Natural'Image (Max_Count));
   Put_Line ("Part Two (100 * X_Zap + Y_Zap):" & Natural'Image (Part_Two));
end December_10;
