with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_08 is

   subtype Widths is Positive range 1 .. 25;
   subtype Heights is Positive range 1 .. 6;
   subtype Pixels is Natural range 0 .. 2;
   subtype Layers is Positive;

   package Pixel_IO is new Ada.Text_IO.Integer_IO (Pixels);
   use Pixel_IO;

   type Space_Images is array (Widths, Heights) of Pixels;

   Package Space_Image_Stores is new
     Ada.Containers.Vectors (Index_Type => Layers,
                             Element_Type => Space_Images);
   use Space_Image_Stores;

   subtype Address_Modes is Natural range 0 .. 1;

   procedure Read_Input
     (Space_Image_Store : out Space_Image_Stores.Vector) is

      Input_File : File_Type;
      Space_Image : Space_Images;

   begin -- Read_Input
      Clear (Space_Image_Store);
      Open (Input_File, In_File, "December_08.txt");
      while not End_Of_File (Input_File) loop
         for Height in Heights loop
            for Width in Widths loop
               Get (Input_File, Space_Image (Width, Height), 1);
            end loop; -- Width in Widths
         end loop; -- Height in Heights
         Append (Space_Image_Store, Space_Image);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Display (Space_Image_Store : in Space_Image_Stores.Vector) is

      Black : constant Pixels := 0;
      White : constant Pixels := 1;
      Layer : Layers;

   begin -- Display
      for Height in Heights loop
         for Width in Widths loop
            Layer := 1;
            loop -- In Pixel depth
               exit when Layer = Layers (Length (Space_Image_Store)) or
                 Space_Image_Store (Layer) (Width, Height) = White or
                 Space_Image_Store (Layer) (Width, Height) = Black;
               Layer := Layer + 1;
            end loop; -- In Pixel depth
            if Space_Image_Store (Layer) (Width, Height) = White then
               Put ('*');
            else
               Put (' ');
            end if; -- Space_Image_Store (Layer) (Width, Height) = White
            Delay 0.1; -- Makes it possible to see the image being built
         end loop; -- Width in Widths
         New_Line;
      end loop; -- Height in Heights
      New_Line;
   end Display;

   function Solution (Space_Image_Store : in Space_Image_Stores.Vector)
                      return Natural is

      Zero_Count : Natural;
      One_Count, Two_Count : Natural := 0;
      Least_Zeroes : Natural := Natural'Last;
      Zero_Layer : Positive;

   begin -- Solution
      for I in Iterate (Space_Image_Store) loop
         Zero_Count := 0;
         for Height in Heights loop
            for Width in Widths loop
               if Space_Image_Store (I) (Width, Height) = 0 then
                  Zero_Count := Zero_Count + 1;
               end if; -- Space_Image_Store (I) (Width, Height) = 0
            end loop; -- Width in Widths
         end loop; -- Height in Heights
         if Least_Zeroes > Zero_Count then
            Zero_Layer := To_Index (I);
            Least_Zeroes := Zero_Count;
         end if; -- Least_Zeroes > Zero_Count
      end loop; -- I in Iterate (Program_Store)
      for Height in Heights loop
         for Width in Widths loop
            if Space_Image_Store (Zero_Layer) (Width, Height) = 1 then
               One_Count := One_Count + 1;
            elsif Space_Image_Store (Zero_Layer) (Width, Height) = 2 then
               Two_Count := Two_Count + 1;
            end if; -- Space_Image_Store (Zero_Layer) (Width, Height) = 2
         end loop; -- Width in Widths
      end loop; -- Height in Heights
      return One_Count * Two_Count;
   end Solution;

   Space_Image_Store : Space_Image_Stores.Vector;

begin -- December_08
   Read_Input (Space_Image_Store);
   Put_Line ("Count of Ones * Twos:" &
               Natural'Image (Solution (Space_Image_Store)));
   Display (Space_Image_Store);
end December_08;
