with SP.Strings;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

package SP.Cache is
    use Ada.Strings.Unbounded;
    use SP.Strings;

    package File_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Ada.Strings.Unbounded.Unbounded_String, Element_Type => String_Vectors.Vector,
         "<"      => Ada.Strings.Unbounded."<", "=" => String_Vectors."=");

    protected type Async_File_Cache is

        procedure Cache_File (File_Name : in Unbounded_String; Lines : in String_Vectors.Vector);

        function Num_Files return Natural;

    private

        Top_Level_Directories : SP.Strings.String_Sets.Set;
        -- A list of all top level directories which need to be searched.

        Contents : File_Maps.Map;
    end Async_File_Cache;

    procedure Add_Directory (A : in out Async_File_Cache; Dir : String);

end SP.Cache;
