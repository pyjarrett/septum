with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with SP.Filters;
with SP.Strings;

package SP.Contexts is
    use SP.Strings;

    type Context_Width is new Natural;
    Full_File_Width : constant := 0;

    package File_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Ada.Strings.Unbounded.Unbounded_String, Element_Type => String_Vectors.Vector,
         "<"      => Ada.Strings.Unbounded."<", "=" => String_Vectors."=");

    --  Gets the current set of matching files. Gets the current set of matching lines.

    type Search is private;
    subtype Search_Result is String_Vectors.Vector;

    procedure Reload_Working_Set (Srch : in out Search);
    procedure Refresh_Directory (Srch : in out Search; Dir_Name : Ada.Strings.Unbounded.Unbounded_String);

    procedure Add_Directory (Srch : in out Search; Dir_Name : String);
    function Search_Directories (Srch : in Search) return String_Vectors.Vector;

    function Contains (Result : Search_Result; Str : String) return Boolean;

    procedure Find_Text (Srch : in out Search; Text : String);

    procedure Exclude_Text (Srch : in out Search; Text : String);

    procedure Pop (Srch : in out Search);
    -- Undoes the last search operations.

    function Get_Filter_Names (Srch : in Search) return String_Vectors.Vector;

    function Matching_Files (Srch : in Search) return String_Vectors.Vector;

    function Num_Cached_Files (Srch : in Search) return Natural;

private

    use SP.Filters;

    -- The lines which match can determine the width of the context to be saved.

    type Search is record
        Directories : String_Sets.Set;
        -- A list of all directories to search.

        File_Cache : File_Maps.Map;
        -- Cached contents of files.

        Filters : Filter_List.Vector;
    end record;

end SP.Contexts;
