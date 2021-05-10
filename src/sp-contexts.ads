with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with SP.Filters;
with SP.Strings;

package SP.Contexts is
    use SP.Strings;

    package File_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Ada.Strings.Unbounded.Unbounded_String, Element_Type => String_Vectors.Vector,
         "<"      => Ada.Strings.Unbounded."<", "=" => String_Vectors."=");

    type Search is private;

    procedure Reload_Working_Set (Srch : in out Search);
    procedure Cache_Directory (Srch : in out Search; Dir_Name : Ada.Strings.Unbounded.Unbounded_String);

    procedure Add_Directory (Srch : in out Search; Dir_Name : String);
    function List_Directories (Srch : in Search) return String_Vectors.Vector;
    -- Lists top level search directories.

    procedure Add_Extension (Srch : in out Search; Extension : String);
    procedure Remove_Extension (Srch : in out Search; Extension : String);
    function List_Extensions (Srch : in Search) return String_Vectors.Vector;

    procedure Find_Text (Srch : in out Search; Text : String);

    procedure Exclude_Text (Srch : in out Search; Text : String);

    procedure Pop_Filter (Srch : in out Search);
    -- Undoes the last search operations.

    function List_Filter_Names (Srch : in Search) return String_Vectors.Vector;

    function Files_Matching_Extensions (Srch : in Search) return String_Vectors.Vector;

    function Matching_Lines (Srch : in Search; File_Name : in Ada.Strings.Unbounded.Unbounded_String) return String_Vectors.Vector;

    function Matching_Files (Srch : in Search) return String_Vectors.Vector;

    function Num_Cached_Files (Srch : in Search) return Natural;

    function Num_Cached_Bytes (Srch : in Search) return Natural;

private

    use SP.Filters;

    -- The lines which match can determine the width of the context to be saved.

    type Search is record
        Directories : String_Sets.Set;
        -- A list of all directories to search.

        File_Cache : File_Maps.Map;
        -- Cached contents of files.

        Filters : Filter_List.Vector;

        Extensions : String_Sets.Set;
    end record;

end SP.Contexts;
