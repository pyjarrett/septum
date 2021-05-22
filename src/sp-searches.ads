with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with SP.Cache;
with SP.Contexts;
with SP.Filters;
with SP.Strings;

package SP.Searches is
    use SP.Strings;

    package File_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Ada.Strings.Unbounded.Unbounded_String, Element_Type => String_Vectors.Vector,
         "<"      => Ada.Strings.Unbounded."<", "=" => String_Vectors."=");

    type Search is limited private;

    procedure Reload_Working_Set (Srch : in out Search);

    procedure Add_Directory (Srch : in out Search; Dir_Name : String);
    function List_Directories (Srch : in Search) return String_Vectors.Vector;
    -- Lists top level search directories.
    procedure Clear_Directories (Srch : in out Search)
        with Post => List_Directories (Srch).Is_Empty;

    procedure Add_Extension (Srch : in out Search; Extension : String);
    procedure Remove_Extension (Srch : in out Search; Extension : String);
    function List_Extensions (Srch : in Search) return String_Vectors.Vector;

    procedure Find_Text (Srch : in out Search; Text : String);

    procedure Exclude_Text (Srch : in out Search; Text : String);

    procedure Find_Regex (Srch : in out Search; Text : String);

    procedure Exclude_Regex (Srch : in out Search; Text : String);

    procedure Pop_Filter (Srch : in out Search);
    -- Undoes the last search operations.

    procedure Clear_Filters (Srch : in out Search);

    No_Context_Width : constant := Natural'Last;
    procedure Set_Context_Width (Srch : in out Search; Context_Width : Natural);
    function Get_Context_Width (Srch : in Search) return Natural;

    No_Max_Results : constant := Natural'Last;
    procedure Set_Max_Results (Srch : in out Search; Max_Results : Natural);
    function Get_Max_Results (Srch : in Search) return Natural;

    function List_Filter_Names (Srch : in Search) return String_Vectors.Vector;

    function Matching_Contexts (Srch : in Search) return SP.Contexts.Context_Vectors.Vector;

    procedure Print_Contexts (Srch : in Search; Contexts : SP.Contexts.Context_Vectors.Vector);

    function Num_Files (Srch : in Search) return Natural;
    function Num_Lines (Srch : in Search) return Natural;

    protected type Concurrent_Context_Results is
        entry Get_Results(Out_Results : out SP.Contexts.Context_Vectors.Vector);
        procedure Wait_For(Num_Results : Natural);
        procedure Add_Result(More : SP.Contexts.Context_Vectors.Vector);
    private
        Pending_Results : Natural := 0;
        Results : SP.Contexts.Context_Vectors.Vector;
    end Concurrent_Context_Results;

private

    use SP.Filters;

    -- The lines which match can determine the width of the context to be saved.

    type Search is limited record
        Directories : String_Sets.Set;
        -- A list of all directories to search.

        File_Cache : SP.Cache.Async_File_Cache;
        -- Cached contents of files.

        Filters : Filter_List.Vector;

        Extensions : String_Sets.Set;

        Context_Width : Natural := 7;-- No_Context_Width;

        Max_Results : Natural := No_Max_Results;
    end record;

end SP.Searches;
