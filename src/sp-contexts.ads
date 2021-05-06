with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GNATCOLL.Refcount;

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

private

    type Filter_Action is (Keep, Exclude);
    -- Filters need to do different things. Some filters match line contents, whereas others want to remove any match
    -- which has a match anywhere in the content. When a filter matches, some action with regards to the search should
    -- be done, whether to include or to exclude the match from the results.

    type Filter (Action : Filter_Action) is abstract tagged null record;
    -- Search filters define which lines match and what to do about a match.

    function Image (F : Filter) return String is abstract;
    -- Describes the filter in an end-user type of way. TODO: This should be localized.

    function Matches (F : Filter; Str : String) return Boolean is abstract;
    -- Determine if a filter matches a string.

    type Case_Sensitive_Match_Filter is new Filter with record
        Text : Ada.Strings.Unbounded.Unbounded_String;
    end record;

    package Pointers is new GNATCOLL.Refcount.Shared_Pointers (Element_Type => Filter'Class);

    subtype Filter_Ptr is Pointers.Ref;

    type Exclude_Filter is new Filter with record
        Wrapped : Filter_Ptr;
    end record;

    overriding function Image (F : Case_Sensitive_Match_Filter) return String;
    overriding function Matches (F : Case_Sensitive_Match_Filter; Str : String) return Boolean;

    overriding function Image (F : Exclude_Filter) return String;
    overriding function Matches (F : Exclude_Filter; Str : String) return Boolean;

    package Filter_List is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => Filter_Ptr, "=" => Pointers."=");

    function Matches (F : Filter'Class; Lines : String_Vectors.Vector) return Boolean;

    -- The lines which match can determine the width of the context to be saved.

    type Search is record
        Directories : String_Sets.Set;
        -- A list of all directories to search.

        File_Cache : File_Maps.Map;
        -- Cached contents of files.

        Filters : Filter_List.Vector;
    end record;

end SP.Contexts;
