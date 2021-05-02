with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Unbounded;

with SP.Strings;

package SP.Contexts is
    use SP.Strings;

    type Context_Width is new Natural;
    Full_File_Width : constant := 0;

    package File_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Ada.Strings.Unbounded.Unbounded_String, Element_Type => String_Vectors.Vector,
         "<"      => Ada.Strings.Unbounded."<", "=" => String_Vectors."=");

-- OLD INTERFACE
-- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    type Context is tagged record
        --  A description of the current search. This includes all of the files of the starting point, their contents,
        --  as well as the currently applied filters.

        Starting_Dir : Ada.Strings.Unbounded.Unbounded_String;
        --  Absolute path of the starting directory.

        Files : File_Maps.Map;
        --  Cached contents of all files.

        Extensions : String_Sets.Set;
        --  List of extensions, without the "." of all of the types of files which should be considered in the search
        --  list.

        Width : Context_Width;
        --  The number of lines above and below a positive search result to include or not include.
    end record;

    function Uses_Extension (Ctx : Context; Extension : String) return Boolean;

-- Commands for processing.
    procedure Add_Directory (Ctx : in out Context; Directory : in String);
    procedure Add_Extensions (Ctx : in out Context; Extensions : in String_Vectors.Vector);
    procedure Remove_Extensions (Ctx : in out Context; Extensions : in String_Vectors.Vector);
    procedure Add_File (Ctx : in out Context; Next_Entry : Ada.Directories.Directory_Entry_Type);
    procedure Refresh (Ctx : in out Context; Starting_Dir : Ada.Strings.Unbounded.Unbounded_String);
    procedure List (Ctx : in Context);
    procedure Set_Context_Width (Ctx : in out Context; Words : in String_Vectors.Vector);

    procedure Add_File (File_Cache : in out File_Maps.Map; Next_Entry : in Ada.Directories.Directory_Entry_Type);
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- OLD INTERFACE

    --  Gets the current set of matching files.
    --  Gets the current set of matching lines.

    type Search is private;
    subtype Search_Result is String_Vectors.Vector;

    procedure Reload_Working_Set (Srch : in out Search);
    procedure Refresh_Directory (Srch : in out Search; Dir_Name : Ada.Strings.Unbounded.Unbounded_String);

    procedure Add_Directory (Srch : in out Search; Dir_Name : String);
    function Search_Directories (Srch : in Search) return String_Vectors.Vector;

    function Contains (Result : Search_Result; Str : String) return Boolean;

private

    type Search is record
        Directories : String_Sets.Set;
        -- A list of all directories to search.

        File_Cache : File_Maps.Map;
        -- Cached contents of files.
    end record;

end SP.Contexts;
