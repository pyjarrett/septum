with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Unbounded;

with SP.Strings;

package SP.Contexts is
    use SP.Strings;

    type Context_Width is new Natural;
    Full_File_Width : constant := 0;

    package File_Maps is new Ada.Containers.Ordered_Maps(Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
                                                         Element_Type => String_Vectors.Vector,
                                                         "<" => Ada.Strings.Unbounded."<",
                                                         "=" => String_Vectors."="
                                                        );

    type Context is tagged record
        --  A description of the current search. This includes all of the files of the starting
        --  point, their contents, as well as the currently applied filters.

        Starting_Dir : Ada.Strings.Unbounded.Unbounded_String;
        --  Absolute path of the starting directory.

        Files : File_Maps.Map;
        -- Cached contents of all files.

        Extensions : String_Vectors.Vector;
        -- List of extensions, without the "." of all of the types of files which should be
        -- considered in the search list.

        Width : Context_Width;
        -- The number of lines above and below a positive search result to include
        -- or not include.
    end record;

    function Add_Extensions (Ctx : in out Context; Extensions : in String_Vectors.Vector) return Boolean;
    function Remove_Extensions (Ctx : in out Context; Extensions : in String_Vectors.Vector) return Boolean;
    function Uses_Extension(Ctx : Context; Extension : String) return Boolean;
    function Add_File(Ctx : in out Context; Next_Entry : Ada.Directories.Directory_Entry_Type) return Boolean;
    function Refresh(Ctx : in out Context; Starting_Dir : Ada.Strings.Unbounded.Unbounded_String) return Boolean;
    function List (Ctx : in Context) return Boolean;
    function Set_Context_Width (Ctx : in out Context; Width : in Context_Width) return Boolean;

end SP.Contexts;
