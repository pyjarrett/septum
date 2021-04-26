with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;

with SP.Strings;

package SP.Interactive is
    use SP.Strings;

    procedure Main(Starting_Dir : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String (Ada.Directories.Current_Directory));
    -- Main program entry point.

    package File_Maps is new Ada.Containers.Ordered_Maps(Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
                                                         Element_Type => String_Vectors.Vector,
                                                         "<" => Ada.Strings.Unbounded."<",
                                                         "=" => String_Vectors."="
                                                        );

    type Context is record
        --  A description of the current search. This includes all of the files of the starting
        --  point, their contents, as well as the currently applied filters.

        Starting_Dir : Ada.Strings.Unbounded.Unbounded_String;
        --  Absolute path of the starting directory.

        Files : File_Maps.Map;
        -- Cached contents of files.

        Extensions : String_Vectors.Vector;
        -- List of extensions, without the "." of all of the types of files which should be
        -- considered in the search list.
    end record;
end SP.Interactive;
