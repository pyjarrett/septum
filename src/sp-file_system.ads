with Ada.Directories;
with SP.Strings;

package SP.File_System is
    use SP.Strings;

    function Is_Current_Or_Parent_Directory (Dir_Entry : Ada.Directories.Directory_Entry_Type) return Boolean;

    type Dir_Contents is record
        Files   : String_Vectors.Vector;
        Subdirs : String_Vectors.Vector;
    end record;

    function Contents (Dir_Name : String) return Dir_Contents;
    -- The immediate contents of the given directory.
end SP.File_System;
