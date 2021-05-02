with Ada.Directories;
with Ada.Strings.Unbounded;

package SP.Interactive is

    procedure Main;
    -- Main program entry point.

    procedure Old_Main
        (Starting_Dir : Ada.Strings.Unbounded.Unbounded_String :=
             Ada.Strings.Unbounded.To_Unbounded_String (Ada.Directories.Current_Directory));

end SP.Interactive;
