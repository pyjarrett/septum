with Ada.Directories;
with Ada.Environment_Variables;

package body SP.Platform is

    function Home_Dir return String is
        package Env renames Ada.Environment_Variables;
        User_Profile : constant String := "HOME";
    begin
        if Env.Exists (User_Profile) then
            return Ada.Directories.Full_Name (Env.Value (User_Profile));
        else
            -- TODO: Add a better fallback case here.
            return "";
        end if;
    end Home_Dir;

    function Path_Separator return Character is ('\');
    function Path_Opposite_Separator return Character is ('/');

end SP.Platform;
