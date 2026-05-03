with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;

package body SP.Platform is

    function Config_Dirs return SP.Strings.String_Sets.Set is
        package Env renames Ada.Environment_Variables;
        package ASU renames Ada.Strings.Unbounded;
        User_Profile : constant String := "USERPROFILE";
    begin
        return S : SP.Strings.String_Sets.Set do
            if Env.Exists (User_Profile) then
                S.Insert (ASU.To_Unbounded_String (Ada.Directories.Full_Name (Env.Value (User_Profile))));
            end if;
        end return;
    end Config_Dirs;

    function Path_Separator return Character is ('\');
    function Path_Opposite_Separator return Character is ('/');

end SP.Platform;
