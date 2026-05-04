with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;

package body SP.Platform is

    function Config_Dirs return SP.Strings.String_Sets.Set is
        package Env renames Ada.Environment_Variables;
        package ASU renames Ada.Strings.Unbounded;
        LocalAppData : constant String := "LOCALAPPDATA";
    begin
        return S : SP.Strings.String_Sets.Set do
            if Env.Exists (LocalAppData) then
                S.Insert (ASU.To_Unbounded_String (Ada.Directories.Full_Name (Env.Value (LocalAppData))));
            end if;
        end return;
    end Config_Dirs;

    function Path_Separator return Character is ('\');
    function Path_Opposite_Separator return Character is ('/');

end SP.Platform;
