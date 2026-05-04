with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with SP.Strings;

package body SP.Platform is

    function Config_Dirs return SP.Strings.String_Sets.Set is
        package ASU renames Ada.Strings.Unbounded;
        package Env renames Ada.Environment_Variables;
    begin
        return S : SP.String_Sets.Set do
            if Env.Exists ("XDG_CONFIG_HOME") then
                S.Insert (ASU.To_String (Ada.Directories.Full_Name (Env.Value ("XDG_CONFIG_HOME"))));
            elsif Env.Exists ("HOME") then
                S.Insert (ASU.To_String (Ada.Directories.Full_Name (Env.Value ("HOME") & "/.config")));
            end if;
        end return;
    end Config_Dirs;

    function Path_Separator return Character is ('/');
    function Path_Opposite_Separator return Character is ('\');

end SP.Platform;
