with Ada.Directories;
with Ada.Environment_Variables;

package body SP.Platform is

    function Config_Dir return SP.Strings.String_Holders.Holder is
        package Env renames Ada.Environment_Variables;
    begin
        return S : SP.Strings.String_Holders.Holder := SP.Strings.String_Holders.Empty_Holder do
            if Env.Exists ("XDG_CONFIG_HOME") then
                S := SP.Strings.String_Holders.To_Holder (Ada.Directories.Full_Name (Env.Value ("XDG_CONFIG_HOME")));
            elsif Env.Exists ("HOME") then
                S := SP.Strings.String_Holders.To_Holder (Ada.Directories.Full_Name (Env.Value ("HOME") & "/.config")));
            end if;
        end return;
    end Config_Dirs;

    function Path_Separator return Character is ('/');
    function Path_Opposite_Separator return Character is ('\');

end SP.Platform;
