with Ada.Directories;
with Ada.Environment_Variables;

package body SP.Platform is

    function Config_Dir return SP.Strings.String_Holders.Holder is
        package Env renames Ada.Environment_Variables;
    begin
        return S : SP.Strings.String_Holders.Holder := SP.Strings.String_Holders.Empty_Holder do
            if Env.Exists ("HOME") then
                S := SP.Strings.String_Holders.To_Holder (
                    Ada.Directories.Full_Name (Env.Value ("HOME")) & "/Library/Application Support");
            end if;
        end return;
    end Config_Dir;

    function Path_Separator return Character is ('/');
    function Path_Opposite_Separator return Character is ('\');

end SP.Platform;
