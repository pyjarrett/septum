with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;

package body SP.Platform is

    function Config_Dirs return SP.Strings.String_Sets.Set is
        package ASU renames Ada.Strings.Unbounded;
        package Env renames Ada.Environment_Variables;
    begin
        return Result : SP.String_Sets.Set do
            if Env.Exists ("HOME") then
                Result.Insert (ASU.To_Unbounded_String (
                    Ada.Directories.Full_Name (Env.Value ("HOME")) & "/Library/Application Support"));
            end if;
        end return;
    end Config_Dirs;

    function Path_Separator return Character is ('/');
    function Path_Opposite_Separator return Character is ('\');

end SP.Platform;
