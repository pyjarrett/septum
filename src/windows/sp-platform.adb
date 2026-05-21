with Ada.Directories;
with Ada.Environment_Variables;

package body SP.Platform is

    package Env renames Ada.Environment_Variables;

    function Global_Config_Dir return SP.Strings.String_Holders.Holder is
        LocalAppData : constant String := "LOCALAPPDATA";
    begin
        return S : SP.Strings.String_Holders.Holder := SP.Strings.String_Holders.Empty_Holder do
            if Env.Exists (LocalAppData) then
                S := SP.Strings.String_Holders.To_Holder (Ada.Directories.Full_Name (Env.Value (LocalAppData)));
            end if;
        end return;
    end Global_Config_Dir;

    function Is_Path_Ok_For_Config (Path : String) return Boolean is
    begin
        pragma Unreferenced (Path);
        return True;
    end Is_Path_Ok_For_Config;

    function Path_Separator return Character is ('\');
    function Path_Opposite_Separator return Character is ('/');

end SP.Platform;
