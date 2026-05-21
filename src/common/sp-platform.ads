with SP.Strings;

package SP.Platform is

    function Global_Config_Dir return SP.Strings.String_Holders.Holder;

    --  Sometimes we don't want to create a config in a specific path,
    --  like in $HOME on Linux.
    function Is_Path_Ok_For_Config (Path : String) return Boolean;

    function Path_Separator return Character;
    function Path_Opposite_Separator return Character;

end SP.Platform;
