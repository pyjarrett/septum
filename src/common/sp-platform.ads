with SP.Strings;

package SP.Platform is

    function Config_Dir return SP.Strings.String_Holders.Holder;

    function Path_Separator return Character;
    function Path_Opposite_Separator return Character;

end SP.Platform;
