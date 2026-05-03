with SP.Strings;

package SP.Platform is

    function Config_Dirs return SP.Strings.String_Sets.Set;

    function Path_Separator return Character;
    function Path_Opposite_Separator return Character;

end SP.Platform;
