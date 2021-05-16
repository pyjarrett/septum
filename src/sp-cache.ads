with SP.Strings;

package SP.Cache is
    type Async_File_Cache is limited private;
    -- @TODO This probably needs to be protected type.

    function Has_Directory (A : in ASync_File_Cache; Dir : String) return Boolean;

    procedure Add_Directory (A : in out Async_File_Cache; Dir : String)
        with Pre => not Has_Directory (A, Dir);

    -- File caching operations.  Loads files in the background as needed.
    protected type File_Cache is

    end File_Cache;

private

    type Async_File_Cache is limited record
        Top_Level_Directories : SP.Strings.String_Sets.Set;
        -- A list of all top level directories which need to be searched.
    end record;

end SP.Cache;
