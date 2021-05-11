with Ada.Directories.Hierarchical_File_Names;
with Ada.Text_IO;

package body SP.Contexts is
    use Ada.Strings.Unbounded;

    procedure Cache_File (File_Cache : in out File_Maps.Map; Next_Entry : Ada.Directories.Directory_Entry_Type) is
        -- Adds the contents of a file to the file cache.
        use Ada.Directories;
        Lines        : String_Vectors.Vector     := String_Vectors.Empty_Vector;
        File_Name    : constant Unbounded_String := To_Unbounded_String (Full_Name (Next_Entry));
        Is_Directory : constant Boolean          := Kind (Next_Entry) /= Directory;
    begin
        if Is_Directory then
            if Read_Lines (To_String (File_Name), Lines) then
                --  Ada.Text_IO.Put_Line ("Next File_Name is: " & To_String (File_Name));
                if File_Cache.Contains (File_Name) then
                    File_Cache.Replace (File_Name, Lines);
                else
                    File_Cache.Insert (File_Name, Lines);
                end if;
            end if;
        end if;
    end Cache_File;

    function Is_Current_Or_Parent_Directory (Dir_Entry : Ada.Directories.Directory_Entry_Type) return Boolean is
        --  Return true if the entry is "." or "..".
        Name : constant String := Ada.Directories.Simple_Name (Dir_Entry);
    begin
        return
            Ada.Directories.Hierarchical_File_Names.Is_Parent_Directory_Name (Name)
            or else Ada.Directories.Hierarchical_File_Names.Is_Current_Directory_Name (Name);
    end Is_Current_Or_Parent_Directory;

    procedure Cache_File (Srch : in out Search; Next_Entry : Ada.Directories.Directory_Entry_Type) is
        use Ada.Directories;
    begin
        if Kind (Next_Entry) = Ordinary_File then
            Cache_File (Srch.File_Cache, Next_Entry);
        end if;

        if Kind (Next_Entry) = Directory then
            -- Recursively add to the search path.
            Cache_Directory (Srch, To_Unbounded_String (Full_Name (Next_Entry)));
        end if;
    end Cache_File;

    procedure Cache_Directory (Srch : in out Search; Dir_Name : Unbounded_String) is
        use Ada.Directories;
        Dir_Search : Search_Type;
        Next_Entry : Directory_Entry_Type;
        Filter     : constant Filter_Type := (Ordinary_File | Directory => True, others => False);
    begin
        Ada.Text_IO.Put_Line ("Adding: " & To_String (Dir_Name));
        Ada.Directories.Start_Search
            (Search => Dir_Search, Directory => To_String (Dir_Name), Pattern => "*", Filter => Filter);
        while More_Entries (Dir_Search) loop
            Get_Next_Entry (Dir_Search, Next_Entry);
            if not Is_Current_Or_Parent_Directory (Next_Entry) then
                Cache_File (Srch, Next_Entry);
            end if;
        end loop;
        End_Search (Dir_Search);
        pragma Unreferenced (Dir_Search);
    end Cache_Directory;

    procedure Reload_Working_Set (Srch : in out Search) is
    begin
        -- TODO: The file cache should watch files to know when it needs a refresh such as examining last time modified
        -- timestamp.
        Srch.File_Cache.Clear;
        for Dir_Name of Srch.Directories loop
            Cache_Directory (Srch, Dir_Name);
        end loop;
    end Reload_Working_Set;

    procedure Add_Directory (Srch : in out Search; Dir_Name : String) is
        use Ada.Directories;
        Unbounded_Name : constant Unbounded_String := To_Unbounded_String (Dir_Name);
        Path_Exists    : constant Boolean          := Exists (Dir_Name);
        Is_Directory   : constant Boolean          := Path_Exists and then Kind (Dir_Name) = Directory;
    begin
        -- TODO: this should also ensure new directories aren't subdirectories of existing directories
        if Is_Directory and then not Srch.Directories.Contains (Unbounded_Name) then
            Srch.Directories.Insert (Unbounded_Name);
            Cache_Directory (Srch, Unbounded_Name);
            Ada.Text_IO.Put_Line ("Added " & Dir_Name & " to search path.");
        else
            Ada.Text_IO.Put_Line ("Could not add " & Dir_Name & " to search path.");
        end if;
    end Add_Directory;

    function List_Directories (Srch : in Search) return String_Vectors.Vector is
    begin
        return Result : String_Vectors.Vector do
            for Directory of Srch.Directories loop
                Result.Append (Directory);
            end loop;
        end return;
    end List_Directories;

    procedure Add_Extension (Srch : in out Search; Extension : String) is
        Ext : constant Unbounded_String := To_Unbounded_String (Extension);
    begin
        if not Srch.Extensions.Contains (Ext) then
            Srch.Extensions.Insert (Ext);
        end if;
    end Add_Extension;

    procedure Remove_Extension (Srch : in out Search; Extension : String) is
        Ext : constant Unbounded_String := To_Unbounded_String (Extension);
    begin
        if Srch.Extensions.Contains (Ext) then
            Srch.Extensions.Delete (Ext);
        end if;
    end Remove_Extension;

    function List_Extensions (Srch : in Search) return String_Vectors.Vector is
    begin
        return Exts : String_Vectors.Vector do
            for Ext of Srch.Extensions loop
                Exts.Append (Ext);
            end loop;
        end return;
    end List_Extensions;

    procedure Find_Text (Srch : in out Search; Text : String) is
    begin
        Srch.Filters.Append (Filters.Find_Text (Text));
    end Find_Text;

    procedure Exclude_Text (Srch : in out Search; Text : String) is
    begin
        Srch.Filters.Append (Filters.Find_Text (Text));
    end Exclude_Text;

    procedure Pop_Filter (Srch : in out Search) is
        Filter_Being_Popped : constant Filter_Ptr :=
            (if Srch.Filters.Is_Empty then Pointers.Null_Ref else Srch.Filters.Last_Element);
    begin
        if Filter_Being_Popped.Is_Null then
            Ada.Text_IO.Put_Line ("No more filters to pop.");
        else
            Ada.Text_IO.Put_Line ("Popping filter: " & Image (Filter_Being_Popped.Get));
            Srch.Filters.Delete_Last;
        end if;
    end Pop_Filter;

    procedure Set_Context_Width (Srch : in out Search; Context_Width : Natural) is
    begin
        Srch.Context_Width := Context_Width;
    end Set_Context_Width;

    function Get_Context_Width (Srch : in Search) return Natural is (Srch.Context_Width);

    function List_Filter_Names (Srch : Search) return String_Vectors.Vector is
    begin
        return V : String_Vectors.Vector do
            for F of Srch.Filters loop
                V.Append (To_Unbounded_String (Image (F.Get)));
            end loop;
        end return;
    end List_Filter_Names;

    function Files_Matching_Extensions (Srch : in Search) return String_Vectors.Vector is
    begin
        return Result : String_Vectors.Vector do
            for Cursor in Srch.File_Cache.Iterate loop
                declare
                    File_Name : constant Unbounded_String := File_Maps.Key (Cursor);
                    File_Ext  : constant String           := Ada.Directories.Extension (To_String (File_Name));
                begin
                    if Srch.Extensions.Is_Empty or else Srch.Extensions.Contains (To_Unbounded_String (File_Ext)) then
                        Result.Append (File_Name);
                    end if;
                end;
            end loop;
        end return;
    end Files_Matching_Extensions;

    function Matching_Lines (Srch : in Search; File_Name : in Unbounded_String) return String_Vectors.Vector is
        Lines : constant String_Vectors.Vector := Srch.File_Cache (File_Name);
    begin
        return Result : String_Vectors.Vector do
            for Line of Lines loop
                if (for all F of Srch.Filters => Matches_Line (F.Get, To_String (Line))) then
                    Result.Append (Line);
                end if;
            end loop;
        end return;
    end Matching_Lines;

    function Matching_Files (Srch : in Search) return String_Vectors.Vector is
        Files : constant String_Vectors.Vector := Files_Matching_Extensions (Srch);
    begin
        return Result : String_Vectors.Vector do
            for File of Files loop
                if (for all F of Srch.Filters => Matches (F.Get, Srch.File_Cache(File))) then
                    Result.Append (File);
                end if;
            end loop;
        end return;
    end Matching_Files;

    function Num_Cached_Files (Srch : in Search) return Natural is
    begin
        return Natural (Matching_Files (Srch).Length);
    end Num_Cached_Files;

    function Num_Cached_Bytes (Srch : in Search) return Natural is
        Matched_Files : constant String_Vectors.Vector := Matching_Files (Srch);
    begin
        return Count : Natural := 0 do
            for File_Name of Matched_Files loop
                for Line of Srch.File_Cache.Element (File_Name) loop
                    Count := Count + Length(Line);
                end loop;
            end loop;
        end return;
    end Num_Cached_Bytes;

    function Num_Cached_Lines (Srch : in Search) return Natural is
        Matched_Files : constant String_Vectors.Vector := Matching_Files (Srch);
    begin
        return Count : Natural := 0 do
            for File_Name of Matched_Files loop
                Count := Count + Natural (Srch.File_Cache.Element (File_Name).Length);
            end loop;
        end return;
    end Num_Cached_Lines;
end SP.Contexts;
