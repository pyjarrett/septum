with Ada.Characters.Latin_1;
with Ada.Directories.Hierarchical_File_Names;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body SP.Contexts is
    use Ada.Strings.Unbounded;

    procedure Add_File (File_Cache : in out File_Maps.Map; Next_Entry : Ada.Directories.Directory_Entry_Type) is
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
    end Add_File;

    function Is_Current_Or_Parent_Directory (Dir_Entry : Ada.Directories.Directory_Entry_Type) return Boolean is
        --  Return true if the entry is "." or "..".
        Name : constant String := Ada.Directories.Simple_Name (Dir_Entry);
    begin
        return
            Ada.Directories.Hierarchical_File_Names.Is_Parent_Directory_Name (Name)
            or else Ada.Directories.Hierarchical_File_Names.Is_Current_Directory_Name (Name);
    end Is_Current_Or_Parent_Directory;


    procedure Add_File (Srch : in out Search; Next_Entry : Ada.Directories.Directory_Entry_Type) is
        use Ada.Directories;
    begin
        if Kind (Next_Entry) =
            Ordinary_File --and
        --Uses_Extension (Srch, Extension (Simple_Name (Next_Entry)))

then
            Add_File (Srch.File_Cache, Next_Entry);
        end if;

        if Kind (Next_Entry) = Directory then
            -- Recursively add to the search path.
            Refresh_Directory (Srch, To_Unbounded_String (Full_Name (Next_Entry)));
        end if;
    end Add_File;

    procedure Refresh_Directory (Srch : in out Search; Dir_Name : Unbounded_String) is
        use Ada.Directories;
        Dir_Search : Search_Type;
        Next_Entry : Directory_Entry_Type;
        Filter     : constant Filter_Type := (Ordinary_File | Directory => True, others => False);
    begin
        Ada.Directories.Start_Search
            (Search => Dir_Search, Directory => To_String (Dir_Name), Pattern => "*", Filter => Filter);
        while More_Entries (Dir_Search) loop
            Get_Next_Entry (Dir_Search, Next_Entry);
            if not Is_Current_Or_Parent_Directory (Next_Entry) then
                Add_File (Srch, Next_Entry);
            end if;
        end loop;
        End_Search (Dir_Search);
        pragma Unreferenced (Dir_Search);
    end Refresh_Directory;

    procedure Reload_Working_Set (Srch : in out Search) is
    begin
        -- TODO: The file cache should watch files to know when it needs a refresh such as examining last time modified
        -- timestamp.
        Srch.File_Cache.Clear;
        for Dir_Name of Srch.Directories loop
            Refresh_Directory (Srch, Dir_Name);
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
            Refresh_Directory (Srch, Unbounded_Name);
            Ada.Text_IO.Put_Line ("Added " & Dir_Name & " to search path.");
        else
            Ada.Text_IO.Put_Line ("Could not add " & Dir_Name & " to search path.");
        end if;
    end Add_Directory;

    function Search_Directories (Srch : in Search) return String_Vectors.Vector is
    begin
        return Result : String_Vectors.Vector do
            for Directory of Srch.Directories loop
                Result.Append (Directory);
            end loop;
        end return;
    end Search_Directories;

    function Contains (Result : Search_Result; Str : String) return Boolean is
    begin
        for Line of Result loop
            if Result.Contains (To_Unbounded_String (Str)) then
                return True;
            end if;
        end loop;
        return False;
    end Contains;

    procedure Find_Text (Srch : in out Search; Text : String) is
        T : constant Case_Sensitive_Match_Filter := (Text => To_Unbounded_String (Text));
        F : Filter_Ptr;
    begin
        F.Set (T);
        Srch.Filters.Append (F);
    end Find_Text;

    procedure Pop (Srch : in out Search) is
        Filter_Being_Popped : constant Filter_Ptr :=
            (if Srch.Filters.Is_Empty then Pointers.Null_Ref else Srch.Filters.Last_Element);
    begin
        if Filter_Being_Popped.Is_Null then
            Ada.Text_IO.Put_Line ("No more filters to pop.");
        else
            Ada.Text_IO.Put_Line ("Popping filter: " & Image (Filter_Being_Popped.Get));
            Srch.Filters.Delete_Last;
        end if;
    end Pop;

    function Get_Filter_Names (Srch : Search) return String_Vectors.Vector is
    begin
        return V : String_Vectors.Vector do
            for F of Srch.Filters loop
                V.Append (To_Unbounded_String (Image (F.Get)));
            end loop;
        end return;
    end Get_Filter_Names;

    overriding function Image (F : Case_Sensitive_Match_Filter) return String is
        use Ada.Characters;
    begin
        return "Case Sensitive Match " & Latin_1.Quotation & To_String (F.Text) & Latin_1.Quotation;
    end Image;

    function Matches (F : Case_Sensitive_Match_Filter; Str : String) return Boolean is
    begin
        return Ada.Strings.Fixed.Index (Str, To_String (F.Text)) > 0;
    end Matches;

    function Matches (F : Filter'Class; Lines : String_Vectors.Vector) return Boolean is
    begin
        return (for some Line of Lines => Matches (F, To_String(Line)));
    end Matches;

    function Matching_Files (Srch : in Search) return String_Vectors.Vector is
    begin
        return Result : String_Vectors.Vector do
            for Cursor in Srch.File_Cache.Iterate loop
                if (for all F of Srch.Filters => Matches (F.Get, File_Maps.Element (Cursor))) then
                    Result.Append (File_Maps.Key (Cursor));
                end if;
            end loop;
        end return;
    end Matching_Files;
end SP.Contexts;
