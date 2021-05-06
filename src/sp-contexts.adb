with Ada.Characters.Latin_1;
with Ada.Directories.Hierarchical_File_Names;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

package body SP.Contexts is
    use Ada.Strings.Unbounded;

    function Uses_Extension (Ctx : Context; Extension : String) return Boolean is
    --  Returns true if the context should contain files with the given extension.
    begin
        return Ctx.Extensions.Contains (Ada.Strings.Unbounded.To_Unbounded_String (Extension));
    end Uses_Extension;

    procedure Add_Directory (Ctx : in out Context; Directory : String) is
        use Ada.Directories;
    begin
        if Ada.Directories.Kind (Directory) = Ada.Directories.Directory then
            if Ctx.Starting_Dir = Ada.Strings.Unbounded.Null_Unbounded_String then
                Ctx.Starting_Dir := Ada.Strings.Unbounded.To_Unbounded_String (Directory);
            else
                Ada.Text_IO.Put_Line ("Context already has a starting directory.");
            end if;
        else
            Ada.Text_IO.Put_Line ("Trying to add a non-directory as a directory: " & Directory);
        end if;
    exception
        when Ada.Directories.Name_Error =>
            Ada.Text_IO.Put_Line ("Directory does not exist: " & Directory);
    end Add_Directory;

    procedure Add_File (Ctx : in out Context; Next_Entry : Ada.Directories.Directory_Entry_Type) is
        use Ada.Directories;
    begin
        if Ada.Directories.Kind (Next_Entry) = Ada.Directories.Ordinary_File and
            Uses_Extension (Ctx, Ada.Directories.Extension (Ada.Directories.Simple_Name (Next_Entry))) then
            Add_File (Ctx.Files, Next_Entry);
        end if;

        if Ada.Directories.Kind (Next_Entry) = Ada.Directories.Directory then
            Ctx.Refresh (Ada.Strings.Unbounded.To_Unbounded_String (Ada.Directories.Full_Name (Next_Entry)));
        end if;
    end Add_File;

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

    procedure Refresh (Ctx : in out Context; Starting_Dir : Ada.Strings.Unbounded.Unbounded_String) is
        --  Refreshes the list of files stored in the context.
        use Ada.Directories;
        Search     : Search_Type;
        Next_Entry : Directory_Entry_Type;
        Filter     : constant Filter_Type := (Ordinary_File | Directory => True, others => False);
    begin
        Ada.Directories.Start_Search
            (Search => Search, Directory => Ada.Strings.Unbounded.To_String (Starting_Dir), Pattern => "*",
             Filter => Filter);
        while Ada.Directories.More_Entries (Search) loop
            Ada.Directories.Get_Next_Entry (Search, Next_Entry);
            if Is_Current_Or_Parent_Directory (Next_Entry) then
                null;
            else
                Add_File (Ctx, Next_Entry);
            end if;
        end loop;
        End_Search (Search);
        pragma Unreferenced (Search);
    exception
        when others =>
            Ada.Text_IO.Put_Line ("Unknown Exception");
    end Refresh;

    procedure List (Ctx : in Context) is
    begin
        for Elem in Ctx.Files.Iterate loop
            Ada.Text_IO.Put_Line
                (Ada.Strings.Unbounded.To_String (File_Maps.Key (Elem)) & " : " &
                 Integer'Image (Integer (File_Maps.Element (Elem).Length)));
        end loop;
    end List;

    procedure Add_Extensions (Ctx : in out Context; Extensions : in String_Vectors.Vector) is
    begin
        for Ext of Extensions loop
            if not Ctx.Extensions.Contains (Ext) then
                Ctx.Extensions.Insert (Ext);
            end if;
        end loop;

        Ada.Text_IO.Put_Line ("Extensions:");
        for Ext of Ctx.Extensions loop
            Ada.Strings.Unbounded.Text_IO.Put_Line (Ext);
        end loop;
    end Add_Extensions;

    procedure Remove_Extensions (Ctx : in out Context; Extensions : in String_Vectors.Vector) is
    begin
        for Ext of Extensions loop
            if Ctx.Extensions.Contains (Ext) then
                Ctx.Extensions.Delete (Ext);
            end if;
        end loop;
    end Remove_Extensions;

    procedure Set_Context_Width (Ctx : in out Context; Words : in String_Vectors.Vector) is
        use Ada.Containers;
        use String_Vectors;
    begin
        if Words.Length > 1 then
            Ada.Text_IO.Put_Line ("Usage: context [width]");
            Ada.Text_IO.Put_Line ("No width removes context.");
        end if;

        if Words.Length = 0 then
            Ctx.Width := Full_File_Width;
            Ada.Text_IO.Put_Line ("Context set to file wide.");
        end if;

        declare
            Width : Integer;
        begin
            Width := Integer'Value (Ada.Strings.Unbounded.To_String (Words.First_Element));
            if Width > 0 then
                Ada.Text_IO.Put_Line ("Context set to " & Integer'Image (Width));
                Ctx.Width := Context_Width (Width);
            else
                Ada.Text_IO.Put_Line ("Context must be one line or greater.");
            end if;
        exception
            when others =>
                Ada.Text_IO.Put_Line ("Invalid context: " & Ada.Strings.Unbounded.To_String (Words.First_Element));
        end;
    end Set_Context_Width;

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

    procedure Push (Srch : in out Search; Text : String) is
        T : constant Case_Sensitive_Match_Filter := (Text => To_Unbounded_String (Text));
        F : Filter_Ptr;
    begin
        F.Set (T);
        Srch.Filters.Append (F);
    end Push;

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
        --  for Line : Adada.Strings.Unbounded.Unbounded_String of Lines loop
        --      if matches (F, To_String(Line)) then
                --  Ada.Textxt_IO.Put_Line (To_String(Line));
                --  return To_Stringrue;
            --  end if;
        --  end loop;
        --  return Falselse;
        return (For some Line of Lines => Matches (F, To_String(Line)));
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
