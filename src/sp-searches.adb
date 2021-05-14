with Ada.Directories.Hierarchical_File_Names;
with Ada.Text_IO;

with GNATCOLL.Atomic;

package body SP.Searches is
    use Ada.Strings.Unbounded;

    procedure Cache_File (File_Cache : in out File_Maps.Map; Next_Entry : Ada.Directories.Directory_Entry_Type) is
        -- Adds the contents of a file to the file cache.
        use Ada.Directories;
        Lines         : String_Vectors.Vector     := String_Vectors.Empty_Vector;
        File_Name     : constant Unbounded_String := To_Unbounded_String (Full_Name (Next_Entry));
        Not_Directory : constant Boolean          := Kind (Next_Entry) /= Directory;
    begin
        if Not_Directory then
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
        File_Ext : constant String :=
            (if Kind (Next_Entry) = Directory then "" else Ada.Directories.Extension (Full_Name (Next_Entry)));
    begin
        if Kind (Next_Entry) = Ordinary_File and then Srch.Extensions.Contains (To_Unbounded_String (File_Ext)) then
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
        Srch.Filters.Append (Filters.Exclude_Text (Text));
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
                if (for all F of Srch.Filters => Matches_File (F.Get, Srch.File_Cache (File))) then
                    Result.Append (File);
                end if;
            end loop;
        end return;
    end Matching_Files;

    function Matching_Contexts
        (File_Name : String; Num_Lines : Natural; Lines : SP.Contexts.Line_Matches.Set; Context_Width : Natural)
         return SP.Contexts.Context_Vectors.Vector is
    begin
        return C : SP.Contexts.Context_Vectors.Vector do
            for Line of Lines loop
                C.Append (SP.Contexts.From (File_Name, Line, Num_Lines, Context_Width));
            end loop;
        end return;
    end Matching_Contexts;

    procedure Matching_Contexts_In_File
        (Srch : in Search; File : in Unbounded_String; Concurrent_Results : in out Concurrent_Context_Results) is
        Excluded_Lines : SP.Contexts.Line_Matches.Set;
        First_Pass     : Boolean := True; -- The first filter pass has nothing to merge into.
        Lines          : SP.Contexts.Line_Matches.Set;
        Last           : SP.Contexts.Context_Vectors.Vector;
        Next           : SP.Contexts.Context_Vectors.Vector;
        Merged         : SP.Contexts.Context_Vectors.Vector;
        Result         : SP.Contexts.Context_Vectors.Vector;
    begin
        -- Process the file using the given filters.
        for F of Srch.Filters loop
            Lines := SP.Filters.Matching_Lines (F.Get, Srch.File_Cache (File));

            case F.Get.Action is
                -- No context should contain an excluded line. This could be more granular by finding contexts smaller
                -- than the given context width with no matching terms outside of the excluded terms.
                when Exclude =>
                    Excluded_Lines.Union (Lines);
                when Keep =>
                    Next :=
                        Matching_Contexts
                            (To_String (File), Natural (Srch.File_Cache (File).Length), Lines, Srch.Context_Width);

                    -- First pass has nothing to merge onto.
                    if First_Pass then
                        First_Pass := False;
                        Merged     := Next;
                    else
                        Last := Merged;
                        Merged.Clear;
                        for L of Last loop
                            for N of Next loop
                                if SP.Contexts.Overlap (L, N) then
                                    Merged.Append (SP.Contexts.Merge (L, N));
                                end if;
                            end loop;
                        end loop;
                    end if;
            end case;
        end loop;

        declare
            All_Matches_In_Contexts : SP.Contexts.Line_Matches.Set;
        begin
            -- Matching contexts of overlapping terms have been merged into single contexts. Remove those contexts with
            -- excluded lines to get the final result for this file.
            for G of Merged loop
                declare
                    Cut : Boolean := False;
                begin
                    for A of Excluded_Lines loop
                        if SP.Contexts.Contains (G, A) then
                            Cut := True;
                            exit;
                        end if;
                    end loop;

                    if not Cut then
                        All_Matches_In_Contexts.Union (G.Internal_Matches);
                    end if;
                end;
            end loop;

            for G of Merged loop
                declare
                    Cut : Boolean := False;
                begin
                    for A of Excluded_Lines loop
                        if SP.Contexts.Contains (G, A) then
                            Cut := True;
                            exit;
                        end if;
                    end loop;

                    if not Cut then
                        for M of All_Matches_In_Contexts loop
                            if SP.Contexts.Contains (G, M) and then not G.Internal_Matches.Contains (M) then
                                G.Internal_Matches.Insert (M);
                            end if;
                        end loop;
                        Result.Append (G);
                    end if;
                end;
            end loop;
        end;
        Concurrent_Results.Add_Result (Result);
    end Matching_Contexts_In_File;

    function Matching_Contexts (Srch : in Search) return SP.Contexts.Context_Vectors.Vector is
        Files          : constant String_Vectors.Vector := Files_Matching_Extensions (Srch);
        Merged_Results : Concurrent_Context_Results;

        Next_File   : aliased GNATCOLL.Atomic.Atomic_Counter := 0;
        Next_Access : constant access GNATCOLL.Atomic.Atomic_Counter  := Next_File'Access;
        One   : constant GNATCOLL.Atomic.Atomic_Counter := 1;

        task type Matching_Context_Search is
            entry Start;
        end Matching_Context_Search;

        task body Matching_Context_Search is
            Next_Index : Natural;
            Next_File  : Unbounded_String;
        begin
            accept Start;
            loop
                Next_Index := Natural(GNATColl.Atomic.Sync_Add_And_Fetch (Next_Access, One));
                if Next_Index <= Natural(Files.Length) then
                    Next_File := Files (Next_Index);
                    Matching_Contexts_In_File (Srch, Next_File, Merged_Results);
                else
                    exit;
                end if;
            end loop;
        end Matching_Context_Search;

        Num_Tasks    : constant := 16;
        All_Searches : array (0 .. Num_Tasks - 1) of Matching_Context_Search;
    begin
        return Result : SP.Contexts.Context_Vectors.Vector do
            Merged_Results.Wait_For(Natural(Files.Length));
            for I in All_Searches'Range loop
                All_Searches(I).Start;
            end loop;
            Merged_Results.Get_Results (Result);
        end return;
    end Matching_Contexts;

    procedure Print_Contexts (Srch : in Search; Contexts : SP.Contexts.Context_Vectors.Vector) is
        use Ada.Text_IO;
    begin
        for C of Contexts loop
            New_Line;
            Put_Line (To_String (C.File_Name));
            for Line_Num in C.Minimum .. C.Maximum loop
                if C.Internal_Matches.Contains (Line_Num) then
                    Put ("->");
                end if;
                Set_Col (3);
                Put (Line_Num'Image);
                Set_Col (7);
                Put_Line (To_String (Srch.File_Cache.Element (C.File_Name).Element (Line_Num)));
            end loop;
            New_Line;
        end loop;
        Put_Line ("Matching contexts: " & Contexts.Length'Image);
    end Print_Contexts;

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
                    Count := Count + Length (Line);
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

    protected body Concurrent_Context_Results is
        entry Get_Results (Out_Results : out SP.Contexts.Context_Vectors.Vector) when Pending_Results = 0 is
        begin
            Out_Results := Results;
        end Get_Results;

        procedure Wait_For (Num_Results : Natural) is
        begin
            Pending_Results := Num_Results;
        end Wait_For;

        procedure Add_Result (More : SP.Contexts.Context_Vectors.Vector) is
        begin
            Results.Append (More);
            Pending_Results := Pending_Results - 1;
        end Add_Result;
    end Concurrent_Context_Results;

end SP.Searches;
