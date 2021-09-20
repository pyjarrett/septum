-------------------------------------------------------------------------------
-- Copyright 2021, The Septum Developers (see AUTHORS file)

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-------------------------------------------------------------------------------

with Ada.Directories;
with ANSI;
with Atomic.Signed;

with SP.Terminal;

with System.Multiprocessors.Dispatching_Domains;

package body SP.Searches is
    use Ada.Strings.Unbounded;
    use SP.Terminal;

    procedure Load_Directory (Srch : in out Search; Dir_Name : String) is
        use Ada.Directories;
        Path_Exists    : constant Boolean          := Exists (Dir_Name);
        Is_Directory   : constant Boolean          := Path_Exists and then Kind (Dir_Name) = Directory;
    begin
        if Is_Directory then
            SP.Cache.Add_Directory_Recursively (Srch.File_Cache, Dir_Name);
        else
            SP.Terminal.Put_Line ("Cannot cache " & Dir_Name & ". It is not a directory.");
        end if;
    end Load_Directory;

    procedure Reload_Working_Set (Srch : in out Search) is
    begin
        -- TODO: The file cache should watch files to know when it needs a refresh such as examining last time modified
        -- timestamp.
        Srch.File_Cache.Clear;
        for Dir_Name of Srch.Directories loop
            Load_Directory (Srch, To_String (Dir_Name));
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
            Load_Directory (Srch, Dir_Name);
            SP.Terminal.Put_Line ("Added " & Dir_Name & " to search path.");
        else
            SP.Terminal.Put_Line ("Could not add " & Dir_Name & " to search path.");
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

    procedure Clear_Directories (Srch : in out Search) is
    begin
        Srch.Directories.Clear;
        Srch.File_Cache.Clear;
    end Clear_Directories;

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

    procedure Clear_Extensions (Srch : in out Search) is
    begin
        Srch.Extensions.Clear;
    end Clear_Extensions;

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
        Srch.Line_Filters.Append (Filters.Find_Text (Text));
    end Find_Text;

    procedure Exclude_Text (Srch : in out Search; Text : String) is
    begin
        Srch.Line_Filters.Append (Filters.Exclude_Text (Text));
    end Exclude_Text;

    procedure Find_Like (Srch : in out Search; Text : String) is
    begin
        Srch.Line_Filters.Append (Filters.Find_Like (Text));
    end Find_Like;

    procedure Exclude_Like (Srch : in out Search; Text : String) is
    begin
        Srch.Line_Filters.Append (Filters.Exclude_Like (Text));
    end Exclude_Like;

    procedure Find_Regex (Srch : in out Search; Text : String) is
        F : constant Filter_Ptr := Filters.Find_Regex (Text);
        use type Filter_Ptr;
    begin
        if F.Is_Valid then
            Srch.Line_Filters.Append (F);
        end if;
    end Find_Regex;

    procedure Exclude_Regex (Srch : in out Search; Text : String) is
        F : constant Filter_Ptr := Filters.Exclude_Regex (Text);
        use type Filter_Ptr;
    begin
        if F.Is_Valid then
            Srch.Line_Filters.Append (F);
        end if;
    end Exclude_Regex;

    procedure Pop_Filter (Srch : in out Search) is
        Filter_Being_Popped : constant Filter_Ptr :=
            (if Srch.Line_Filters.Is_Empty then Pointers.Make_Null else Srch.Line_Filters.Last_Element);
    begin
        if Filter_Being_Popped.Is_Valid then
            SP.Terminal.Put_Line ("No more filters to pop.");
        else
            SP.Terminal.Put_Line ("Popping filter: " & Image (Filter_Being_Popped.Get));
            Srch.Line_Filters.Delete_Last;
        end if;
    end Pop_Filter;

    procedure Clear_Filters (Srch : in out Search) is
    begin
        Srch.Line_Filters.Clear;
    end Clear_Filters;

    procedure Set_Context_Width (Srch : in out Search; Context_Width : Natural) is
    begin
        Srch.Context_Width := Context_Width;
    end Set_Context_Width;

    function Get_Context_Width (Srch : in Search) return Natural is (Srch.Context_Width);

    procedure Set_Max_Results (Srch : in out Search; Max_Results : Natural) is
    begin
        Srch.Max_Results := Max_Results;
    end Set_Max_Results;

    function Get_Max_Results (Srch : in Search) return Natural is (Srch.Max_Results);

    procedure Set_Search_On_Filters_Changed (Srch : in out Search; Enabled : Boolean) is
    begin
        Srch.Search_On_Filters_Changed := Enabled;
    end Set_Search_On_Filters_Changed;

    function Get_Search_On_Filters_Changed (Srch : in out Search) return Boolean is (Srch.Search_On_Filters_Changed);

    procedure Set_Line_Colors_Enabled (Srch : in out Search; Enabled : Boolean) is
    begin
        Srch.Enable_Line_Colors := Enabled;
    end Set_Line_Colors_Enabled;

    procedure Set_Print_Line_Numbers (Srch : in out Search; Enabled : Boolean) is
    begin
        Srch.Print_Line_Numbers := Enabled;
    end Set_Print_Line_Numbers;

    function Get_Print_Line_Numbers (Srch : in Search) return Boolean is (Srch.Print_Line_Numbers);

    function List_Filter_Names (Srch : Search) return String_Vectors.Vector is
    begin
        return V : String_Vectors.Vector do
            for F of Srch.Line_Filters loop
                V.Append (To_Unbounded_String (F.Get.Action'Image & " : " & Image (F.Get)));
            end loop;
        end return;
    end List_Filter_Names;

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
        -- TODO: This code is a horrible mess and needs to be split up.
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
        for F of Srch.Line_Filters loop
            Lines := SP.Filters.Matching_Lines (F.Get, Srch.File_Cache.Lines (File));

            case F.Get.Action is
                -- No context should contain an excluded line. This could be more granular by finding contexts smaller
                -- than the given context width with no matching terms outside of the excluded terms.
                when Exclude =>
                    Excluded_Lines.Union (Lines);
                when Keep =>
                    Next :=
                        Matching_Contexts
                            (To_String (File), Natural (Srch.File_Cache.Lines (File).Length), Lines,
                             Srch.Context_Width);

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
            Matching_Contexts       : SP.Contexts.Context_Vectors.Vector;
        begin
            -- Matching contexts of overlapping terms have been merged into single contexts. Remove those contexts with
            -- excluded lines to get the final result for this file.
            for G of Merged loop
                declare
                    Cut : Boolean := False;
                begin
                    -- Matching contexts cannot contain any excluded lines.
                    for A of Excluded_Lines loop
                        if SP.Contexts.Contains (G, A) then
                            Cut := True;
                            exit;
                        end if;
                    end loop;

                    if not Cut then
                        All_Matches_In_Contexts.Union (G.Internal_Matches);
                        Matching_Contexts.Append (G);
                    end if;
                end;
            end loop;

            -- Merge down
            for C of Matching_Contexts loop
                declare
                    Duplicate : Boolean := False;
                    use type SP.Contexts.Context_Match;
                begin
                    for D of Matching_Contexts loop
                        if C /= D and then SP.Contexts.Contains(D, C) then
                            Duplicate := True;
                            exit;
                        end if;
                    end loop;

                    if not Duplicate then
                        -- It's nice to have the lines which contain a match to be marked as such.
                        for M of All_Matches_In_Contexts loop
                            if SP.Contexts.Contains (C, M) and then not C.Internal_Matches.Contains (M) then
                                C.Internal_Matches.Insert (M);
                            end if;
                        end loop;
                        Result.Append (C);
                    end if;
                end;
            end loop;
        end;
        Concurrent_Results.Add_Result (Result);
    end Matching_Contexts_In_File;

    function Files_To_Search (Srch : in Search) return String_Vectors.Vector is
    begin
        return Result : String_Vectors.Vector do
            if Srch.Extensions.Is_Empty then
                Result := Srch.File_Cache.Files;
                return;
            end if;

            for File of Srch.File_Cache.Files loop
                declare
                    Extension : constant String := Ada.Directories.Extension (To_String(File));
                begin
                    if Srch.Extensions.Contains (To_Unbounded_String(Extension)) then
                        Result.Append (File);
                    end if;
                end;
            end loop;
        end return;
    end Files_To_Search;

    function Matching_Contexts (Srch : in Search) return SP.Contexts.Context_Vectors.Vector is
        package Atomic_Int is new Atomic.Signed (T => Integer);

        Files          : constant String_Vectors.Vector := Files_To_Search (Srch);
        Merged_Results : Concurrent_Context_Results;
        Next_File      : aliased Atomic_Int.Instance := Atomic_Int.Init (1);
        Next_Access    : constant access Atomic_Int.Instance := Next_File'Access;

        task type Matching_Context_Search is
            entry Start;
        end Matching_Context_Search;

        task body Matching_Context_Search is
            Next_Index : Natural;
            Next_File  : Unbounded_String;
        begin
            accept Start;
            loop
                Next_Index := Natural (Atomic_Int.Fetch_Add (Next_Access.all, 1));
                if Next_Index <= Natural (Files.Length) then
                    Next_File := Files (Next_Index);
                    Matching_Contexts_In_File (Srch, Next_File, Merged_Results);
                else
                    exit;
                end if;
            end loop;
        end Matching_Context_Search;

        Num_Tasks    : constant System.Multiprocessors.CPU := System.Multiprocessors.Number_Of_CPUs;
        use System.Multiprocessors;
        All_Searches : array (0 .. Num_Tasks - 1) of Matching_Context_Search;
    begin
        return Result : SP.Contexts.Context_Vectors.Vector do
            Merged_Results.Wait_For (Natural (Files.Length));
            for I in All_Searches'Range loop
                System.Multiprocessors.Dispatching_Domains.Set_CPU (I, All_Searches (I)'Identity);
                All_Searches (I).Start;
            end loop;
            Merged_Results.Get_Results (Result);
        end return;
    end Matching_Contexts;

    procedure Print_Context (Srch : SP.Searches.Search; Context : SP.Contexts.Context_Match) is
    begin
        Put_Line (To_String (Context.File_Name));
        for Line_Num in Context.Minimum .. Context.Maximum loop
            if Context.Internal_Matches.Contains (Line_Num) then
                Put ("->");
            end if;
            Set_Col (3);
            if Srch.Print_Line_Numbers then
                Put (Line_Num'Image);
                Set_Col (13);
            else
                Set_Col(5);
            end if;
            if Srch.Enable_Line_Colors and then Context.Internal_Matches.Contains (Line_Num) then
                Put_Line (SP.Terminal.Colorize (
                    To_String (Srch.File_Cache.File_Line (Context.File_Name, Line_Num)),
                    ANSI.Green));
            else
                Put_Line (To_String (Srch.File_Cache.File_Line (Context.File_Name, Line_Num)));
            end if;
        end loop;
        New_Line;
    end Print_Context;

    procedure Print_Contexts (Srch : in Search; Contexts : SP.Contexts.Context_Vectors.Vector; First_Num : Natural := No_Limit) is
        Max_Results : constant Natural := Srch.Max_Results;
        Num_Results_Printed : Natural := 0;
    begin
        if Natural(Contexts.Length) > Max_Results and then First_Num > Max_Results then
            Put_Line ("Found" & Contexts.Length'Image & " results.");
            return;
        end if;

        for C of Contexts loop
            New_Line;
            Print_Context (Srch, C);

            Num_Results_Printed := Num_Results_Printed + 1;
            exit when First_Num /= No_Limit and then Num_Results_Printed >= First_Num;
        end loop;
        Put_Line ("Matching contexts: " & Contexts.Length'Image);
        Put_Line ("Matching files:" & SP.Contexts.Files_In (Contexts).Length'Image);
    end Print_Contexts;

    function Num_Files (Srch : in Search) return Natural is
    begin
        return Srch.File_Cache.Num_Files;
    end Num_Files;

    function Num_Lines (Srch : in Search) return Natural is
    begin
        return Srch.File_Cache.Num_Lines;
    end Num_Lines;

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
