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
with Ada.Containers.Vectors;

with SP.Cache;
with SP.Contexts;
with SP.Filters;
with SP.Strings;

package SP.Searches is
    use SP.Strings;

    type Search is limited private;

    package Positive_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Positive);
    use all type Ada.Containers.Count_Type;

    function Reload_Working_Set (Srch : in out Search) return Boolean;
    -- Dumps currently loaded search text and loads it again.

    function Add_Directory (Srch : in out Search; Dir_Name : String) return Boolean;

    function List_Directories (Srch : in Search) return String_Vectors.Vector;
    -- Lists top level search directories.
    procedure Clear_Directories (Srch : in out Search)
        with Post => List_Directories (Srch).Is_Empty;

    procedure Add_Extension (Srch : in out Search; Extension : String);
    procedure Remove_Extension (Srch : in out Search; Extension : String);
    procedure Clear_Extensions (Srch : in out Search);
    function List_Extensions (Srch : in Search) return String_Vectors.Vector;

    procedure Find_Text (Srch : in out Search; Text : String);

    procedure Exclude_Text (Srch : in out Search; Text : String);

    procedure Find_Like (Srch : in out Search; Text : String);

    procedure Exclude_Like (Srch : in out Search; Text : String);

    procedure Find_Regex (Srch : in out Search; Text : String);

    procedure Exclude_Regex (Srch : in out Search; Text : String);

    procedure Drop_Filter (Srch : in out Search; Index : Positive);

    procedure Pop_Filter (Srch : in out Search);
    -- Undoes the last search operations.

    procedure Reorder_Filters (Srch : in out Search; Indices : Positive_Vectors.Vector)
        with Pre => (for all Index of Indices => Natural (Index) <= Num_Filters (Srch))
            and then (Natural (Indices.Length) = Num_Filters (Srch))
            and then (for all I in 1 .. Num_Filters (Srch) => Indices.Contains (I));

    procedure Clear_Filters (Srch : in out Search);

    No_Context_Width : constant := Natural'Last;
    procedure Set_Context_Width (Srch : in out Search; Context_Width : Natural);
    function Get_Context_Width (Srch : in Search) return Natural;

    No_Max_Results : constant := Natural'Last;
    procedure Set_Max_Results (Srch : in out Search; Max_Results : Natural);
    function Get_Max_Results (Srch : in Search) return Natural;

    procedure Set_Search_On_Filters_Changed (Srch : in out Search; Enabled : Boolean);
    function Get_Search_On_Filters_Changed (Srch : in out Search) return Boolean;

    procedure Set_Line_Colors_Enabled (Srch : in out Search; Enabled : Boolean);

    procedure Set_Print_Line_Numbers (Srch : in out Search; Enabled : Boolean);
    function Get_Print_Line_Numbers (Srch : in Search) return Boolean;

    function List_Filter_Names (Srch : in Search) return String_Vectors.Vector;
    function Num_Filters (Srch : in Search) return Natural;

    function Matching_Contexts (Srch : in Search) return SP.Contexts.Context_Vectors.Vector;

    No_Limit : constant := Natural'Last;
    procedure Print_Contexts (
        Srch     : in Search;
        Contexts : SP.Contexts.Context_Vectors.Vector;
        First    : Natural;
        Last     : Natural);

    procedure Print_Contexts_With_Cancellation(
        Srch     : in Search;
        Contexts : SP.Contexts.Context_Vectors.Vector;
        First    : Natural;
        Last     : Natural);

    function Num_Files (Srch : in Search) return Natural;
    function Num_Lines (Srch : in Search) return Natural;

    protected type Concurrent_Context_Results is
        entry Get_Results(Out_Results : out SP.Contexts.Context_Vectors.Vector);
        procedure Wait_For(Num_Results : Natural);
        procedure Add_Result(More : SP.Contexts.Context_Vectors.Vector);
    private
        Pending_Results : Natural := 0;
        Results : SP.Contexts.Context_Vectors.Vector;
    end Concurrent_Context_Results;

    function Is_Running_Script (Srch : Search; Script_Path : String) return Boolean;
    procedure Push_Script (Srch : in out Search; Script_Path : String)
        with Pre => not Is_Running_Script (Srch, Script_Path);
    procedure Pop_Script (Srch : in out Search; Script_Path : String)
        with Pre => Is_Running_Script (Srch, Script_Path);

    procedure Test (Srch : Search; Input : String);

private

    use SP.Filters;

    -- The lines which match can determine the width of the context to be saved.

    type Search is limited record
        Directories : String_Sets.Set;
        -- A list of all directories to search.

        File_Cache : SP.Cache.Async_File_Cache;
        -- Cached contents of files.

        Line_Filters : Filter_List.Vector;

        Extensions : String_Sets.Set;

        Context_Width : Natural := 7;-- No_Context_Width;

        Max_Results : Natural := No_Max_Results;

        Print_Line_Numbers : Boolean := True;

        Search_On_Filters_Changed : Boolean := False;

        Enable_Line_Colors : Boolean := False;

        -- The stack of currently executing scripts.
        -- Intuitively this is a stack, but a set should work just a well,
        -- since the focus is the membership test.
        Script_Stack : String_Sets.Set;
    end record;

end SP.Searches;
