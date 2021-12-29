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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with SP.Config;
with SP.Contexts;
with SP.File_System;
with SP.Terminal;

package body SP.Commands is
    pragma Assertion_Policy (Pre => Check, Post => Check);

    use Ada.Strings.Unbounded;
    use SP.Terminal;

    type Help_Proc is not null access procedure;
    -- Prints a detailed help description for a command.

    type Exec_Proc is not null access function
        (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector)
            return Command_Result;
    -- Executes a command.

    type Executable_Command is record
        Simple_Help : Unbounded_String;
        -- A brief help description.

        Help : Help_Proc;
        -- Prints a much longer help description.

        Exec : Exec_Proc;
        -- Executes the command.
    end record;

    package Command_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Unbounded_String, Element_Type => Executable_Command);

    -- The command map is split between a the procedure to execute, and also a
    -- command to print help information.
    Command_Map : Command_Maps.Map;

    function Is_Command (S : String) return Boolean is (Command_Map.Contains (To_Unbounded_String (S)));

    function Target_Command (Command_Name : Unbounded_String) return Unbounded_String
    is
        Best_Match      : Unbounded_String := Null_Unbounded_String;
        Best_Match_Size : Natural          := 0;
        Next_Match      : Unbounded_String;
        Next_Match_Size : Natural          := 0;
        Ambiguous       : Boolean          := False;
    begin
        if Command_Map.Contains (Command_Name) then
            return Command_Name;
        end if;

        for Cursor in Command_Map.Iterate loop
            Next_Match      := Command_Maps.Key (Cursor);
            Next_Match_Size := Common_Prefix_Length (Next_Match, Command_Name);
            if Next_Match_Size = Length(Command_Name) then
                if Next_Match_Size = Best_Match_Size then
                    -- Two things with the same prefix, the prefix is ambiguous.
                    Best_Match := Null_Unbounded_String;
                    Ambiguous  := True;
                elsif Next_Match_Size > Best_Match_Size then
                    Best_Match_Size := Next_Match_Size;
                    Best_Match      := Next_Match;
                    Ambiguous       := False;
                end if;
            end if;
        end loop;

        return (if Ambiguous then Null_Unbounded_String else Best_Match);
    end Target_Command;

    function Is_Like_Command (S : String) return Boolean is (Target_Command (To_Unbounded_String (S)) /= Null_Unbounded_String);

    function Try_Parse (Str : String; Value : in out Positive) return Boolean is
    begin
        Value := Positive'Value (Str);
        return True;
    exception
        when Constraint_Error =>
            return False;
    end Try_Parse;

    function Execute (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
        Command_Name : constant Unbounded_String :=
            (if Command_Line.Is_Empty then To_Unbounded_String ("") else Command_Line.First_Element);
        Best_Command : constant Unbounded_String := Target_Command (Command_Name);
    begin
        if Command_Map.Contains (Best_Command) then
            declare
                It         : constant Command_Maps.Cursor := Command_Map.Find (Best_Command);
                Command    : constant Executable_Command  := Command_Maps.Element (It);
                Parameters : String_Vectors.Vector        := Command_Line;
            begin
                Parameters.Delete_First;
                if Best_Command /= Command_Name then
                    Put_Line ("Resolved to: " & To_String (Best_Command));
                end if;
                New_Line;
                return Command.Exec.all (Srch, Parameters);
            end;
        end if;
        return Command_Unknown;
    end Execute;

    function Run_Commands_From_File (Srch : in out SP.Searches.Search; File : String) return Command_Result is
        Commands : SP.Strings.String_Vectors.Vector;
        function "+" (S : String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;
    begin
        if not Ada.Directories.Exists (File) then
            Put_Line ("No config to read at: " & Ada.Directories.Full_Name (File));
            return Command_Failed;
        end if;

        Put_Line ("Loading commands from: " & Ada.Directories.Full_Name (File));

        if not SP.File_System.Read_Lines (Ada.Directories.Full_Name (File), Commands) then
            Put_Line ("Unable to load configuration file from: " & Ada.Directories.Full_Name (File));
        end if;

        for Command of Commands loop
            declare
                Exploded : constant SP.Strings.Exploded_Line := SP.Strings.Make (To_String (Command));
                Command_Line : constant String_Vectors.Vector := Exploded.Words;
                Result : Command_Result;
            begin
                New_Line;
                Put_Line (+" > " & Command);
                Result := SP.Commands.Execute (Srch, Command_Line);

                case Result is
                    when Command_Success => null;
                    when Command_Failed =>
                        Put_Line (+"Command failed: " & Command);
                        return Command_Failed;
                    when Command_Unknown =>
                        Put_Line (+"Unable to execute: " & Command);
                        return Command_Unknown;
                    when Command_Exit_Requested =>
                        return Command_Exit_Requested;
                end case;
            end;
        end loop;

        return Command_Success;
    end Run_Commands_From_File;

    ----------------------------------------------------------------------------

    procedure Search_Updated (Srch : in out SP.Searches.Search) is
        use SP.Searches;
        Contexts : SP.Contexts.Context_Vectors.Vector;
    begin
        if Get_Search_On_Filters_Changed (Srch) then
            Contexts := Matching_Contexts (Srch);
            Print_Contexts (Srch, Contexts, 1, Get_Max_Results (Srch));
        end if;
    end Search_Updated;

    ----------------------------------------------------------------------------

    procedure Help_Help is
        use Command_Maps;
    begin
        Put_Line ("Septum is an interactive search tool for code discovery.");
        New_Line;

        Put_Line ("Searches occur across multi-line 'contexts'.  Specify what");
        Put_Line ("those must include with 'find' commands, and skip contexts");
        Put_Line ("containing elements with 'exclude' commands.");
        New_Line;

        Put_Line ("Configurations are loaded from " & SP.Config.Config_Dir_Name & " directories,");
        Put_Line ("in the user's home directory and the current directory when Septum is started.");
        Put_Line ("Commands will be executed from the " & SP.Config.Config_File_Name & " files in these on startup.");
        New_Line;

        -- Print commands.
        for Cursor in Command_Map.Iterate loop
            Put ("    " & Key (Cursor));
            Set_Col (30);
            Put_Line (Element (Cursor).Simple_Help);
        end loop;
    end Help_Help;

    function Help_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
        Command : constant Unbounded_String :=
            (if Command_Line.Is_Empty then Null_Unbounded_String else Command_Line.First_Element);
        Target : constant Unbounded_String := Target_Command (Command);
        use Command_Maps;
    begin
        pragma Unreferenced (Srch);

        case Command_Line.Length is
            when 0 =>
                Help_Help;
            when 1 =>
                if Command_Map.Contains (Target) then
                    declare
                        Cursor  : constant Command_Maps.Cursor := Command_Map.Find (Target);
                        Command : constant Executable_Command  := Command_Maps.Element (Cursor);
                    begin
                        Command.Help.all;
                    end;
                end if;
            when others =>
                Put_Line ("Unknown command");
        end case;
        return Command_Success;
    end Help_Exec;

    ----------------------------------------------------------------------------

    procedure Reload_Help is
    begin
        Put_Line ("Reload help");
    end Reload_Help;

    function Reload_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Refresh should have an empty command line.");
            return Command_Failed;
        end if;
        if not SP.Searches.Reload_Working_Set (Srch) then
            Put_Line ("Aborted reload.");
        end if;
        return Command_Success;
    end Reload_Exec;

    ----------------------------------------------------------------------------

    procedure Stats_Help is
    begin
        Put_Line ("Prints statistics about the file cache.");
    end Stats_Help;

    function Stats_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Stats should have an empty command line.");
            return Command_Failed;
        end if;
        Put_Line ("Files: " & SP.Searches.Num_Files (Srch)'Image);
        Put_Line ("Lines: " & SP.Searches.Num_Lines (Srch)'Image);
        return Command_Success;
    end Stats_Exec;

    ----------------------------------------------------------------------------

    procedure Source_Help is
    begin
        Put_Line ("Loads and runs commands from a file.");
    end Source_Help;

    function Source_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if Command_Line.Is_Empty then
            Put_Line ("Must provide one or more config files to run.");
            return Command_Failed;
        end if;

        for File of Command_Line loop
            declare
                Result : Command_Result;
            begin
                if SP.Searches.Is_Running_Script (Srch, ASU.To_String (File)) then
                    Put_Line ("Script file being sourced is being sourced again.");
                    return Command_Failed;
                end if;

                SP.Searches.Push_Script (Srch, ASU.To_String (File));
                Result := Run_Commands_From_File (Srch, ASU.To_String (File));
                if Result /= Command_Success then
                    SP.Searches.Pop_Script (Srch, ASU.To_String (File));
                    return Result;
                end if;
                SP.Searches.Pop_Script (Srch, ASU.To_String (File));

            exception
                when others =>
                    Put_Line ("Unknown exception");
                    SP.Searches.Pop_Script (Srch, ASU.To_String (File));
            end;
        end loop;

        return Command_Success;
    end Source_Exec;

    ----------------------------------------------------------------------------

    procedure Test_Help is
    begin
        Put_Line ("Tests arguments against all filters.");
    end Test_Help;

    function Test_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if Command_Line.Is_Empty then
            Put_Line ("No filters to test.");
            return Command_Failed;
        end if;

        for Input of Command_Line loop
            Put_Line (Input);

            SP.Searches.Test (Srch, ASU.To_String (Input));

            New_Line;
        end loop;

        return Command_Success;
    end Test_Exec;

    ----------------------------------------------------------------------------

    procedure Add_Dirs_Help is
    begin
        Put_Line ("Adds a directory to the search list.");
    end Add_Dirs_Help;

    function Add_Dirs_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if Command_Line.Is_Empty then
            Put_Line ("Must provide directories to add to the search path.");
            return Command_Failed;
        end if;

        for Directory of Command_Line loop
            if not SP.Searches.Add_Directory (Srch, To_String (Directory)) then
                Put_Line ("Directory load aborted.");
            end if;
        end loop;
        return Command_Success;
    end Add_Dirs_Exec;

    ----------------------------------------------------------------------------

    procedure List_Dirs_Help is
    begin
        Put_Line ("List the directories of the search list.");
    end List_Dirs_Help;

    function List_Dirs_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("No arguments are allowed for directory listing.");
            return Command_Failed;
        end if;
        for Directory of SP.Searches.List_Directories (Srch) loop
            Put_Line (To_String (Directory));
        end loop;
        return Command_Success;
    end List_Dirs_Exec;

    ----------------------------------------------------------------------------

    procedure Clear_Dirs_Help is
    begin
        Put_Line ("Clears all search directories.");
    end Clear_Dirs_Help;

    function Clear_Dirs_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("No arguments are allowed for clearing directories.");
            return Command_Failed;
        end if;
        SP.Searches.Clear_Directories (Srch);
        return Command_Success;
    end Clear_Dirs_Exec;

    ----------------------------------------------------------------------------

    procedure Add_Extensions_Help is
    begin
        Put_Line ("Adds extension to the search list.");
    end Add_Extensions_Help;

    function Add_Extensions_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if Command_Line.Is_Empty then
            Put_Line ("Must provide extensions to filter.");
            return Command_Failed;
        end if;

        for Extension of Command_Line loop
            SP.Searches.Add_Extension (Srch, To_String (Extension));
        end loop;
        return Command_Success;
    end Add_Extensions_Exec;

    ----------------------------------------------------------------------------

    procedure Clear_Extensions_Help is
    begin
        Put_Line ("Clears extension to the search list.");
    end Clear_Extensions_Help;

    function Clear_Extensions_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("No arguments allowed for clearing extension filtering.");
            return Command_Failed;
        end if;

        SP.Searches.Clear_Extensions (Srch);
        return Command_Success;
    end Clear_Extensions_Exec;

    ----------------------------------------------------------------------------

    procedure Remove_Extensions_Help is
    begin
        Put_Line ("Removes extension to the search list.");
    end Remove_Extensions_Help;

    function Remove_Extensions_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if Command_Line.Is_Empty then
            Put_Line ("Must provide extensions to remove from the filter.");
            return Command_Failed;
        end if;

        for Extension of Command_Line loop
            SP.Searches.Remove_Extension (Srch, To_String (Extension));
        end loop;
        return Command_Success;
    end Remove_Extensions_Exec;

    ----------------------------------------------------------------------------

    procedure List_Extensions_Help is
    begin
        Put_Line ("Lists extensions to filter by.");
    end List_Extensions_Help;

    function List_Extensions_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
        Extensions : constant String_Vectors.Vector := SP.Searches.List_Extensions (Srch);
    begin
        pragma Unreferenced (Command_Line);
        for Ext of Extensions loop
            Put_Line (To_String (Ext));
        end loop;
        return Command_Success;
    end List_Extensions_Exec;

    ----------------------------------------------------------------------------

    procedure Find_Text_Help is
    begin
        Put_Line ("Provides text to search for.");
    end Find_Text_Help;

    function Find_Text_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Find_Text (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Find_Text_Exec;

    ----------------------------------------------------------------------------

    procedure Exclude_Text_Help is
    begin
        Put_Line ("Provides text to search for.");
    end Exclude_Text_Help;

    function Exclude_Text_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Exclude_Text (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Exclude_Text_Exec;

    ----------------------------------------------------------------------------

    procedure Find_Like_Help is
    begin
        Put_Line ("Provides text to search for (case insensitive).");
    end Find_Like_Help;

    function Find_Like_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Find_Like (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Find_Like_Exec;

    ----------------------------------------------------------------------------

    procedure Exclude_Like_Help is
    begin
        Put_Line ("Provides text to search for (case insensitive).");
    end Exclude_Like_Help;

    function Exclude_Like_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Exclude_Like (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Exclude_Like_Exec;

    ----------------------------------------------------------------------------

    procedure Find_Regex_Help is
    begin
        Put_Line ("Provides regex to search for.");
    end Find_Regex_Help;

    function Find_Regex_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Find_Regex (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Find_Regex_Exec;

    ----------------------------------------------------------------------------

    procedure Exclude_Regex_Help is
    begin
        Put_Line ("Provides Regex to search for.");
    end Exclude_Regex_Help;

    function Exclude_Regex_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Exclude_Regex (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Exclude_Regex_Exec;

    ----------------------------------------------------------------------------

    procedure List_Filters is
    begin
        Put_Line ("Lists the currently bound filters.");
    end List_Filters;

    function List_Filters_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
        Filter_Names : constant String_Vectors.Vector := SP.Searches.List_Filter_Names (Srch);
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Ignoring unnecessary command line parameters.");
            return Command_Failed;
        end if;
        for Name of Filter_Names loop
            Put_Line (To_String (Name));
        end loop;
        return Command_Success;
    end List_Filters_Exec;
    ----------------------------------------------------------------------------

    procedure Reorder_Help is
    begin
        Put_Line ("Reorders filters, possibly dropping some of them.");
    end Reorder_Help;

    function Parse_Positive_Vector (Command_Line : in String_Vectors.Vector) return SP.Searches.Positive_Vectors.Vector is
        Index : Positive := Positive'Last;
    begin
        return Indices : SP.Searches.Positive_Vectors.Vector do
            for Index_String of Command_Line loop
                if Try_Parse (ASU.To_String (Index_String), Index) then
                    Indices.Append (Index);
                else
                    Put_Line (Index_String & " is not an index");
                end if;
            end loop;
        end return;
    end Parse_Positive_Vector;

    function Reorder_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if SP.Searches.Num_Filters (Srch) = 0 then
            Put_Line ("There are no filters to reorder.");
            return Command_Failed;
        end if;

        if Command_Line.Is_Empty then
            Put_Line ("Filter indices to keep must be provided with reorder.");
            return Command_Failed;
        end if;

        declare
            Indices : constant SP.Searches.Positive_Vectors.Vector := Parse_Positive_Vector (Command_Line);
            Max_Filter_Index : constant Natural := SP.Searches.Num_Filters (Srch);
            use type Ada.Containers.Count_Type;
        begin

            -- Prefer to not alter anything if the parameters are borked.
            if Indices.Length /= Command_Line.Length then
                return Command_Failed;
            end if;

            if (for some Index of Indices => Natural (Index) > Max_Filter_Index) then
                Put_Line ("There are" & Max_Filter_Index'Image & " filters.");
                Put_Line ("All filter indices must be in the range 1 .." & Max_Filter_Index'Image);
                return Command_Failed;
            end if;

            if (for some I in 1 .. Max_Filter_Index => not Indices.Contains (I)) then
                Put_Line ("All filter indices must be provided.");
                Put_Line ("Use 'drop' to remove filters you don't want by index.");
                return Command_Failed;
            end if;

            SP.Searches.Reorder_Filters (Srch, Indices);
            return Command_Success;
        end;
    end Reorder_Exec;

    ----------------------------------------------------------------------------

    procedure Drop_Help is
    begin
        Put_Line ("Drops given filters, or the most recent filter if non given.");
    end Drop_Help;

    function Drop_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if Command_Line.Is_Empty then
            SP.Searches.Pop_Filter (Srch);
            Search_Updated (Srch);
            return Command_Success;
        end if;

        declare
            package Positive_Vector_Sorting is new SP.Searches.Positive_Vectors.Generic_Sorting ("<" => ">");
            Index   : Positive := Positive'Last;
            Indices : SP.Searches.Positive_Vectors.Vector;
            use type Ada.Containers.Count_Type;
        begin
            for Index_String of Command_Line loop
                if Try_Parse (ASU.To_String (Index_String), Index) then
                    if Natural (Index) > SP.Searches.Num_Filters (Srch) then
                        Put_Line ("Filter index out of range:" & Index'Image);
                    else
                        Indices.Append (Index);
                    end if;
                else
                    Put_Line (Index_String & " is not an index.");
                end if;
            end loop;

            -- Prefer to not alter anything if the parameters are borked.
            if Indices.Length /= Command_Line.Length then
                return Command_Failed;
            end if;

            -- Drop filters in reverse order to preserve semantics while keeping
            -- the interface of SP.Searches simple.
            Positive_Vector_Sorting.Sort (Indices);
            for I of Indices loop
                SP.Searches.Drop_Filter (Srch, I);
            end loop;
            return Command_Success;
        end;
    end Drop_Exec;

    ----------------------------------------------------------------------------

    procedure Pop_Help is
    begin
        Put_Line ("Pops the last applied filter from the search.");
    end Pop_Help;

    function Pop_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Ignoring unnecessary command line parameters.");
            return Command_Failed;
        end if;
        SP.Searches.Pop_Filter (Srch);

        Search_Updated (Srch);
        return Command_Success;
    end Pop_Exec;

    ----------------------------------------------------------------------------

    procedure Clear_Filters_Help is
    begin
        Put_Line ("Pops all filters.");
    end Clear_Filters_Help;

    function Clear_Filters_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        pragma Unreferenced (Command_Line);
        SP.Searches.Clear_Filters (Srch);
        return Command_Success;
    end Clear_Filters_Exec;

    ----------------------------------------------------------------------------

    procedure Matching_Contexts_Help is
    begin
        Put_Line ("Lists the Contexts currently matching all filters.");
        New_Line;
        Put_Line ("match-contexts        Prints up to max-results results");
        Put_Line ("match-contexts N      Prints the first N results");
        Put_Line ("match-contexts M N    Prints the M ... N results");
    end Matching_Contexts_Help;

    function Matching_Contexts_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
        Contexts : constant SP.Contexts.Context_Vectors.Vector := SP.Searches.Matching_Contexts (Srch);
        First    : Positive := 1;
        Last     : Positive := Positive'Last;
    begin
        case Command_Line.Length is
        when 2 =>
            if Try_Parse (To_String (Command_Line.First_Element), First)
                and then Try_Parse (To_String (Command_Line.Element (2)), Last)
                and then First <= Last
            then
                SP.Searches.Print_Contexts_With_Cancellation (Srch, Contexts, First, Last);
            else
                SP.Terminal.Put_Line ("Bad number of results to give.");
                return Command_Failed;
            end if;
        when 1 =>
            if not Try_Parse (To_String(Command_Line.First_Element), Last) then
                SP.Terminal.Put_Line ("Bad number of results to give.");
                return Command_Failed;
            end if;

            SP.Searches.Print_Contexts_With_Cancellation (Srch, Contexts, 1, Last);
        when 0 =>
            SP.Searches.Print_Contexts_With_Cancellation (Srch, Contexts, 1, SP.Searches.Get_Max_Results (Srch));
        when others =>
            SP.Terminal.Put_Line ("Expected either no parameter or 1 to give a maximum number of results to return.");
            return Command_Failed;
        end case;
        return Command_Success;
    end Matching_Contexts_Exec;

    ----------------------------------------------------------------------------

    procedure Matching_Files_Help is
    begin
        Put_Line ("Lists files currently matching all filters.");
    end Matching_Files_Help;

    function Matching_Files_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
        Contexts : constant SP.Contexts.Context_Vectors.Vector := SP.Searches.Matching_Contexts (Srch);
        Files : constant String_Sets.Set := SP.Contexts.Files_In (Contexts);
    begin
        pragma Unreferenced (Command_Line);

        SP.Terminal.New_Line;
        for File of Files loop
            SP.Terminal.Put_Line (File);
        end loop;
        New_Line;
        Put_Line ("Matching files:" & Files.Length'Image);

        return Command_Success;
    end Matching_Files_Exec;

    ----------------------------------------------------------------------------

    procedure Quit_Help is
    begin
        Put_Line ("Quits this program.");
    end Quit_Help;

    function Quit_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        pragma Unreferenced (Srch, Command_Line);
        return Command_Exit_Requested;
    end Quit_Exec;

    ----------------------------------------------------------------------------

    procedure Set_Context_Width_Help is
    begin
        Put_Line ("List lines matching the current filter.");
    end Set_Context_Width_Help;

    function Set_Context_Width_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
        Context_Width : Natural := 0;
    begin
        case Natural (Command_Line.Length) is
            when 0 =>
                Put_Line ("Removing context width restriction");
                SP.Searches.Set_Context_Width (Srch, SP.Searches.No_Context_Width);
            when 1 =>
                Context_Width := Natural'Value (To_String (Command_Line.First_Element));
                SP.Searches.Set_Context_Width (Srch, Context_Width);
                Put_Line ("Context width set to " & Context_Width'Image);
            when others =>
                Put_Line
                    ("Expected a single value for the context width or no value to remove context width restriction.");
                return Command_Failed;
        end case;
        return Command_Success;
    exception
        when Constraint_Error =>
            Put_Line ("Invalid context width: " & To_String (Command_Line.First_Element));
        return Command_Failed;
    end Set_Context_Width_Exec;

    ----------------------------------------------------------------------------

    procedure Set_Max_Results_Help is
    begin
        Put_Line ("Sets the maximum number of results which can be returned.");
    end Set_Max_Results_Help;

    function Set_Max_Results_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
        Max_Results : Natural := SP.Searches.No_Max_Results;
    begin
        case Natural (Command_Line.Length) is
            when 0 =>
                Put_Line ("Removing maximum result restriction");
                SP.Searches.Set_Max_Results (Srch, SP.Searches.No_Max_Results);
            when 1 =>
                Max_Results := Natural'Value (To_String (Command_Line.First_Element));
                if Max_Results = 0 then
                    Put_Line ("Must return at least 1 result.");
                    return Command_Failed;
                end if;
                SP.Searches.Set_Max_Results (Srch, Max_Results);
                Put_Line ("Maximum results set to " & Max_Results'Image);
            when others =>
                Put_Line
                    ("Expected a single value for the number of maximum results or no value to remove restriction on number of results.");
                return Command_Failed;
        end case;
        return Command_Success;
    exception
        when Constraint_Error =>
            Put_Line ("Invalid number of maximum results: " & To_String (Command_Line.First_Element));
        return Command_Failed;
    end Set_Max_Results_Exec;

    ----------------------------------------------------------------------------

    procedure Enable_Search_On_Filters_Changed_Help is
    begin
        Put_Line ("Enables searching automatically when filters are changed.");
    end Enable_Search_On_Filters_Changed_Help;

    function Enable_Search_On_Filters_Changed_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
            return Command_Failed;
        end if;
        SP.Searches.Set_Search_On_Filters_Changed (Srch, True);
        return Command_Success;
    end Enable_Search_On_Filters_Changed_Exec;

    procedure Disable_Search_On_Filters_Changed_Help is
    begin
        Put_Line ("Disables searching automatically when filters are changed.");
    end Disable_Search_On_Filters_Changed_Help;

    function Disable_Search_On_Filters_Changed_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
            return Command_Failed;
        end if;
        SP.Searches.Set_Search_On_Filters_Changed (Srch, False);
        return Command_Success;
    end Disable_Search_On_Filters_Changed_Exec;

    ----------------------------------------------------------------------------

    procedure Enable_Line_Numbers_Help is
    begin
        Put_Line ("Enables line numbers in context output.");
    end Enable_Line_Numbers_Help;

    function Enable_Line_Numbers_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
            return Command_Failed;
        end if;
        SP.Searches.Set_Print_Line_Numbers (Srch, True);
        return Command_Success;
    end Enable_Line_Numbers_Exec;

    procedure Disable_Line_Numbers_Help is
    begin
        Put_Line ("Disables line numbers in context output.");
    end Disable_Line_Numbers_Help;

    function Disable_Line_Numbers_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
            return Command_Failed;
        end if;
        SP.Searches.Set_Print_Line_Numbers (Srch, False);
        return Command_Success;
    end Disable_Line_Numbers_Exec;

    ----------------------------------------------------------------------------

    procedure Enable_Line_Colors_Help is
    begin
        Put_Line ("Enables line colors in context output.");
    end Enable_Line_Colors_Help;

    function Enable_Line_Colors_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
            return Command_Failed;
        end if;
        SP.Searches.Set_Line_Colors_Enabled (Srch, True);
        return Command_Success;
    end Enable_Line_Colors_Exec;

    procedure Disable_Line_Colors_Help is
    begin
        Put_Line ("Disables line colors in context output.");
    end Disable_Line_Colors_Help;

    function Disable_Line_Colors_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
            return Command_Failed;
        end if;
        SP.Searches.Set_Line_Colors_Enabled (Srch, False);
        return Command_Success;
    end Disable_Line_Colors_Exec;


    ----------------------------------------------------------------------------

    procedure Make_Command (Command : String; Simple_Help : String; Help : Help_Proc; Exec : Exec_Proc) with
        Pre => Command'Length > 0 and then not Command_Map.Contains (To_Unbounded_String (Command))
    is
    begin
        Command_Map.Insert (To_Unbounded_String (Command), (To_Unbounded_String (Simple_Help), Help, Exec));
    end Make_Command;

begin

    -- Actions

    Make_Command ("help", "Print commands or help for a specific command", Help_Help'Access, Help_Exec'Access);
    Make_Command ("reload", "Reloads the file cache.", Reload_Help'Access, Reload_Exec'Access);
    Make_Command ("stats", "Print file cache statistics.", Stats_Help'Access, Stats_Exec'Access);
    Make_Command ("source", "Loads a configuration from file.", Source_Help'Access, Source_Exec'Access);
    Make_Command ("test", "Check to see which filters would trigger on a line of text.", Test_Help'Access, Test_Exec'Access);

    -- Filters

    Make_Command ("find-text", "Adds filter text.", Find_Text_Help'Access, Find_Text_Exec'Access);
    Make_Command ("exclude-text", "Adds text to exclude.", Exclude_Text_Help'Access, Exclude_Text_Exec'Access);
    Make_Command ("find-like", "Adds filter text (case insensitive).", Find_Like_Help'Access, Find_Like_Exec'Access);
    Make_Command ("exclude-like", "Adds text to exclude (case insensitive).", Exclude_Like_Help'Access, Exclude_Like_Exec'Access);
    Make_Command ("find-regex", "Adds filter regex.", Find_Regex_Help'Access, Find_Regex_Exec'Access);
    Make_Command ("exclude-regex", "Adds regex to exclude.", Exclude_Regex_Help'Access, Exclude_Regex_Exec'Access);
    Make_Command ("list-filters", "Lists all applied filters.", List_Filters'Access, List_Filters_Exec'Access);

    Make_Command ("reorder", "Reorder filters by index.", Reorder_Help'Access, Reorder_Exec'Access);
    Make_Command ("drop", "Drops the filters at the given indices.", Drop_Help'Access, Drop_Exec'Access);
    Make_Command ("pop", "Pops the last applied filter.", Pop_Help'Access, Pop_Exec'Access);
    Make_Command ("clear-filters", "Pops all filters.", Clear_Filters_Help'Access, Clear_Filters_Exec'Access);

    -- Results

    Make_Command
        ("match-contexts", "Lists contexts matching the current filter.", Matching_Contexts_Help'Access,
         Matching_Contexts_Exec'Access);
    Make_Command
        ("match-files", "Lists files matching the current filter.", Matching_Files_Help'Access,
         Matching_Files_Exec'Access);

    -- Global configuration

    Make_Command ("add-dirs", "Adds directory to the search list.", Add_Dirs_Help'Access, Add_Dirs_Exec'Access);
    Make_Command
        ("list-dirs", "List the directories in the search list.", List_Dirs_Help'Access, List_Dirs_Exec'Access);
    Make_Command
        ("clear-dirs", "Removes all directories from the search list.", Clear_Dirs_Help'Access, Clear_Dirs_Exec'Access);

    Make_Command ("only-exts", "Adds extensions to find results in.", Add_Extensions_Help'Access, Add_Extensions_Exec'Access);
    Make_Command
        ("remove-exts", "Removes an extension filter from the search.", Remove_Extensions_Help'Access,
         Remove_Extensions_Exec'Access);
    Make_Command ("clear-exts", "Clears extension filters.", Clear_Extensions_Help'Access, Clear_Extensions_Exec'Access);
    Make_Command ("list-exts", "List current extensions.", List_Extensions_Help'Access, List_Extensions_Exec'Access);

    Make_Command
        ("set-context-width", "Sets the width of the context in which to find matches.", Set_Context_Width_Help'Access,
         Set_Context_Width_Exec'Access);
    Make_Command
        ("set-max-results", "Sets the maximum results returned before only the total number of results are returned.",
         Set_Max_Results_Help'Access, Set_Max_Results_Exec'Access);

    Make_Command
        ("enable-auto-search", "Search when filters are changed automatically", Enable_Search_On_Filters_Changed_Help'Access,
         Enable_Search_On_Filters_Changed_Exec'Access);

    Make_Command
        ("disable-auto-search", "Turn off search when filters are changed automatically", Disable_Search_On_Filters_Changed_Help'Access,
         Disable_Search_On_Filters_Changed_Exec'Access);

    Make_Command
        ("enable-line-numbers", "Enables prefixing of lines with line numbers.", Enable_Line_Numbers_Help'Access,
         Enable_Line_Numbers_Exec'Access);
    Make_Command
        ("disable-line-numbers", "Disables prefixing of lines with line numbers.", Disable_Line_Numbers_Help'Access,
         Disable_Line_Numbers_Exec'Access);

    Make_Command
        ("enable-line-colors", "Enables colorizing lines with matches.", Enable_Line_Colors_Help'Access,
         Enable_Line_Colors_Exec'Access);
    Make_Command
        ("disable-line-colors", "Disables colorizing lines with matches.", Disable_Line_Colors_Help'Access,
         Disable_Line_Colors_Exec'Access);

    -- Quit

    Make_Command ("quit", "Exits the search program.", Quit_Help'Access, Quit_Exec'Access);
    Make_Command ("exit", "Exits the search program.", Quit_Help'Access, Quit_Exec'Access);
end SP.Commands;
