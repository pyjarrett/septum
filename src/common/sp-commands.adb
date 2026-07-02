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

with Ada.Calendar;
with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with AnsiAda;
with SP.Config;
with SP.Contexts;
with SP.File_System;
with SP.Platform;
with SP.Output;

package body SP.Commands is
    pragma Assertion_Policy (Pre => Check, Post => Check);

    use Ada.Strings.Unbounded;
    use SP.Output;

    type Help_Proc is not null access procedure;
    -- Prints a detailed help description for a topic.

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
        return Result : Command_Result := Command_Unknown do
            if Command_Map.Contains (Best_Command) then
                declare
                    It         : constant Command_Maps.Cursor := Command_Map.Find (Best_Command);
                    Command    : constant Executable_Command  := Command_Map.Constant_Reference (It);
                    Parameters : String_Vectors.Vector        := Command_Line;
                begin
                    Parameters.Delete_First;
                    if Best_Command /= Command_Name then
                        Put_Line (SP.Output.UI, "Resolved to: " & To_String (Best_Command));
                    end if;
                    New_Line (SP.Output.UI);

                    declare
                        Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
                        Finish : Ada.Calendar.Time;
                        Delta_Time : Duration;
                        use all type Ada.Calendar.Time;
                    begin
                        --  Start the clock.
                        Result := Command.Exec.all (Srch, Parameters);

                        if SP.Searches.Get_Show_Timings (Srch) then
                            --  End the clock.
                            Finish := Ada.Calendar.Clock;
                            Delta_Time := Finish - Start;
                            Put_Line (SP.Output.UI, To_String (Best_Command) & ": " & Delta_Time'Image);
                        end if;
                    end;
                end;
            elsif Command_Name = ASU.Null_Unbounded_String or else ASU.Element (Command_Name, 1) = '#' then
                -- Comments always succeed.
                Result := SP.Commands.Command_Ignored;
                return;
            end if;
        end return;
    end Execute;

    function Run_Commands_From_File (Srch : in out SP.Searches.Search; File : String) return Command_Result is
        Commands : SP.Strings.String_Vectors.Vector;
        function "+" (S : String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;
    begin
        if not Ada.Directories.Exists (File) then
            Put_Line ("No config to read at: " & Ada.Directories.Full_Name (File));
            return Command_Failed;
        end if;

        Put_Line (SP.Output.UI, "Loading commands from: " & Ada.Directories.Full_Name (File));

        if not SP.File_System.Read_Lines (Ada.Directories.Full_Name (File), Commands) then
            Put_Line ("Unable to load configuration file from: " & Ada.Directories.Full_Name (File));
        end if;

        for Command of Commands loop
            declare
                Exploded : constant SP.Strings.Exploded_Line := SP.Strings.Make (To_String (Command));
                Command_Line : constant String_Vectors.Vector := Exploded.Words;
                Result : Command_Result;
            begin
                SP.Output.New_Line (SP.Output.UI);
                SP.Output.Put_Line (SP.Output.UI, +" > " & Command);
                Result := SP.Commands.Execute (Srch, Command_Line);

                case Result is
                    when Command_Success => null;
                    when Command_Ignored => null;
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

    package Help_Text is
        function Colorize_Command (Command_Name : String) return String;
        procedure Header (Command_Name : String; Simple_Help : String := "");
        procedure Block (Contents : String; Extra_Space : Boolean := True);
        procedure Describe_Command (Command_Name : String; Options : String; Description : String);
    end Help_Text;

    package body Help_Text is
        function Colorize_Command (Command_Name : String) return String is
        begin
            return "|" & SP.Output.Colorize (Command_Name, AnsiAda.Green) & "|";
        end Colorize_Command;

        procedure Header (Command_Name : String; Simple_Help : String := "") is
        begin
            New_Line;
            Put_Line ("-------------------------------------------------------");
            Put_Line (Colorize_Command (Command_Name));
            Put_Line ("-------------------------------------------------------");

            if Simple_Help'Length > 0 then
                Put_Line (Simple_Help);
            end if;
            New_Line;
        end Header;

        procedure Block (Contents : String; Extra_Space : Boolean := True) is
            Width : constant := 80;
            Cursor : Positive := Contents'First;
            Last_In_Line : Positive;

            -- Terminates lines early to avoid overfilling a line past the limit.
            function Last_Space return Natural is
            begin
                if Cursor + Width >= Contents'Last then
                    return Contents'Last;
                end if;

                return Ada.Strings.Fixed.Index (
                    Source => Contents,
                    Set => Ada.Strings.Maps.To_Set (Ada.Strings.Space),
                    From => Positive'Min (Cursor + Width, Contents'Last), -- tries to fill the entire line.
                    Test => Ada.Strings.Inside,
                    Going => Ada.Strings.Backward);
            end Last_Space;

            function First_Non_Space return Natural is
            begin
                if Last_In_Line + 1 >= Contents'Last then
                    return Last_In_Line + 1;
                end if;

                return Ada.Strings.Fixed.Index_Non_Blank (
                    Source => Contents,
                    From => Last_In_Line + 1,
                    Going => Ada.Strings.Forward);
            end First_Non_Space;

        begin
            while Cursor <= Contents'Last loop
                Last_In_Line := Last_Space;
                Put_Line (Contents (Cursor .. Last_In_Line));
                Cursor := First_Non_Space;
            end loop;

            if Extra_Space then
                New_Line;
            end if;
        end Block;

        procedure Describe_Command (Command_Name : String; Options : String; Description : String) is
            Spacer : constant String (1 .. 18 - Command_Name'Length) := (others => ' ');
        begin
            Block (
                Colorize_Command (Command_Name)
                & Spacer
                & " " & Options
                & "   " & Description,
                Extra_Space => False
            );
        end Describe_Command;
    end Help_Text;

    ----------------------------------------------------------------------------

    procedure Help_Help is
        use Command_Maps;
        Global_Config_Dir : constant SP.Strings.String_Holders.Holder := SP.Platform.Global_Config_Dir;
    begin
        Put_Line ("Septum is an interactive search tool for code discovery.");
        New_Line;

        Help_Text.Block("Searches occur across multi-line 'contexts'.  Specify what "
            & "those must include with `find-*` commands, and skip contexts "
            & "containing elements with `exclude-*` commands.");

        Put_Line ("Configurations are loaded from " &
            SP.Output.Colorize (SP.Config.Local_Config_Dir_Name & "/" & SP.Config.Config_File_Name, AnsiAda.Magenta)
            & ".");

        if not Global_Config_Dir.Is_Empty then
            Put_Line ("A global config will be loaded from " &
            SP.Output.Colorize (SP.File_System.Rewrite_Path (Global_Config_Dir.Element & "/" &
                SP.Config.Global_Config_Dir_Name & "/" & SP.Config.Config_File_Name), AnsiAda.Magenta));
        end if;
        Put_Line ("Commands will be executed from the " & SP.Config.Config_File_Name & " files in these on startup.");
        Put_Line ("Blank lines and lines which start with # are ignored.");
        New_Line;

        -- Print commands.
        for Cursor in Command_Map.Iterate loop
            declare
                Command_Padding : constant String (1 .. 26 - Length (Key (Cursor))) := [others => ' '];
            begin
               Put ("    ");
               Put (Key (Cursor));
               Put (Command_Padding);
               Put_Line (Constant_Reference (Command_Map, Cursor).Simple_Help);
            end;
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
                        Command : constant Executable_Command  := Command_Map.Constant_Reference (Cursor);
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

    function Reload_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Reload should have an empty command line.");
            return Command_Failed;
        end if;
        if not SP.Searches.Reload_Working_Set (Srch) then
            Put_Line ("Aborted reload.");
        end if;
        return Command_Success;
    end Reload_Exec;

    ----------------------------------------------------------------------------

    function Unload_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Unload should have an empty command line.");
            return Command_Failed;
        end if;
        SP.Searches.Unload_Working_Set (Srch);
        return Command_Success;
    end Unload_Exec;

    ----------------------------------------------------------------------------

    function Stats_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line (SP.Output.Error, "Stats should have an empty command line.");
            return Command_Failed;
        end if;
        Put_Line (SP.Output.UI, "Files:      " & SP.Searches.Num_Files (Srch)'Image);
        Put_Line (SP.Output.UI, "Lines:      " & SP.Searches.Num_Lines (Srch)'Image);
        Put_Line (SP.Output.UI, "Characters: " & SP.Searches.Num_Characters (Srch)'Image);
        return Command_Success;
    end Stats_Exec;

    ----------------------------------------------------------------------------

    procedure Source_Help is
    begin
        Help_Text.Block (
            Help_Text.Colorize_Command ("run")
            & " executes septum commands from a file, as-if they were run by a user. "
            & "This provides a mechanism for simple configuration, or re-running specific setups "
            & "for complicated searches."
        );
        Help_Text.Block ("`source` is the deprecated alias for `run`.");
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

    function Test_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if Command_Line.Is_Empty then
            Put_Line ("No filters to test.");
            return Command_Failed;
        end if;

        for Input of Command_Line loop
            Put_Line (Input);

            SP.Searches.Test (Srch, ASU.To_String (Input));

            New_Line (Data);
        end loop;

        return Command_Success;
    end Test_Exec;

    ----------------------------------------------------------------------------

    function Add_Files_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) return Command_Result is
    begin
        if Command_Line.Is_Empty then
            Put_Line ("Must provide files to add to the search path.");
            return Command_Failed;
        end if;

        for File of Command_Line loop
            if not SP.Searches.Add_File (Srch, To_String (File)) then
                Put_Line ("File load failed.");
            end if;
        end loop;
        return Command_Success;
    end Add_Files_Exec;

    ----------------------------------------------------------------------------

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

    function Clear_Files_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("No arguments are allowed for clearing files.");
            return Command_Failed;
        end if;
        SP.Searches.Clear_Files (Srch);
        return Command_Success;
    end Clear_Files_Exec;

    ----------------------------------------------------------------------------

    function List_Files_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
        Full : Boolean := False;
        Count : Natural := 0;
        Max_Count : Positive := 100;
        use all type Ada.Containers.Count_Type;
    begin
        if Command_Line.Length = 1 then
            if Command_Line.Constant_Reference (1) = "full" then
                Full := True;
            else
                begin
                   Max_Count := Positive'Value (To_String (Command_Line.Constant_Reference (1)));
                exception
                   when others =>
                       Put_Line ("Invalid parameter, expected 'full' or a positive number.");
                       return Command_Failed;
                end;
            end if;
        elsif Command_Line.Length /= 0 then
            Put_Line ("Unsupported argument for list-files, 'full' is optional.");
            return Command_Failed;
        end if;

        for File of SP.Searches.Files_To_Search (Srch) loop
            Count := Count + 1;
            exit when Count > Max_Count and then not Full;
            Put_Line (File);
        end loop;
        return Command_Success;
    end List_Files_Exec;

    ----------------------------------------------------------------------------

    function Find_Path_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Find_Path (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Find_Path_Exec;

    ----------------------------------------------------------------------------

    function Exclude_Paths_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Exclude_Path (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Exclude_Paths_Exec;

    ----------------------------------------------------------------------------

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

    function Find_Text_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Find_Text (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Find_Text_Exec;

    ----------------------------------------------------------------------------

    function Exclude_Text_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Exclude_Text (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Exclude_Text_Exec;

    ----------------------------------------------------------------------------

    function Find_Like_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Find_Like (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Find_Like_Exec;

    ----------------------------------------------------------------------------

    function Exclude_Like_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Exclude_Like (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Exclude_Like_Exec;

    ----------------------------------------------------------------------------

    function Find_Regex_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Find_Regex (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Find_Regex_Exec;

    ----------------------------------------------------------------------------

    function Exclude_Regex_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        for Word of Command_Line loop
            SP.Searches.Exclude_Regex (Srch, To_String (Word));
        end loop;

        Search_Updated (Srch);
        return Command_Success;
    end Exclude_Regex_Exec;

    ----------------------------------------------------------------------------

    function List_Line_Filters_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
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
    end List_Line_Filters_Exec;

    ----------------------------------------------------------------------------

    function List_Path_Filters_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
        Filter_Names : constant String_Vectors.Vector := SP.Searches.Path_Filter_Names (Srch);
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Ignoring unnecessary command line parameters.");
            return Command_Failed;
        end if;
        for Name of Filter_Names loop
            Put_Line (To_String (Name));
        end loop;
        return Command_Success;
    end List_Path_Filters_Exec;

    ----------------------------------------------------------------------------

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
            Search_Updated (Srch);
            return Command_Success;
        end;
    end Drop_Exec;

    ----------------------------------------------------------------------------

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

    function Clear_Line_Filters_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Ignoring unnecessary command line parameters.");
            return Command_Failed;
        end if;

        pragma Unreferenced (Command_Line);
        SP.Searches.Clear_Filters (Srch);
        return Command_Success;
    end Clear_Line_Filters_Exec;

    ----------------------------------------------------------------------------

    function Clear_Path_Filters_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Ignoring unnecessary command line parameters.");
            return Command_Failed;
        end if;

        pragma Unreferenced (Command_Line);
        SP.Searches.Clear_Path_Filters (Srch);
        return Command_Success;
    end Clear_Path_Filters_Exec;

    ----------------------------------------------------------------------------

    function Match_Contexts_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
        Contexts : constant SP.Contexts.Context_Vectors.Vector := SP.Searches.Matching_Contexts (Srch);
        First    : Positive := 1;
        Last     : Positive := Positive'Last;
    begin
        case Command_Line.Length is
        when 2 =>
            if Try_Parse (To_String (Command_Line.First_Element), First)
                and then Try_Parse (To_String (Command_Line.Constant_Reference (2)), Last)
                and then First <= Last
            then
                SP.Searches.Print_Contexts_With_Cancellation (Srch, Contexts, First, Last);
            else
                SP.Output.Put_Line ("Bad number of results to give.");
                return Command_Failed;
            end if;
        when 1 =>
            if not Try_Parse (To_String(Command_Line.First_Element), Last) then
                SP.Output.Put_Line ("Bad number of results to give.");
                return Command_Failed;
            end if;

            SP.Searches.Print_Contexts_With_Cancellation (Srch, Contexts, 1, Last);
        when 0 =>
            SP.Searches.Print_Contexts_With_Cancellation (Srch, Contexts, 1, SP.Searches.Get_Max_Results (Srch));
        when others =>
            SP.Output.Put_Line ("Expected either no parameter or 1 to give a maximum number of results to return.");
            return Command_Failed;
        end case;
        return Command_Success;
    end Match_Contexts_Exec;

    ----------------------------------------------------------------------------

    function Match_Files_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
        Contexts : constant SP.Contexts.Context_Vectors.Vector := SP.Searches.Matching_Contexts (Srch);
        Files : constant String_Sets.Set := SP.Contexts.Files_In (Contexts);
        Needs_Comma : Boolean := False;
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Ignoring unnecessary command line parameters.");
            return Command_Failed;
        end if;

        if SP.Output.Is_Pipeline then
            Start_Pipeline_Result;
            Put_Line ("{");
                Put ("    ");
                Put_JSON_Key_Value ("command", "match-files");
                Put_Line (",");
                Put ("    ""results"": [");
                for File of Files loop
                    if Needs_Comma then
                        Put_Line (",");
                    else
                        New_Line;
                    end if;
                    Put ("        ");
                    Put (File);
                    Needs_Comma := True;
                end loop;

                if Needs_Comma then
                    New_Line;
                    Put ("    ");
                end if;
                Put_Line ("]");
            Put ("}");
        else
            SP.Output.New_Line (SP.Output.Data);
            for File of Files loop
                SP.Output.Put_Line (File);
            end loop;
            SP.Output.New_Line (SP.Output.Data);
            SP.Output.Put_Line (SP.Output.Data, "Matching files:" & Files.Length'Image);
        end if;

        return Command_Success;
    end Match_Files_Exec;

    ----------------------------------------------------------------------------

    function Quit_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        pragma Unreferenced (Srch, Command_Line);
        return Command_Exit_Requested;
    end Quit_Exec;

    ----------------------------------------------------------------------------

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

    function Set_Max_Results_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
        Max_Results : Natural := SP.Searches.No_Max_Results;
    begin
        case Natural (Command_Line.Length) is
            when 0 =>
                Put_Line (SP.Output.UI, "Removing maximum result restriction");
                SP.Searches.Set_Max_Results (Srch, SP.Searches.No_Max_Results);
            when 1 =>
                Max_Results := Natural'Value (To_String (Command_Line.First_Element));
                if Max_Results = 0 then
                    Put_Line (SP.Output.Error, "Must return at least 1 result.");
                    return Command_Failed;
                end if;
                SP.Searches.Set_Max_Results (Srch, Max_Results);
                Put_Line (SP.Output.UI, "Maximum results set to " & Max_Results'Image);
            when others =>
                Put_Line
                    (SP.Output.Error, "Expected a single value for the number of maximum results or no value to remove restriction on number of results.");
                return Command_Failed;
        end case;
        return Command_Success;
    exception
        when Constraint_Error =>
            Put_Line (SP.Output.Error, "Invalid number of maximum results: " & To_String (Command_Line.First_Element));
        return Command_Failed;
    end Set_Max_Results_Exec;

    ----------------------------------------------------------------------------

    function Enable_Search_On_Filters_Changed_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
            return Command_Failed;
        end if;
        SP.Searches.Set_Search_On_Filters_Changed (Srch, True);
        return Command_Success;
    end Enable_Search_On_Filters_Changed_Exec;

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

    function Enable_Line_Numbers_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
            return Command_Failed;
        end if;
        SP.Searches.Set_Print_Line_Numbers (Srch, True);
        return Command_Success;
    end Enable_Line_Numbers_Exec;

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

    function Enable_Line_Colors_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
            return Command_Failed;
        end if;
        SP.Searches.Set_Line_Colors_Enabled (Srch, True);
        return Command_Success;
    end Enable_Line_Colors_Exec;

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

    function Enable_Timing_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
            return Command_Failed;
        end if;
        SP.Searches.Set_Show_Timings (Srch, True);
        return Command_Success;
    end Enable_Timing_Exec;

    function Disable_Timing_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
            return Command_Failed;
        end if;
        SP.Searches.Set_Show_Timings (Srch, False);
        return Command_Success;
    end Disable_Timing_Exec;

    ----------------------------------------------------------------------------

    procedure Help_Topic_File_Cache is
    begin
        Help_Text.Header ("File Cache", "");

        Help_Text.Block (
            "Septum maintains files and directory contents in memory to speed"
            & " searches.  Anecdotally, this results in ~100 MiB per 1 million"
            & " lines of code."
        );


        Help_Text.Block (
            "Adding directories causes septum to recursively add every file"
            & " which looks like text to the search pool. ""Looks like"" covers "
            & "popularly known extensions (.txt, .cpp, .rs, etc.) while "
            & "ignoring other known binary extensions (.jpg, .png, .zip). "
        );
        Help_Text.Block (
            "If a file's extensions don't match the built-in filters, then "
            & "the first 4 KiB of characters are loaded and the file is "
            & "considered text if a null byte is not found."
        );

        Help_Text.Block (
            "Normally, directories get added for search, and then every file "
            & "is evaluated in turn to decide whether or not it should be loaded."
            & "`add-files` provides a mechanism to add specific files, while not "
            & "loading the containing directory."
        );

        Help_Text.Block (
            Help_Text.Colorize_Command ("add-dirs")
            & " is the primary mechanism through which files get added "
            & "for search."
        );

        Help_Text.Block (
            Help_Text.Colorize_Command ("add-files")
            & " provides a mechanism for target loads, such as for logfiles,"
            & " or otherwise isolated files."
        );

        Help_Text.Block (
            Help_Text.Colorize_Command ("clear-dirs")
            & " removes all search directories and their contents from the file cache."
            & " Files added directly via " & Help_Text.Colorize_Command ("add-files")
            & " are not affected."
        );

        Help_Text.Block (
            Help_Text.Colorize_Command ("clear-files")
            & " removes files directly added to the file cache.  Files discovered "
            & " by recursive directory search are unaffected."
        );

        Help_Text.Block (
            Help_Text.Colorize_Command ("list-dirs")
            & " lists all directories which recursively get traversed looking"
            & " for files to add to the file cache."
        );

        Help_Text.Block (
            Help_Text.Colorize_Command ("list-files")
            & "List the files of the search list."
            & " Supports an optional 'full' argument, otherwise the number"
            & " of printed files is capped."
        );

        Help_Text.Block ("Septum currently doesn't track updates to files to "
            & "loaded directories."
        );
        Help_Text.Block (
            Help_Text.Colorize_Command ("reload")
            & " provides the means to update all currently loaded files with the "
            & "current contents on disk."
        );
        Help_Text.Block (Help_Text.Colorize_Command ("reload")
            & " also provides the counterpart to `unload` which is used to drop "
            & "the file cache."
        );


        Help_Text.Block ("Anecdotally, septum uses ~100 MB per million lines of code loaded "
            & "for search. When dealing with extremely large amounts of text this "
            & "can interfere with other operations.  Instead of shutting down the "
            & "program, instead you can "
            & Help_Text.Colorize_Command ("unload")
            & " the data set, do whatever operations "
            & "you need and then "
            & Help_text.Colorize_Command ("reload")
            & " to bring the files back for search.");

        Help_Text.Block (
            Help_Text.Colorize_Command ("stats")
            & " septum maintains all search context in memory within the file cache. "
            & "Due to the large amount of text that can be loaded, it can be useful "
            & "to examine where and how this storage is used. "
        );

    end Help_Topic_File_Cache;

    ----------------------------------------------------------------------------

    procedure Help_Topic_Line_Filters is
    begin
        Help_Text.Header ("Line Filters", "");

        Help_Text.Describe_Command(
            "find-text",
            "TEXT...",
            "provides a case-sensitive search filter."
        );

        Help_Text.Describe_Command (
            "find-like",
            "TEXT...",
            "provides a case-insensitive filter.");

        Help_Text.Block (
            "Each space separated text parameter to "
            & Help_Text.Colorize_Command ("find-text")
            & " is treated as an additional filter. This supports applying "
            & "multiple text filters and then being able to manipulate individual "
            & "ones using commands like "
            & Help_Text.Colorize_Command ("drop")
            & " and "
            & Help_Text.Colorize_Command ("reorder")
            & "."
        );

        Help_Text.Block(
            Help_Text.Colorize_Command ("exclude-regex")
            & " provides a regex to exclude."
        );

        Help_Text.Block(
            Help_Text.Colorize_Command ("find-regex")
            & " provides a regex to search for."
        );

        Help_Text.Block(
            Help_Text.Colorize_Command ("clear-line-filters")
            & " Removes all line filters, while maintaining currently"
            & " excluded file extensions and path filters."
        );

        Help_Text.Block(
            Help_Text.Colorize_Command ("list-line-filters")
            & " provides a case-insensitive filter."
        );

        Help_Text.Block(
            Help_Text.Colorize_Command ("reorder")
            & " Sometimes you have the right filters, but want to reorder them"
            & " so it's easier to pop specific ones, or to better organize them. "
        );

        Help_Text.Block(
            Help_Text.Colorize_Command ("drop")
            & " drops given filters, or the most recent filter if non given."
        );

        Help_Text.Block (
            Help_Text.Colorize_Command ("pop")
            & " sometimes the filter you just applied is too restrictive"
            & " or you want to try a different approach. "
            & " let's you remove the most recently applied line filter."
        );

        Help_Text.Block (
            Help_Text.Colorize_Command ("test")
            & " provides a mechanism to see why a specific line is getting"
            & " matched or excluded from a search.  This command shows how "
            & " each filter matches against a line of text."
        );

    end Help_Topic_Line_Filters;

    procedure Help_Topic_Path_Filters is
    begin
        Help_Text.Header ("Path Filters");

        Help_Text.Block (
            "All files are considered during the search, unless specific paths"
            & " are requested to be found. "
            & Help_Text.Colorize_Command ("find-path")
            & " restricts the search to only files which match this filter."
        );

        Help_Text.Block (
            "Path filtering occurs at both the path and the extension level."
            & " Paths containing specific elements can be removed, and the results"
            & " can also be tailored to only produce results which match specific"
            & " file extensions."
        );

        Help_Text.Block (
            "All paths are considered search candidates unless a find-path is"
            & " provided.  This means "
            & Help_text.Colorize_Command ("exclude-path")
            & " can be used as an axe to more easily shave off entire portions"
            & " of the search space."
        );

        Help_Text.Describe_Command(
            "clear-exts",
            "",
            "Clears extension filters."
        );

        Help_Text.Describe_Command(
            "clear-path-filters",
            "",
            "Removes all path filters."
        );

        Help_Text.Describe_Command(
            "exclude-path",
            "",
            "Provides path elements to exclude from the search."
        );


        Help_Text.Describe_Command (
            "list-exts",
            "",
            "Lists extensions to filter by."
        );

        Help_Text.Describe_Command(
            "list-path-filters",
            "",
            "Lists the currently bound path filters."
        );

        Help_Text.Describe_Command(
            "only-exts",
            "",
            "Adds extension to the search list."
        );

        Help_Text.Describe_Command(
            "remove-exts",
            "",
            "Removes extension from the search list."
        );

    end Help_Topic_Path_Filters;

    procedure Help_Topic_Results is
    begin
        Help_Text.Header ("Results");

        Put_Line ("Lists the Contexts currently matching all filters.");
        New_Line;
        Put_Line ("match-contexts        Prints up to max-results results");
        Put_Line ("match-contexts N      Prints the first N results");
        Put_Line ("match-contexts M N    Prints the M ... N results");

        Help_Text.Block (
            Help_Text.Colorize_Command ("match-files")
            & " lists all files which match the current filters. "
            & "This command is particularly useful to determine if path filters "
            & "would be effective to cull search results. "
        );

        Help_Text.Block (
            "Due to partial matching of commands, the abbreviated versions"
            & " of match commands are often used instead of the full ones."
        );


        Help_Text.Describe_Command (
            "set-context-width",
            "[COUNT]",
            "Sets the number of lines form the search neighborhood above and below a search term."
        );

        Help_Text.Describe_Command (
            "set-max-results",
            "[COUNT]",
            "Sets the maximum results printed before truncating."
        );

        Help_Text.Describe_Command (
            "enable-auto-search",
            "",
            "Automatically search when filters are updated."
        );

        Help_Text.Describe_Command (
            "enable-line-numbers",
            "",
            "Whether to print line numbers in results."
        );

        Help_Text.Describe_Command (
            "enable-line-colors",
            "",
            "Whether to colorize lines with found search terms."
        );
    end Help_Topic_Results;

    procedure Help_Topic_System is
    begin
        Help_Text.Block (
            "Septum is designed as a interactive search application. "
            & "In typical usage, the program remains 'live' in the background "
            & "in a separate tmux tab or terminal."
        );
        Help_Text.Block (
            Help_Text.Colorize_Command ("reload")
            & " is needed to update text files during heavy edits or when "
            & " rebasing during the day."
        );

        Help_Text.Block (
            Help_Text.Colorize_Command ("enable-timing")
            & " enables reporting of time it takes to run commands."
        );

        Help_Text.Block (
            Help_Text.Colorize_Command ("disable-timing")
            & " disables reporting of time it takes to run commands."
        );

    end Help_Topic_System;

    ----------------------------------------------------------------------------

    procedure Make_Command (Command : String; Simple_Help : String; Help : Help_Proc; Exec : Exec_Proc) with
        Pre => Command'Length > 0 and then not Command_Map.Contains (To_Unbounded_String (Command))
    is
    begin
        Command_Map.Insert (To_Unbounded_String (Command), (To_Unbounded_String (Simple_Help), Help, Exec));
    end Make_Command;

begin

    -- Commands

    Make_Command ("help", "Print commands or help for a specific command", Help_Help'Access, Help_Exec'Access);

    -- File Cache

    Make_Command ("add-files", "Adds files to the search list.", Help_Topic_File_Cache'Access, Add_Files_Exec'Access);
    Make_Command ("add-dirs", "Adds directory to the search list.", Help_Topic_File_Cache'Access, Add_Dirs_Exec'Access);
    Make_Command
        ("list-dirs", "List the directories in the search list.", Help_Topic_File_Cache'Access, List_Dirs_Exec'Access);
    Make_Command
        ("clear-dirs", "Removes all directories from the search list.", Help_Topic_File_Cache'Access, Clear_Dirs_Exec'Access);
    Make_Command
        ("clear-files", "Removes all custom added files from the search list.", Help_Topic_File_Cache'Access, Clear_Files_Exec'Access);
    Make_Command
        ("list-files", "List the files in the search list.", Help_Topic_File_Cache'Access, List_Files_Exec'Access);

    Make_Command ("reload", "Reloads the file cache.", Help_Topic_File_Cache'Access, Reload_Exec'Access);
    Make_Command ("unload", "Unloads the file cache.", Help_Topic_File_Cache'Access, Unload_Exec'Access);
    Make_Command ("stats", "Print file cache statistics.", Help_Topic_File_Cache'Access, Stats_Exec'Access);

    --

    Make_Command ("source", "[DEPRECATED] Loads a configuration from file. Use 'run' instead.", Source_Help'Access, Source_Exec'Access);
    Make_Command ("run", "Loads a configuration from file.", Source_Help'Access, Source_Exec'Access);

    -- Filters

    Make_Command ("find-text", "Adds filter text.", Help_Topic_Line_Filters'Access, Find_Text_Exec'Access);
    Make_Command ("exclude-text", "Adds text to exclude.", Help_Topic_Line_Filters'Access, Exclude_Text_Exec'Access);
    Make_Command ("find-like", "Adds filter text (case insensitive).", Help_Topic_Line_Filters'Access, Find_Like_Exec'Access);
    Make_Command ("exclude-like", "Adds text to exclude (case insensitive).", Help_Topic_Line_Filters'Access, Exclude_Like_Exec'Access);
    Make_Command ("find-regex", "Adds filter regex.", Help_Topic_Line_Filters'Access, Find_Regex_Exec'Access);
    Make_Command ("exclude-regex", "Adds regex to exclude.", Help_Topic_Line_Filters'Access, Exclude_Regex_Exec'Access);

    Make_Command ("reorder", "Reorder filters by index.", Help_Topic_Line_Filters'Access, Reorder_Exec'Access);
    Make_Command ("drop", "Drops the filters at the given indices.", Help_Topic_Line_Filters'Access, Drop_Exec'Access);
    Make_Command ("pop", "Pops the last applied filter.", Help_Topic_Line_Filters'Access, Pop_Exec'Access);
    Make_Command ("clear-line-filters", "Pops all filters.", Help_Topic_Line_Filters'Access, Clear_Line_Filters_Exec'Access);
    Make_Command ("list-line-filters", "Lists all applied line filters.", Help_Topic_Line_Filters'Access, List_Line_Filters_Exec'Access);
    Make_Command ("test", "Check to see which filters would trigger on a line of text.", Help_Topic_Line_Filters'Access, Test_Exec'Access);

    -- Results

    Make_Command
        ("match-contexts", "Lists contexts matching the current filter.", Help_Topic_Results'Access,
         Match_Contexts_Exec'Access);
    Make_Command
        ("match-files", "Lists files matching the current filter.", Help_Topic_Results'Access,
         Match_Files_Exec'Access);

    Make_Command
        ("set-context-width", "Sets the width of the context in which to find matches.", Help_Topic_Results'Access,
         Set_Context_Width_Exec'Access);
    Make_Command
        ("set-max-results", "Sets the maximum results returned before only the total number of results are returned.",
         Help_Topic_Results'Access, Set_Max_Results_Exec'Access);

    Make_Command
        ("enable-auto-search", "Search when filters are changed automatically", Help_Topic_Results'Access,
         Enable_Search_On_Filters_Changed_Exec'Access);
    Make_Command
        ("disable-auto-search", "Turn off search when filters are changed automatically", Help_Topic_Results'Access,
         Disable_Search_On_Filters_Changed_Exec'Access);

    Make_Command
        ("enable-line-numbers", "Enables prefixing of lines with line numbers.", Help_Topic_Results'Access,
         Enable_Line_Numbers_Exec'Access);
    Make_Command
        ("disable-line-numbers", "Disables prefixing of lines with line numbers.", Help_Topic_Results'Access,
         Disable_Line_Numbers_Exec'Access);

    Make_Command
        ("enable-line-colors", "Enables colorizing lines with matches.", Help_Topic_Results'Access,
         Enable_Line_Colors_Exec'Access);
    Make_Command
        ("disable-line-colors", "Disables colorizing lines with matches.", Help_Topic_Results'Access,
         Disable_Line_Colors_Exec'Access);

    -- Path Filtering

    Make_Command ("find-path", "Only look in paths containing this.", Help_Topic_Path_Filters'Access, Find_Path_Exec'Access);
    Make_Command ("exclude-path", "Exclude paths containing this from the search", Help_Topic_Path_Filters'Access, Exclude_Paths_Exec'Access);
    Make_Command ("clear-path-filters", "Pops all filters.", Help_Topic_Path_Filters'Access, Clear_Path_Filters_Exec'Access);
    Make_Command ("list-path-filters", "Lists all applied path filters.", Help_Topic_Path_Filters'Access, List_Path_Filters_Exec'Access);

    Make_Command ("only-exts", "Adds extensions to find results in.", Help_Topic_Path_Filters'Access, Add_Extensions_Exec'Access);
    Make_Command
        ("remove-exts", "Removes an extension filter from the search.", Help_Topic_Path_Filters'Access,
         Remove_Extensions_Exec'Access);
    Make_Command ("clear-exts", "Clears extension filters.", Help_Topic_Path_Filters'Access, Clear_Extensions_Exec'Access);
    Make_Command ("list-exts", "List current extensions.", Help_Topic_Path_Filters'Access, List_Extensions_Exec'Access);

    -- Settings
    Make_Command
        ("enable-timing", "Enables timing of command run time.", Help_Topic_System'Access,
         Enable_Timing_Exec'Access);
    Make_Command
        ("disable-timing", "Disables timing of command run time.", Help_Topic_System'Access,
         Disable_Timing_Exec'Access);

    -- Quit

    Make_Command ("quit", "Exits the search program.", Help_Topic_System'Access, Quit_Exec'Access);
    Make_Command ("exit", "Exits the search program.", Help_Topic_System'Access, Quit_Exec'Access);
end SP.Commands;
