-------------------------------------------------------------------------------
-- Septum, a tool for interactive file search and analysis.
--
-- Copyright (C) 2021, The Septum developers (see AUTHORS file)
--
-- Septum is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Septum is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with GNATCOLL.VFS;
with SP.Contexts;
with SP.Terminal;

package body SP.Commands is
    pragma Assertion_Policy (Pre => Check, Post => Check);

    use Ada.Strings.Unbounded;
    use SP.Terminal;

    type Help_Proc is not null access procedure;
    -- Prints a detailed help description for a command.

    type Exec_Proc is not null access procedure
        (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector);
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

    Command_Map : Command_Maps.Map;

    function Target_Command (Command_Name : Unbounded_String) return Unbounded_String with
        Post => Target_Command'Result = Null_Unbounded_String or else Command_Map.Contains (Target_Command'Result)
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
            if Next_Match_Size = Best_Match_Size then
                -- Two things with the same prefix, the prefix is ambiguous.
                Best_Match := Null_Unbounded_String;
                Ambiguous  := True;
            elsif Next_Match_Size > Best_Match_Size then
                Best_Match_Size := Next_Match_Size;
                Best_Match      := Next_Match;
                Ambiguous       := False;
            end if;
        end loop;

        return (if Ambiguous then Null_Unbounded_String else Best_Match);
    end Target_Command;

    function Execute (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Boolean is
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
                Command.Exec.all (Srch, Parameters);
                return True;
            end;
        end if;
        return False;
    end Execute;

    procedure Load_Local_Config (Srch : in out SP.Searches.Search; File : String) is
        use GNATCOLL.VFS;
        Config           : constant Virtual_File := GNATCOLL.VFS.Create(+File);
        Config_File_Name : constant String       := +Config.Full_Name;
        Commands         : String_Vectors.Vector;
    begin
        if not Is_Readable (Config) then
            Put_Line ("No config to read at: " & Config_File_Name);
            return;
        end if;

        Put_Line ("Loading commands from: " & Config_File_Name);

        if not SP.Strings.Read_Lines (+Config.Full_Name, Commands) then
            Put_Line ("Unable to load configuration file from: " & Config_File_Name);
        end if;

        for Command of Commands loop
            declare
                Command_Line : constant String_Vectors.Vector := Shell_Split (Command);
            begin
                Put_Line (" > " & Command);
                if not SP.Commands.Execute (Srch, Command_Line) then
                    Put_Line ("Unable to execute: " & Command);
                end if;
            end;
        end loop;
    end Load_Local_Config;

    ----------------------------------------------------------------------------

    procedure Help_Help is
        use Command_Maps;
    begin
        for Cursor in Command_Map.Iterate loop
            Put ("    " & Key (Cursor));
            Set_Col (30);
            Put_Line (Element (Cursor).Simple_Help);
        end loop;
    end Help_Help;

    procedure Help_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) is
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
    end Help_Exec;

    ----------------------------------------------------------------------------

    procedure Reload_Help is
    begin
        Put_Line ("Reload help");
    end Reload_Help;

    procedure Reload_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Refresh should have an empty command line.");
        end if;
        SP.Searches.Reload_Working_Set (Srch);
    end Reload_Exec;

    ----------------------------------------------------------------------------

    procedure Stats_Help is
    begin
        Put_Line ("Prints statistics about the file cache.");
    end Stats_Help;

    procedure Stats_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Stats should have an empty command line.");
        end if;
        Put_Line ("Files: " & SP.Searches.Num_Files (Srch)'Image);
        Put_Line ("Lines: " & SP.Searches.Num_Lines (Srch)'Image);
    end Stats_Exec;

    ----------------------------------------------------------------------------

    procedure Add_Dirs_Help is
    begin
        Put_Line ("Adds a directory to the search list.");
    end Add_Dirs_Help;

    procedure Add_Dirs_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) is
    begin
        if Command_Line.Is_Empty then
            Put_Line ("Must provide directories to add to the search path.");
            return;
        end if;

        for Directory of Command_Line loop
            SP.Searches.Add_Directory (Srch, To_String (Directory));
        end loop;
    end Add_Dirs_Exec;

    ----------------------------------------------------------------------------

    procedure List_Dirs_Help is
    begin
        Put_Line ("List the directories of the search list.");
    end List_Dirs_Help;

    procedure List_Dirs_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("No arguments are allowed for directory listing.");
        end if;
        for Directory of SP.Searches.List_Directories (Srch) loop
            Put_Line (To_String (Directory));
        end loop;
    end List_Dirs_Exec;

    ----------------------------------------------------------------------------

    procedure Clear_Dirs_Help is
    begin
        Put_Line ("Clears all search directories.");
    end Clear_Dirs_Help;

    procedure Clear_Dirs_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("No arguments are allowed for clearing directories.");
        end if;
        SP.Searches.Clear_Directories (Srch);
    end Clear_Dirs_Exec;

    ----------------------------------------------------------------------------

    procedure Add_Extensions_Help is
    begin
        Put_Line ("Adds extension to the search list.");
    end Add_Extensions_Help;

    procedure Add_Extensions_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) is
    begin
        if Command_Line.Is_Empty then
            Put_Line ("Must provide extensions to filter.");
            return;
        end if;

        for Extension of Command_Line loop
            SP.Searches.Add_Extension (Srch, To_String (Extension));
        end loop;
    end Add_Extensions_Exec;

    ----------------------------------------------------------------------------

    procedure Remove_Extensions_Help is
    begin
        Put_Line ("Removes extension to the search list.");
    end Remove_Extensions_Help;

    procedure Remove_Extensions_Exec (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) is
    begin
        if Command_Line.Is_Empty then
            Put_Line ("Must provide extensions to filter.");
            return;
        end if;

        for Extension of Command_Line loop
            SP.Searches.Remove_Extension (Srch, To_String (Extension));
        end loop;
    end Remove_Extensions_Exec;

    ----------------------------------------------------------------------------

    procedure List_Extensions_Help is
    begin
        Put_Line ("Lists extensions to filter by.");
    end List_Extensions_Help;

    procedure List_Extensions_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
        Extensions : constant String_Vectors.Vector := SP.Searches.List_Extensions (Srch);
    begin
        pragma Unreferenced (Command_Line);
        for Ext of Extensions loop
            Put_Line (To_String (Ext));
        end loop;
    end List_Extensions_Exec;

    ----------------------------------------------------------------------------

    procedure Find_Text_Help is
    begin
        Put_Line ("Provides text to search for.");
    end Find_Text_Help;

    procedure Find_Text_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
    begin
        for Word of Command_Line loop
            SP.Searches.Find_Text (Srch, To_String (Word));
        end loop;
    end Find_Text_Exec;

    ----------------------------------------------------------------------------

    procedure Exclude_Text_Help is
    begin
        Put_Line ("Provides text to search for.");
    end Exclude_Text_Help;

    procedure Exclude_Text_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
    begin
        for Word of Command_Line loop
            SP.Searches.Exclude_Text (Srch, To_String (Word));
        end loop;
    end Exclude_Text_Exec;

    ----------------------------------------------------------------------------

    procedure Find_Regex_Help is
    begin
        Put_Line ("Provides regex to search for.");
    end Find_Regex_Help;

    procedure Find_Regex_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
    begin
        for Word of Command_Line loop
            SP.Searches.Find_Regex (Srch, To_String (Word));
        end loop;
    end Find_Regex_Exec;

    ----------------------------------------------------------------------------

    procedure Exclude_Regex_Help is
    begin
        Put_Line ("Provides Regex to search for.");
    end Exclude_Regex_Help;

    procedure Exclude_Regex_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
    begin
        for Word of Command_Line loop
            SP.Searches.Exclude_Regex (Srch, To_String (Word));
        end loop;
    end Exclude_Regex_Exec;

    ----------------------------------------------------------------------------

    procedure List_Filters is
    begin
        Put_Line ("Lists the currently bound filters.");
    end List_Filters;

    procedure List_Filters_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
        Filter_Names : constant String_Vectors.Vector := SP.Searches.List_Filter_Names (Srch);
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Ignoring unnecessary command line parameters.");
        end if;
        for Name of Filter_Names loop
            Put_Line (To_String (Name));
        end loop;
    end List_Filters_Exec;

    ----------------------------------------------------------------------------

    procedure Pop_Help is
    begin
        Put_Line ("Pops the last applied filter from the search.");
    end Pop_Help;

    procedure Pop_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Ignoring unnecessary command line parameters.");
        end if;
        SP.Searches.Pop_Filter (Srch);
    end Pop_Exec;

    ----------------------------------------------------------------------------

    procedure Clear_Filters_Help is
    begin
        Put_Line ("Pops all filters.");
    end Clear_Filters_Help;

    procedure Clear_Filters_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
    begin
        pragma Unreferenced (Command_Line);
        SP.Searches.Clear_Filters (Srch);
    end Clear_Filters_Exec;

    ----------------------------------------------------------------------------

    procedure Matching_Contexts_Help is
    begin
        Put_Line ("Lists the Contexts currently matching all filters.");
    end Matching_Contexts_Help;

    procedure Matching_Contexts_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
        C : constant SP.Contexts.Context_Vectors.Vector := SP.Searches.Matching_Contexts (Srch);
    begin
        pragma Unreferenced (Command_Line);
        SP.Searches.Print_Contexts (Srch, C);
    end Matching_Contexts_Exec;

    ----------------------------------------------------------------------------

    procedure Quit_Help is
    begin
        Put_Line ("Quits this program.");
    end Quit_Help;

    procedure Quit_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
    begin
        pragma Unreferenced (Srch, Command_Line);
        GNAT.OS_Lib.OS_Exit (Status => 0);
    end Quit_Exec;

    ----------------------------------------------------------------------------

    procedure Set_Context_Width_Help is
    begin
        Put_Line ("List lines matching the current filter.");
    end Set_Context_Width_Help;

    procedure Set_Context_Width_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
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
        end case;
    exception
        when Constraint_Error =>
            Put_Line ("Invalid context width: " & To_String (Command_Line.First_Element));
    end Set_Context_Width_Exec;

    ----------------------------------------------------------------------------

    procedure Set_Max_Results_Help is
    begin
        Put_Line ("Sets the maximum number of results which can be returned.");
    end Set_Max_Results_Help;

    procedure Set_Max_Results_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
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
                    return;
                end if;
                SP.Searches.Set_Max_Results (Srch, Max_Results);
                Put_Line ("Maximum results set to " & Max_Results'Image);
            when others =>
                Put_Line
                    ("Expected a single value for the number of maximum results or no value to remove restriction on number of results.");
        end case;
    exception
        when Constraint_Error =>
            Put_Line ("Invalid number of maximum results: " & To_String (Command_Line.First_Element));
    end Set_Max_Results_Exec;

    ----------------------------------------------------------------------------

    procedure Enable_Line_Numbers_Help is
    begin
        Put_Line ("Enables line numbers in context output.");
    end Enable_Line_Numbers_Help;

    procedure Enable_Line_Numbers_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
        end if;
        SP.Searches.Set_Print_Line_Numbers (Srch, True);
    end Enable_Line_Numbers_Exec;

    procedure Disable_Line_Numbers_Help is
    begin
        Put_Line ("Disables line numbers in context output.");
    end Disable_Line_Numbers_Help;

    procedure Disable_Line_Numbers_Exec (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Command line should be empty.");
        end if;
        SP.Searches.Set_Print_Line_Numbers (Srch, False);
    end Disable_Line_Numbers_Exec;

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

    -- Filters

    Make_Command ("find-text", "Adds filter text.", Find_Text_Help'Access, Find_Text_Exec'Access);
    Make_Command ("exclude-text", "Adds text to exclude.", Exclude_Text_Help'Access, Exclude_Text_Exec'Access);
    Make_Command ("find-regex", "Adds filter regex.", Find_Regex_Help'Access, Find_Regex_Exec'Access);
    Make_Command ("exclude-regex", "Adds regex to exclude.", Exclude_Regex_Help'Access, Exclude_Regex_Exec'Access);
    Make_Command ("list-filters", "Lists all applied filters.", List_Filters'Access, List_Filters_Exec'Access);

    Make_Command ("pop", "Pops the last applied filter.", Pop_Help'Access, Pop_Exec'Access);
    Make_Command ("clear-filters", "Pops all filters.", Clear_Filters_Help'Access, Clear_Filters_Exec'Access);

    -- Results

    Make_Command
        ("matching-contexts", "Lists contexts matching the current filter.", Matching_Contexts_Help'Access,
         Matching_Contexts_Exec'Access);

    -- Global configuration

    Make_Command ("add-dirs", "Adds directory to the search list.", Add_Dirs_Help'Access, Add_Dirs_Exec'Access);
    Make_Command
        ("list-dirs", "List the directories in the search list.", List_Dirs_Help'Access, List_Dirs_Exec'Access);
    Make_Command
        ("clear-dirs", "Removes all directories from the search list.", Clear_Dirs_Help'Access, Clear_Dirs_Exec'Access);

    Make_Command ("add-exts", "Adds extensions to filter by.", Add_Extensions_Help'Access, Add_Extensions_Exec'Access);
    Make_Command
        ("remove-exts", "Removes extensions from the search.", Remove_Extensions_Help'Access,
         Remove_Extensions_Exec'Access);
    Make_Command ("list-exts", "List current extensions.", List_Extensions_Help'Access, List_Extensions_Exec'Access);

    Make_Command
        ("set-context-width", "Sets the width of the context in which to find matches.", Set_Context_Width_Help'Access,
         Set_Context_Width_Exec'Access);
    Make_Command
        ("set-max-results", "Sets the maximum results returned before only the total number of results are returned.",
         Set_Max_Results_Help'Access, Set_Max_Results_Exec'Access);

    Make_Command
        ("enable-line-numbers", "Enables prefixing of lines with line numbers.", Enable_Line_Numbers_Help'Access,
         Enable_Line_Numbers_Exec'Access);
    Make_Command
        ("disable-line-numbers", "Disables prefixing of lines with line numbers.", Disable_Line_Numbers_Help'Access,
         Disable_Line_Numbers_Exec'Access);

    -- Quit

    Make_Command ("quit", "Exits the search program.", Quit_Help'Access, Quit_Exec'Access);
    Make_Command ("exit", "Exits the search program.", Quit_Help'Access, Quit_Exec'Access);
end SP.Commands;
