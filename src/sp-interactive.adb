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

with Ada.Directories;
with Ada.Strings.Unbounded;
with SP.Commands;
with SP.Searches; use SP.Searches;
with SP.Strings;  use SP.Strings;
with SP.Terminal;

with GNATCOLL.VFS;

package body SP.Interactive is
    use Ada.Strings.Unbounded;
    use SP.Terminal;

    procedure Write_Prompt (Srch : in Search) is
        -- Writes the prompt and get ready to read user input.
        Default_Prompt : constant String  := " > ";
        Context_Width  : constant Natural := SP.Searches.Get_Context_Width (Srch);
    begin
        New_Line;
        Put ("Files: " & SP.Searches.Num_Files (Srch)'Image);
        Set_Col (20);
        Put ("Distance: " & (if Context_Width = SP.Searches.No_Context_Width then "Any" else Context_Width'Image));
        New_Line;
        Put (Default_Prompt);
    end Write_Prompt;

    function Read_Command return String_Vectors.Vector is
    begin
        declare
            Input : constant Unbounded_String := Get_Line;
        begin
            -- This might want to be a more complicated algorithm for splitting, such as handling quotes
            return Shell_Split (Input);
        end;
    end Read_Command;

    procedure Load_Local_Config (Srch : in out SP.Searches.Search) is
        use GNATCOLL.VFS;
        Config : constant Virtual_File := GNATCOLL.VFS.Get_Home_Directory / ".septum";
        Config_File_Name : constant String := +Config.Full_Name;
        Commands : String_Vectors.Vector;
    begin
        if not Is_Readable (Config) then
            Put_Line ("No config to read at: " & Config_File_Name);
            return;
        end if;

        Put_Line ("Loading commands from local config: " & Config_File_Name);

        if not SP.Strings.Read_Lines (+Config.Full_Name, Commands) then
            Put_Line ("Unable to load configuration file from: " & Config_File_Name);
        end if;

        for Command of Commands loop
            declare
                Command_Line : constant String_Vectors.Vector := Shell_Split(Command);
            begin
                Put_Line (" > " & Command);
                if not SP.Commands.Execute (Srch, Command_Line) then
                    Put_Line ("Unable to execute: " & Command);
                end if;
            end;
        end loop;
    end Load_Local_Config;

    procedure Main is
        -- The interactive loop through which the user starts a search context and then interatively refines it by
        -- pushing and popping operations.
        Command_Line : String_Vectors.Vector;
        Srch         : SP.Searches.Search;
    begin
        Put_Line ("septum v" & SP.Version);
        New_Line;

        Load_Local_Config (Srch);
        Add_Directory (Srch, Ada.Directories.Current_Directory);

        loop
            Write_Prompt (Srch);
            Command_Line := Read_Command;
            if not SP.Commands.Execute (Srch, Command_Line) then
                Put_Line ("Unknown command");
            end if;
        end loop;
    end Main;
end SP.Interactive;
