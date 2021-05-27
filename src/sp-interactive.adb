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

with Ada.Strings.Unbounded;
with SP.Commands;
with SP.Config;
with SP.Searches; use SP.Searches;
with SP.Strings;  use SP.Strings;
with SP.Terminal;

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

    procedure Main is
        -- The interactive loop through which the user starts a search context and then interatively refines it by
        -- pushing and popping operations.
        Command_Line : String_Vectors.Vector;
        Srch         : SP.Searches.Search;
        Configs      : constant String_Vectors.Vector := SP.Config.Config_Locations;
    begin
        Put_Line ("septum v" & SP.Version);
        New_Line;

        for Config of Configs loop
            if not SP.Commands.Run_Commands_From_File (Srch, To_String(Config)) then
                Put_Line ("Failing running commands from: " & To_String(Config));
                return;
            end if;
        end loop;

        loop
            Write_Prompt (Srch);
            Command_Line := Read_Command;
            if not SP.Commands.Execute (Srch, Command_Line) then
                Put_Line ("Unknown command");
            end if;
        end loop;
    end Main;
end SP.Interactive;
