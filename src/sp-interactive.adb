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

with Ada.Strings.Unbounded;
with ANSI;
with SP.Commands;
with SP.Config;
with SP.Searches; use SP.Searches;
with SP.Strings;  use SP.Strings;
with SP.Terminal;

with Trendy_Terminal;

package body SP.Interactive is
    use Ada.Strings.Unbounded;
    use SP.Terminal;

    procedure Write_Prompt (Srch : in Search) is
        -- Writes the prompt and get ready to read user input.
        Default_Prompt : constant String  := " > ";
        Extensions     : constant String_Vectors.Vector := List_Extensions (Srch);
        Context_Width  : constant Natural := SP.Searches.Get_Context_Width (Srch);
        Max_Results    : constant Natural := SP.Searches.Get_Max_Results (Srch);
        Second_Col     : constant := 30;
    begin
        New_Line;
        Put ("Files:     " & SP.Searches.Num_Files (Srch)'Image);
        Set_Col (Second_Col);
        Put ("Extensions: ");
        if Extensions.Is_Empty then
            Put ("Any");
        else
            Put ("(only) ");
            for Extension of Extensions loop
                Put (Extension);
                Put (" ");
            end loop;
        end if;
        New_Line;
        Put ("Distance:  " & (if Context_Width = SP.Searches.No_Context_Width then "Any" else Context_Width'Image));
        Set_Col (Second_Col);
        Put ("Max Results: " & (if Max_Results = SP.Searches.No_Max_Results then "Unlimited" else Max_Results'Image));
        New_Line;
        Put (Default_Prompt);
    end Write_Prompt;

    function Format_Input (S : String) return String is
    begin
        if SP.Commands.Is_Command (S) then
            return ANSI.Foreground (ANSI.Green) & S & ANSI.Foreground (ANSI.Default);
        else
            return ANSI.Foreground (ANSI.Red) & S & ANSI.Foreground (ANSI.Default);
        end if;

    end Format_Input;

    function Read_Command return String_Vectors.Vector is
    begin
        declare
            Input : constant Unbounded_String := To_Unbounded_String(Trendy_Terminal.Get_Line(Format_Input'Access));
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

        if not Trendy_Terminal.Init then
            return;
        end if;
        Trendy_Terminal.Set (Trendy_Terminal.Echo, False);
        Trendy_Terminal.Set (Trendy_Terminal.Line_Input, False);
        Trendy_Terminal.Set (Trendy_Terminal.Escape_Sequences, True);

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

        --
        -- Trendy_Terminal.Shutdown;
    end Main;
end SP.Interactive;
