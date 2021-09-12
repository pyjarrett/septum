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
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with ANSI;
with SP.Commands;
with SP.Config;
with SP.File_System;
with SP.Searches; use SP.Searches;
with SP.Strings;  use SP.Strings;
with SP.Terminal;

with Trendy_Terminal.IO;
with Trendy_Terminal.Platform;

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
        -- Put (Default_Prompt);
        New_Line;
    end Write_Prompt;

    function Apply_Formatting (V : SP.Strings.String_Vectors.Vector) return SP.Strings.String_Vectors.Vector is
        Result : SP.Strings.String_Vectors.Vector;
    begin
        for Index in 1 .. V.Length loop
            declare
                US : constant Unbounded_String := V ( Positive (Index));
                S  : constant String := To_String (US);
            begin
                if Positive (Index) = 1 then
                    if SP.Commands.Is_Command (S) then
                        Result.Append (SP.Terminal.Colorize(US, ANSI.Green));
                    elsif SP.Commands.Is_Like_Command (S) then
                        declare
                            Command       : constant ASU.Unbounded_String := SP.Commands.Target_Command (US);
                            Prefix_Length : constant Natural := SP.Strings.Common_Prefix_Length (US, Command);
                            Suffix        : constant ASU.Unbounded_String := ASU.Unbounded_Slice (Command, Prefix_Length + 1, ASU.Length (Command));
                        begin
                            Result.Append (
                                SP.Terminal.Colorize (US, ANSI.Yellow)
                                & SP.Terminal.Colorize (Suffix, ANSI.Light_Cyan));
                        end;
                    else
                        Result.Append (SP.Terminal.Colorize (US, ANSI.Red));
                    end if;
                else
                    if SP.File_System.Is_File (S) or else SP.File_System.Is_Dir (S) then
                        Result.Append (SP.Terminal.Colorize (US, ANSI.Magenta));
                    else
                        Result.Append (US);
                    end if;
                end if;
            end;
        end loop;
        return Result;
    end Apply_Formatting;

    function Format_Input (S : String) return String is
        Exploded : constant SP.Strings.Exploded_Line := SP.Strings.Make (S);
    begin
        return To_String (SP.Strings.Zip (Exploded.Spacers, Apply_Formatting (Exploded.Words)));
    end Format_Input;

    function Debug_Input (S : String) return String is
        Exploded : constant SP.Strings.Exploded_Line := SP.Strings.Make (S);
        Output   : Unbounded_String;
    begin
        Append (Output, SP.Strings.Zip (Exploded.Spacers, Exploded.Words));
        return To_String (Output);
    end Debug_Input;

    function Read_Command return String_Vectors.Vector is
    begin
        declare
            Input : constant Unbounded_String := To_Unbounded_String(
                Trendy_Terminal.IO.Get_Line(
                    Format_Fn => Format_Input'Access
                    -- Debug_Fn => Debug_Input'Access
                )
            );
            Exploded : constant SP.Strings.Exploded_Line := SP.Strings.Make (To_String (Input));
            Result : SP.Strings.String_Vectors.Vector;
        begin
            Trendy_Terminal.IO.Put_Line("");

            for Word of Exploded.Words loop
                if SP.Strings.Is_Quoted (To_String (Word)) then
                    Result.Append (Unbounded_Slice (Word, 2, Length (Word) - 1));
                else
                    Result.Append (Word);
                end if;
            end loop;

            return Result;
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

        if not Trendy_Terminal.Platform.Init then
            return;
        end if;
        Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Echo, False);
        Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Line_Input, False);
        Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Escape_Sequences, True);

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
