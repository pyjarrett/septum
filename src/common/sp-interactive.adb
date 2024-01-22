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
with Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with ANSI;
with SP.Commands;
with SP.Config;
with SP.File_System;
with SP.Filters;
with SP.Searches;
with SP.Strings;
with SP.Terminal;
with Trendy_Terminal.Environments;
with Trendy_Terminal.Histories;
with Trendy_Terminal.IO.Line_Editors;
with Trendy_Terminal.Lines.Line_Vectors;
with Trendy_Terminal.Platform;

package body SP.Interactive is
    package ASU renames Ada.Strings.Unbounded;
    use SP.Terminal;

    procedure Write_Prompt (Srch : in SP.Searches.Search) is
        -- Writes the prompt and get ready to read user input.
        Filter_Names   : constant Sp.Strings.String_Vectors.Vector := SP.Searches.List_Filter_Names (Srch);
        Default_Prompt : constant String  := " > ";
        Extensions     : constant SP.Strings.String_Vectors.Vector := SP.Searches.List_Extensions (Srch);
        Context_Width  : constant Natural := SP.Searches.Get_Context_Width (Srch);
        Max_Results    : constant Natural := SP.Searches.Get_Max_Results (Srch);
        Second_Col     : constant := 30;
    begin
        New_Line;
        Put ("Files:     " & SP.Searches.Num_Files (Srch)'Image);
        Set_Col (Second_Col);
        Put ("Extensions:   ");
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

        Put ("Filters: ");
        if Integer (Filter_Names.Length) = 0 then
            Put ("None");
        end if;
        New_Line;
        for Index in 1 .. SP.Strings.String_Vectors.Length (Filter_Names) loop
            Put ("  " & Ada.Strings.Fixed.Trim (Index'Image, Ada.Strings.Left));
            Set_Col (6);
            for Spacer in 1 .. Index loop
                Put ("    ");
            end loop;
            Put_Line (Filter_Names.Element (Integer(Index)));
        end loop;

        New_Line;
        Put (Default_Prompt);
    end Write_Prompt;

    function Apply_Formatting (V : SP.Strings.String_Vectors.Vector) return SP.Strings.String_Vectors.Vector is
        Result : SP.Strings.String_Vectors.Vector;
        use all type ASU.Unbounded_String;
    begin
        for Index in 1 .. V.Length loop
            declare
                US : constant ASU.Unbounded_String := ASU.To_Unbounded_String (V ( Positive (Index)));
                S  : constant String := ASU.To_String (US);
                use all type Ada.Containers.Count_Type;
            begin
                if Positive (Index) = 1 then
                    if SP.Commands.Is_Command (S) or else (SP.Commands.Is_Like_Command (S) and then V.Length > 1) then
                        Result.Append (SP.Terminal.Colorize (S, ANSI.Green));
                    elsif SP.Commands.Is_Like_Command (S) and then V.Length = 1 then
                        declare
                            Command : constant ASU.Unbounded_String := SP.Commands.Target_Command (US);
                            Suffix  : constant ASU.Unbounded_String := SP.Strings.Matching_Suffix (US, Command);
                        begin
                            Result.Append (
                                SP.Terminal.Colorize (S, ANSI.Yellow)
                                & SP.Terminal.Colorize (To_String (Suffix), ANSI.Light_Cyan));
                        end;
                    else
                        Result.Append (SP.Terminal.Colorize (S, ANSI.Red));
                    end if;
                elsif SP.Commands.Target_Command (Asu.To_Unbounded_String (V (1))) = "find-regex"
                    or else SP.Commands.Target_Command (Asu.To_Unbounded_String (V (1))) = "exclude-regex"
                then
                    if SP.Filters.Is_Valid_Regex (S) then
                        Result.Append (SP.Terminal.Colorize (S, ANSI.Green));
                    else
                        Result.Append (SP.Terminal.Colorize (S, ANSI.Red));
                    end if;
                else
                    if SP.File_System.Is_File (S) or else SP.File_System.Is_Dir (S) then
                        Result.Append (SP.Terminal.Colorize (S, ANSI.Magenta));
                    else
                        Result.Append (S);
                    end if;
                end if;
            end;
        end loop;
        return Result;
    end Apply_Formatting;

    function Format_Input (L : Trendy_Terminal.Lines.Line) return Trendy_Terminal.Lines.Line is
        Exploded : constant SP.Strings.Exploded_Line := SP.Strings.Make (Trendy_Terminal.Lines.Current (L));
        New_Line : constant String := ASU.To_String (SP.Strings.Zip (Exploded.Spacers, Apply_Formatting (Exploded.Words)));
    begin
        return Trendy_Terminal.Lines.Make (New_Line, New_Line'Length + 1);
    end Format_Input;

    -- Completion callback based on the number of history inputs.
    function Complete_Input (L : Trendy_Terminal.Lines.Line)
        return Trendy_Terminal.Lines.Line_Vectors.Vector
    is
        E           : SP.Strings.Exploded_Line := SP.Strings.Make (Trendy_Terminal.Lines.Current (L));
        Cursor_Word : constant Positive := SP.Strings.Get_Cursor_Word (E, Trendy_Terminal.Lines.Get_Cursor_Index (L));
        Result      : Trendy_Terminal.Lines.Line_Vectors.Vector;
        Completion  : ASU.Unbounded_String;
        Suffix      : ASU.Unbounded_String;
--        use all type ASU.Unbounded_String;
        use SP.Strings.String_Vectors;
        use type Ada.Containers.Count_Type;
    begin
        if E.Words.Length < Ada.Containers.Count_Type (Cursor_Word) then
            return Result;
        end if;

        -- Find the position of the cursor within line.
        if Cursor_Word = 1 then
            if SP.Commands.Is_Like_Command (E.Words(1)) then
                Completion := SP.Commands.Target_Command (Asu.To_Unbounded_String (E.Words(1)));
                Suffix := SP.Strings.Matching_Suffix (Asu.To_Unbounded_String (E.Words (1)), Completion);
                E.Words (1) := E.Words (1) & Asu.To_String (Suffix);
                Result.Append (Trendy_Terminal.Lines.Make (ASU.To_String (SP.Strings.Zip (E.Spacers, E.Words)),
                    Trendy_Terminal.Lines.Get_Cursor_Index (L) + Trendy_Terminal.Lines.Num_Cursor_Positions (ASU.To_String (Suffix))));
                return Result;
            end if;
        else
            declare
                Completions : SP.Strings.String_Vectors.Vector := SP.File_System.File_Completions (E.Words (Cursor_Word));
                package String_Sorting is new SP.Strings.String_Vectors.Generic_Sorting;
            begin
                String_Sorting.Sort (Completions);
                for Completion of Completions loop
                    E.Words (Cursor_Word) := Completion;
                    Result.Append (Trendy_Terminal.Lines.Make (ASU.To_String (SP.Strings.Zip (E.Spacers, E.Words)),
                        SP.Strings.Cursor_Position_At_End_Of_Word (E, Cursor_Word)));
                end loop;
            end;
        end if;

        if Result.Is_Empty then
            Result.Append (L);
        end if;

        return Result;
    end Complete_Input;

    function Read_Command (Line_History : aliased in out Trendy_Terminal.Histories.History) return ASU.Unbounded_String is
        Input : constant ASU.Unbounded_String := ASU.To_Unbounded_String(
            Trendy_Terminal.IO.Line_Editors.Get_Line (
                Format_Fn     => Format_Input'Access,
                Completion_Fn => Complete_Input'Access,
                Line_History  => Line_History'Unchecked_Access
            ));
    begin
        -- Keep the input remaining on the line without clearing it.
        New_Line;

        return Input;
    end Read_Command;

    -- The interactive loop through which the user starts a search context and then interatively refines it by
    -- pushing and popping operations.
    procedure Main is
        Input        : ASU.Unbounded_String;
        Command_Line : SP.Strings.String_Vectors.Vector;
        Srch         : SP.Searches.Search;
        Configs      : constant SP.Strings.String_Vectors.Vector := SP.Config.Config_Locations;
        Environment  : Trendy_Terminal.Environments.Environment;
        Result       : SP.Commands.Command_Result;
        Line_History : aliased Trendy_Terminal.Histories.History;
    begin
        if not Environment.Is_Available then
            Ada.Text_IO.Put_Line ("[ERROR] No support either for UTF-8 or VT100.");
            Ada.Text_IO.Put_Line ("[ERROR] Try another terminal.");
            return;
        end if;

        Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Echo, False);
        Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Line_Input, False);
        Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Escape_Sequences, True);
        Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Signals_As_Input, True);

        Set_Col(1);
        Put_Line ("septum v" & SP.Version);
        New_Line;

        for Config of Configs loop
            Result := SP.Commands.Run_Commands_From_File (Srch, Config);
            case Result is
                when SP.Commands.Command_Success => null;
                when SP.Commands.Command_Failed =>
                    Put_Line ("Failing running commands from: " & Config);
                    return;
                when SP.Commands.Command_Unknown =>
                    Put_Line ("Unknown command in: " & Config);
                when SP.Commands.Command_Exit_Requested =>
                    return;
            end case;
        end loop;

        loop
            Write_Prompt (Srch);
            Input := Read_Command (Line_History);
            Command_Line := SP.Strings.Split_Command (Input);

            if not Command_Line.Is_Empty then
                Result := SP.Commands.Execute (Srch, Command_Line);
                case Result is
                    when SP.Commands.Command_Success => null;
                        -- Add command to history
                        Trendy_Terminal.Histories.Add (Line_History, ASU.To_String (Input));
                    when SP.Commands.Command_Failed =>
                        Put_Line ("Command failed");
                    when SP.Commands.Command_Unknown =>
                        Put_Line ("Unknown command");
                    when SP.Commands.Command_Exit_Requested =>
                        return;
                end case;
            end if;
        end loop;
    end Main;
end SP.Interactive;
