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
with SP.File_System;
with SP.Searches;
with SP.Strings;
with SP.Terminal;
with Trendy_Terminal.Input;
with Trendy_Terminal.IO;
with Trendy_Terminal.Platform;

package body SP.Interactive is
    package ASU renames Ada.Strings.Unbounded;
    use SP.Terminal;

    procedure Write_Prompt (Srch : in SP.Searches.Search) is
        -- Writes the prompt and get ready to read user input.
        Default_Prompt : constant String  := " > ";
        Extensions     : constant SP.Strings.String_Vectors.Vector := SP.Searches.List_Extensions (Srch);
        Context_Width  : constant Natural := SP.Searches.Get_Context_Width (Srch);
        Max_Results    : constant Natural := SP.Searches.Get_Max_Results (Srch);
        Second_Col     : constant := 30;
    begin
        pragma Unreferenced (Default_Prompt);
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

    function Trailing_End (Current, Desired : ASU.Unbounded_String) return ASU.Unbounded_String is
        Prefix_Length : constant Natural := SP.Strings.Common_Prefix_Length (Current, Desired);
        Suffix        : constant ASU.Unbounded_String := ASU.Unbounded_Slice (Desired, Prefix_Length + 1, ASU.Length (Desired));
    begin
        return Suffix;
    end Trailing_End;

    function Apply_Formatting (V : SP.Strings.String_Vectors.Vector) return SP.Strings.String_Vectors.Vector is
        Result : SP.Strings.String_Vectors.Vector;
        use all type ASU.Unbounded_String;
    begin
        for Index in 1 .. V.Length loop
            declare
                US : constant ASU.Unbounded_String := V ( Positive (Index));
                S  : constant String := ASU.To_String (US);
            begin
                if Positive (Index) = 1 then
                    if SP.Commands.Is_Command (S) then
                        Result.Append (SP.Terminal.Colorize(US, ANSI.Green));
                    elsif SP.Commands.Is_Like_Command (S) then
                        declare
                            Command : constant ASU.Unbounded_String := SP.Commands.Target_Command (US);
                            Suffix  : constant ASU.Unbounded_String := Trailing_End (US, Command);
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
        return ASU.To_String (SP.Strings.Zip (Exploded.Spacers, Apply_Formatting (Exploded.Words)));
    end Format_Input;

    function Get_Cursor_Word (E : SP.Strings.Exploded_Line; Cursor_Position : Positive) return Natural
    is
        Next           : Natural := 1;
        Current_Cursor : Natural := 1;
    begin
        while Next <= Natural (E.Spacers.Length) loop
            Current_Cursor := Current_Cursor + Trendy_Terminal.Input.Num_Cursor_Positions (ASU.To_String (E.Spacers (Next)));

            if Next <= Positive (E.Words.Length) then
                Current_Cursor := Current_Cursor + Trendy_Terminal.Input.Num_Cursor_Positions (ASU.To_String (E.Words (Next)));
            end if;
            exit when Current_Cursor >= Cursor_Position;
            Next := Next + 1;
        end loop;
        -- SP.Terminal.Put_Line ("Cursor stopped at: " & Natural'Image (Current_Cursor)
        --     & " on word: " & Positive'Image (Next));
        return Next;
    end Get_Cursor_Word;

    function Complete_Input (L : Trendy_Terminal.Input.Line_Input; History_Index : Integer)
        return Trendy_Terminal.Input.Line_Input
    is
        E           : SP.Strings.Exploded_Line := SP.Strings.Make (Trendy_Terminal.Input.Current (L));
        Cursor_Word : constant Positive := Get_Cursor_Word (E, Trendy_Terminal.Input.Get_Cursor_Index (L));
        Result      : Trendy_Terminal.Input.Line_Input := L;
        Completion  : ASU.Unbounded_String;
        Suffix      : ASU.Unbounded_String;

        use all type ASU.Unbounded_String;
        use SP.Strings.String_Vectors;
    begin
        pragma Unreferenced (History_Index);

        -- Find the position of the cursor within line.
        if Cursor_Word = 1 then
            if SP.Commands.Is_Like_Command (ASU.To_String (E.Words(1))) then
                Completion := SP.Commands.Target_Command (E.Words(1));
                Suffix := Trailing_End (E.Words (1), Completion);
                E.Words (1) := E.Words (1) & Suffix;
                Trendy_Terminal.Input.Set (Result, ASU.To_String (SP.Strings.Zip (E.Spacers, E.Words)),
                    Trendy_Terminal.Input.Get_Cursor_Index (L) + Trendy_Terminal.Input.Num_Cursor_Positions (ASU.To_String (Suffix)));
                return Result;
            end if;
        end if;

        return L;
    end Complete_Input;

    function Read_Command return SP.Strings.String_Vectors.Vector is
    begin
        declare
            Input : constant ASU.Unbounded_String := ASU.To_Unbounded_String(
                Trendy_Terminal.IO.Get_Line(
                    Format_Fn => Format_Input'Access,
                    Completion_Fn => Complete_Input'Access
                )
            );
            Exploded : constant SP.Strings.Exploded_Line := SP.Strings.Make (ASU.To_String (Input));
            Result : SP.Strings.String_Vectors.Vector;
        begin
            New_Line;

            for Word of Exploded.Words loop
                if SP.Strings.Is_Quoted (ASU.To_String (Word)) then
                    Result.Append (ASU.Unbounded_Slice (Word, 2, ASU.Length (Word) - 1));
                else
                    Result.Append (Word);
                end if;
            end loop;

            return Result;
        end;
    end Read_Command;

    -- The interactive loop through which the user starts a search context and then interatively refines it by
    -- pushing and popping operations.
    procedure Main is
        Command_Line : SP.Strings.String_Vectors.Vector;
        Srch         : SP.Searches.Search;
        Configs      : constant SP.Strings.String_Vectors.Vector := SP.Config.Config_Locations;
    begin
        if not Trendy_Terminal.Platform.Init then
            return;
        end if;
        Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Echo, False);
        Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Line_Input, False);
        Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Escape_Sequences, True);

        Put_Line ("septum v" & SP.Version);
        New_Line;

        for Config of Configs loop
            if not SP.Commands.Run_Commands_From_File (Srch, ASU.To_String(Config)) then
                Put_Line ("Failing running commands from: " & ASU.To_String(Config));
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
