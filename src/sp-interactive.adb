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
-- with ANSI;
with SP.Commands;
with SP.Config;
-- with SP.File_System;
with SP.Searches; use SP.Searches;
with SP.Strings;  use SP.Strings;
with SP.Terminal;

with Trendy_Terminal;

package body SP.Interactive is
    use Ada.Strings.Unbounded;
    use SP.Terminal;

    type Exploded_Line is record
        -- The first space is "Leading spacing"
        -- Spaces(i) is what preceeds Words(i)
        Spacers : SP.Strings.String_Vectors.Vector;
        Words   : SP.Strings.String_Vectors.Vector;
    end record;

    function Get_Word (E : Exploded_Line; Index : Positive) return String is (To_String (E.Words (Index)));
    function Num_Words (E : Exploded_Line) return Natural is (Natural (E.Words.Length));

    -- TODO: This will eventually need to be rewritten to account for multi-byte
    -- sequences in UTF-8.  Incurring technical debt here on purpose to try to get
    -- the command line formatter stood up more quickly.
    function Make (S : String) return Exploded_Line is
        -- Use half-open ranges here.  The next slice is going to be
        -- [First, After_Last).  This allows "empty" ranges when Fire = After_Last.
        After_Last : Natural := 1;
        Result     : Exploded_Line;
    begin
        if S'Length = 0 then
            return E : Exploded_Line do null; end return;
        end if;

        while After_Last <= S'Length loop
            -- This section is a spacer since, either a new line is being split
            -- or this is starting a whitespace section after consuming some text.
            --
            -- To reduce special casing, empty leading space is added to the
            -- exploded line, this maintains the property that Spacers(i) is what
            -- preceeds Words(i).
            declare
                First : constant Natural := After_Last;
            begin
                After_Last := Ada.Strings.Fixed.Index_Non_Blank (S, After_Last);

                -- No more text follows the whitespace.
                exit when After_Last = 0;

                Result.Spacers.Append (To_Unbounded_String (S (First .. After_Last - 1)));
                exit when After_Last > S'Length;
            end;

            -- A non-space section, as designated as starting with a non-blank character.
            -- This section is trickier as multiple cases have to be resolved.
            -- 1. Dealing with quoted sections.  Once a quoted section has started,
            --    it can only be undone by an unescaped quoted character.
            -- 2. Escaped characters.  Escaped spaces might appear which hamper the
            --    ability to delineate words by spaces alone.
            -- 3. Don't run off the end of the string.
            --
            -- N.B the usage of / on Windows is commonplace, so requiring uses
            --     to use / or \\ for a "\" seems reasonable.
            --
            -- In practice, spaces appear quite often in queries, especially when looking
            -- for error messages and some Window directories.
            declare
                Escaped    : Boolean := False;
                Quoted     : Boolean := False;
                Quote_Char : Character := ' ';
                Next_Char  : Character;
                Word       : Unbounded_String;
            begin
                while After_Last <= S'Length loop
                    pragma Assert (After_Last <= S'Length);
                    Next_Char := S (After_Last);

                    -- The previous character was escaped, so treat the next
                    -- character as a literal.
                    --
                    -- This appears before quote checks to prevent escaped
                    -- quotes from changing the quote state.
                    if Escaped then
                        Append (Word, Next_Char);
                        Escaped := False;
                    else
                        case Next_Char is
                            when '\' =>
                                Escaped := True;
                            when Ada.Characters.Latin_1.Quotation =>
                                if not Quoted then
                                    Quoted := True;
                                    Quote_Char := Ada.Characters.Latin_1.Quotation;
                                elsif Quote_Char = Ada.Characters.Latin_1.Quotation then
                                    Quoted := False;
                                else
                                    Append (Word, Next_Char);
                                end if;
                            when Ada.Characters.Latin_1.Apostrophe =>
                                if not Quoted then
                                    Quoted := True;
                                    Quote_Char := Ada.Characters.Latin_1.Apostrophe;
                                elsif Quote_Char = Ada.Characters.Latin_1.Apostrophe then
                                    Quoted := False;
                                else
                                    Append (Word, Next_Char);
                                end if;
                            -- Whitespace is only the end of the word if it's not
                            -- escaped or in a quoted section.
                            when Ada.Characters.Latin_1.Space | Ada.Characters.Latin_1.CR | Ada.Characters.Latin_1.HT | Ada.Characters.Latin_1.FF =>
                                -- Exit the loop here to keep Current pointing
                                -- to the start of the whitespace.
                                if Quoted then
                                    Append (Word, Next_Char);
                                else
                                    exit;
                                end if;
                            when others =>
                                Append (Word, Next_Char);
                        end case;
                    end if;

                    After_Last := After_Last + 1;
                end loop;

                pragma Assert (Length (Word) > 0);
                Result.Words.Append (Word);
            end;
        end loop;

        return Result;
    end Make;


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
        return S;
    end Format_Input;

    function Format_Array (S : SP.Strings.String_Vectors.Vector) return Unbounded_String is
        Result : Unbounded_String;
    begin
        Append (Result, To_Unbounded_String ("["));
        for Elem of S loop
            Append (Result, Ada.Characters.Latin_1.Quotation);
            Append (Result, Elem);
            Append (Result, Ada.Characters.Latin_1.Quotation);
            Append (Result, Ada.Characters.Latin_1.Comma);
            Append (Result, To_Unbounded_String (" "));
        end loop;
        Append (Result, To_Unbounded_String ("]"));
        return Result;
    end Format_Array;

    function Debug_Input (S : String) return String is
        Exploded : constant Exploded_Line := Make (S);
        Output   : Unbounded_String;
    begin
        Append (Output, Format_Array (Exploded.Words));
        Append (Output, Format_Array (Exploded.Spacers));
        return To_String (Output);
    end Debug_Input;

    function Read_Command return String_Vectors.Vector is
    begin
        declare
            Input : constant Unbounded_String := To_Unbounded_String(
                Trendy_Terminal.Debug_Get_Line(
                    Format_Fn => Format_Input'Access,
                    Debug_Fn => Debug_Input'Access
                )
            );
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
