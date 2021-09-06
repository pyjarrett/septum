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
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with Ada.Characters.Latin_1;

with GNAT.OS_Lib;

package body SP.Strings is
    function Zip (Left : SP.Strings.String_Vectors.Vector; Right : SP.Strings.String_Vectors.Vector)
        return Ada.Strings.Unbounded.Unbounded_String
    is
        use Ada.Strings.Unbounded;
        use SP.Strings.String_Vectors;
        L : Natural := 1;
        R : Natural := 1;
    begin
        return Result : Ada.Strings.Unbounded.Unbounded_String do
            while L <= Natural (Length (Left)) or else R <= Natural (Length (Right)) loop
                if L <= Natural (Length (Left)) then
                    Append (Result, Left (L));
                    L := L + 1;
                end if;

                if R <= Natural (Length (Right)) then
                    Append (Result, Right (R));
                    R := R + 1;
                end if;
            end loop;
        end return;
    end Zip;

    -- TODO: This will eventually need to be rewritten to account for multi-byte
    -- sequences in UTF-8.  Incurring technical debt here on purpose to try to get
    -- the command line formatter stood up more quickly.
    function Make (S : String) return Exploded_Line is
        -- Use half-open ranges here.  The next slice is going to be
        -- [First, After_Last).  This allows "empty" ranges when Fire = After_Last.
        After_Last : Natural := 1;
        Result     : Exploded_Line;

        use Ada.Strings.Unbounded;
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
                                Append (Word, Next_Char);
                            when Ada.Characters.Latin_1.Quotation =>
                                if not Quoted then
                                    Quoted := True;
                                    Quote_Char := Ada.Characters.Latin_1.Quotation;
                                elsif Quote_Char = Ada.Characters.Latin_1.Quotation then
                                    Quoted := False;
                                end if;
                                Append (Word, Next_Char);
                            when Ada.Characters.Latin_1.Apostrophe =>
                                if not Quoted then
                                    Quoted := True;
                                    Quote_Char := Ada.Characters.Latin_1.Apostrophe;
                                elsif Quote_Char = Ada.Characters.Latin_1.Apostrophe then
                                    Quoted := False;
                                end if;
                                Append (Word, Next_Char);
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

    function Read_Lines (File_Name : String; Result : out String_Vectors.Vector) return Boolean is
        --  Reads all the lines from a file.
        File : Ada.Text_IO.File_Type;
        Line : Ada.Strings.Unbounded.Unbounded_String;
    begin
        String_Vectors.Clear (Result);
        Ada.Text_IO.Open (File => File, Mode => Ada.Text_IO.In_File, Name => File_Name);
        while not Ada.Text_IO.End_Of_File (File) loop
            Line := Ada.Strings.Unbounded.Text_IO.Get_Line (File);
            Result.Append (Line);
        end loop;

        Ada.Text_IO.Close (File);
        return True;
    exception
        when Ada.Text_IO.End_Error =>
            if Ada.Text_IO.Is_Open (File) then
                Ada.Text_IO.Close (File);
            end if;
            return True;
        when others =>
            Ada.Text_IO.Put_Line ("Unable to read contents of: " & File_Name);
            return False;
    end Read_Lines;

    function Common_Prefix_Length
        (A : Ada.Strings.Unbounded.Unbounded_String; B : Ada.Strings.Unbounded.Unbounded_String) return Natural is
        use Ada.Strings.Unbounded;
        -- Finds the number of common starting characters between two strings.
    begin
        return Count : Natural := 0 do
            while Count < Length (A) and then Count < Length (B)
                and then Element (A, Count + 1) = Element (B, Count + 1) loop
                Count := Count + 1;
            end loop;
        end return;
    end Common_Prefix_Length;

    function Is_Quoted (S : String) return Boolean is
        use Ada.Characters.Latin_1;
        Quote_Types : constant array (Positive range <>) of Character := (Quotation, Apostrophe);
    begin
        return S'Length > 0 and then S (S'First) = S (S'Last) and then (for some X of Quote_Types => X = S (S'First));
    end Is_Quoted;

    function Next_Word_Start (S : String; Start : Positive) return Natural is
    begin
        return Ada.Strings.Fixed.Index_Non_Blank (S, Start);
    end Next_Word_Start;

    function Next_Word_End (S : String; Start : Positive) return Natural is
    begin
        return 0;
    end Next_Word_End;

end SP.Strings;
