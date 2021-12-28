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
with Ada.Characters.Latin_1;

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

    function Format_Array (S : SP.Strings.String_Vectors.Vector) return Ada.Strings.Unbounded.Unbounded_String is
        use Ada.Strings.Unbounded;
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
                if SP.Strings.Is_Quoted (ASU.To_String (Word)) and then Length (Word) > 1 then
                    Result.Words.Append (Unbounded_Slice (Word, 2, Length (Word) - 1));
                else
                    Result.Words.Append (Word);
                end if;
            end;
        end loop;

        return Result;
    end Make;

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

    function Matching_Suffix (Current, Desired : ASU.Unbounded_String) return ASU.Unbounded_String is
        Prefix_Length : constant Natural := SP.Strings.Common_Prefix_Length (Current, Desired);
        Suffix        : constant ASU.Unbounded_String := ASU.Unbounded_Slice (Desired, Prefix_Length + 1, ASU.Length (Desired));
    begin
        return Suffix;
    end Matching_Suffix;

    function Is_Quoted (S : String) return Boolean is
        use Ada.Characters.Latin_1;
        Quote_Types : constant array (Positive range <>) of Character := (Quotation, Apostrophe);
    begin
        return S'Length > 0 and then S (S'First) = S (S'Last) and then (for some X of Quote_Types => X = S (S'First));
    end Is_Quoted;

    function Split_Command (Input : ASU.Unbounded_String) return SP.Strings.String_Vectors.Vector is
        Exploded : constant SP.Strings.Exploded_Line := SP.Strings.Make (ASU.To_String (Input));

    begin
        return Result : SP.Strings.String_Vectors.Vector do
            for Word of Exploded.Words loop
                if SP.Strings.Is_Quoted (ASU.To_String (Word)) then
                    Result.Append (ASU.Unbounded_Slice (Word, 2, ASU.Length (Word) - 1));
                else
                    Result.Append (Word);
                end if;
            end loop;
        end return;
    end Split_Command;

    function Get_Cursor_Word (E : SP.Strings.Exploded_Line; Cursor_Position : Positive) return Natural
    is
        Next           : Natural := 1;
        Current_Cursor : Natural := 1;
    begin
        while Next <= Natural (E.Spacers.Length) loop
            Current_Cursor := Current_Cursor + ASU.To_String (E.Spacers (Next))'Length;

            if Next <= Positive (E.Words.Length) then
                Current_Cursor := Current_Cursor + ASU.To_String (E.Words (Next))'Length;
            end if;
            exit when Current_Cursor >= Cursor_Position;
            Next := Next + 1;
        end loop;
        return Next;
    end Get_Cursor_Word;

    function Cursor_Position_At_End_Of_Word (E : SP.Strings.Exploded_Line; Word : Positive) return Positive is
    begin
        return Cursor_Position : Positive := 1 do
            for I in 1 .. Word loop
                Cursor_Position := Cursor_Position + ASU.Length (E.Spacers (I));
                Cursor_Position := Cursor_Position + ASU.Length (E.Words (I));
            end loop;
        end return;
    end Cursor_Position_At_End_Of_Word;

end SP.Strings;
