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

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

-- A lot of what happens in Septum is related to strings.  It reads them from
-- file, uses them as input for commands, looks for them with filters, attempts
-- to match them with regular expressions and prints them to users.
--
-- ## UTF-8 Compatibility
--
-- Any solution to handling all of these in a UTF-8 compatible manner, must
-- then deal appropriately with all of the interfaces with which these things
-- touch.  Due to the tight binding within Septum of all of these behaviors,
-- it may not be possible to extricate enough of string handling for a drop in
-- replacement, and a complete refactoring to properly handle UTF-8 in all
-- situations may be impossible.
--
-- The binding of strings to fixed sizes also remains painful, as it requires
-- the use of an additional unbounded type, and often semantically meaningless
-- and inefficient conversions between the two types.  In many situations,
-- fixed strings aren't possible, such as being stored in vectors, sets or used
-- as keys in maps.  This often results in doubling the size of the interface,
-- or clumsily converting as needed.
--
-- My approach thus far has been to write interfaces using `String` as much as
-- possible, falling back to unbounded strings only when absolutely necessary.
-- There is likely a considerable amount of time needed to convert due to this
-- approach.
--
-- What I probably should have done initially was to define a private string
-- type to use everywhere with easy conversions and use either string interning
-- or underlying unbounded strings.  The current form of `Unbounded_String` is
-- also somewhat unwieldly.
--
-- The [VSS](https://github.com/AdaCore/VSS) library looks like a viable
-- alternative to `Unbounded_String`, though it is marked with "Warning: This is
-- experimental work in progress, everything is subject to change. It may be or
-- may be not part of GNATCOLL or standard Ada library in the future."
--
-- Known systems which use strings:
-- - Terminal I/O (Trendy Terminal)
--   - Formatting
--   - Hinting system
--   - Autocomplete
-- - Search
--   - Regular expressions
-- - File I/O
-- - Command interpretation
--
package SP.Strings
    with Preelaborate
is
    package ASU renames Ada.Strings.Unbounded;

    package String_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => ASU.Unbounded_String, "<" => ASU."<",
         "="          => ASU."=");
    package String_Vectors is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => ASU.Unbounded_String,
         "="        => ASU."=");

    function Zip (Left, Right : String_Vectors.Vector) return ASU.Unbounded_String;
    function Format_Array (S : String_Vectors.Vector) return ASU.Unbounded_String;

    function Common_Prefix_Length (A, B : ASU.Unbounded_String) return Natural
    with
        Post => Common_Prefix_Length'Result <= Natural'Max (ASU.Length (A), ASU.Length (B));

    function Matching_Suffix (Current, Desired : ASU.Unbounded_String) return ASU.Unbounded_String;

    -- Quoted strings must start and end with either a single or a double quote.
    function Is_Quoted (S : String) return Boolean;

    function Split_Command (Input : ASU.Unbounded_String) return SP.Strings.String_Vectors.Vector;

    -- An exploded form of a line which allows the line to be recombined
    -- transparently to a user, by reapplying the appropriate amounts and types
    -- of spacing between words.
    --
    -- This looks like:
    -- [_space_]*[WORD][_space_][WORD][_space_][WORD][_space_]
    --
    -- To prevent complications regarding whether a word or space is first, and
    -- simplify iteration over words, the leading space is always stored, and
    -- may be empty.
    type Exploded_Line is record
        -- The first space is "Leading spacing"
        -- Spaces(i) is what preceeds Words(i)
        Spacers : String_Vectors.Vector;
        Words   : String_Vectors.Vector;
    end record;

    -- TODO: This will eventually need to be rewritten to account for multi-byte
    -- sequences in UTF-8.  Incurring technical debt here on purpose to try to get
    -- the command line formatter stood up more quickly.
    function Make (S : String) return Exploded_Line;
    function Get_Word (E : Exploded_Line; Index : Positive) return String is (ASU.To_String (E.Words.Element (Index)));
    function Num_Words (E : Exploded_Line) return Natural is (Natural (E.Words.Length));

    function Get_Cursor_Word (E : SP.Strings.Exploded_Line; Cursor_Position : Positive) return Natural;
    function Cursor_Position_At_End_Of_Word (E : SP.Strings.Exploded_Line; Word : Positive) return Positive;

end SP.Strings;
