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

package SP.Strings is
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
        Post => Common_Prefix_Length'Result < Natural'Max (ASU.Length (A), ASU.Length (B));

    -- Quoted strings must start and end with either a single or a double quote.
    function Is_Quoted (S : String) return Boolean;

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
    function Get_Word (E : Exploded_Line; Index : Positive) return String is (ASU.To_String (E.Words (Index)));
    function Num_Words (E : Exploded_Line) return Natural is (Natural (E.Words.Length));

end SP.Strings;
