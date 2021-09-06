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
    package String_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => Ada.Strings.Unbounded.Unbounded_String, "<" => Ada.Strings.Unbounded."<",
         "="          => Ada.Strings.Unbounded."=");
    package String_Vectors is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => Ada.Strings.Unbounded.Unbounded_String,
         "="        => Ada.Strings.Unbounded."=");

    type Exploded_Line is record
        -- The first space is "Leading spacing"
        -- Spaces(i) is what preceeds Words(i)
        Spacers : SP.Strings.String_Vectors.Vector;
        Words   : SP.Strings.String_Vectors.Vector;
    end record;

    function Get_Word (E : Exploded_Line; Index : Positive) return String is (Ada.Strings.Unbounded.To_String (E.Words (Index)));
    function Num_Words (E : Exploded_Line) return Natural is (Natural (E.Words.Length));

    -- TODO: This will eventually need to be rewritten to account for multi-byte
    -- sequences in UTF-8.  Incurring technical debt here on purpose to try to get
    -- the command line formatter stood up more quickly.
    function Make (S : String) return Exploded_Line;

    function Read_Lines (File_Name : in String; Result : out String_Vectors.Vector) return Boolean;

    function Common_Prefix_Length
        (A : Ada.Strings.Unbounded.Unbounded_String; B : Ada.Strings.Unbounded.Unbounded_String) return Natural with
        Post => Common_Prefix_Length'Result <
        Natural'Max (Ada.Strings.Unbounded.Length (A), Ada.Strings.Unbounded.Length (B));

    function Is_Quoted (S : String) return Boolean;
    -- Quoted strings must start and end with either a single or a double quote.

end SP.Strings;
