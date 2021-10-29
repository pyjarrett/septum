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

with SP.Strings;

package SP.Contexts
    with Preelaborate
is

    package Line_Matches is new Ada.Containers.Ordered_Sets (Element_Type => Positive);

    type Context_Match is record
        File_Name        : Ada.Strings.Unbounded.Unbounded_String;
        Internal_Matches : Line_Matches.Set;
        Minimum          : Positive;
        Maximum          : Positive;
    end record;

    function From
        (File_Name : String; Line : Natural; Num_Lines : Natural; Context_Width : Natural) return Context_Match with
        Pre  => Line <= Num_Lines,
        Post => Is_Valid (From'Result);

    function Real_Min (C : Context_Match) return Positive with
        Pre  => Is_Valid (C),
        Post => C.Minimum <= Real_Min'Result and then Real_Min'Result <= C.Maximum;

    function Real_Max (C : Context_Match) return Positive with
        Pre  => Is_Valid (C),
        Post => C.Minimum <= Real_Max'Result and then Real_Max'Result <= C.Maximum;

    function Is_Valid (C : Context_Match) return Boolean;

    function Overlap (A, B : Context_Match) return Boolean with
        Pre => Is_Valid (A) and then Is_Valid (B);

    function Contains (A : Context_Match; Line_Num : Positive) return Boolean with
        Pre => Is_Valid (A);

    function Contains (A, B : Context_Match) return Boolean with
        Pre => Is_Valid (A) and then Is_Valid (B);

    function Merge (A, B : Context_Match) return Context_Match with
        Pre  => Is_Valid (A) and then Is_Valid (B),
        Post => Is_Valid (Merge'Result);

    function Image (A : Context_Match) return String with
        Pre => Is_Valid (A);

    overriding
    function "="(A, B : Context_Match) return Boolean with
        Pre => Is_Valid (A) and then Is_Valid (B);

    package Context_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Context_Match);

    function Files_In (V : Context_Vectors.Vector) return SP.Strings.String_Sets.Set;

end SP.Contexts;
