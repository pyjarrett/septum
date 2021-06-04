-------------------------------------------------------------------------------
-- Septum, a tool for interactive file search and analysis.
--
-- Copyright (C) 2021, The Septum developers (see AUTHORS file)
--
-- Septum is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Septum is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with SP.Strings;

package SP.Contexts is

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

    function "="(A, B : Context_Match) return Boolean with
        Pre => Is_Valid (A) and then Is_Valid (B);

    package Context_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Context_Match);

    function Files_In (V : Context_Vectors.Vector) return SP.Strings.String_Sets.Set;

end SP.Contexts;
