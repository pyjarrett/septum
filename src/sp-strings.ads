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

package SP.Strings is
    package String_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => Ada.Strings.Unbounded.Unbounded_String, "<" => Ada.Strings.Unbounded."<",
         "="          => Ada.Strings.Unbounded."=");
    package String_Vectors is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => Ada.Strings.Unbounded.Unbounded_String,
         "="        => Ada.Strings.Unbounded."=");

    function Shell_Split (S : Ada.Strings.Unbounded.Unbounded_String) return String_Vectors.Vector;
    -- Performs a shell-like split, grouping elements surrounded by single or double quotes. This is based on the
    -- behavior of GNAT.OS_Lib.Argument_String_To_List, so behavior of \ changes based on if running under Windows
    -- or not.

    function Read_Lines (File_Name : in String; Result : out String_Vectors.Vector) return Boolean;

    function Common_Prefix_Length
        (A : Ada.Strings.Unbounded.Unbounded_String; B : Ada.Strings.Unbounded.Unbounded_String) return Natural with
        Post => Common_Prefix_Length'Result <
        Natural'Max (Ada.Strings.Unbounded.Length (A), Ada.Strings.Unbounded.Length (B));

    function Is_Quoted (S : String) return Boolean;
    -- Quoted strings must start and end with either a single or a double quote.

end SP.Strings;
