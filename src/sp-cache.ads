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

with SP.Strings;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

package SP.Cache is
    -- Provides a means to load entire directory structures into memory and then use it as needed. This is intended for
    -- text files only, in particular, to speed text searches of large read-only code bases.
    --
    -- While `Async_File_Cache` provides parallel loading, access to the cache itself is protected.

    use Ada.Strings.Unbounded;
    use SP.Strings;

    package File_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Ada.Strings.Unbounded.Unbounded_String, Element_Type => String_Vectors.Vector,
         "<"      => Ada.Strings.Unbounded."<", "=" => String_Vectors."=");

    protected type Async_File_Cache is
        -- The available in-memory contents of files loaded from files.
        --
        -- TODO: Add monitoring of files for changes.

        procedure Clear;

        procedure Cache_File (File_Name : in Unbounded_String; Lines : in String_Vectors.Vector);

        function Num_Files return Natural;

        function Num_Lines return Natural;

        function Lines (File_Name : in Unbounded_String) return String_Vectors.Vector;

        function Files return String_Vectors.Vector;

        function File_Line (File_Name : in Unbounded_String; Line : in Positive) return Unbounded_String;

    private

        Top_Level_Directories : SP.Strings.String_Sets.Set;
        -- A list of all top level directories which need to be searched.

        Contents : File_Maps.Map;
    end Async_File_Cache;

    procedure Add_Directory_Recursively (A : in out Async_File_Cache; Dir : String);
    -- Adds a directory and all of its recursive subdirectories into the file cache.

end SP.Cache;
