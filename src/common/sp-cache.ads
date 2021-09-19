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
