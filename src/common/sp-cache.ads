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

-- Super simple text file caching.
--
-- While `Async_File_Cache` provides parallel loading, access to the cache
-- itself is protected.
--
-- Provides a means to load entire directory structures into memory and then
-- use it as needed. This is intended for text files only, in particular, to
-- speed text searches of large read-only code bases.
--
-- This is super simple and straightforward, but works well enough.
-- It probably better done with mmap to load files directly to memory.  It
-- eliminates line-splitting when printing output.  If this were in C++,
-- it would be possible to do something like store the file as a huge byte
-- block with mmap and then replace newlines with '\0' and store byte counts
-- to the initial part of every string.
--
package SP.Cache is

    use Ada.Strings.Unbounded;
    use SP.Strings;

    package File_Maps is new Ada.Containers.Ordered_Maps (
        Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
        Element_Type => String_Vectors.Vector,
         "<"         => Ada.Strings.Unbounded."<", "=" => String_Vectors."=");

    -- The available in-memory contents of files loaded from files.
    --
    -- Files are stored by full path name, with the OS's preference for path
    -- separators.
    --
    -- TODO: Add monitoring of files for changes.
    protected type Async_File_Cache is

        procedure Clear;

        -- Cache the contents of a file, replacing any existing contents.
        procedure Cache_File (File_Name : Unbounded_String; Lines : String_Vectors.Vector);

        -- The total number of loaded files in the file cache.
        function Num_Files return Natural;

        -- The total number of loaded lines in the file cache.
        function Num_Lines return Natural;

        function Lines (File_Name : Unbounded_String) return String_Vectors.Vector;

        function Files return String_Vectors.Vector;

        function File_Line (File_Name : Unbounded_String; Line : Positive) return Unbounded_String;

    private

        -- A list of all top level directories which need to be searched.
        Top_Level_Directories : SP.Strings.String_Sets.Set;

        Contents : File_Maps.Map;
    end Async_File_Cache;

    -- Adds a directory and all of its recursive subdirectories into the file cache.
    function Add_Directory_Recursively (A : in out Async_File_Cache; Dir : String) return Boolean;

end SP.Cache;
