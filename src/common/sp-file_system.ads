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

with Ada.Directories;
with SP.Strings;

-- Wraps file system operations to make them simpler, and handle cases without
-- using exceptions.
package SP.File_System is
    use SP.Strings;

    -- Checks that a file at the given path exists.
    function Is_File (Target : String) return Boolean;

    -- Checks that a dir at the given path exists.
    function Is_Dir (Target : String) return Boolean;

    -- Ada.Directories.Hierarchical_File_Names is optional, and doesn't exist
    -- on some of the Linux platforms tested for Alire crates.
    function Is_Current_Or_Parent_Directory (Dir_Entry : Ada.Directories.Directory_Entry_Type) return Boolean;

    type Dir_Contents is record
        Files   : String_Vectors.Vector;
        Subdirs : String_Vectors.Vector;
    end record;

    -- The immediate, non-recursive, contents of the given directory.
    function Contents (Dir_Name : String) return Dir_Contents;

    -- Pulls the contents of a textual file, which might possibly fail due to
    -- the file not existing or being a directory instead of a file.
    function Read_Lines (File_Name : in String; Result : out String_Vectors.Vector) return Boolean;

    -- Finds a path similar to the given one with the same basic stem.
    function Similar_Path (Path : String) return String;

    -- Rewrite a path with all forward slashes for simplicity.
    function Rewrite_Path (Path : String) return String;

    -- Produces all of the possible options for a path.
    function File_Completions (Path : String) return String_Vectors.Vector;

end SP.File_System;
