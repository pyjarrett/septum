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

package SP.File_System is
    use SP.Strings;

    function Is_File (Target : String) return Boolean;
    function Is_Dir (Target : String) return Boolean;

    function Is_Current_Or_Parent_Directory (Dir_Entry : Ada.Directories.Directory_Entry_Type) return Boolean;

    type Dir_Contents is record
        Files   : String_Vectors.Vector;
        Subdirs : String_Vectors.Vector;
    end record;

    function Contents (Dir_Name : String) return Dir_Contents;
    -- The immediate contents of the given directory.

end SP.File_System;
