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

with Ada.Directories;
with SP.Strings;

package SP.File_System is
    use SP.Strings;

    function Is_Current_Or_Parent_Directory (Dir_Entry : Ada.Directories.Directory_Entry_Type) return Boolean;

    type Dir_Contents is record
        Files   : String_Vectors.Vector;
        Subdirs : String_Vectors.Vector;
    end record;

    function Contents (Dir_Name : String) return Dir_Contents;
    -- The immediate contents of the given directory.
end SP.File_System;
