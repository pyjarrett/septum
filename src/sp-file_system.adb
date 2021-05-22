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

with Ada.Directories.Hierarchical_File_Names;
with Ada.Strings.Unbounded;

package body SP.File_System is

    function Is_Current_Or_Parent_Directory (Dir_Entry : Ada.Directories.Directory_Entry_Type) return Boolean is
        --  Return true if the entry is "." or "..".
        Name : constant String := Ada.Directories.Simple_Name (Dir_Entry);
    begin
        return
            Ada.Directories.Hierarchical_File_Names.Is_Parent_Directory_Name (Name)
            or else Ada.Directories.Hierarchical_File_Names.Is_Current_Directory_Name (Name);
    end Is_Current_Or_Parent_Directory;

   function Contents (Dir_Name : String) return Dir_Contents is
        use Ada.Directories;
        Dir_Search : Search_Type;
        Next_Entry : Directory_Entry_Type;
        Filter     : constant Filter_Type := (Ordinary_File | Directory => True, others => False);
    begin
        return Result : Dir_Contents do
            Ada.Directories.Start_Search
                (Search => Dir_Search, Directory => Dir_Name, Pattern => "*", Filter => Filter);
            while More_Entries (Dir_Search) loop
                Get_Next_Entry (Dir_Search, Next_Entry);
                if not Is_Current_Or_Parent_Directory (Next_Entry) then
                    case Kind (Next_Entry) is
                        when Directory => Result.Subdirs.Append (Ada.Strings.Unbounded.To_Unbounded_String(Full_Name (Next_Entry)));
                        when Ordinary_File => Result.Files.Append (Ada.Strings.Unbounded.To_Unbounded_String(Full_Name (Next_Entry)));
                            when others => null;
                    end case;
                end if;
            end loop;
            End_Search (Dir_Search);
        end return;
    end Contents;


end SP.File_System;
