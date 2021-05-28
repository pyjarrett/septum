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
with Ada.Strings.Unbounded;
with GNATCOLL.VFS; use GNATCOLL.VFS;

package body SP.Config is
    -- Septum data is stored locally in the Next_Dir working directory on load or in the home directory of the user
    -- running the command.  This allows users to maintain general configuration in their home directory based
    -- on the settings they want to work with, and then have per-project settings that they can use.
    --
    -- Septum configuration setup.
    -- Containing_Directory/
    --     .septum/                 Directory to contain all Septum related data.
    --         .config              Commands to run on startup.

    Config_Dir       : constant Filesystem_String := ".septum";
    Config_File_Name : constant Filesystem_String := ".config";

    function Closest_Config (Dir_Name : String) return Virtual_File with
        Pre => Ada.Directories.Exists (Dir_Name),
        Post => Is_Regular_File (Closest_Config'Result) or else Closest_Config'Result = No_File
    is
        -- Looks for a config in the tree starting at Dir_Name, which might be somewhere in the project tree.
        Next_Dir : Virtual_File := Create (+Dir_Name);
    begin
        return Result : Virtual_File do
            loop
                Result := Next_Dir / Config_Dir / Config_File_Name;
                if Result.Is_Regular_File then
                    return;
                end if;

                -- Move up the tree.
                Next_Dir := Next_Dir.Get_Parent;
                if Next_Dir = No_File then
                    -- Reached the top, so no parent exists.
                    Result := No_File;
                    return;
                end if;
            end loop;
        end return;
    end Closest_Config;

    function Config_Locations return String_Vectors.Vector is
        use Ada.Strings.Unbounded;
        Home_Dir_Config    : constant Virtual_File := Get_Home_Directory / Config_Dir / Config_File_Name;
        Current_Dir_Config : constant Virtual_File := Closest_Config (+Get_Current_Dir.Full_Name);
    begin
        return V : String_Vectors.Vector do
            -- Look for the global user config.
            if Is_Regular_File (Home_Dir_Config) then
                V.Append (To_Unbounded_String (+Home_Dir_Config.Full_Name));
            end if;

            if Is_Regular_File (Current_Dir_Config) then
                V.Append (To_Unbounded_String (+Current_Dir_Config.Full_Name));
            end if;
        end return;
    end Config_Locations;

end SP.Config;
