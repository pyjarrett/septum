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
with Ada.Strings.Unbounded;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Dir_Iterators.Ancestor;
with SP.File_System;

package body SP.Config is
    package AD renames Ada.Directories;
    package ASU renames Ada.Strings.Unbounded;
    package FS renames SP.File_System;

    use type Ada.Directories.File_Kind;
    use type ASU.Unbounded_String;

    Config_Dir_Name  : constant String := ".septum";
    Config_File_Name : constant String := ".config";

    -- Septum data is stored locally in the Next_Dir working directory on load or in the home directory of the user
    -- running the command.  This allows users to maintain general configuration in their home directory based
    -- on the settings they want to work with, and then have per-project settings that they can use.
    --
    -- Septum configuration setup.
    -- Containing_Directory/
    --     .septum/                 Directory to contain all Septum related data.
    --         .config              Commands to run on startup.
    function Closest_Config (Dir_Name : String) return ASU.Unbounded_String with
        Pre  => AD.Exists (Dir_Name),
        Post => (Closest_Config'Result = ASU.Null_Unbounded_String)
        or else FS.Is_File (ASU.To_String (Closest_Config'Result))
    is
        Ancestors  : constant Dir_Iterators.Ancestor.Ancestor_Dir_Walk := Dir_Iterators.Ancestor.Walk (Dir_Name);
        Next_Trial : ASU.Unbounded_String;
    begin
        for Ancestor of Ancestors loop
            Next_Trial := ASU.To_Unbounded_String (Ancestor & "/" & Config_Dir_Name & "/" & Config_File_Name);
            if FS.Is_File (ASU.To_String (Next_Trial)) then
                return Next_Trial;
            end if;
        end loop;
        return ASU.Null_Unbounded_String;
    end Closest_Config;

    function Config_Locations return String_Vectors.Vector is
        Home_Dir_Config : constant ASU.Unbounded_String :=
            ASU.To_Unbounded_String
                (+Get_Home_Directory.Full_Name.all & "/" & Config_Dir_Name & "/" & Config_File_Name);
        Current_Dir_Config : constant ASU.Unbounded_String := Closest_Config (Ada.Directories.Current_Directory);
    begin
        return V : String_Vectors.Vector do
            -- Look for the global user config.
            if FS.Is_File (ASU.To_String (Home_Dir_Config)) then
                V.Append (Home_Dir_Config);
            end if;

            if Current_Dir_Config /= ASU.Null_Unbounded_String
                and then FS.Is_File (ASU.To_String (Current_Dir_Config))
            then
                V.Append (Current_Dir_Config);
            end if;
        end return;
    end Config_Locations;
end SP.Config;
