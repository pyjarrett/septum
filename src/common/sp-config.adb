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
with Ada.Text_IO;
with Dir_Iterators.Ancestor;
with SP.File_System;
with SP.Platform;

package body SP.Config is
    package AD renames Ada.Directories;
    package ASU renames Ada.Strings.Unbounded;
    package FS renames SP.File_System;

    use type ASU.Unbounded_String;

    procedure Create_Local_Config is
        Current_Dir : constant String := AD.Current_Directory;
        Config_Dir  : constant String := Current_Dir & "/" & Config_Dir_Name;
        Config_File : constant String := Config_Dir & "/" & Config_File_Name;
    begin
        if not AD.Exists (Config_Dir) then
            begin
                AD.Create_Directory (Config_Dir);
            exception
                when AD.Name_Error | AD.Use_Error =>
                    return;
            end;
        end if;

        if SP.File_System.Is_File (Config_File)
            or else SP.File_System.Is_Dir (Config_File) then
                Ada.Text_IO.Put_Line ("Unable to create config file, something already exists there: " &
                    Config_File);
                return;
        end if;

        declare
            File : Ada.Text_IO.File_Type;
        begin
            Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Config_File);
            Ada.Text_IO.Put_Line (File, "enable-line-numbers");
            Ada.Text_IO.Put_Line (File, "enable-line-colors");
            Ada.Text_IO.Put_Line (File, "set-max-results 200");
            declare
                Current_Dir : constant String := Ada.Directories.Full_Name(Ada.Directories.Current_Directory);
            begin
                Ada.Text_IO.Put_Line (File, "add-dirs " & Current_Dir);
            exception
                when Ada.Directories.Use_Error => null;
            end;
            Ada.Text_IO.Close (File);

            -- Compiler bug?
            -- warning: "File" modified by call, but value might not be referenced
            pragma Unreferenced (File);

            Ada.Text_IO.Put_Line ("Configuration directory: " & Ada.Directories.Full_Name (Config_Dir));
            Ada.Text_IO.Put_Line ("Configuration file:      " & Ada.Directories.Full_Name (Config_File));
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line (Config_Dir_Name & " is for Septum settings and configuration.");
            Ada.Text_IO.Put_Line (Config_File_Name & " contains commands to run when starting in this directory.");
        exception
            when Ada.Text_IO.Name_Error | Ada.Text_IO.Use_Error =>
                Ada.Text_IO.Put_Line ("Unable to create configuration file.");
        end;
    end Create_Local_Config;

    -- Finds the config which is the closest ancestor to the given directory.
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
                (SP.Platform.Home_Dir & "/" & Config_Dir_Name & "/" & Config_File_Name);
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
