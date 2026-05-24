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
with SP.File_System;
with SP.Platform;
with SP.Terminal;

package body SP.Config is
    package AD renames Ada.Directories;
    package ASU renames Ada.Strings.Unbounded;
    package FS renames SP.File_System;

    use type ASU.Unbounded_String;

    procedure Create_Local_Config is
        package SH renames SP.Strings.String_Holders;
        Current_Dir : constant String := AD.Current_Directory;
        Is_Global   : constant Boolean := not SP.Platform.Is_Path_Ok_For_Config (Current_Dir);
        Config_Dir  : SH.Holder;
        Config_File : SH.Holder;
    begin
        if Is_Global then
            Terminal.Put_Line (Terminal.UI, "In home directory, trying to create a user global config instead.");
            Config_Dir := SH.To_Holder (SP.Platform.Global_Config_Dir.Constant_Reference
                & "/" & Global_Config_Dir_Name);
            Config_File := SH.To_Holder (Config_Dir.Constant_Reference & "/" & Config_File_Name);
        else
            Config_Dir := SH.To_Holder (Current_Dir & "/" & Local_Config_Dir_Name);
            Config_File := SH.To_Holder (Config_Dir.Constant_Reference & "/" & Config_File_Name);
        end if;

        Terminal.Put_Line (Terminal.UI, "Creating config at: " & Config_File.Constant_Reference);

        if not AD.Exists (Config_Dir.Constant_Reference) then
            begin
                AD.Create_Directory (Config_Dir.Constant_Reference);
            exception
                when AD.Name_Error | AD.Use_Error =>
                    Ada.Text_IO.Put_Line ("Unable to create config directory: "
                        & Config_Dir.Constant_Reference);
                    return;
            end;
        end if;

        if SP.File_System.Is_File (Config_File.Constant_Reference)
            or else SP.File_System.Is_Dir (Config_File.Constant_Reference)
        then
                Ada.Text_IO.Put_Line ("Config already exists.");
                return;
        end if;

        declare
            File : Ada.Text_IO.File_Type;
        begin
            Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Config_File.Constant_Reference);
            Ada.Text_IO.Put_Line (File, "enable-line-numbers");
            Ada.Text_IO.Put_Line (File, "enable-line-colors");
            Ada.Text_IO.Put_Line (File, "enable-auto-search");
            Ada.Text_IO.Put_Line (File, "set-max-results 200");
            if not Is_Global then
                declare
                    Current_Dir : constant String := Ada.Directories.Full_Name(Ada.Directories.Current_Directory);
                begin
                    Ada.Text_IO.Put_Line (File, "add-dirs " & Current_Dir);
                exception
                    when Ada.Directories.Use_Error => null;
                end;
            end if;
            Ada.Text_IO.Close (File);

            -- Compiler bug?
            -- warning: "File" modified by call, but value might not be referenced
            pragma Unreferenced (File);

            Ada.Text_IO.Put_Line ("Configuration directory: " & Ada.Directories.Full_Name (Config_Dir.Constant_Reference));
            Ada.Text_IO.Put_Line ("Configuration file:      " & Ada.Directories.Full_Name (Config_File.Constant_Reference));
        exception
            when Ada.Text_IO.Name_Error | Ada.Text_IO.Use_Error =>
                Ada.Text_IO.Put_Line ("Unable to create configuration file at " & Config_File.Constant_Reference);
        end;
    end Create_Local_Config;

    function Config_Locations return String_Sets.Set is
        package SH renames SP.Strings.String_Holders;
        Config_Dir : constant SH.Holder := SP.Platform.Global_Config_Dir;
        Current_Dir_Config : constant ASU.Unbounded_String := ASU.To_Unbounded_String (
            Ada.Directories.Current_Directory & "/" & Local_Config_Dir_Name & "/" & Config_File_Name);
    begin
        return Result : String_Sets.Set do
            --  Look for a global user config.
            if not Config_Dir.Is_Empty then
                declare
                    Config_Path : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Config_Dir.Element & "/" & Global_Config_Dir_Name & "/" & Config_File_Name);
                begin
                    if not Result.Contains (Config_Path) and then FS.Is_File (ASU.To_String (Config_Path)) then
                        Result.Insert (Config_Path);
                    end if;
                end;
            end if;

            if Current_Dir_Config /= ASU.Null_Unbounded_String
                and then FS.Is_File (ASU.To_String (Current_Dir_Config))
            then
                if not Result.Contains (Current_Dir_Config) then
                    Result.Insert (Current_Dir_Config);
                end if;
            end if;
        end return;
    end Config_Locations;
end SP.Config;
