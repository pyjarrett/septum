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

-- Septum data is stored locally in the Next_Dir working directory on load or in the home directory of the user
-- running the command.  This allows users to maintain general configuration in their home directory based
-- on the settings they want to work with, and then have per-project settings that they can use.
--
-- Septum configuration setup.
-- Containing_Directory/
--     .septum/                 Directory to contain all Septum related data.
--         .config              Commands to run on startup.
package SP.Config is
    use SP.Strings;

    Config_Dir_Name  : constant String := ".septum";
    Config_File_Name : constant String := "config";

    -- A list of all possible locations which might have a configuration which
    -- can be read on program startup.
    function Config_Locations return String_Vectors.Vector;

    -- Creates a configuration in the given directory.
    procedure Create_Local_Config;

end SP.Config;
