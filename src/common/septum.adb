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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with GNAT.Traceback.Symbolic;

with SP.Config;
with SP.Interactive;

procedure Septum is
    use Ada.Text_IO;
begin
    -- Look for a single "--version" flag
    if Ada.Command_Line.Argument_Count = 1
        and then Ada.Command_Line.Argument (1) = "--version"
    then
        Put_Line (SP.Version);
        return;
    end if;

    -- Create a local configuration file in the current directory.
    if Ada.Command_Line.Argument_Count = 1
        and then Ada.Command_Line.Argument (1) = "init"
    then
        SP.Config.Create_Local_Config;
        return;
    end if;

    -- Don't recognize any other arguments.
    if Ada.Command_Line.Argument_Count /= 0 then
        Put_Line ("Unrecognized command line arguments.");
        New_Line;
        Put_Line ("Usage: septum --version        print program version");
        Put_Line ("       septum init             creates config directory with empty config");
        Put_Line ("       septum                  run interactive search mode");
        return;
    end if;

    SP.Interactive.Main;
exception
    when Err : others =>
        Put_Line (Ada.Exceptions.Exception_Information (Err));
        Put_Line ("Exception traceback: " & GNAT.Traceback.Symbolic.Symbolic_Traceback (Err));
end Septum;
