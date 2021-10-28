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

with Ada.Strings.Unbounded;

with SP.Searches;
with SP.Strings;

-- At its core, Septum is just a program that loads text and then executes
-- commands on the current context.  Whether these commands come from a user
-- or a script is immaterial.
package SP.Commands is
    pragma Elaborate_Body;

    use SP.Strings;
    use type Ada.Strings.Unbounded.Unbounded_String;

    -- Checks to see if a command is understood.
    function Is_Command (S : String) return Boolean;

    -- Checks to see if a command matches a known command prefix.
    function Is_Like_Command (S : String) return Boolean;

    -- Attempts to match a partially completed command against one of the
    -- commands understood by Septum.
    --
    -- A completion might be ambiguous, which would result in a null string
    -- being returned.
    function Target_Command (Command_Name : ASU.Unbounded_String) return ASU.Unbounded_String
        with Post => Target_Command'Result = ASU.Null_Unbounded_String
            or else Is_Command (ASU.To_String (Target_Command'Result));

    type Command_Result is (
        Command_Success,
        Command_Failed,
        Command_Unknown,
        Command_Exit_Requested);

    function Run_Commands_From_File (Srch : in out SP.Searches.Search; File : String) return Command_Result;
    function Execute (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Command_Result;

end SP.Commands;
