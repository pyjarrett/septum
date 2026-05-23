-------------------------------------------------------------------------------
-- Copyright 2026, The Septum Developers (see AUTHORS file)

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

package SP.Command_Line is

    type Command_Line_Parser is private;

    function Has_More_Arguments (CL : Command_Line_Parser) return Boolean;

    procedure Skip_Argument (CL : in out Command_Line_Parser);

    function Next_Argument (CL : in out Command_Line_Parser) return String;

    function Try_Match (CL : in out Command_Line_Parser; S : String) return Boolean;

    function Is_Flag (CL : Command_Line_Parser) return Boolean;

private

    type Command_Line_Parser is record
        Next_Index : Natural := 1;
    end record;

end SP.Command_Line;
