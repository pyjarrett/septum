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
with Ada.Integer_Text_IO;
with Ada.Text_IO;

package body SP.Debug is
    procedure Print_Command_Line is
        use Ada;
    begin
        Text_IO.Put_Line ("Command: " & Command_Line.Command_Name);
        Text_IO.Put_Line
            ("Arguments: " & Integer'Image (Command_Line.Argument_Count));
        for Arg_Idx in 1 .. Command_Line.Argument_Count loop

            Integer_Text_IO.Put (Arg_Idx);
            Text_IO.Set_Col (5);
            Text_IO.Put_Line (Command_Line.Argument (Arg_Idx));
        end loop;
    end Print_Command_Line;

end SP.Debug;
