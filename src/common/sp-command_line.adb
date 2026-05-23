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

with Ada.Command_Line;

package body SP.Command_Line is

    function Has_More_Arguments (CL : Command_Line_Parser) return Boolean is
    begin
        return CL.Next_Index <= Ada.Command_Line.Argument_Count;
    end Has_More_Arguments;

    procedure Skip_Argument (CL : in out Command_Line_Parser) is
    begin
        CL.Next_Index := CL.Next_Index + 1;
    end Skip_Argument;

    function Next_Argument (CL : in out Command_Line_Parser) return String is
    begin
        return Result : constant String := Ada.Command_Line.Argument (CL.Next_Index) do
            CL.Next_Index := CL.Next_Index + 1;
        end return;
    end Next_Argument;

    function Try_Match (CL : in out Command_Line_Parser; S : String) return Boolean is
    begin
        if not Has_More_Arguments (CL) then
            return False;
        end if;

        if Ada.Command_Line.Argument (CL.Next_Index) = S then
            CL.Next_Index := @ + 1;
            return True;
        end if;

        return False;
    end Try_Match;

    function Is_Flag (CL : Command_Line_Parser) return Boolean is
    begin
        return (
            if Has_More_Arguments (CL) and then Ada.Command_Line.Argument (CL.Next_Index)'Length >= 2
            then Ada.Command_Line.Argument (CL.Next_Index)(1 .. 2) = "--"
            else False);
    end Is_Flag;

end SP.Command_Line;
