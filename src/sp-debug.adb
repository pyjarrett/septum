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
