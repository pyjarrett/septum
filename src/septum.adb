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

with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;

with SP.Interactive;

procedure Septum is
begin
    SP.Interactive.Main;
exception
    when Err : others =>
        Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Err));
        Ada.Text_IO.Put_Line ("Exception traceback: " & GNAT.Traceback.Symbolic.Symbolic_Traceback (Err));
end Septum;
