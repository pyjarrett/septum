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

with SP.Searches;
with SP.Strings;            use SP.Strings;

package SP.Commands is

    pragma Elaborate_Body;

    function Execute (Srch : in out SP.Searches.Search; Command_Line : in String_Vectors.Vector) return Boolean;

end SP.Commands;
