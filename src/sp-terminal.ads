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

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

package SP.Terminal is
    -- Functions for operations to the terminal. This hides the usage of Ada.Text_IO and may silently ignore
    -- capabilities if the terminal does not support them, such as if coloring text or line clearing is added.
    --
    -- This module also hides dependencies on unbounded IO.

    procedure Put (Str : String) renames Ada.Text_IO.Put;
    procedure Put (Str : Ada.Strings.Unbounded.Unbounded_String) renames Ada.Text_IO.Unbounded_IO.Put;

    procedure Put_Line (Str : String) renames Ada.Text_IO.Put_Line;
    procedure Put_Line (Str : Ada.Strings.Unbounded.Unbounded_String) renames Ada.Text_IO.Unbounded_IO.Put_Line;

    function Get_Line return String renames Ada.Text_IO.Get_Line;
    function Get_Line return Ada.Strings.Unbounded.Unbounded_String renames Ada.Text_IO.Unbounded_IO.Get_Line;

    procedure New_Line (Spacing : in Ada.Text_IO.Positive_Count := 1) renames Ada.Text_IO.New_Line;

    procedure Set_Col (Spacing : in Ada.Text_IO.Positive_Count) renames Ada.Text_IO.Set_Col;

    -- I'm not convinced that these aren't useful. I haven't figured out how best to deal with the really long and
    -- verbose terminology of Ada.Strings.Unbounded.Unbounded_String.

    --  function "&" (A : String; B : Unbounded_String) return Unbounded_String renames Ada.Strings.Unbounded."&";
    --  function "&" (Ada : Unbounded_String; B : String) return Unbounded_String renames Ada.Strings.Unbounded."&";

end SP.Terminal;
