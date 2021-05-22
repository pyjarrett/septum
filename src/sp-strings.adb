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

with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with Ada.Characters.Latin_1;

with GNAT.OS_Lib;

package body SP.Strings is
    function Shell_Split (S : Ada.Strings.Unbounded.Unbounded_String) return String_Vectors.Vector is
        use Ada.Strings.Unbounded;
        use GNAT.OS_Lib;
        Args : Argument_List_Access := Argument_String_To_List (To_String (S));
    begin
        return Result : String_Vectors.Vector do
            for Arg : GNAT.OS_Lib.String_Access of Args.all loop
                declare
                    Real_Arg : constant Unbounded_String := To_Unbounded_String (Arg.all);
                begin
                    if SP.Strings.Is_Quoted (Arg.all) then
                        Result.Append(To_Unbounded_String (Slice(Real_Arg, 2, Length(Real_Arg) - 1)));
                    else
                        Result.Append (Real_Arg);
                    end if;
                end;
            end loop;
            GNAT.OS_Lib.Free (Args);
        end return;
    end Shell_Split;

    function Read_Lines (File_Name : String; Result : out String_Vectors.Vector) return Boolean is
        --  Reads all the lines from a file.
        File : Ada.Text_IO.File_Type;
        Line : Ada.Strings.Unbounded.Unbounded_String;
    begin
        String_Vectors.Clear (Result);
        Ada.Text_IO.Open (File => File, Mode => Ada.Text_IO.In_File, Name => File_Name);
        while not Ada.Text_IO.End_Of_File (File) loop
            Line := Ada.Strings.Unbounded.Text_IO.Get_Line (File);
            Result.Append (Line);
        end loop;

        Ada.Text_IO.Close (File);
        return True;
    exception
        when Ada.Text_IO.End_Error =>
            if Ada.Text_IO.Is_Open (File) then
                Ada.Text_IO.Close (File);
            end if;
            return True;
        when others =>
            Ada.Text_IO.Put_Line ("Unable to read contents of: " & File_Name);
            return False;
    end Read_Lines;

    function Common_Prefix_Length
        (A : Ada.Strings.Unbounded.Unbounded_String; B : Ada.Strings.Unbounded.Unbounded_String) return Natural is
        use Ada.Strings.Unbounded;
        -- Finds the number of common starting characters between two strings.
    begin
        return Count : Natural := 0 do
            while Count < Length (A) and then Count < Length (B)
                and then Element (A, Count + 1) = Element (B, Count + 1) loop
                Count := Count + 1;
            end loop;
        end return;
    end Common_Prefix_Length;

    function Is_Quoted (S : String) return Boolean is
        use Ada.Characters.Latin_1;
        Quote_Types : constant array (Positive range <>) of Character := (Quotation, Apostrophe);
    begin
        return S'Length > 0 and then S (S'First) = S (S'Last) and then (for some X of Quote_Types => X = S (S'First));
    end Is_Quoted;

end SP.Strings;
