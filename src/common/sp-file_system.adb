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

with Ada.IO_Exceptions;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with SP.Platform;
with SP.Terminal;

package body SP.File_System is

    package AD renames Ada.Directories;

    function Is_File (Target : String) return Boolean is
        use type Ada.Directories.File_Kind;
    begin
        return AD.Exists (Target) and then AD.Kind (Target) = AD.Ordinary_File;
    exception
        when others =>
            return False;
    end Is_File;

    function Is_Dir (Target : String) return Boolean is
        use type Ada.Directories.File_Kind;
    begin
        return AD.Exists (Target) and then AD.Kind (Target) = AD.Directory;
    exception
        when others =>
            return False;
    end Is_Dir;

    function Is_Current_Or_Parent_Directory (Dir_Entry : Ada.Directories.Directory_Entry_Type) return Boolean is
        --  Return true if the entry is "." or "..".
        Name : constant String := Ada.Directories.Simple_Name (Dir_Entry);
    begin
        return Name = "." or else Name = "..";
    end Is_Current_Or_Parent_Directory;

   function Contents (Dir_Name : String) return Dir_Contents is
        use Ada.Directories;
        Dir_Search : Search_Type;
        Next_Entry : Directory_Entry_Type;
        Filter     : constant Filter_Type := (Ordinary_File | Directory => True, others => False);
    begin
        return Result : Dir_Contents do
            Ada.Directories.Start_Search
                (Search => Dir_Search, Directory => Dir_Name, Pattern => "*", Filter => Filter);
            while More_Entries (Dir_Search) loop
                Get_Next_Entry (Dir_Search, Next_Entry);
                if not Is_Current_Or_Parent_Directory (Next_Entry) then
                    case Kind (Next_Entry) is
                        when Directory => Result.Subdirs.Append (Ada.Strings.Unbounded.To_Unbounded_String(Full_Name (Next_Entry)));
                        when Ordinary_File => Result.Files.Append (Ada.Strings.Unbounded.To_Unbounded_String(Full_Name (Next_Entry)));
                            when others => null;
                    end case;
                end if;
            end loop;
            End_Search (Dir_Search);
        end return;
    end Contents;

    --  Reads all the lines from a file.
    function Read_Lines (File_Name : String; Result : out String_Vectors.Vector) return Boolean is
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
            SP.Terminal.Put_Line ("Unable to read contents of: " & File_Name);
            return False;
    end Read_Lines;

    -- Finds a path similar to the given one with the same basic stem.
    function Similar_Path (Path : String) return String is
    begin
        -- TODO: This is bad.
        -- Naive loop cutting off the end of the string one character at a time.
        for Last_Index in reverse 2 .. Path'Length loop
            declare
                Shortened_Path : constant String := Path (Path'First .. Last_Index);
            begin
                if Is_File (Shortened_Path) then
                    return Shortened_Path;
                elsif Is_Dir (Shortened_Path) then
                    return Shortened_Path;
                end if;
            end;
        end loop;
        return "";
    exception
        when others => return "";
    end Similar_Path;

    -- Rewrite a path with all forward slashes for simplicity.
    function Rewrite_Path (Path : String) return String is
        S        : String := Path;
        Opposite : constant Character := SP.Platform.Path_Opposite_Separator;
        Local    : constant Character := SP.Platform.Path_Separator;
    begin
        for I in 1 .. S'Length loop
            if (Path (I) = Opposite) then
                S(I) := Local;
            else
                S(I) := Path (I);
            end if;
        end loop;
        return S;
    end Rewrite_Path;

    -- Produces all of the possible options for a path.
    function File_Completions (Path : String) return SP.Strings.String_Vectors.Vector
    is
        Result      : SP.Strings.String_Vectors.Vector;
        Files       : Dir_Contents;
        Rewritten   : ASU.Unbounded_String := ASU.To_Unbounded_String (Rewrite_Path (Path));
        Similar     : ASU.Unbounded_String := ASU.To_Unbounded_String (Similar_Path (ASU.To_String (Rewritten)));
    begin
        -- Has no higher directory.
        if ASU.Length (Similar) = 0 then
            return Result;
        end if;

        begin
            if (Is_Dir (ASU.To_String (Similar))
                and then ASU.Element (Similar, ASU.Length (Similar)) = SP.Platform.Path_Separator)
                or else ASU.Length (Similar) = 1
            then
                Files := Contents (ASU.To_String (Similar));
            else
                declare
                    Parent : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Similar_Path (ASU.Slice (Similar, 1, ASU.Length (Similar) - 1)));
                begin
                    if not Is_Dir (ASU.To_String (Parent)) then
                        return Result;
                    end if;

                    Files  := Contents (ASU.To_String (Parent));
                    Similar   := Parent;
                    Rewritten := ASU.To_Unbounded_String (Rewrite_Path (ASU.To_String (Similar)));
                end;
            end if;
        exception
            -- Skip over files we're not allowed to read.
            when Ada.IO_Exceptions.Use_Error =>
                null;
        end;


        -- The directory file contain paths with similar completions to the name.
        -- Filter out paths which don't have a matching prefix with the original.
        for Dir of Files.Subdirs loop
            if SP.Strings.Common_Prefix_Length (Rewritten, Dir) = ASU.Length (Rewritten) then
                Result.Append (Dir);
            end if;
        end loop;

        return Result;
    end File_Completions;

end SP.File_System;
