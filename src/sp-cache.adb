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

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Directories;

with SP.Cache;
with SP.File_System;
with SP.Terminal;

with System.Multiprocessors;

package body SP.Cache is
    function "+" (Str : String) return Ada.Strings.Unbounded.Unbounded_String renames To_Unbounded_String;
    -- Convenience function for converting strings to unbounded.

    function Is_Text (File_Name : String) return Boolean is
        -- This is probably better written to look at encoding (such as invalid sequences in UTF-8, etc.)
        -- instead of being a hodgepodge of various formats I know that I care about right now.
        -- TODO: Adding more file types I care about now, this needs to be fixed properly.
        Ext : constant Ada.Strings.Unbounded.Unbounded_String :=
            To_Unbounded_String (Ada.Directories.Extension (File_Name));
        Known_Text : constant array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String :=
            (+"ads", +"adb",          -- Ada
             +"c", +"h",              -- c
             +"cpp", +"C",             -- C++
             +"hpp", +"hh", +"inl",
             +"cs",                   -- C#
             +"hs",                   -- Haskell
             +"py",                   -- Python
             +"rs"                    -- Rust
            );
    begin
        return (for some X of Known_Text => Ext = X);
    end Is_Text;

    procedure Cache_File (File_Cache : in out Async_File_Cache; File_Name : Ada.Strings.Unbounded.Unbounded_String) is
        -- Adds the contents of a file to the file cache.
        Lines : String_Vectors.Vector := String_Vectors.Empty_Vector;
    begin
        if Read_Lines (To_String (File_Name), Lines) then
            File_Cache.Cache_File (File_Name, Lines);
        end if;
    end Cache_File;

    protected body Async_File_Cache is
        procedure Clear is
        begin
            Contents.Clear;
        end Clear;

        procedure Cache_File (File_Name : in Unbounded_String; Lines : in String_Vectors.Vector) is
        begin
            if Contents.Contains (File_Name) then
                SP.Terminal.Put_Line ("Already contains: " & To_String (File_Name));
            else
                Contents.Insert (File_Name, Lines);
            end if;
        end Cache_File;

        function Num_Files return Natural is
        begin
            return Natural (Contents.Length);
        end Num_Files;

        function Num_Lines return Natural is
        begin
            return N : Natural := 0 do
                for Cursor in Contents.Iterate loop
                    N := N + Natural(File_Maps.Element (Cursor).Length);
                end loop;
            end return;
        end Num_Lines;

        function Lines (File_Name : in Unbounded_String) return String_Vectors.Vector is
        begin
            return Contents (File_Name);
        end Lines;

        function Files return String_Vectors.Vector is
        begin
            return Result : String_Vectors.Vector do
                for Cursor in Contents.Iterate loop
                    Result.Append (SP.Cache.File_Maps.Key (Cursor));
                end loop;
            end return;
        end Files;

        function File_Line (File_Name : in Unbounded_String; Line : in Positive) return Unbounded_String is
        begin
            return Contents.Element (File_Name).Element (Line);
        end File_Line;

    end Async_File_Cache;

    procedure Add_Directory_Recursively (A : in out Async_File_Cache; Dir : String) is
        package String_Queue_Interface is new Ada.Containers.Synchronized_Queue_Interfaces
            (Element_Type => Ada.Strings.Unbounded.Unbounded_String);
        package String_Unbounded_Queue is new Ada.Containers.Unbounded_Synchronized_Queues
            (Queue_Interfaces => String_Queue_Interface);

        Dir_Queue  : String_Unbounded_Queue.Queue;
        File_Queue : String_Unbounded_Queue.Queue;
    begin
        Dir_Queue.Enqueue (New_Item => Ada.Strings.Unbounded.To_Unbounded_String (Dir));
        declare
            task type Dir_Loader_Task is
                entry Wake;
            end Dir_Loader_Task;

            task body Dir_Loader_Task is
                Elem     : Ada.Strings.Unbounded.Unbounded_String;
                Contents : SP.File_System.Dir_Contents;
            begin
                loop
                    -- Allowing queueing of many tasks, some of which might not be used, but will not prevent the
                    -- program from continuing.
                    select
                        accept Wake;
                    or
                        terminate;
                    end select;

                    loop
                        select
                            Dir_Queue.Dequeue (Elem);
                        or
                            delay 1.0;
                            exit;
                        end select;

                        Contents := SP.File_System.Contents (Ada.Strings.Unbounded.To_String (Elem));
                        for Dir of Contents.Subdirs loop
                            Dir_Queue.Enqueue (Dir);
                        end loop;

                        for File of Contents.Files loop
                            File_Queue.Enqueue (File);
                        end loop;
                    end loop;
                end loop;
            end Dir_Loader_Task;

            task type File_Loader_Task is
                entry Wake;
            end File_Loader_Task;

            task body File_Loader_Task is
                Elem : Ada.Strings.Unbounded.Unbounded_String;
            begin
                loop
                    -- Allowing queueing of many tasks, some of which might not be used, but will not prevent the
                    -- program from continuing.
                    select
                        accept Wake;
                    or
                        terminate;
                    end select;

                    loop
                        select
                            File_Queue.Dequeue (Elem);
                        or
                            delay 1.0;
                            exit;
                        end select;

                        if Is_Text (To_String (Elem)) then
                            Cache_File (A, Elem);
                        end if;
                    end loop;
                end loop;
            end File_Loader_Task;

            Num_CPUs : constant System.Multiprocessors.CPU := System.Multiprocessors.Number_Of_CPUs;
            Dir_Loader  : array (1 .. Num_CPUs) of Dir_Loader_Task;
            File_Loader : array (1 .. Num_CPUs) of File_Loader_Task;
        begin
            SP.Terminal.Put_Line ("Loading with" & Num_CPUs'Image & " tasks.");
            for DL of Dir_Loader loop
                DL.Wake;
            end loop;

            for FL of File_Loader loop
                FL.Wake;
            end loop;
        end;
    end Add_Directory_Recursively;

end SP.Cache;
