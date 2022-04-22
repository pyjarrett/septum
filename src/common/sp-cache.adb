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

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Directories;
with Ada.Task_Identification;

with SP.Cache;
with SP.File_System;
with SP.Progress;
with SP.Terminal;

with System.Multiprocessors.Dispatching_Domains;

with Dir_Iterators.Recursive;
with Progress_Indicators.Work_Trackers;

package body SP.Cache is
    -- Convenience function for converting strings to unbounded.
    function "+" (Str : String) return Ada.Strings.Unbounded.Unbounded_String renames To_Unbounded_String;

    function Is_Text (File_Name : String) return Boolean is
        -- This is probably better written to look at encoding (such as invalid sequences in UTF-8, etc.)
        -- instead of being a hodgepodge of various formats I know that I care about right now.
        -- TODO: Adding more file types I care about now, this needs to be fixed properly.
        Extension : String renames Ada.Directories.Extension (File_Name);
    begin
        return Extension in
            "ads"  |  -- Ada
            "adb"  |
            "c"    |  -- c
            "h"    |
            "cpp"  |  -- C++
            "C"    |
            "hpp"  |
            "hh"   |
            "inl"  |
            "lock" |
            "toml" |
            "cs"   |  -- C#
            "hs"   |  -- Haskell
            "py"   |  -- Python
            "rs";     -- Rust
    end Is_Text;

    procedure Cache_File (File_Cache : in out Async_File_Cache; File_Name : Ada.Strings.Unbounded.Unbounded_String) is
        Lines : String_Vectors.Vector := String_Vectors.Empty_Vector;
    begin
        if SP.File_System.Read_Lines (To_String (File_Name), Lines) then
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
                SP.Terminal.Put_Line ("Replacing contents of " & To_String (File_Name));
                Contents.Replace (File_Name, Lines);
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
                    N := N + Natural (File_Maps.Element (Cursor).Length);
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

    -- Adds all directories to the file cache.
    --
    -- Most users will probably only have source on a single medium, so
    -- parallelizing the load probably won't improve speed.  The split of
    -- parsing tasks is to support more complicated caching methods in the
    -- future, as we're I/O bound here based on the disk speed.
    function Add_Directory_Recursively (
        A    : in out Async_File_Cache;
        Dir  : String) return Boolean
    is
        package String_Queue_Interface is new Ada.Containers.Synchronized_Queue_Interfaces
            (Element_Type => Ada.Strings.Unbounded.Unbounded_String);
        package String_Unbounded_Queue is new Ada.Containers.Unbounded_Synchronized_Queues
            (Queue_Interfaces => String_Queue_Interface);

        File_Queue : String_Unbounded_Queue.Queue;

        package PI renames Progress_Indicators;
        Progress     : aliased PI.Work_Trackers.Work_Tracker;
    begin
        declare
            -- A directory loading task builds a queue of files to parse for the
            -- file loader tasks.
            task Dir_Loader_Task with CPU => 1 is end;

            task body Dir_Loader_Task is
                Dir_Walk : constant Dir_Iterators.Recursive.Recursive_Dir_Walk := Dir_Iterators.Recursive.Walk (Dir);
                use type Ada.Directories.File_Kind;
            begin
                for Dir_Entry of Dir_Walk loop
                    if Ada.Directories.Kind (Dir_Entry) = Ada.Directories.Ordinary_File then
                        File_Queue.Enqueue
                            (Ada.Strings.Unbounded.To_Unbounded_String (Ada.Directories.Full_Name (Dir_Entry)));
                        Progress.Start_Work (1);
                    end if;
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
                        Progress.Finish_Work (1);
                    end loop;
                end loop;
            end File_Loader_Task;

            Progress_Tracker : SP.Progress.Update_Progress (Progress'Access);
            Num_CPUs : constant System.Multiprocessors.CPU := System.Multiprocessors.Number_Of_CPUs;
        begin
            SP.Terminal.Put_Line ("Loading with" & Num_CPUs'Image & " tasks.");
            SP.Terminal.New_Line;

            declare
                File_Loader : array (1 .. Num_CPUs) of File_Loader_Task;
            begin
                for I in File_Loader'Range loop
                    begin
                        System.Multiprocessors.Dispatching_Domains.Set_CPU (I, File_Loader(I)'Identity);
                    exception
                        when System.Multiprocessors.Dispatching_Domains.Dispatching_Domain_Error => null;
                    end;
                    File_Loader(I).Wake;
                end loop;
            end;

            Progress_Tracker.Stop;
            SP.Terminal.New_Line;

            return True;
        end;
    end Add_Directory_Recursively;

end SP.Cache;
