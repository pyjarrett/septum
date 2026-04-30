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

with Ada.Characters.Handling;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Directories;

with SP.File_System;
with SP.Progress;
with SP.Terminal;

with System.Multiprocessors.Dispatching_Domains;

with Dir_Iterators.Recursive;
with Progress_Indicators.Work_Trackers;

package body SP.Cache is
    --  Loading and then testing the file for utf-8 validity is slow with Ada
    --  standard library, so use text extension and binary extension checks
    --  to speed up the loading pass.
    function Is_Text_Extension (File_Name : String) return Boolean is
        Extension : constant String := Ada.Characters.Handling.To_Lower (Ada.Directories.Extension (File_Name));
    begin
        return Extension in
            "ads"   |  -- Ada
            "adb"   |
            "c"     |  -- c
            "h"     |
            "cpp"   |  -- C++
            "C"     |
            "cc"    |
            "cr"    |  -- Crystal
            "cs"    |  -- C#
            "css"   |
            "d"     |  -- D language
            "dart"  |
            "fs"    |  -- F#
            "fsx"   |  -- F# script
            "glsl"  |
            "go"    |
            "hpp"   |  -- C++ header
            "hh"    |  -- C++ header
            "hxx"   |  -- C++ header
            "hlsl"  |
            "html"  |
            "inl"   |  -- C++ include
            "ini"   |
            "ipp"   |  -- C++ include
            "lean"  |
            "ll"    |  -- LLVM
            "m"     |  -- objective-c
            "mm"    |  -- objective-c++
            "java"  |
            "json"  |
            "jsonc" |
            "json5" |
            "lock"  |
            "log"   |
            "lua"   |
            "natvis"|
            "hs"    |  -- Haskell
            "md"    |  -- Markdown
            "odin"  |
            "py"    |  -- Python
            "rb"    |  -- Ruby
            "rs"    |  -- Rust
            "tcc"   |  -- C++
            "tpp"   |  -- C++
            "swift" |
            "toml"  |
            "txt"   |
            "xml"   |
            "yaml"  |
            "yml";

    end Is_Text_Extension;

    --  Binary files like to be large, so use a deecent list to skip common ones.
    function Is_Binary_Extension (File_Name : String) return Boolean is
        Extension : constant String := Ada.Characters.Handling.To_Lower (Ada.Directories.Extension (File_Name));
    begin
        return Extension in
            "a"            |
            "ali"          |  -- Ada (GNAT) intermediate file
            "avi"          |
            "aux"          |
            "bin"          |
            "blend"        |
            "bmp"          |
            "class"        |
            "db"           |
            "dll"          |
            "dmp"          |
            "doc"          |
            "docx"         |
            "exe"          |
            "fbx"          |
            "fossil"       |
            "gif"          |
            "glb"          |  -- Binary GLTF
            "gz"           |
            "jar"          |
            "jpg"          |
            "jpeg"         |
            "lib"          |
            "mkv"          |
            "mov"          |
            "mp3"          |
            "mp4"          |
            "msi"          |  -- Installer
            "o"            |  -- Binary object file
            "obj"          |  -- Object files sometimes called ".obj"
            "ogg"          |
            "pak"          |
            "pdb"          |
            "pdf"          |
            "pem"          |
            "png"          |
            "pyc"          |  -- Python
            "pyo"          |
            "rar"          |
            "raw"          |
            "so"           |  -- Shared objects
            "sqlite"       |
            "tar"          |
            "tgz"          |
            "tga"          |
            "tiff"         |
            "ttf"          |  -- Truetype font
            "ucas"         |  -- Unreal engine
            "uasset"       |
            "umap"         |
            "unitypackage" |  -- Unity package file
            "unity"        |  -- Unity scene file
            "vhd"          |  -- Virtual hard drive
            "vsix"         |
            "wav"          |
            "zip";
    end Is_Binary_Extension;

    procedure Cache_File (File_Cache : in out Async_File_Cache; File_Name : Ada.Strings.Unbounded.Unbounded_String) is
        Lines : String_Vectors.Vector := String_Vectors.Empty_Vector;
        String_File_Name : constant String := To_String (File_Name);
    begin
        if Is_Binary_Extension (String_File_Name) then
            return;
        end if;

        if Is_Text_Extension (String_File_Name) or else File_System.Should_Load (String_File_Name) then
            if SP.File_System.Read_Lines (To_String (File_Name), Lines) then
                File_Cache.Cache_File (File_Name, Lines);
            end if;
        end if;
    end Cache_File;

    protected body Async_File_Cache is
        procedure Clear is
        begin
            Contents.Clear;
        end Clear;

        procedure Cache_File (File_Name : in Unbounded_String; Lines : in out String_Vectors.Vector) is
            procedure Swap_Lines (Key : Unbounded_String; Element : in out String_Vectors.Vector) is
            begin
               pragma Unreferenced (Key);
               String_Vectors.Move (Element, Lines);
            end Swap_Lines;

            Position : File_Maps.Cursor;
            Inserted : Boolean;
        begin
            Contents.Insert (File_Name, String_Vectors.Empty_Vector, Position, Inserted);
            if not Inserted then
                SP.Terminal.Put_Line ("Replacing contents of " & To_String (File_Name));
            end if;
            Contents.Update_Element (Position, Swap_Lines'Access);
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
        type Work_Element_Kind is (Path, Stop);
        type Work_Element (Kind : Work_Element_Kind := Stop) is record
            case Kind is
               when Path =>
                  Name : ASU.Unbounded_String;
               when Stop =>
                  null;
            end case;
         end record;

        package Work_Element_Queue_Interface is new Ada.Containers.Synchronized_Queue_Interfaces
            (Element_Type => Work_Element);
        package Work_Element_Queue is new Ada.Containers.Unbounded_Synchronized_Queues
            (Queue_Interfaces => Work_Element_Queue_Interface);

        File_Queue : Work_Element_Queue.Queue;

        package PI renames Progress_Indicators;
        Progress       : aliased PI.Work_Trackers.Work_Tracker;

        use all type System.Multiprocessors.CPU;
        Num_CPUs       : constant System.Multiprocessors.CPU := System.Multiprocessors.Number_Of_CPUs;
        File_CPU_Start : constant System.Multiprocessors.CPU := (if Num_CPUs > 1 then 2 else 1);
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
                            ((Kind => Path,
                              Name => Ada.Strings.Unbounded.To_Unbounded_String (Ada.Directories.Full_Name (Dir_Entry))));
                        Progress.Start_Work (1);
                    end if;
                end loop;
                File_Queue.Enqueue ((Kind => Stop));
            end Dir_Loader_Task;

            task type File_Loader_Task is
                entry Wake;
            end File_Loader_Task;

            task body File_Loader_Task is
                Elem : Work_Element;
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
                        File_Queue.Dequeue (Elem);
                        if Elem.Kind = Stop then
                            File_Queue.Enqueue (Elem);
                            exit;
                        else
                            Cache_File (A, Elem.Name);
                            Progress.Finish_Work (1);
                        end if;
                    end loop;
                end loop;
            end File_Loader_Task;

            Progress_Tracker : SP.Progress.Update_Progress (Progress'Access);
        begin
            SP.Terminal.Put_Line ("Loading with" & Num_CPUs'Image & " tasks.");
            SP.Terminal.New_Line;

            declare
                File_Loader : array (File_CPU_Start .. Num_CPUs) of File_Loader_Task;
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
