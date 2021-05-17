with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Directories;
with Ada.Text_IO;

with SP.Cache;
with SP.File_System;

package body SP.Cache is
    function "+" (Str : String) return Ada.Strings.Unbounded.Unbounded_String renames To_Unbounded_String;

    function Is_Text (File_Name : String) return Boolean is
        Ext : constant Ada.Strings.Unbounded.Unbounded_String :=
            To_Unbounded_String (Ada.Directories.Extension (File_Name));
        Known_Text : constant array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String :=
            (+"ads", +"adb", +"c", +"cpp", +"h", +"hpp", +"hh", +"cs");
    begin
        return (for some X of Known_Text => Ext = X);
    end Is_Text;

    procedure Cache_File (File_Cache : in out Async_File_Cache; File_Name : Ada.Strings.Unbounded.Unbounded_String) is
        -- Adds the contents of a file to the file cache.
        Lines : String_Vectors.Vector := String_Vectors.Empty_Vector;
    begin
        if Read_Lines (To_String (File_Name), Lines) then
            --  Ada.Text_IOO.Put_Line ("Asynchronous file caching of: " & To_String (File_Name));
            File_Cache.Cache_File (File_Name, Lines);
            --  Ada.Text_IOO.Put_Line (File_Cache.Files.Length'Image);
            --  Ada.Text_IO.Put_Line (File_Cache.Num_Files'Image);
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
                Ada.Text_IO.Put_Line ("Already contains: " & To_String (File_Name));
            else
                Contents.Insert (File_Name, Lines);
            end if;
        end Cache_File;

        function Num_Files return Natural is
        begin
            return Natural (Contents.Length);
        end Num_Files;

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

    procedure Add_Directory (A : in out Async_File_Cache; Dir : String) is
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
                entry Wake (New_Id : Natural);
            end Dir_Loader_Task;

            task body Dir_Loader_Task is
                Id       : Natural := 0;
                Elem     : Ada.Strings.Unbounded.Unbounded_String;
                Contents : SP.File_System.Dir_Contents;
            begin
                Ada.Text_IO.Put_Line ("Starting the directory loading task.");
                loop
                    Ada.Text_IO.Put_Line ("Directory loading task is waiting.");

                    -- Allowing queueing of many tasks, some of which might not be used, but will not prevent the
                    -- program from continuing.
                    select
                        accept Wake (New_Id : Natural) do
                            Id := New_Id;
                        end Wake;
                    or
                        terminate;
                    end select;

                    loop
                        select
                            Dir_Queue.Dequeue (Elem);
                            --  Ada.Text_IO.Put_Line (Id'Image & " dequeued: " & Ada.Strings.Unbounded.To_String
                            --  (Elem));
                        or
                            delay 1.0;
                            Ada.Text_IO.Put_Line (Id'image & " no more elements.");
                            exit;
                        end select;

                        Contents := SP.File_System.Contents (Ada.Strings.Unbounded.To_String (Elem));
                        for Dir of Contents.Subdirs loop
                            Dir_Queue.Enqueue (Dir);
                        end loop;

                        for File of Contents.Files loop
                            File_Queue.Enqueue (File);
                        end loop;
                        --  Ada.Text_IO.Put_Line ("FILES: " & File_Queue.Current_Use'Image);
                    end loop;
                end loop;
            end Dir_Loader_Task;

            task type File_Loader_Task is
                entry Wake (New_Id : Natural);
            end File_Loader_Task;

            task body File_Loader_Task is
                --Id : Natural := 0;
                Elem : Ada.Strings.Unbounded.Unbounded_String;
            begin
                Ada.Text_IO.Put_Line ("Starting the directory loading task.");
                loop
                    Ada.Text_IO.Put_Line ("Directory loading task is waiting.");

                    -- Allowing queueing of many tasks, some of which might not be used, but will not prevent the
                    -- program from continuing.
                    select
                        accept Wake (New_Id : Natural) do
                            --Id := New_Id;
                            pragma Unreferenced (New_Id);
                        end Wake;
                    or
                        terminate;
                    end select;

                    loop
                        select
                            File_Queue.Dequeue (Elem);
                        --Ada.Text_IO.Put_Line (Id'Image & " dequeued FILE: " & Ada.Strings.Unbounded.To_String (Elem));
                        or
                            delay 1.0;
                            --Ada.Text_IO.Put_Line (Id'image & " no more FILES.");
                            exit;
                        end select;

                        if Is_Text (To_String (Elem)) then
                            Cache_File (A, Elem);
                        end if;
                    end loop;
                end loop;
            end File_Loader_Task;

            Dir_Loader  : array (1 .. 12) of Dir_Loader_Task;
            File_Loader : array (1 .. 12) of File_Loader_Task;
            Id          : Natural := 0;
        begin
            for DL of Dir_Loader loop
                Id := Id + 1;
                DL.Wake (Id);
            end loop;

            for FL of File_Loader loop
                Id := Id + 1;
                FL.Wake (Id);
            end loop;

            Ada.Text_IO.Put_Line (A.Files.Length'Image);
        end;
    end Add_Directory;

end SP.Cache;
