with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with SP.File_System;

package body SP.Cache is

    function Has_Directory (A : in Async_File_Cache; Dir : String) return Boolean is
    begin
        return A.Top_Level_Directories.Contains (Ada.Strings.Unbounded.To_Unbounded_String (Dir));
    end Has_Directory;

    procedure Add_Directory (A : in out Async_File_Cache; Dir : String) is
        package String_Queue_Interface is new Ada.Containers.Synchronized_Queue_Interfaces
            (Element_Type => Ada.Strings.Unbounded.Unbounded_String);
        package String_Unbounded_Queue is new Ada.Containers.Unbounded_Synchronized_Queues
            (Queue_Interfaces => String_Queue_Interface);

        Dir_Queue : String_Unbounded_Queue.Queue;
        File_Queue : String_Unbounded_Queue.Queue;
    begin
        A.Top_Level_Directories.Insert (Ada.Strings.Unbounded.To_Unbounded_String (Dir));

        Dir_Queue.Enqueue (New_Item => Ada.Strings.Unbounded.To_Unbounded_String (Dir));

        declare

            task type Dir_Loader_Task is
                entry Wake(New_Id : Natural);
            end Dir_Loader_Task;

            task body Dir_Loader_Task is
                Id : Natural := 0;
                Elem : Ada.Strings.Unbounded.Unbounded_String;
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
                            Ada.Text_IO.Put_Line (Id'Image & " dequeued: " & Ada.Strings.Unbounded.To_String (Elem));
                        or
                            delay 1.0;
                            Ada.Text_IO.Put_Line (Id'image & " no more elements.");
                            exit;
                        end select;

                        Contents := SP.File_System.Contents (Ada.Strings.Unbounded.To_String(Elem));
                        for Dir of Contents.Subdirs loop
                            Dir_Queue.Enqueue (Dir);
                        end loop;

                        for File of Contents.Files loop
                            File_Queue.Enqueue (File);
                        end loop;
                        Ada.Text_IO.Put_Line ("Files: " & File_Queue.Current_Use'Image);
                    end loop;
                end loop;
            end Dir_Loader_Task;

            Dir_Loader : array (1 .. 12) of Dir_Loader_Task;
            Id : Natural := 0;
        begin
            for DL of Dir_Loader loop
                Id := Id + 1;
                DL.Wake (Id);
            end loop;
        end;
    end Add_Directory;

    protected body File_Cache is
    end File_Cache;

end SP.Cache;
