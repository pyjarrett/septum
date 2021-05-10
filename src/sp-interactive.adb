with Ada.Directories;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with SP.Strings;  use SP.Strings;
with SP.Contexts; use SP.Contexts;
with SP.Commands;

package body SP.Interactive is
    use Ada.Strings.Unbounded;
    use Ada.Strings.Unbounded.Text_IO;
    use Ada.Text_IO;

    procedure Write_Prompt (Srch : in Search) is
        -- Writes the prompt and get ready to read user input.
        Default_Prompt : constant String := " > ";
    begin
        New_Line;
        Put ("Files: " & Integer'Image (SP.Contexts.Num_Cached_Files (Srch)));
        Set_Col (20);
        Put_Line ("Bytes: " & Integer'Image (SP.Contexts.Num_Cached_Bytes (Srch)));
        if not SP.Contexts.List_Extensions (Srch).Is_Empty then
            Put ("Exts:  ");
            for Ext of SP.Contexts.List_Extensions (Srch) loop
                Put (Ext & " ");
            end loop;
        end if;
        New_Line;
        Put (Default_Prompt);
    end Write_Prompt;

    function Read_Command return String_Vectors.Vector is
    begin
        declare
            Input : constant Unbounded_String := Get_Line;
        begin
            -- This might want to be a more complicated algorithm for splitting, such as handling quotes
            return Split (Input);
        end;
    end Read_Command;

    procedure Execute (Srch : in out SP.Contexts.Search; Command_Line : String_Vectors.Vector) is
        Parameters   : String_Vectors.Vector     := Command_Line;
        Command_Name : constant Unbounded_String :=
            (if Parameters.Is_Empty then Null_Unbounded_String else Parameters.First_Element);
    begin
        if Command_Line.Is_Empty then
            -- An empty command is always successfully executed.
            return;
        end if;

        Parameters.Delete_First;
        if not SP.Commands.Execute (Srch, Command_Name, Parameters) then
            Put_Line ("Unknown command: " & To_String (Command_Name));
        end if;
    end Execute;

    procedure Main is
        -- The interactive loop through which the user starts a search context and then interatively refines it by
        -- pushing and popping operations.
        Command_Line : String_Vectors.Vector;
        Srch         : SP.Contexts.Search;
    begin
        Add_Directory (Srch, Ada.Directories.Current_Directory);
        Reload_Working_Set (Srch);

        loop
            Write_Prompt (Srch);
            Command_Line := Read_Command;
            Execute (Srch, Command_Line);
        end loop;
    end Main;
end SP.Interactive;
