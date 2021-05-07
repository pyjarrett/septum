with Ada.Containers;
with Ada.Directories;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with SP.Strings;
use SP.Strings;
with SP.Contexts;
use SP.Contexts;
with SP.Commands;

package body SP.Interactive is
    use Ada.Strings.Unbounded;
    use Ada.Strings.Unbounded.Text_IO;
    use Ada.Text_IO;

    Quit_Commands : String_Sets.Set;

    procedure Build_Command_Map is
    begin
        Quit_Commands.Insert (To_Unbounded_String ("quit"));
        Quit_Commands.Insert (To_Unbounded_String ("exit"));
    end Build_Command_Map;

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

    function Execute (Srch : in out SP.Contexts.Search; Command_Line : String_Vectors.Vector) return Boolean is
        use Ada.Containers;
        Parameters   : String_Vectors.Vector     := Command_Line;
        Command_Name : constant Unbounded_String :=
            (if Parameters.Is_Empty then Null_Unbounded_String else Parameters.First_Element);
    begin
        if Command_Line.Is_Empty then
            return True;
        else
            Parameters.Delete_First;
        end if;

        if Command_Line.Length = 1 and then Quit_Commands.Contains (Command_Name) then
            return False;
        elsif not SP.Commands.Execute (Srch, Command_Name, Parameters) then
            Put_Line ("Unknown command: " & To_String (Command_Name));
        end if;
        return True;
    end Execute;

    procedure Main is
        -- The interactive loop through which the user starts a search context and then interatively refines it by
        -- pushing and popping operations.
        Command_Line : String_Vectors.Vector;
        Srch         : SP.Contexts.Search;
    begin
        Build_Command_Map;
        Add_Directory (Srch, Ada.Directories.Current_Directory);
        Reload_Working_Set (Srch);

        Write_Prompt (Srch);
        Command_Line := Read_Command;
        while Execute (Srch, Command_Line) loop
            Write_Prompt (Srch);
            Command_Line := Read_Command;
        end loop;
    end Main;
end SP.Interactive;
