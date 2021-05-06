with Ada.Containers;
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

    Quit_Commands  : String_Sets.Set;
    Default_Prompt : constant String := "> ";

    procedure Build_Command_Map is
    begin
        Quit_Commands.Insert (To_Unbounded_String ("quit"));
        Quit_Commands.Insert (To_Unbounded_String ("exit"));
    end Build_Command_Map;

    function Read_Prompt (Prompt : String) return String_Vectors.Vector is
    begin
        Put (Prompt);
        declare
            Input : constant Unbounded_String := Get_Line;
        begin
            -- This might want to be a more complicated algorithm for splitting, such as handling quotes
            return Split (Input);
        end;
    end Read_Prompt;

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

        Command_Line := Read_Prompt (Default_Prompt);
        while Execute (Srch, Command_Line) loop
            Command_Line := Read_Prompt (Default_Prompt);
        end loop;
    end Main;
end SP.Interactive;
