with Ada.Directories;
with Ada.Strings.Unbounded;
with SP.Commands;
with SP.Searches; use SP.Searches;
with SP.Strings;  use SP.Strings;
with SP.Terminal;

package body SP.Interactive is
    use Ada.Strings.Unbounded;
    use SP.Terminal;

    procedure Write_Prompt (Srch : in Search) is
        -- Writes the prompt and get ready to read user input.
        Default_Prompt : constant String  := " > ";
        Context_Width  : constant Natural := SP.Searches.Get_Context_Width (Srch);
    begin
        New_Line;
        Put ("Files: " & SP.Searches.Num_Files (Srch)'Image);
        Set_Col (20);
        Put ("Distance: " & (if Context_Width = SP.Searches.No_Context_Width then "Any" else Context_Width'Image));
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

    procedure Execute (Srch : in out SP.Searches.Search; Command_Line : String_Vectors.Vector) is
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
            Put_Line ("Unknown command: " & Command_Name);
        end if;
    end Execute;

    procedure Main is
        -- The interactive loop through which the user starts a search context and then interatively refines it by
        -- pushing and popping operations.
        Command_Line : String_Vectors.Vector;
        Srch         : SP.Searches.Search;
    begin
        Add_Directory (Srch, Ada.Directories.Current_Directory);

        loop
            Write_Prompt (Srch);
            Command_Line := Read_Command;
            Execute (Srch, Command_Line);
        end loop;
    end Main;
end SP.Interactive;
