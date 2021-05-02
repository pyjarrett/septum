with Ada.Containers.Ordered_Maps;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with SP.Strings; use SP.Strings;
with SP.Contexts; use SP.Contexts;
with SP.Debug;

package body SP.Interactive is
    use Ada.Strings.Unbounded;
    use Ada.Strings.Unbounded.Text_IO;
    use Ada.Text_IO;

    type Executable_Command is record
        Help_Proc : not null access procedure;
        Exec_Proc : not null access procedure (Command_Line : String_Vectors.Vector);
    end record;

    package Command_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Unbounded_String, Element_Type => Executable_Command);

    Command_Map    : Command_Maps.Map;
    Quit_Commands  : String_Sets.Set;
    Default_Prompt : constant String := "> ";

    package Help is
        procedure Help;
        procedure Exec (Command_Line : String_Vectors.Vector);
    end Help;

    package body Help is
        procedure Help is
        begin
            Put_Line ("Help help");
        end Help;

        procedure Exec (Command_Line : String_Vectors.Vector) is
        begin
            pragma Unreferenced (Command_Line);
            Put_Line ("Help");
        end Exec;
    end Help;

    procedure Build_Command_Map is
    begin
        Quit_Commands.Insert (To_Unbounded_String ("quit"));
        Quit_Commands.Insert (To_Unbounded_String ("exit"));

        Command_Map.Insert (To_Unbounded_String ("help"), (Help.Help'Access, Help.Exec'Access));
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

    function Execute (Command_Line : String_Vectors.Vector) return Boolean is
        use Ada.Containers;
        Parameters   : constant String_Vectors.Vector := Command_Line;
        Command_Name : constant Unbounded_String      :=
            (if Parameters.Is_Empty then Null_Unbounded_String else Parameters.First_Element);
    begin
        if Command_Line.Is_Empty then
            return True;
        elsif Command_Line.Length = 1 and then Quit_Commands.Contains (Command_Name) then
            return False;
        elsif Command_Map.Contains (Command_Name) then
            declare
                It      : constant Command_Maps.Cursor := Command_Map.Find (Command_Name);
                Command : constant Executable_Command  := Command_Maps.Element (It);
            begin
                Command.Exec_Proc.all (Parameters);
            end;
        end if;
        return True;
    end Execute;

    procedure Main is
        -- The interactive loop through which the user starts a search context and then interatively refines it by
        -- pushing and popping operations.
        Command_Line : String_Vectors.Vector := Read_Prompt (Default_Prompt);
    begin
        Build_Command_Map;
        while Execute (Command_Line) loop
            Command_Line := Read_Prompt (Default_Prompt);
        end loop;
    end Main;

    type Evaluate_Result is (Continue, Quit); -- Describes if the evaluation loop should continue.

    function Evaluate (Ctx : in out Context; Line : in Ada.Strings.Unbounded.Unbounded_String) return Evaluate_Result is
        use Ada.Containers;
        use type Ada.Strings.Unbounded.Unbounded_String;
        Whitespace     : constant Ada.Strings.Maps.Character_Set         := Ada.Strings.Maps.To_Set (" ");
        Sanitized_Line : constant Ada.Strings.Unbounded.Unbounded_String :=
            Ada.Strings.Unbounded.Trim (Line, Whitespace, Whitespace);
        Words   : String_Vectors.Vector                           := Split (Sanitized_Line);
        Command : constant Ada.Strings.Unbounded.Unbounded_String :=
            (if Words.Length > 0 then Words.Element (1) else Ada.Strings.Unbounded.Null_Unbounded_String);
    begin
        if Words.Length > 0 then
            Words.Delete_First;
        end if;
        if Command = "exit" or Sanitized_Line = "quit" then
            return Quit;
        elsif Command = "refresh" then
            Ctx.Refresh (Ctx.Starting_Dir);
        elsif Command = "list" then
            Ctx.List;
        elsif Command = "ext" then
            Ctx.Add_Extensions (Words);
        elsif Command = "remove-ext" then
            Ctx.Remove_Extensions (Words);
        elsif Command = "context" then
            Ctx.Set_Context_Width (Words);
        else
            Ada.Text_IO.Put_Line ("Unknown command: " & Ada.Strings.Unbounded.To_String (Sanitized_Line));
            Ada.Strings.Unbounded.Text_IO.Put_Line (Ctx.Starting_Dir);
        end if;
        return Continue;
    end Evaluate;

    procedure Old_Main
        (Starting_Dir : Ada.Strings.Unbounded.Unbounded_String :=
             Ada.Strings.Unbounded.To_Unbounded_String (Ada.Directories.Current_Directory)) is
        -- Entry point for program execution.
        --
        -- Starts a new empty search
        --
        --  Called to start an interactive search at the given directory.
        Done      : Boolean := False;
        Next_Line : Ada.Strings.Unbounded.Unbounded_String;
        Ctx       : Context;
    begin
        --  Starts the search in the current directory.
        Ctx.Add_Directory (Ada.Strings.Unbounded.To_String (Starting_Dir));

        --  Print the command line while we're debugging.
        if (SP.Debug.Enabled) then
            SP.Debug.Print_Command_Line;
        end if;

        -- The main REPL.
        while not Done loop
            Ada.Text_IO.Put ("> ");
            Next_Line := Ada.Strings.Unbounded.Text_IO.Get_Line;
            Ada.Strings.Unbounded.Text_IO.Put_Line (Next_Line);

            case Evaluate (Ctx, Next_Line) is
                when Quit =>
                    Done := True;
                when Continue =>
                    null;
            end case;
        end loop;
    end Old_Main;

end SP.Interactive;
