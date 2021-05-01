with Ada.Containers;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with SP.Contexts; use SP.Contexts;
with SP.Debug;
with SP.Strings; use SP.Strings;

package body SP.Interactive is

    type Evaluate_Result is (Continue, Quit);
    -- Describes if the evaluation loop should continue.

    function Evaluate
        (Ctx : in out Context; Line : in Ada.Strings.Unbounded.Unbounded_String)
          return Evaluate_Result is
        use Ada.Containers;
        use type Ada.Strings.Unbounded.Unbounded_String;
        Whitespace     : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (" ");
        Sanitized_Line : constant Ada.Strings.Unbounded.Unbounded_String :=
                           Ada.Strings.Unbounded.Trim (Line, Whitespace, Whitespace);
        Words          : String_Vectors.Vector := Split (Sanitized_Line);
        Command        : constant Ada.Strings.Unbounded.Unbounded_String := (if Words.Length > 0 then Words.Element(1) else Ada.Strings.Unbounded.Null_Unbounded_String);
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
            Ctx.Remove_Extensions(Words);
        elsif Command = "context" then
            Ctx.Set_Context_Width(Words);
        else
            Ada.Text_IO.Put_Line
                ("Unknown command: " & Ada.Strings.Unbounded.To_String (Sanitized_Line));
            Ada.Strings.Unbounded.Text_IO.Put_Line (Ctx.Starting_Dir);
        end if;
        return Continue;
    end Evaluate;

    procedure Main
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
        Ctx.Add_Directory(Ada.Strings.Unbounded.To_String(Starting_Dir));

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
    end Main;

end SP.Interactive;
