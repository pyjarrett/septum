with Ada.Strings.Maps;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with SP.Debug;

package body SP.Interactive is
    function Refresh(Ctx: in out Context; Starting_Dir : Ada.Strings.Unbounded.Unbounded_String) return Boolean is
        -- Refreshes the list of files stored in the context.
        use Ada.Directories;
        use Ada.Text_IO;
        Search     : Search_Type;
        Next_Entry : Directory_Entry_Type;
        Filter     : constant Filter_Type := (Ordinary_File | Directory => True, others => False);
    begin
        Ada.Directories.Start_Search(Search    => Search,
                                     Directory => Ada.Strings.Unbounded.To_String(Starting_Dir),
                                     Pattern   => "*",
                                     Filter    => Filter);
        while Ada.Directories.More_Entries(Search) loop
            Ada.Directories.Get_Next_Entry(Search, Next_Entry);
            if Ada.Directories.Simple_Name(Next_Entry) = "." or Ada.Directories.Simple_Name(Next_Entry) = ".." then
                null;
            else
                if Ada.Directories.Kind(Next_Entry) = Ordinary_File then
                    if Ctx.Files.Contains(Ada.Strings.Unbounded.To_Unbounded_String(Ada.Directories.Full_Name(Next_Entry))) then
                        Ctx.Files.Replace(Ada.Strings.Unbounded.To_Unbounded_String(Ada.Directories.Full_Name(Next_Entry)), Ada.Strings.Unbounded.To_Unbounded_String(""));
                    else
                        Ctx.Files.Insert(Ada.Strings.Unbounded.To_Unbounded_String(Full_Name(Next_Entry)), Ada.Strings.Unbounded.To_Unbounded_String(""));
                    end if;
                    Put_Line("Next File is: " & Ada.Directories.Full_Name(Next_Entry));
                end if;

                if Ada.Directories.Kind(Next_Entry) = Directory and then not Refresh(Ctx, Ada.Strings.Unbounded.To_Unbounded_String(Ada.Directories.Full_Name(Next_Entry))) then
                    return False;
                end if;
            end if;
        end loop;
        End_Search(Search);
        return True;
    exception
        when others =>
            Ada.Text_IO.Put_Line("Unknown Exception");
            return False;
    end Refresh;

    function List (Ctx: in Context) return Boolean is
    begin
        for Elem in Ctx.Files.Iterate loop
            Ada.Strings.Unbounded.Text_IO.Put_Line(File_Maps.Key(Elem));
        end loop;
        return True;
    end List;

    function Evaluate (Ctx : in out Context; Line : in Ada.Strings.Unbounded.Unbounded_String) return Evaluate_Result is
        Whitespace     : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (" ");
        Sanitized_Line : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Trim (Line, Whitespace, Whitespace);
        use type Ada.Strings.Unbounded.Unbounded_String;
    begin
        if Sanitized_Line = "exit" or Sanitized_Line = "quit" then
            return Quit;
        elsif Sanitized_Line = "refresh" then
            if not Refresh (Ctx, Ctx.Starting_Dir) then
                return Quit;
            end if;
        elsif Sanitized_Line = "list" then
            if not List (Ctx) then
                return Quit;
            end if;
        else
            Ada.Text_IO.Put_Line("Unknown command: " & Ada.Strings.Unbounded.To_String(Sanitized_Line));
            Ada.Strings.Unbounded.Text_IO.Put_Line (Ctx.Starting_Dir);
        end if;
        return Continue;
    end Evaluate;

    procedure Main
        (Starting_Dir : Ada.Strings.Unbounded.Unbounded_String :=
             Ada.Strings.Unbounded.To_Unbounded_String (Ada.Directories.Current_Directory)) is
        --  Called to start an interactive search at the given directory.
        Done      : Boolean := False;
        Next_Line : Ada.Strings.Unbounded.Unbounded_String;
        Ctx       : Context;
    begin
        --  Starts the search in the current directory.
        Ctx.Starting_Dir := Starting_Dir;

        --  Print the command line while we're debugging.
        if (SP.Debug.Enabled) then
            SP.Debug.Print_Command_Line;
        end if;

        -- The main REPL.
        while not Done loop
            Ada.Text_IO.Put_Line ("> ");
            Ada.Strings.Unbounded.Text_IO.Put_Line (Next_Line);
            Next_Line := Ada.Strings.Unbounded.Text_IO.Get_Line;

            case Evaluate (Ctx, Next_Line) is
                when Quit =>
                    Done := True;
                when Continue =>
                    null;
            end case;
        end loop;
    end Main;

end SP.Interactive;
