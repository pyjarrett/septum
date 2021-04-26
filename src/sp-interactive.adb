with Ada.Directories.Hierarchical_File_Names;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with SP.Debug;

package body SP.Interactive is
    function Is_Current_Or_Parent_Directory(Dir : Ada.Directories.Directory_Entry_Type) return Boolean is
        Name : constant String := Ada.Directories.Simple_Name(Dir);
    begin
        return Ada.Directories.Hierarchical_File_Names.Is_Parent_Directory_Name(Name) or else
            Ada.Directories.Hierarchical_File_Names.Is_Current_Directory_Name(Name);
    end Is_Current_Or_Parent_Directory;

    function Split (S : Ada.Strings.Unbounded.Unbounded_String) return String_Vectors.Vector is
        -- Splits an unbounded string on spaces.
        Start  : Positive := 1;
        Finish : Natural  := 0;
    begin
        return Result : String_Vectors.Vector do
            while Start <= Ada.Strings.Unbounded.Length (S) loop
                Ada.Strings.Unbounded.Find_Token
                    (Source => S,
                     Set => Ada.Strings.Maps.To_Set (" "),
                     From => Start,
                     Test   => Ada.Strings.Outside,
                     First => Start,
                     Last => Finish);
                String_Vectors.Append
                    (Container => Result,
                     New_Item  =>
                         Ada.Strings.Unbounded.To_Unbounded_String
                             (Ada.Strings.Unbounded.Slice (S, Start, Finish)));
                Start := Finish + 1;
            end loop;
        end return;
    end Split;

    function Read_Lines (File_Name : String; Result : out String_Vectors.Vector) return Boolean is
        -- Reads all the lines from a file.
        File : Ada.Text_IO.File_Type;
        Line : Ada.Strings.Unbounded.Unbounded_String;
    begin
        String_Vectors.Clear(Result);
        Ada.Text_IO.Open(File => File,
                         Mode => Ada.Text_IO.In_File,
                         Name => File_Name);
        while not Ada.Text_IO.End_Of_File(File) loop
            Line := Ada.Strings.Unbounded.Text_IO.Get_Line(File);
            Result.Append(Line);
        end loop;

        Ada.Text_IO.Close(File);
        return True;
    exception
        when Ada.Text_IO.End_Error =>
            if Ada.Text_IO.Is_Open(File) then
                Ada.Text_IO.Close(File);
            end if;
            return True;
        when others =>
            Ada.Text_IO.Put_Line("Unable to read contents of: " & File_Name);
            return False;
    end Read_Lines;

    function Refresh
        (Ctx : in out Context; Starting_Dir : Ada.Strings.Unbounded.Unbounded_String)
          return Boolean is
        --  Refreshes the list of files stored in the context.
        use Ada.Directories;
        use Ada.Text_IO;
        Search     : Search_Type;
        Next_Entry : Directory_Entry_Type;
        Filter     : constant Filter_Type := (Ordinary_File | Directory => True, others => False);
    begin
        Ada.Directories.Start_Search
            (Search  => Search, Directory => Ada.Strings.Unbounded.To_String (Starting_Dir),
             Pattern => "*", Filter => Filter);
        while Ada.Directories.More_Entries (Search) loop
            Ada.Directories.Get_Next_Entry (Search, Next_Entry);
            if Is_Current_Or_Parent_Directory(Next_Entry) then
                null;
            else
                if Ada.Directories.Kind (Next_Entry) = Ordinary_File then
                    declare
                        Lines : String_Vectors.Vector := String_Vectors.Empty_Vector;
                    begin
                        if Read_Lines(Ada.Directories.Full_Name(Next_Entry), Lines) then
                            if Ctx.Files.Contains
                                (Ada.Strings.Unbounded.To_Unbounded_String
                                     (Ada.Directories.Full_Name (Next_Entry)))
                            then
                                Ctx.Files.Replace
                                    (Ada.Strings.Unbounded.To_Unbounded_String
                                         (Ada.Directories.Full_Name (Next_Entry)),
                                     Lines);
                            else
                                Ctx.Files.Insert
                                    (Ada.Strings.Unbounded.To_Unbounded_String (Full_Name (Next_Entry)),
                                     Lines);
                            end if;
                            Put_Line ("Next File is: " & Ada.Directories.Full_Name (Next_Entry));
                        else
                            return False;
                        end if;
                    end;
                end if;

                if Ada.Directories.Kind (Next_Entry) = Directory
                    and then not Refresh
                        (Ctx,
                         Ada.Strings.Unbounded.To_Unbounded_String
                             (Ada.Directories.Full_Name (Next_Entry)))
                then
                    return False;
                end if;
            end if;
        end loop;
        End_Search (Search);
        return True;
    exception
        when others =>
            Ada.Text_IO.Put_Line ("Unknown Exception");
            return False;
    end Refresh;

    function List (Ctx : in Context) return Boolean is
    begin
        for Elem in Ctx.Files.Iterate loop
            Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String(File_Maps.Key (Elem)) & " : " & Integer'Image(Integer(File_Maps.Element(Elem).Length)));
        end loop;
        return True;
    end List;

    function Ext (Ctx : in out Context; Extensions : in String_Vectors.Vector) return Boolean is
    begin
        for Ext of Extensions loop
            if not Ctx.Extensions.Contains(Ext) then
                Ctx.Extensions.Append(Ext);
            end if;
        end loop;

        Ada.Text_IO.Put_Line("Extensions:");
        for Ext of Ctx.Extensions loop
            Ada.Strings.Unbounded.Text_IO.Put_Line(Ext);
        end loop;
        return True;
    end Ext;


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
            if not Refresh (Ctx, Ctx.Starting_Dir) then
                return Quit;
            end if;
        elsif Command = "list" then
            if not List (Ctx) then
                return Quit;
            end if;
        elsif Command = "ext" then
            if not Ext (Ctx, Words) then
                return Quit;
            end if;
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
