with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SP.Command_Line;
with SP.File_System;
with SP.Strings;

procedure Make_Septum_Help is
    Command_Line_Parser_Error : exception;
    Cannot_Read_File          : exception;

    type Make_Help_Config is record
        Files      : SP.Strings.String_Vectors.Vector;
        Output_Dir : Ada.Strings.Unbounded.Unbounded_String;
    end record;

    package ASU renames Ada.Strings.Unbounded;

    package Help_Printing is
        --  Provides a common way of converting markdown-esque format into in-program
        --  help or another form.
        type Help_Printer is limited interface;
        use type Ada.Strings.Unbounded.Unbounded_String;

        procedure Start (Self : in out Help_Printer) is abstract;
        procedure Finish (Self : in out Help_Printer) is abstract;
        procedure Print_Title (Self : in out Help_Printer; Title : String; Heading_Level : Positive) is abstract;
        procedure Print_Content (Self : in out Help_Printer; Content : String) is abstract;
        procedure Print_line (Self : in out Help_Printer) is abstract;
        procedure Print_Literal (Self : in out Help_Printer; Content : String) is abstract;

        type Ada_Help_Printer is limited new Help_Printer with record
            File            : aliased Ada.Text_IO.File_Type;
            Target          : Ada.Text_IO.File_Access := Ada.Text_IO.Standard_Output;
            Current_Block   : Ada.Strings.Unbounded.Unbounded_String;
            Current_Section : Ada.Strings.Unbounded.Unbounded_String;
        end record;

        procedure Open (Self : in out Ada_Help_Printer; Path : String);

        overriding
        procedure Start (Self : in out Ada_Help_Printer);

        overriding
        procedure Finish (Self : in out Ada_Help_Printer);

        overriding
        procedure Print_Title (Self : in out Ada_Help_Printer; Title : String; Heading_Level : Positive);

        overriding
        procedure Print_Content (Self : in out Ada_Help_Printer; Content : String);

        overriding
        procedure Print_Line (Self : in out Ada_Help_Printer);

        overriding
        procedure Print_Literal (Self : in out Ada_Help_Printer; Content : String);

        procedure Print_Block (Self : in out Ada_Help_Printer)
        with Pre => In_Block (Self), Post => not In_Block (Self);

        function In_Section (Self : in out Ada_Help_Printer) return Boolean
        is (Self.Current_Section /= Ada.Strings.Unbounded.Null_Unbounded_String);

        function In_Block (Self : in out Ada_Help_Printer) return Boolean
        is (Self.Current_Block /= Ada.Strings.Unbounded.Null_Unbounded_String);
    end Help_Printing;

    --  This really probably should be in a separate package, but I don't want to take the time or effort to split it
    --  out, and I just want this done.
    package body Help_Printing is
        procedure Open (Self : in out Ada_Help_Printer; Path : String) is
        begin
            if Ada.Text_IO.Is_Open (Self.File) then
                raise Program_Error with "Help printer file is already open";
            end if;

            Ada.Text_IO.Create (Self.File, Ada.Text_IO.Out_File, Path);
        end Open;

        function Title_Sanitize (Title : String) return String is
        begin
            return Result : String := Title do
                for X in Result'Range loop
                    if (Result (X) = ' ') then
                        Result (X) := '_';
                    end if;
                end loop;
            end return;
        end Title_Sanitize;

        function Text_Sanitize (Title : String) return String is
            Temp : ASU.Unbounded_String;
        begin
            for X of Title loop
                ASU.Append (Temp, X);
                if X = '"' then
                    ASU.Append (Temp, X);
                end if;
            end loop;
            return ASU.To_String (Temp);
        end Text_Sanitize;

        procedure Start_Section (Self : in out Ada_Help_Printer) is
        begin
            Ada.Text_IO.Put_Line (Self.Target.all, "   pragma Style_Checks(Off);");
            Ada.Text_IO.Put
               (Self.Target.all,
                "   procedure " & Title_Sanitize (Ada.Strings.Unbounded.To_String (Self.Current_Section)));
            Ada.Text_IO.Put_Line (Self.Target.all, " is");
            Ada.Text_IO.Put_Line (Self.Target.all, "   begin");
        end Start_Section;

        procedure End_Section (Self : in out Ada_Help_Printer) is
        begin
            if not In_Section (Self) then
                raise Program_Error with "No current section.";
            end if;

            Ada.Text_IO.Put_Line
               (Self.Target.all,
                "   end " & Title_Sanitize (Ada.Strings.Unbounded.To_String (Self.Current_Section)) & ";");
            Ada.Text_IO.Put_Line (Self.Target.all, "   pragma Style_Checks(On);");
            Ada.Text_IO.New_Line (Self.Target.all);
            Self.Current_Section := Ada.Strings.Unbounded.Null_Unbounded_String;
        end End_Section;

        overriding
        procedure Start (Self : in out Ada_Help_Printer) is
        begin
            if Ada.Text_IO.Is_Open (Self.File) then
                Self.Target := Self.File'Unchecked_Access;
            end if;
            Ada.Text_IO.Put_Line (Self.Target.all, "with SP.Help;");
            Ada.Text_IO.Put_Line (Self.Target.all, "package body SP.Help_Topics is");
        end Start;

        overriding
        procedure Finish (Self : in out Ada_Help_Printer) is
        begin
            if In_Section (Self) then
                End_Section (Self);
            end if;
            Ada.Text_IO.Put_Line (Self.Target.all, "end SP.Help_Topics;");

            if Ada.Text_IO.Is_Open (Self.File) then
                Ada.Text_IO.Close (Self.File);
                Ada.Text_IO.Put_Line ("Closed output file");
            end if;
        end Finish;

        overriding
        procedure Print_Title (Self : in out Ada_Help_Printer; Title : String; Heading_Level : Positive) is
        begin
            if In_Block (Self) then
                Print_Block (Self);
            end if;

            if In_Section (Self) then
                End_Section (Self);
            end if;
            Self.Current_Section := Ada.Strings.Unbounded.To_Unbounded_String (Title);
            Start_Section (Self);
        end Print_Title;

        overriding
        procedure Print_Content (Self : in out Ada_Help_Printer; Content : String) is
        begin
            Ada.Strings.Unbounded.Append (Self.Current_Block, Content & " ");
            Ada.Strings.Unbounded.Append (Self.Current_Block, " ");
        end Print_Content;

        overriding
        procedure Print_Line (Self : in out Ada_Help_Printer) is
        begin
            if In_Block (Self) then
                Print_Block (Self);
            else
                Ada.Text_IO.Put_Line (Self.Target.all, "      SP.Help.Plain("""");");
            end if;
            Ada.Text_IO.New_Line (Self.Target.all);
        end Print_Line;

        overriding
        procedure Print_Literal (Self : in out Ada_Help_Printer; Content : String) is
        begin
            Ada.Text_IO.Put (Self.Target.all, "      SP.Help.Plain(""");
            Ada.Text_IO.Put (Self.Target.all, Text_Sanitize (Content));
            Ada.Text_IO.Put_Line (Self.Target.all, """);");
        end Print_Literal;

        procedure Print_Block (Self : in out Ada_Help_Printer) is
        begin
            Ada.Text_IO.Put (Self.Target.all, "      SP.Help.Block(""");
            Ada.Text_IO.Put (Self.Target.all, Text_Sanitize (Ada.Strings.Unbounded.To_String (Self.Current_Block)));
            Ada.Text_IO.Put_Line (Self.Target.all, """);");
            Self.Current_Block := Ada.Strings.Unbounded.Null_Unbounded_String;
        end Print_Block;
    end Help_Printing;

    function Parse_Command_Line return Make_Help_Config is
        package AD renames Ada.Directories;
        package CL renames SP.Command_Line;
        Parser : CL.Command_Line_Parser;
    begin
        return Result : Make_Help_Config do
            while CL.Has_More_Arguments (Parser) loop
                --  The only argument right now is an output directory.
                if CL.Is_Flag (Parser) then
                    if CL.Try_Match (Parser, "--output") then
                        if CL.Has_More_Arguments (Parser) and then not CL.Is_Flag (Parser) then
                            if not Ada.Directories.Exists (CL.Peek_Argument (Parser)) then
                                raise Command_Line_Parser_Error
                                   with "Output directory does not exist: " & CL.Peek_Argument (Parser);
                            end if;
                            Ada.Strings.Unbounded.Set_Unbounded_String (Result.Output_Dir, CL.Peek_Argument (Parser));
                            CL.Skip_Argument (Parser);
                        else
                            raise Command_Line_Parser_Error with "Expected a directory after --output";
                        end if;
                    else
                        raise Command_Line_Parser_Error with "Expected a directory after --output";
                    end if;
                else
                    declare
                        Next_Arg : constant String := CL.Peek_Argument (Parser);
                        use type Ada.Directories.File_Kind;
                    begin
                        --  A list of files or directories.
                        --  File argument
                        if not AD.Exists (Next_Arg) then
                            raise Command_Line_Parser_Error with "No file or directory exists at: " & Next_Arg;
                        end if;

                        if AD.Kind (Next_Arg) = AD.Directory then
                            Result.Files.Append (SP.File_System.Recursive_Contents (Next_Arg).Files);
                        elsif Ada.Strings.Equal_Case_Insensitive (AD.Extension (Next_Arg), "MD") then
                            Result.Files.Append (Ada.Strings.Unbounded.To_Unbounded_String (Next_Arg));
                        else
                            raise Command_Line_Parser_Error
                               with "Argument is not a Markdown file or directory: " & Next_Arg;
                        end if;
                        Result.Files.Append (Ada.Strings.Unbounded.To_Unbounded_String (Next_Arg));
                        CL.Skip_Argument (Parser);
                    end;
                end if;
            end loop;
        end return;
    end Parse_Command_Line;

    procedure Write_Help_File (Path : String; Lines : in out SP.Strings.String_Vectors.Vector) is
        Printer : Help_Printing.Ada_Help_Printer;

        function Leading_Hash_Count (Str : String) return Natural is
        begin
            return Count : Natural := 0 do
                for C of Str loop
                    exit when C /= '#';
                    Count := @ + 1;
                end loop;
            end return;
        end Leading_Hash_Count;
    begin
        Help_Printing.Open (Printer, Path);
        Help_Printing.Start (Printer);
        for Line of Lines loop
            ASU.Trim (Line, Ada.Strings.Right);
            declare
                Str   : constant String := ASU.To_String (Line);
                Level : constant Natural := Leading_Hash_Count (Str);
            begin
                if Level > 0 then
                    Help_Printing.Print_Title
                       (Printer, Ada.Strings.Fixed.Trim (Str (Str'First + Level .. Str'Last), Ada.Strings.Left), Level);
                elsif Ada.Strings.Fixed.Trim (Str, Ada.Strings.Left)'Length = 0 then
                    Help_Printing.Print_Line (Printer);
                elsif Str'Length > 4 and Str (Str'First .. Str'First + 3) = "    " then
                    Help_Printing.Print_Literal (Printer, Str);
                else
                    Help_Printing.Print_Content (Printer, Str);
                end if;
            end;
        end loop;
        Help_Printing.Finish (Printer);
    end Write_Help_File;

    Config : Make_Help_Config;
begin
    --  Parse the command line into a Make_Help_Config.
    Config := Parse_Command_Line;
    Ada.Text_IO.Put_Line (Config'Image);

    for File of Config.Files loop
        declare
            Lines : SP.Strings.String_Vectors.Vector;
        begin
            if SP.File_System.Read_Lines (ASU.To_String (File), Lines) then
                Write_Help_File
                   (Ada.Directories.Compose (ASU.To_String (Config.Output_Dir), "sp-help_topics.adb"), Lines);
            else
                raise Cannot_Read_File with "Cannot read file: " & ASU.To_String (File);
            end if;
        end;
    end loop;

exception
    when File : Cannot_Read_File =>
        Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, "File does not exist: " & Ada.Exceptions.Exception_Message (File));

    when Arg : Command_Line_Parser_Error =>
        Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, "Missing an argument: " & Ada.Exceptions.Exception_Message (Arg));
end Make_Septum_Help;
