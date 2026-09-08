with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Equal_Case_Insensitive;
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

    procedure Write_Help_File (Lines : in out SP.Strings.String_Vectors.Vector) is
    begin
        for Line of Lines loop
            ASU.Trim (Line, Ada.Strings.Both);
            Ada.Text_IO.Put_Line (ASU.To_String (Line));
        end loop;
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
                Write_Help_File (Lines);
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
