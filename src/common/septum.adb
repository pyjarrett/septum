-------------------------------------------------------------------------------
-- Copyright 2021, The Septum Developers (see AUTHORS file)

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with GNAT.Traceback.Symbolic;

with SP.Commands;
with SP.Config;
with SP.Interactive;
with SP.Searches;
with SP.Terminal;

procedure Septum is
    use Ada.Text_IO;

    type Commands is (Init, Help, Version, Run);

    procedure Print_Usage is
    begin
        Put_Line ("Usage:");
        Put_Line ("   septum                                    run interactive search mode");
        Put_Line ("   septum init                               creates config directory with default config");
        Put_Line ("   septum help                               print this usage information");
        Put_Line ("   septum version                            print program version");
        Put_Line ("   septum run [--script | --tool] [file]...  run command files");
    end Print_Usage;

    procedure Print_Version is
    begin
        Put_Line ("septum v" & SP.Version);
    end Print_Version;

    function Has_Num_Command_Arguments (N : Natural) return Boolean is
    begin
        if Ada.Command_Line.Argument_Count /= N + 1 then
            New_Line;
            Put_Line ("Incorrect number of arguments.");
            New_Line;
            Print_Usage;
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return False;
        end if;

        return True;
    end Has_Num_Command_Arguments;

    -- The idea of run is to be able to run searches and produce output for LLM
    -- tool calls.
    --
    -- For testing, it's useful to run septum as if it were running, but without
    -- interactive UI elements.
    --
    -- This results in two similar, but different modes.
    -- 1. Tool - outputs JSON output for consumption by LLMs, non-interactively.
    -- 2. Scripted - running "as-if" a human, but non-interactively.
    --
    -- Config files run like Scripting mode, except under the same interactivity
    -- setting of the parent call.
    procedure Execute_Run is
        Next_Arg : Natural := 2;
        Srch : SP.Searches.Search;
        Result : SP.Commands.Command_Result;
        use type SP.User;
        use type SP.Commands.Command_Result;

        function Next_Arg_Is (S : String) return Boolean is
        begin
            return Next_Arg <= Ada.Command_Line.Argument_Count
                and then Ada.Command_Line.Argument (Next_Arg) = S;
        end Next_Arg_Is;
    begin
        if Next_Arg_Is ("--tool") then
            Next_Arg := Next_Arg + 1;
            SP.Current_User := SP.Tool;
            SP.Terminal.Stop_Interactivity;
        end if;

        if Next_Arg_Is ("--script") then
            if SP.Current_User /= SP.Human then
                Ada.Text_IO.Put_Line ("Cannot set both tool and scripted mode.");
                Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
                return;
            end if;
            Next_Arg := Next_Arg + 1;
            SP.Current_User := SP.Script;
            SP.Terminal.Stop_Interactivity;
        end if;

        -- Cannot run as a user, so run as a tool by default for LLM usage.
        if SP.Current_User = SP.Human then
            SP.Current_User := SP.Tool;
        end if;

        if Next_Arg > Ada.Command_Line.Argument_Count then
            Ada.Text_IO.Put_Line ("Expected one or more source files to run.");
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return;
        end if;

        for Arg in Next_Arg .. Ada.Command_Line.Argument_Count loop
            Result := SP.Commands.Run_Commands_From_File (Srch, Ada.Command_Line.Argument (Arg));
        Ada.Command_Line.Set_Exit_Status
        ((if Result = SP.Commands.Command_Success
            then Ada.Command_Line.Success
            else Ada.Command_Line.Failure));
            if Result not in SP.Commands.Command_Success | SP.Commands.Command_Ignored then
                Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
                exit;
            end if;
        end loop;
    end Execute_Run;

    --  Default to printing help, so that if the command is unrecognized, the
    --  help gets printed.
    Command : Commands := Help;
begin
    if Ada.Command_Line.Argument_Count = 0 then
        if not SP.Interactive.Main then
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
        end if;
        return;
    end if;

    begin
        Command := Commands'Value (Ada.Command_Line.Argument (1));
    exception
        when Constraint_Error =>
            Put_Line ("Unrecognized command line arguments.");
            New_Line;
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
    end;

    case Command is
        -- Create a local configuration file in the current directory.
        when Init =>
            if Has_Num_Command_Arguments (0) then
                SP.Config.Create_Local_Config;
            end if;

        when Help =>
            if Has_Num_Command_Arguments (0) then
                Print_Usage;
            end if;

        when Version =>
            if Has_Num_Command_Arguments (0) then
                Print_Version;
            end if;

        when Run =>
            Execute_Run;
    end case;

exception
    when Err : others =>
        Put_Line (Ada.Exceptions.Exception_Information (Err));
        Put_Line ("Exception traceback: " & GNAT.Traceback.Symbolic.Symbolic_Traceback (Err));
end Septum;
