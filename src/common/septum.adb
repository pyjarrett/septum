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

procedure Septum is
    use Ada.Text_IO;

    type Commands is (Init, Help, Version, Run);

    procedure Print_Usage is
    begin
        Put_Line ("Usage:");
        Put_Line ("       septum                  run interactive search mode");
        Put_Line ("       septum init             creates config directory with default config");
        Put_Line ("       septum help             print this usage information");
        Put_Line ("       septum version          print program version");
        Put_Line ("       septum run [file]...    run command files");
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

    --  Default to printing help, so that if the command is unrecognized, the
    --  help gets printed.
    Command : Commands := Help;
begin
    if Ada.Command_Line.Argument_Count = 0 then
        SP.Interactive.Main;
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
            if Ada.Command_Line.Argument_Count < 2 then
                Ada.Text_IO.Put_Line ("Expected one or more source files to run.");
                Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
                return;
            end if;

            declare
                Srch : SP.Searches.Search;
                Result : SP.Commands.Command_Result;
                use type SP.Commands.Command_Result;
            begin
                for Arg in 2 .. Ada.Command_Line.Argument_Count loop
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
            end;
    end case;

exception
    when Err : others =>
        Put_Line (Ada.Exceptions.Exception_Information (Err));
        Put_Line ("Exception traceback: " & GNAT.Traceback.Symbolic.Symbolic_Traceback (Err));
end Septum;
