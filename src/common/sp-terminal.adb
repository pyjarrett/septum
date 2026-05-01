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

with Trendy_Terminal.Platform;
with Trendy_Terminal.Maps;

package body SP.Terminal is

    function Colorize (S : String; Color : AnsiAda.Colors) return String is
    begin
        return AnsiAda.Foreground (Color)
            & S
            & AnsiAda.Foreground (AnsiAda.Default);
    end Colorize;

    function Colorize (US : Ada.Strings.Unbounded.Unbounded_String; Color : AnsiAda.Colors)
        return Ada.Strings.Unbounded.Unbounded_String
    is
        use all type Ada.Strings.Unbounded.Unbounded_String;
    begin
        return AnsiAda.Foreground (Color)
            & US
            & AnsiAda.Foreground (AnsiAda.Default);
    end Colorize;

    protected body Cancellation_Gate is
        entry Closed when Finished is
        begin
            null;
        end Closed;

        procedure Cancel is
        begin
            Cancelled := True;
        end Cancel;

        function Is_Cancelled return Boolean is
        begin
            return Cancelled;
        end Is_Cancelled;

        procedure Finish is
        begin
            Finished := True;
        end Finish;

        function Is_Finished return Boolean is
        begin
            return Finished;
        end Is_Finished;
    end Cancellation_Gate;

    task body Terminal_Cancellation_Monitor is
        task Input_Thread is end;

        task body Input_Thread is
            use all type Trendy_Terminal.Maps.Key;
        begin
            loop
                declare
                    Input : constant String := Trendy_Terminal.Platform.Get_Input;
                begin
                    if Trendy_Terminal.Maps.Key_For (Input) = Trendy_Terminal.Maps.Key_Ctrl_C then
                        select
                            Cancel;
                            exit;
                        else
                            null;
                        end select;
                    elsif Trendy_Terminal.Maps.Key_For (Input) = Trendy_Terminal.Maps.Key_Ctrl_D then
                        exit;
                    else
                        null;
                    end if;
                end;
            end loop;
        end Input_Thread;

        Done : Boolean := False;
    begin
        loop
            select
                accept Cancel do
                    Done := True;
                    Gate.Cancel;
                end;
            or
                accept Stop do
                    Done := True;
                end;
            or
                terminate;
            end select;

            exit when Done;
        end loop;

        abort Input_Thread;
    end Terminal_Cancellation_Monitor;

end SP.Terminal;