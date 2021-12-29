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

    function Colorize (S : String; Color : ANSI.Colors) return String is
    begin
        return ANSI.Foreground (Color)
            & S
            & ANSI.Foreground (ANSI.Default);
    end Colorize;

    function Colorize (US : Ada.Strings.Unbounded.Unbounded_String; Color : ANSI.Colors)
        return Ada.Strings.Unbounded.Unbounded_String
    is
        use all type Ada.Strings.Unbounded.Unbounded_String;
    begin
        return ANSI.Foreground (Color)
            & US
            & ANSI.Foreground (ANSI.Default);
    end Colorize;

    protected body Cancellation_Gate is
        procedure Cancel is
        begin
            Cancelled := True;
        end Cancel;

        function Is_Cancelled return Boolean is
        begin
            return Cancelled;
        end Is_Cancelled;
    end Cancellation_Gate;

    task body Terminal_Cancellation_Monitor is
    begin
        loop
            select
                delay 0.2;
            then abort
                declare
                    Input : constant String := Trendy_Terminal.Platform.Get_Input;
                    use all type Trendy_Terminal.Maps.Key;
                begin
                    if Trendy_Terminal.Maps.Key_For (Input) = Trendy_Terminal.Maps.Key_Ctrl_C then
                        Put_Line ("Gate cancelled");
                        Gate.Cancel;
                        exit;
                    end if;
                end;
            end select;
        end loop;
    end Terminal_Cancellation_Monitor;

end SP.Terminal;