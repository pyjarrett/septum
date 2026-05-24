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

with Ada.Characters.Latin_1;
with Trendy_Terminal.Platform;
with Trendy_Terminal.Maps;

package body SP.Output is

    function Is_Interactive return Boolean is (SP.Current_User = SP.Human and then Environment.Is_Available);
    function Is_Pipeline return Boolean is (SP.Current_User = SP.Tool);

    function Has_Colors return Boolean is (Environment.Is_Available);

    procedure Stop_Interactivity is
    begin
        Environment.Shutdown;
    end Stop_Interactivity;

    function Should_Show (Form : Mode) return Boolean is (
        case Form is
            when UI => SP.Current_User /= SP.Tool,
            when Data => True,
            when Error => True
    );

    -- New style output.

    procedure Put (Form : Mode; C : Character) is
    begin
        if Should_Show (Form) then
            Trendy_Terminal.IO.Put (C);
        end if;
    end Put;

    procedure Put (Form : Mode; Str : String)  is
    begin
        if Should_Show (Form) then
            Trendy_Terminal.IO.Put (Str);
        end if;
    end Put;

    procedure Put (Form : Mode; Str : Ada.Strings.Unbounded.Unbounded_String) is
    begin
        if Should_Show (Form) then
            Trendy_Terminal.IO.Put (Str);
        end if;
    end Put;

    procedure Put_Line (Form : Mode; Str : String) is
    begin
        if Should_Show (Form) then
            Trendy_Terminal.IO.Put_Line (Str);
        end if;
    end Put_Line;

    procedure Put_Line (Form : Mode; Str : Ada.Strings.Unbounded.Unbounded_String) is
    begin
        if Should_Show (Form) then
            Trendy_Terminal.IO.Put_Line (Str);
        end if;
    end Put_Line;

    procedure New_Line (Form : Mode) is
    begin
        if Should_Show (Form) then
            Trendy_Terminal.IO.New_Line;
        end if;
    end New_Line;

    -- Formatting functions

    function Colorize (S : String; Color : AnsiAda.Colors) return String is
    begin
        if not Has_Colors then
            return S;
        end if;

        return AnsiAda.Foreground (Color)
            & S
            & AnsiAda.Foreground (AnsiAda.Default);
    end Colorize;

    function Colorize (US : Ada.Strings.Unbounded.Unbounded_String; Color : AnsiAda.Colors)
        return Ada.Strings.Unbounded.Unbounded_String
    is
        use all type Ada.Strings.Unbounded.Unbounded_String;
    begin
        if not Has_Colors then
            return US;
        end if;

        return AnsiAda.Foreground (Color)
            & US
            & AnsiAda.Foreground (AnsiAda.Default);
    end Colorize;

    -- User interface controls

    procedure Show_Cursor is
    begin
        if SP.Output.Is_Interactive then
            Trendy_Terminal.VT100.Show_Cursor;
        end if;
    end Show_Cursor;

    procedure Hide_Cursor is
    begin
        if SP.Output.Is_Interactive then
            Trendy_Terminal.VT100.Hide_Cursor;
        end if;
    end Hide_Cursor;

    -- Old-style printing

    -- JSON

    -- Need to track if any pipeline results have been returned so commas can be
    -- added for each.
    Has_Pipeline_Result : Boolean := False;

    procedure Start_Pipeline_Result is
    begin
        if Has_Pipeline_Result then
            Put_Line (",");
        else
            Has_Pipeline_Result := True;
        end if;
    end Start_Pipeline_Result;

    procedure Put_JSON_Key_Value (Key : String; Value : String) is
    begin
        Put_JSON_String (Key);
        Put (": ");
        Put_JSON_String (Value);
    end Put_JSON_Key_Value;

    procedure Put_JSON_Key_Value (Key : String; Value : Ada.Strings.Unbounded.Unbounded_String) is
    begin
        Put_JSON_Key_Value (Key, Ada.Strings.Unbounded.To_String (Value));
    end Put_JSON_Key_Value;

    procedure Put_JSON_String (S : String) is
    begin
        Put ('"');
        for C of S loop
            if C in '\' | ''' | '"' | Ada.Characters.Latin_1.HT | Ada.Characters.Latin_1.LF | Ada.Characters.Latin_1.VT then
                Put ('\');
            end if;
            Put (C);
        end loop;
        Put ('"');
    end Put_JSON_String;

    -- Cancellation

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

end SP.Output;