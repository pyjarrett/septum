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

package body SP.Output is

    function Is_Interactive return Boolean is (SP.Current_User = SP.Human and then Environment.Is_Available);

    function Has_Colors return Boolean is (Environment.Is_Available);

    procedure Stop_Interactivity is
    begin
        Environment.Shutdown;
    end Stop_Interactivity;

    -- A transition has been started but not completed to move to multiple-types of output to separate it
    -- for scripts or other tooling.
    function Should_Show (Form : Mode) return Boolean is (
        case Form is
            when UI => True,
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

end SP.Output;
