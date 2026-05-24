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

with Ada.Strings.Unbounded;
with AnsiAda;

with Trendy_Terminal.Environments;
with Trendy_Terminal.IO;
with Trendy_Terminal.VT100;

package SP.Output is

    -- Functions for operations to the terminal. This hides the usage of Ada.Text_IO and may silently ignore
    -- capabilities if the terminal does not support them, such as if coloring text or line clearing is added.
    --
    -- This module also hides dependencies on unbounded IO.

    --  If the program is not interactive, then don't show spinners, etc.
    function Is_Interactive return Boolean;
    procedure Stop_Interactivity;

    -- Is the programming running as a tool for another program?
    --
    -- Pipelines don't output any user interface elements.
    function Is_Pipeline return Boolean;

    -- Some output is only for UI purposes.  Some is data which should always
    -- be shown.  Other output is important error messaging.
    --
    -- The semantics here are a bit different than the typical, Verbose, Info, Error
    -- Warning, etc. you'd see in a logging system.  This is WHY we want to
    -- print the thing.
    type Mode is (UI, Data, Error);

    -- New style output which is mode aware.
    procedure Put (Form : Mode; C : Character);
    procedure Put (Form : Mode; Str : String);
    procedure Put (Form : Mode; Str : Ada.Strings.Unbounded.Unbounded_String);

    procedure Put_Line (Form : Mode; Str : String);
    procedure Put_Line (Form : Mode; Str : Ada.Strings.Unbounded.Unbounded_String);
    procedure New_Line (Form : Mode);

    -- Formatting functions

    function Colorize (S : String; Color : AnsiAda.Colors) return String;
    function Colorize (US : Ada.Strings.Unbounded.Unbounded_String; Color : AnsiAda.Colors)
        return Ada.Strings.Unbounded.Unbounded_String;

    procedure Set_Col (Spacing : Positive) renames Trendy_Terminal.IO.Set_Col;

    -- User interface controls

    procedure Show_Cursor;
    procedure Hide_Cursor;

    procedure Beginning_Of_Line renames Trendy_Terminal.VT100.Beginning_Of_Line;
    procedure Clear_Line renames Trendy_Terminal.VT100.Clear_Line;

    -- Old-style printing

    procedure Put (C : Character) renames Trendy_Terminal.IO.Put;
    procedure Put (Str : String) renames Trendy_Terminal.IO.Put;
    procedure Put (Str : Ada.Strings.Unbounded.Unbounded_String) renames Trendy_Terminal.IO.Put;

    procedure Put_Line (Str : String) renames Trendy_Terminal.IO.Put_Line;
    procedure Put_Line (Str : Ada.Strings.Unbounded.Unbounded_String) renames Trendy_Terminal.IO.Put_Line;

    procedure New_Line (Spacing : Positive := 1) renames Trendy_Terminal.IO.New_Line;

    -- JSON

    procedure Start_Pipeline_Result;
    procedure Put_JSON_Key_Value (Key : String; Value : String);
    procedure Put_JSON_Key_Value (Key : String; Value : Ada.Strings.Unbounded.Unbounded_String);
    procedure Put_JSON_String (S : String);

    -- Cancellation

    protected type Cancellation_Gate is
        entry Closed;
        procedure Finish;
        procedure Cancel;
        function Is_Cancelled return Boolean;
        function Is_Finished return Boolean;
    private
        Cancelled : Boolean := False;
        Finished  : Boolean := False;
    end Cancellation_Gate;

    task type Terminal_Cancellation_Monitor(Gate : not null access Cancellation_Gate) is
        entry Cancel;
        entry Stop;
    end;

private
    Environment : Trendy_Terminal.Environments.Environment;

end SP.Output;
