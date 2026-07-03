with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;
with AnsiAda;
with SP.Output;

package body SP.Help is

    function Colorize_Command (Command_Name : String) return String is
    begin
        return "|" & SP.Output.Colorize (Command_Name, AnsiAda.Green) & "|";
    end Colorize_Command;

    procedure Header (Command_Name : String; Simple_Help : String := "") is
    begin
        New_Line;
        Put_Line ("-------------------------------------------------------");
        Put_Line (Colorize_Command (Command_Name));
        Put_Line ("-------------------------------------------------------");

        if Simple_Help'Length > 0 then
            Put_Line (Simple_Help);
        end if;
        New_Line;
    end Header;

    procedure Block (Contents : String; Extra_Space : Boolean := True) is
        Width : constant := 80;
        Cursor : Positive := Contents'First;
        Last_In_Line : Positive;

        -- Terminates lines early to avoid overfilling a line past the limit.
        function Last_Space return Natural is
        begin
            if Cursor + Width >= Contents'Last then
                return Contents'Last;
            end if;

            return Ada.Strings.Fixed.Index (
                Source => Contents,
                Set => Ada.Strings.Maps.To_Set (Ada.Strings.Space),
                From => Positive'Min (Cursor + Width, Contents'Last), -- tries to fill the entire line.
                Test => Ada.Strings.Inside,
                Going => Ada.Strings.Backward);
        end Last_Space;

        function First_Non_Space return Natural is
        begin
            if Last_In_Line + 1 >= Contents'Last then
                return Last_In_Line + 1;
            end if;

            return Ada.Strings.Fixed.Index_Non_Blank (
                Source => Contents,
                From => Last_In_Line + 1,
                Going => Ada.Strings.Forward);
        end First_Non_Space;

    begin
        while Cursor <= Contents'Last loop
            Last_In_Line := Last_Space;
            Put_Line (Contents (Cursor .. Last_In_Line));
            Cursor := First_Non_Space;
        end loop;

        if Extra_Space then
            New_Line;
        end if;
    end Block;

    procedure Describe_Command (Command_Name : String; Options : String; Description : String) is
        Spacer : constant String (1 .. 18 - Command_Name'Length) := (others => ' ');
    begin
        Block (
            Colorize_Command (Command_Name)
            & Spacer
            & " " & Options
            & "   " & Description,
            Extra_Space => False
        );
    end Describe_Command;
end SP.Help;