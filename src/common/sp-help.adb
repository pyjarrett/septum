with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with AnsiAda;
with SP.Output; use SP.Output;

package body SP.Help is
    function Colorize_Command (Command_Name : String) return String is
    begin
        return "|" & SP.Output.Colorize (Command_Name, AnsiAda.Green) & "|";
    end Colorize_Command;

    procedure Header (Command_Name : String; Simple_Help : String) is
    begin
        New_Line;
        Put_Line ("-------------------------------------------------------");
        Put_Line (Colorize_Command (Command_Name));
        Put_Line ("-------------------------------------------------------");
        Put_Line (Simple_Help);
        New_Line;
    end Header;

    procedure Block (Contents : String) is
        Width        : constant := 80;
        Cursor       : Positive := Contents'First;
        Last_In_Line : Positive;

        -- Terminates lines early to avoid overfilling a line past the limit.
        function Last_Space return Natural is
        begin
            if Cursor + Width >= Contents'Last then
                return Contents'Last;
            end if;

            return
               Ada.Strings.Fixed.Index
                  (Source => Contents,
                   Set    => Ada.Strings.Maps.To_Set (Ada.Strings.Space),
                   From   => Positive'Min (Cursor + Width, Contents'Last), -- tries to fill the entire line.
                   Test   => Ada.Strings.Inside,
                   Going  => Ada.Strings.Backward);
        end Last_Space;

        function First_Non_Space return Natural is
        begin
            if Last_In_Line + 1 >= Contents'Last then
                return Last_In_Line + 1;
            end if;

            return
               Ada.Strings.Fixed.Index_Non_Blank
                  (Source => Contents, From => Last_In_Line + 1, Going => Ada.Strings.Forward);
        end First_Non_Space;

    begin
        while Cursor <= Contents'Last loop
            Last_In_Line := Last_Space;
            Output.Put_Line (Contents (Cursor .. Last_In_Line));
            Cursor := First_Non_Space;
        end loop;
        New_Line;
    end Block;

    procedure Example (Contents : Strings.String_Vectors.Vector) is
    begin
        New_Line;
        for V of Contents loop
            Output.Put_Line (Output.Colorize (V, AnsiAda.Yellow));
        end loop;
        New_Line;
    end Example;
end SP.Help;
