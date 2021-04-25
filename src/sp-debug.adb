with Ada.Command_Line;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

package body SP.Debug is
    procedure Print_Command_Line is
        use Ada;
    begin
        Text_IO.Put_Line ("Command: " & Command_Line.Command_Name);
        Text_IO.Put_Line
            ("Arguments: " & Integer'Image (Command_Line.Argument_Count));
        for Arg_Idx in 1 .. Command_Line.Argument_Count loop

            Integer_Text_IO.Put (Arg_Idx);
            Text_IO.Set_Col (5);
            Text_IO.Put_Line (Command_Line.Argument (Arg_Idx));
        end loop;
    end Print_Command_Line;

end SP.Debug;
