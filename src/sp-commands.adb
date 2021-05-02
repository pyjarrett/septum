with Ada.Containers.Ordered_Maps;
with Ada.Text_IO;

package body SP.Commands is
    use Ada.Text_IO;

    type Executable_Command is record
        Help_Proc : not null access procedure;
        Exec_Proc : not null access procedure (Srch : in out SP.Contexts.Search; Command_Line : String_Vectors.Vector);
    end record;

    package Command_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Unbounded_String, Element_Type => Executable_Command);

    Command_Map : Command_Maps.Map;

    procedure Help_Help is
    begin
        Put_Line ("Help help");
    end Help_Help;

    procedure Help_Exec (Srch : in out SP.Contexts.Search; Command_Line : String_Vectors.Vector) is
    begin
        pragma Unreferenced (Srch, Command_Line);
        Put_Line ("Help");
    end Help_Exec;

    procedure Refresh_Help is
    begin
        Put_Line ("Refresh help");
    end Refresh_Help;

    procedure Refresh_Exec (Srch : in out SP.Contexts.Search; Command_Line : String_Vectors.Vector) is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Refresh should have an empty command line.");
        end if;
        SP.Contexts.Refresh(Srch);
    end Refresh_Exec;

    function Execute
        (Srch : in out SP.Contexts.Search; Command_Name : Unbounded_String; Parameters : String_Vectors.Vector)
         return Boolean is
    begin
        if Command_Map.Contains (Command_Name) then
            declare
                It      : constant Command_Maps.Cursor := Command_Map.Find (Command_Name);
                Command : constant Executable_Command  := Command_Maps.Element (It);
            begin
                Command.Exec_Proc.all (Srch, Parameters);
                return True;
            end;
        end if;
        return False;
    end Execute;

begin
    Command_Map.Insert (To_Unbounded_String ("help"), (Help_Help'Access, Help_Exec'Access));
    Command_Map.Insert (To_Unbounded_String ("refresh"), (Refresh_Help'Access, Refresh_Exec'Access));
end SP.Commands;
