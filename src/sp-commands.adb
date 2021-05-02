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

    ----------------------------------------------------------------------------

    procedure Help_Help is
    begin
        Put_Line ("Help help");
    end Help_Help;

    procedure Help_Exec (Srch : in out SP.Contexts.Search; Command_Line : String_Vectors.Vector) is
    begin
        pragma Unreferenced (Srch, Command_Line);
        Put_Line ("Help");
    end Help_Exec;

    ----------------------------------------------------------------------------

    procedure Reload_Help is
    begin
        Put_Line ("Reload help");
    end Reload_Help;

    procedure Reload_Exec (Srch : in out SP.Contexts.Search; Command_Line : String_Vectors.Vector) is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Refresh should have an empty command line.");
        end if;
        SP.Contexts.Reload_Working_Set (Srch);
    end Reload_Exec;

    ----------------------------------------------------------------------------

    procedure Add_Dirs_Help is
    begin
        Put_Line ("Adds a directory to the search list.");
    end Add_Dirs_Help;

    procedure Add_Dirs_Exec (Srch : in out SP.Contexts.Search; Command_Line : String_Vectors.Vector) is
    begin
        if Command_Line.Is_Empty then
            Put_Line ("Must provide directories to add to the search path.");
            return;
        end if;

        for Directory of Command_Line loop
            SP.Contexts.Add_Directory (Srch, To_String (Directory));
        end loop;
    end Add_Dirs_Exec;

    ----------------------------------------------------------------------------

    procedure List_Dirs_Help is
    begin
        Put_Line ("List the directories of the search list.");
    end List_Dirs_Help;

    procedure List_Dirs_Exec (Srch : in out SP.Contexts.Search; Command_Line : in String_Vectors.Vector) is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("No arguments are allowed for directory listing.");
        end if;
        for Directory of SP.Contexts.Search_Directories (Srch) loop
            Put_Line (To_String (Directory));
        end loop;
    end List_Dirs_Exec;

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
    Command_Map.Insert (To_Unbounded_String ("reload"), (Reload_Help'Access, Reload_Exec'Access));
    Command_Map.Insert (To_Unbounded_String ("add-dirs"), (Add_Dirs_Help'Access, Add_Dirs_Exec'Access));
    Command_Map.Insert (To_Unbounded_String ("list-dirs"), (List_Dirs_Help'Access, List_Dirs_Exec'Access));
end SP.Commands;
