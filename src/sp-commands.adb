with Ada.Containers.Ordered_Maps;
with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;

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
        use Command_Maps;
        use Ada.Strings.Unbounded.Text_IO;
    begin
        for Cursor in Command_Map.Iterate loop
            Put_Line (Key(Cursor));
        end loop;
    end Help_Help;

    procedure Help_Exec (Srch : in out SP.Contexts.Search; Command_Line : String_Vectors.Vector) is
    begin
        pragma Unreferenced (Srch, Command_Line);
        Help_Help;
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

    ----------------------------------------------------------------------------

    procedure Find_Text_Help is
    begin
        Put_Line ("Provides text to search for.");
    end Find_Text_Help;

    procedure Find_Text_Exec (Srch : in out SP.Contexts.Search; Command_Line : in String_Vectors.Vector) is
    begin
        for Word of Command_Line loop
            SP.Contexts.Push (Srch, To_String (Word));
        end loop;
    end Find_Text_Exec;

    ----------------------------------------------------------------------------

    procedure List_Text_Help is
    begin
        Put_Line ("List text to search for within the context size.");
    end List_Text_Help;

    procedure List_Text_Exec (Srch : in out SP.Contexts.Search; Command_Line : in String_Vectors.Vector) is
        Filter_Names : constant String_Vectors.Vector := SP.Contexts.Get_Filter_Names (Srch);
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Ignoring unnecessary command line parameters.");
        end if;
        for Name of Filter_Names loop
            Put_Line (To_String (Name));
        end loop;
    end List_Text_Exec;

    ----------------------------------------------------------------------------

    procedure Pop_Help is
    begin
        Put_Line ("Pops the last applied filter from the search.");
    end Pop_Help;

    procedure Pop_Exec (Srch : in out SP.Contexts.Search; Command_Line : in String_Vectors.Vector) is
    begin
        if not Command_Line.Is_Empty then
            Put_Line ("Ignoring unnecessary command line parameters.");
        end if;
        SP.Contexts.Pop(Srch);
    end Pop_Exec;

    ----------------------------------------------------------------------------

    procedure Matching_Files_Help is
    begin
        Put_Line ("Lists the files currently matching all filters.");
    end Matching_Files_Help;

    procedure Matching_Files_Exec (Srch : in out SP.Contexts.Search; Command_Line : in String_Vectors.Vector) is
        File_Names : String_Vectors.Vector := SP.Contexts.Matching_Files (Srch);
    begin
        for File of File_Names loop
            Put_Line (To_String(File));
        end loop;
    end Matching_Files_Exec;


begin
    Command_Map.Insert (To_Unbounded_String ("help"), (Help_Help'Access, Help_Exec'Access));
    Command_Map.Insert (To_Unbounded_String ("reload"), (Reload_Help'Access, Reload_Exec'Access));
    Command_Map.Insert (To_Unbounded_String ("add-dirs"), (Add_Dirs_Help'Access, Add_Dirs_Exec'Access));
    Command_Map.Insert (To_Unbounded_String ("list-dirs"), (List_Dirs_Help'Access, List_Dirs_Exec'Access));

    Command_Map.Insert (To_Unbounded_String ("find-text"), (Find_Text_Help'Access, Find_Text_Exec'Access));
    Command_Map.Insert (To_Unbounded_String ("list-text"), (List_Text_Help'Access, List_Text_Exec'Access));

    Command_Map.Insert (To_Unbounded_String("pop"), (Pop_Help'Access, Pop_Exec'Access));

    Command_Map.Insert (To_Unbounded_String ("matching-files"), (Matching_Files_Help'Access, Matching_Files_Exec'Access));
end SP.Commands;
