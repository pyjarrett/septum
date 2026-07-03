package SP.Help is

    function Colorize_Command (Command_Name : String) return String;
    procedure Header (Command_Name : String; Simple_Help : String := "");
    procedure Block (Contents : String; Extra_Space : Boolean := True);
    procedure Describe_Command (Command_Name : String; Options : String; Description : String);

end SP.Help;