with SP.Strings;

package SP.Help is
    function Colorize_Command (Command_Name : String) return String;
    procedure Header (Command_Name : String; Simple_Help : String);
    procedure Block (Contents : String);
    procedure Plain (Contents : String);
    procedure Example (Contents : Strings.String_Vectors.Vector);
end SP.Help;
