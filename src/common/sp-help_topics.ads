with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

package SP.Help_Topics is

    package ASU renames Ada.Strings.Unbounded;

    procedure About;
    procedure Usage;
    procedure Line_Filters;
    procedure File_Cache;
    procedure Path_Filters;
    procedure Results;

    type Topic_Help is access procedure;
    use type Ada.Strings.Unbounded.Unbounded_String;
    package String_Maps is new
       Ada.Containers.Ordered_Maps (Key_Type => ASU.Unbounded_String, Element_Type => Topic_Help);

    Topics : String_Maps.Map;

    --  TODO:
    --  procedure Find_Related (Term : String);

end SP.Help_Topics;
