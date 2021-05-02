with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SP.Contexts;
with SP.Strings;            use SP.Strings;

package SP.Commands is

    pragma Elaborate_Body;
    function Execute(Srch : in out SP.Contexts.Search; Command_Name : Unbounded_String; Parameters : String_Vectors.Vector) return Boolean;

end SP.Commands;
