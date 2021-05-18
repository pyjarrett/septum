with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

package SP.Terminal is
    -- Functions for operations to the terminal. This hides the usage of Ada.Text_IO and may silently ignore
    -- capabilities if the terminal does not support them, such as if coloring text or line clearing is added.
    --
    -- This module also hides dependencies on unbounded IO.

    procedure Put (Str : String) renames Ada.Text_IO.Put;
    procedure Put (Str : Ada.Strings.Unbounded.Unbounded_String) renames Ada.Text_IO.Unbounded_IO.Put;

    procedure Put_Line (Str : String) renames Ada.Text_IO.Put_Line;
    procedure Put_Line (Str : Ada.Strings.Unbounded.Unbounded_String) renames Ada.Text_IO.Unbounded_IO.Put_Line;

    function Get_Line return String renames Ada.Text_IO.Get_Line;
    function Get_Line return Ada.Strings.Unbounded.Unbounded_String renames Ada.Text_IO.Unbounded_IO.Get_Line;

    procedure New_Line (Spacing : in Ada.Text_IO.Positive_Count := 1) renames Ada.Text_IO.New_Line;

    procedure Set_Col (Spacing : in Ada.Text_IO.Positive_Count) renames Ada.Text_IO.Set_Col;

    -- I'm not convinced that these aren't useful. I haven't figured out how best to deal with the really long and
    -- verbose terminology of Ada.Strings.Unbounded.Unbounded_String.

    --  function "&" (A : String; B : Unbounded_String) return Unbounded_String renames Ada.Strings.Unbounded."&";
    --  function "&" (Ada : Unbounded_String; B : String) return Unbounded_String renames Ada.Strings.Unbounded."&";

end SP.Terminal;
