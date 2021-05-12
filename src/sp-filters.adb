with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

package body SP.Filters is
    use Ada.Strings.Unbounded;

    function Find_Text (Text : String) return Filter_Ptr is
        Base : constant Case_Sensitive_Match_Filter := (Action => Keep, Text => To_Unbounded_String (Text));
        Ptr  : Filter_Ptr;
    begin
        Ptr.Set (Base);
        return Ptr;
    end Find_Text;

    function Exclude_Text (Text : String) return Filter_Ptr is
        Base : constant Case_Sensitive_Match_Filter := (Action => Exclude, Text => To_Unbounded_String (Text));
        Ptr  : Filter_Ptr;
    begin
        Ptr.Set (Base);
        return Ptr;
    end Exclude_Text;

    ----------------------------------------------------------------------------

    overriding function Image (F : Case_Sensitive_Match_Filter) return String is
        use Ada.Characters;
    begin
        return "Case Sensitive Match " & Latin_1.Quotation & To_String (F.Text) & Latin_1.Quotation;
    end Image;

    function Matches_Line (F : Case_Sensitive_Match_Filter; Str : String) return Boolean is
    begin
        return Ada.Strings.Fixed.Index (Str, To_String (F.Text)) > 0;
    end Matches_Line;

    ----------------------------------------------------------------------------

    function Matches_File (F : Filter'Class; Lines : String_Vectors.Vector) return Boolean is
        Match : constant Boolean := (for some Line of Lines => Matches_Line (F, To_String (Line)));
    begin
        case F.Action is
            when Keep =>
                return Match;
            when Exclude =>
                return not Match;
        end case;
    end Matches_File;

    ----------------------------------------------------------------------------

    function Matching_Lines (F : Filter'Class; Lines : String_Vectors.Vector) return SP.Contexts.Line_Matches.Set is
        Line_Num : Integer := 1;
    begin
        return L : SP.Contexts.Line_Matches.Set do
            for Line of Lines loop
                if Matches_Line (F, To_String (Line)) then
                    L.Insert (Line_Num);
                end if;
                Line_Num := Line_Num + 1;
            end loop;
        end return;
    end Matching_Lines;

end SP.Filters;
