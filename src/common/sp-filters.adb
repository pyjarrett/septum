-------------------------------------------------------------------------------
-- Copyright 2021, The Septum Developers (see AUTHORS file)

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

package body SP.Filters is
    use Ada.Strings.Unbounded;

    function To_Upper_Case (Text : String) return String is
        use Ada.Strings.Maps;
    begin
        return Ada.Strings.Fixed.Translate (Text, Constants.Upper_Case_Map);
    end To_Upper_Case;

    function Find_Text (Text : String) return Filter_Ptr is
    begin
        return Pointers.Make (new Case_Sensitive_Match_Filter' (Action => Keep, Text => To_Unbounded_String (Text)));
    end Find_Text;

    function Exclude_Text (Text : String) return Filter_Ptr is
    begin
        return Pointers.Make (new Case_Sensitive_Match_Filter' (Action => Exclude, Text => To_Unbounded_String (Text)));
    end Exclude_Text;

    function Find_Like (Text : String) return Filter_Ptr is
    begin
        return Pointers.Make (new Case_Insensitive_Match_Filter' (
            Action => Keep,
            Text   => To_Unbounded_String (To_Upper_Case (Text))));
    end Find_Like;

    function Exclude_Like (Text : String) return Filter_Ptr is
    begin
        return Pointers.Make (new Case_Insensitive_Match_Filter' (
            Action => Exclude,
            Text   => To_Unbounded_String (To_Upper_Case (Text))));
    end Exclude_Like;

    function Find_Regex (Text : String) return Filter_Ptr is
        Matcher : Rc_Regex.Arc;
    begin
        Matcher := Rc_Regex.Make (new GNAT.Regpat.Pattern_Matcher'(GNAT.Regpat.Compile (Text)));
        return Pointers.Make (new Regex_Filter' (Action => Keep, Source => To_Unbounded_String(Text), Regex => Matcher));
    exception
        -- Unable to compile the regular expression.
        when GNAT.Regpat.Expression_Error =>
            return Pointers.Make_Null;
    end Find_Regex;

    function Exclude_Regex (Text : String) return Filter_Ptr is
        Matcher : Rc_Regex.Arc;
    begin
        Matcher := Rc_Regex.Make (new GNAT.Regpat.Pattern_Matcher'(GNAT.Regpat.Compile (Text)));
        return Pointers.Make (new Regex_Filter' (Action => Exclude, Source => To_Unbounded_String(Text), Regex => Matcher));
    exception
        -- Unable to compile the regular expression.
        when GNAT.Regpat.Expression_Error =>
            return Pointers.Make_Null;
    end Exclude_Regex;

    ----------------------------------------------------------------------------

    function Is_Valid_Regex (S : String) return Boolean is
    begin
        declare
            Matcher : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile (S);
        begin
            pragma Unreferenced (Matcher);
            null;
        end;
        return True;
    exception
        when GNAT.Regpat.Expression_Error =>
            return False;
    end Is_Valid_Regex;

    ----------------------------------------------------------------------------

    overriding function Image (F : Case_Sensitive_Match_Filter) return String is
        use Ada.Characters;
    begin
        return "Case Sensitive Match " & Latin_1.Quotation & To_String (F.Text) & Latin_1.Quotation;
    end Image;

    overriding function Matches_Line (F : Case_Sensitive_Match_Filter; Str : String) return Boolean is
    begin
        return Ada.Strings.Fixed.Index (Str, To_String (F.Text)) > 0;
    end Matches_Line;

    ----------------------------------------------------------------------------

    overriding function Image (F : Case_Insensitive_Match_Filter) return String is
        use Ada.Characters;
    begin
        return "Case Insensitive Match " & Latin_1.Quotation & To_String (F.Text) & Latin_1.Quotation;
    end Image;

    overriding function Matches_Line (F : Case_Insensitive_Match_Filter; Str : String) return Boolean is
        Upper_Cased : constant String := To_Upper_Case (Str);
    begin
        return Ada.Strings.Fixed.Index (Upper_Cased, To_String (F.Text)) > 0;
    end Matches_Line;

    ----------------------------------------------------------------------------

    overriding function Image (F : Regex_Filter) return String is
    begin
        return "Regex " & Ada.Strings.Unbounded.To_String (F.Source);
    end Image;

    overriding function Matches_Line (F : Regex_Filter; Str : String) return Boolean is
    begin
        return GNAT.Regpat.Match (F.Regex.Get, Str);
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
