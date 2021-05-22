-------------------------------------------------------------------------------
-- Septum, a tool for interactive file search and analysis.
--
-- Copyright (C) 2021, The Septum developers (see AUTHORS file)
--
-- Septum is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Septum is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

with SP.Terminal;

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

    function Find_Regex (Text : String) return Filter_Ptr is
        Matcher : Rc_Regex.Ref;
        Ptr     : Filter_Ptr;
    begin
        Matcher.Set (GNAT.Regpat.Compile (Text));
        declare
            Base : constant Regex_Filter := (Action => Keep, Source => To_Unbounded_String(Text), Regex => Matcher);
        begin
            Ptr.Set (Base);
        end;
        return Ptr;
    exception
        -- Unable to compile the regular expression.
        when GNAT.Regpat.Expression_Error =>
            SP.Terminal.Put_Line ("Unable to create regex filter from: " & Text);
            return Pointers.Null_Ref;
    end Find_Regex;

    function Exclude_Regex (Text : String) return Filter_Ptr is
        Matcher : Rc_Regex.Ref;
        Ptr     : Filter_Ptr;
    begin
        Matcher.Set (GNAT.Regpat.Compile (Text));
        declare
            Base : constant Regex_Filter := (Action => Exclude, Source => To_Unbounded_String(Text), Regex => Matcher);
        begin
            Ptr.Set (Base);
        end;
        return Ptr;
    exception
        -- Unable to compile the regular expression.
        when GNAT.Regpat.Expression_Error =>
            SP.Terminal.Put_Line ("Unable to create regex filter from: " & Text);
            return Pointers.Null_Ref;
    end Exclude_Regex;

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

    function Image (F : Regex_Filter) return String is
    begin
        return Ada.Strings.Unbounded.To_String (F.Source);
    end Image;

    function Matches_Line (F : Regex_Filter; Str : String) return Boolean is
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
