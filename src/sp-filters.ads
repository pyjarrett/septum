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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GNAT.Regpat;
with GNATCOLL.Refcount;
with SP.Contexts;
with SP.Strings;

package SP.Filters is
    use SP.Strings;

    type Filter_Action is (Keep, Exclude);
    -- Filters need to do different things. Some filters match line contents, whereas others want to remove any match
    -- which has a match anywhere in the content. When a filter matches, some action with regards to the search should
    -- be done, whether to include or to exclude the match from the results.

    type Filter (Action : Filter_Action) is abstract tagged null record;
    -- Search filters define which lines match and what to do about a match.

    function Image (F : Filter) return String is abstract;
    -- Describes the filter in an end-user type of way. TODO: This should be localized.

    function Matches_Line (F : Filter; Str : String) return Boolean is abstract;
    -- Determine if a filter matches a string.

    package Pointers is new GNATCOLL.Refcount.Shared_Pointers (Element_Type => Filter'Class);

    subtype Filter_Ptr is Pointers.Ref;
    -- Provides a means to store many types of filters in the same list.

    package Filter_List is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => Filter_Ptr, "=" => Pointers."=");

    package Rc_Regex is new GNATCOLL.Refcount.Shared_Pointers (Element_Type => GNAT.Regpat.Pattern_Matcher);

    function Find_Text (Text : String) return Filter_Ptr;
    -- Matches lines which match this text.

    function Exclude_Text (Text : String) return Filter_Ptr;
    -- Matches lines which don't have this text.

    function Find_Regex (Text : String) return Filter_Ptr;
    -- Matches lines which match this regex.

    function Exclude_Regex (Text : String) return Filter_Ptr;
    -- Matches lines which don't have this regex.

    function Matches_File (F : Filter'Class; Lines : String_Vectors.Vector) return Boolean;
    -- Looks for a match in any of the given lines.

    function Matching_Lines (F : Filter'Class; Lines : String_Vectors.Vector) return SP.Contexts.Line_Matches.Set;

private

    type Case_Sensitive_Match_Filter is new Filter with record
        Text : Ada.Strings.Unbounded.Unbounded_String;
    end record;

    type Regex_Filter is new Filter with record
        Source : Ada.Strings.Unbounded.Unbounded_String;
        Regex : Rc_Regex.Ref;
    end record;

    overriding function Image (F : Case_Sensitive_Match_Filter) return String;
    overriding function Matches_Line (F : Case_Sensitive_Match_Filter; Str : String) return Boolean;

    overriding function Image (F : Regex_Filter) return String;
    overriding function Matches_Line (F : Regex_Filter; Str : String) return Boolean;

end SP.Filters;
