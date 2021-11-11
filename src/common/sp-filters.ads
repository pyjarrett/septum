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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GNAT.Regpat;
with SP.Contexts;
with SP.Memory;
with SP.Strings;

package SP.Filters
    with Preelaborate
is
    use SP.Strings;

    -- Filters need to do different things. Some filters match line contents, whereas others want to remove any match
    -- which has a match anywhere in the content. When a filter matches, some action with regards to the search should
    -- be done, whether to include or to exclude the match from the results.
    type Filter_Action is (Keep, Exclude);

    -- Search filters define which lines match and what to do about a match.
    type Filter (Action : Filter_Action) is abstract tagged null record;

    -- Describes the filter in an end-user type of way. TODO: This should be localized.
    function Image (F : Filter) return String is abstract;

    -- Determine if a filter matches a string.
    function Matches_Line (F : Filter; Str : String) return Boolean is abstract;

    type Filter_Access is access Filter'Class;
    package Pointers is new SP.Memory (T => Filter'Class, T_Access => Filter_Access);

    subtype Filter_Ptr is Pointers.Arc;

    -- Provides a means to store many types of filters in the same list.
    package Filter_List is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => Filter_Ptr, "=" => Pointers."=");


    function Find_Text (Text : String) return Filter_Ptr;
    function Exclude_Text (Text : String) return Filter_Ptr;

    function Find_Like (Text : String) return Filter_Ptr;
    function Exclude_Like (Text : String) return Filter_Ptr;

    function Find_Regex (Text : String) return Filter_Ptr;
    function Exclude_Regex (Text : String) return Filter_Ptr;

    function Is_Valid_Regex (S : String) return Boolean;

    -- Looks for a match in any of the given lines.
    function Matches_File (F : Filter'Class; Lines : String_Vectors.Vector) return Boolean;
    function Matching_Lines (F : Filter'Class; Lines : String_Vectors.Vector) return SP.Contexts.Line_Matches.Set;

private

    type Regex_Access is access GNAT.Regpat.Pattern_Matcher;
    package Rc_Regex is new SP.Memory (T => GNAT.Regpat.Pattern_Matcher, T_Access => Regex_Access);

    type Case_Sensitive_Match_Filter is new Filter with record
        Text : Ada.Strings.Unbounded.Unbounded_String;
    end record;

    type Case_Insensitive_Match_Filter is new Filter with record
        Text : Ada.Strings.Unbounded.Unbounded_String;
    end record;

    type Regex_Filter is new Filter with record
        Source : Ada.Strings.Unbounded.Unbounded_String;
        Regex : Rc_Regex.Arc;
    end record;

    overriding function Image (F : Case_Sensitive_Match_Filter) return String;
    overriding function Matches_Line (F : Case_Sensitive_Match_Filter; Str : String) return Boolean;

    overriding function Image (F : Case_Insensitive_Match_Filter) return String;
    overriding function Matches_Line (F : Case_Insensitive_Match_Filter; Str : String) return Boolean;

    overriding function Image (F : Regex_Filter) return String;
    overriding function Matches_Line (F : Regex_Filter; Str : String) return Boolean;

end SP.Filters;
