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

package body SP.Contexts is

    function From
        (File_Name : String; Line : Natural; Num_Lines : Natural; Context_Width : Natural) return Context_Match is
        Minimum : constant Positive :=
            (if Context_Width > Num_Lines or else Line - Context_Width < 1 then 1 else Line - Context_Width);
        Maximum : constant Positive :=
            (if Context_Width > Num_Lines or else Line + Context_Width > Num_Lines then Num_Lines
             else Line + Context_Width);
    begin
        return C : Context_Match do
            C.File_Name := Ada.Strings.Unbounded.To_Unbounded_String (File_Name);
            C.Internal_Matches.Insert (Line);
            C.Minimum := Minimum;
            C.Maximum := Maximum;
        end return;
    end From;

    function Is_Valid (C : Context_Match) return Boolean is
    begin
        return
            (C.Minimum <= C.Maximum and then not C.Internal_Matches.Is_Empty
             and then C.Minimum <= C.Internal_Matches.First_Element
             and then C.Internal_Matches.Last_Element <= C.Maximum);
    end Is_Valid;

    function Real_Min (C : Context_Match) return Positive is (C.Internal_Matches.First_Element);
    function Real_Max (C : Context_Match) return Positive is (C.Internal_Matches.Last_Element);

    function Overlap (A, B : Context_Match) return Boolean is
        A_To_Left_Of_B  : constant Boolean  := A.Maximum < B.Minimum;
        A_To_Right_Of_B : constant Boolean  := B.Maximum < A.Minimum;
        New_Min         : constant Positive := Positive'Max (A.Minimum, B.Minimum);
        New_Max         : constant Positive := Positive'Min (A.Maximum, B.Maximum);
        use Ada.Strings.Unbounded;
    begin
        return
            A.File_Name = B.File_Name and then not (A_To_Left_Of_B or else A_To_Right_Of_B)
            and then
            (New_Min <= Real_Min (A) and then New_Min <= Real_Min (B) and then Real_Max (A) <= New_Max
             and then Real_Max (B) <= New_Max);
    end Overlap;

    function Contains (A : Context_Match; Line_Num : Positive) return Boolean is
    begin
        return A.Minimum <= Line_Num and then Line_Num <= A.Maximum;
    end Contains;

    function Contains (A, B : Context_Match) return Boolean is
        -- Does A fully contain B?
        use type Ada.Strings.Unbounded.Unbounded_String;
    begin
        return A.File_Name = B.File_Name and then A.Minimum <= B.Minimum and then B.Maximum <= A.Maximum;
    end Contains;

    function Merge (A, B : Context_Match) return Context_Match is
        use Line_Matches;
    begin
        return C : Context_Match do
            C.File_Name        := A.File_Name;
            C.Internal_Matches := A.Internal_Matches or B.Internal_Matches;
            C.Minimum          := Positive'Max (A.Minimum, B.Minimum);
            C.Maximum          := Positive'Min (A.Maximum, B.Maximum);
        end return;
    end Merge;

    function Image (A : Context_Match) return String is
        use Ada.Strings.Unbounded;
    begin
        return
            To_String
                (A.File_Name & ": " & A.Minimum'Image & " -> " & A.Maximum'Image & "  " &
                 A.Internal_Matches.Length'Image & " matches");
    end Image;

    overriding
    function "=" (A, B : Context_Match) return Boolean is
        use Ada.Strings.Unbounded;
        use SP.Contexts.Line_Matches;
    begin
        return
            A.File_Name = B.File_Name and then A.Minimum = B.Minimum and then B.Maximum = A.Maximum
            and then A.Internal_Matches = B.Internal_Matches;
    end "=";

    function Files_In (V : Context_Vectors.Vector) return SP.Strings.String_Sets.Set is
    begin
        return Files : SP.Strings.String_Sets.Set do
            for Context of V loop
                if not Files.Contains (Context.File_Name) then
                    Files.Insert (Context.File_Name);
                end if;
            end loop;
        end return;
    end Files_In;

end SP.Contexts;
