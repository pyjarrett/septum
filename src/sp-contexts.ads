with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package SP.Contexts is

    package Line_Matches is new Ada.Containers.Ordered_Sets (Element_Type => Positive);

    type Context_Match is record
        File_Name        : Ada.Strings.Unbounded.Unbounded_String;
        Internal_Matches : Line_Matches.Set;
        Minimum          : Positive;
        Maximum          : Positive;
    end record;

    function From
        (File_Name : String; Line : Natural; Num_Lines : Natural; Context_Width : Natural) return Context_Match with
        Pre  => Line <= Num_Lines,
        Post => Is_Valid (From'Result);

    function Real_Min (C : Context_Match) return Positive with
        Pre => Is_Valid (C),
        Post => C.Minimum <= Real_Min'Result and then Real_Min'Result <= C.Maximum;

    function Real_Max (C : Context_Match) return Positive with
        Pre => Is_Valid (C),
        Post => C.Minimum <= Real_Max'Result and then Real_Max'Result <= C.Maximum;

    function Is_Valid (C : Context_Match) return Boolean;

    function Overlap (A, B : Context_Match) return Boolean with
        Pre => Is_Valid (A) and then Is_Valid (B);

    function Contains (A : Context_Match; Line_Num : Positive) return Boolean with
        Pre => Is_Valid (A);

    function Merge (A, B : Context_Match) return Context_Match with
        Pre  => Is_Valid (A) and then Is_Valid (B),
        Post => Is_Valid (Merge'Result);

    package Context_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Context_Match);

end SP.Contexts;
