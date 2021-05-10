with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package SP.Strings is
    package String_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => Ada.Strings.Unbounded.Unbounded_String, "<" => Ada.Strings.Unbounded."<",
         "="          => Ada.Strings.Unbounded."=");
    package String_Vectors is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => Ada.Strings.Unbounded.Unbounded_String,
         "="        => Ada.Strings.Unbounded."=");

    function Split (S : Ada.Strings.Unbounded.Unbounded_String) return String_Vectors.Vector;

    function Read_Lines (File_Name : in String; Result : out String_Vectors.Vector) return Boolean;

    function Common_Prefix_Length
        (A : Ada.Strings.Unbounded.Unbounded_String; B : Ada.Strings.Unbounded.Unbounded_String) return Natural with
        Post => Common_Prefix_Length'Result <
        Natural'Max (Ada.Strings.Unbounded.Length (A), Ada.Strings.Unbounded.Length (B));

end SP.Strings;
