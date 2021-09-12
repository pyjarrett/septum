with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Trendy_Test.Assertions;

package body SP.Strings.Tests is

    package ASU renames Ada.Strings.Unbounded;
    package TT renames Trendy_Test;
    use Trendy_Test.Assertions.Integer_Assertions;

    procedure Test_String_Split (Op : in out TT.Operation'Class) is
    begin
        Op.Register;

    end Test_String_Split;

    procedure Test_Is_Quoted (Op : in out TT.Operation'Class) is
        use Ada.Characters.Latin_1;
    begin
        Op.Register;

        Op.Assert (not Is_Quoted(""));
        Op.Assert (not Is_Quoted ("not quoted"));

        -- Unbalanced "
        Op.Assert (not Is_Quoted (Quotation & "some text"));
        Op.Assert (not Is_Quoted ("some text" & Quotation));

        -- Unbalanced '
        Op.Assert (not Is_Quoted (Apostrophe & "some text"));
        Op.Assert (not Is_Quoted ("some text" & Apostrophe));

        -- Mismatched ' and "
        Op.Assert (not Is_Quoted (Quotation & "some text" & Apostrophe));
        Op.Assert (not Is_Quoted (Apostrophe & "some text" & Quotation));

        -- Matched " and '
        Op.Assert (Is_Quoted (Apostrophe & "some text" & Apostrophe));
        Op.Assert (Is_Quoted (Quotation & "some text" & Quotation));

        -- Internal " or '
        Op.Assert (Is_Quoted (Apostrophe & Quotation & "some text" & Quotation & Apostrophe));
        Op.Assert (Is_Quoted (Apostrophe & Quotation & "some text" & Quotation & Apostrophe));
    end Test_Is_Quoted;

    procedure Test_Common_Prefix_Length (Op : in out TT.Operation'Class) is
        function "+" (S : String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;
    begin
        Op.Register;

        Assert_Eq (Op, Common_Prefix_Length(+"", +"SP.Strings"), 0);
        Assert_Eq (Op, Common_Prefix_Length(+"SP.Strings", +""), 0);
        Assert_Eq (Op, Common_Prefix_Length(+"", +""), 0);

        Assert_EQ (Op, Common_Prefix_Length(+"SP.Searches", +"SP.Strings"), 4);
        Assert_EQ (Op, Common_Prefix_Length(+"SP.Strings", +"SP.Strings"), ASU.Length (+"SP.Strings"));
    end Test_Common_Prefix_Length;

    ---------------------------------------------------------------------------
    -- Test Registry
    ---------------------------------------------------------------------------
    function All_Tests return Trendy_Test.Test_Group is (
        Test_String_Split'Access,
        Test_Is_Quoted'Access,
        Test_Common_Prefix_Length'Access
        );

end SP.Strings.Tests;
