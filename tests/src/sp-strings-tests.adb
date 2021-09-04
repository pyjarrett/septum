with Ada.Characters.Latin_1;

package body SP.Strings.Tests is

    package TT renames Trendy_Test;

    procedure Test_String_Split (Op : in out TT.Operation'Class) is
    begin
        Op.Register;
    end Test_String_Split;

    procedure Test_Is_Quoted (Op : in out TT.Operation'Class) is
        use Ada.Characters.Latin_1;
    begin
        Op.Register;

        Op.Assert (not Is_Quoted ("not quoted"));

        Op.Assert (not Is_Quoted (Quotation & "some text"));
        Op.Assert (not Is_Quoted ("some text" & Quotation));
        
        Op.Assert (not Is_Quoted (Apostrophe & "some text"));
        Op.Assert (not Is_Quoted ("some text" & Apostrophe));

        Op.Assert (not Is_Quoted (Quotation & "some text" & Apostrophe));
        Op.Assert (not Is_Quoted (Apostrophe & "some text" & Quotation));

        Op.Assert (Is_Quoted (Apostrophe & "some text" & Apostrophe));
        Op.Assert (Is_Quoted (Quotation & "some text" & Quotation));
    end Test_Is_Quoted;

    ---------------------------------------------------------------------------
    -- Test Registry
    ---------------------------------------------------------------------------
    function All_Tests return Trendy_Test.Test_Group is (
        Test_String_Split'Access,
        Test_Is_Quoted'Access
        );

end SP.Strings.Tests;
