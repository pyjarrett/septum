with SP.Memory;
with Trendy_Test.Assertions.Integer_Assertions;

use Trendy_Test.Assertions;
use Trendy_Test.Assertions.Integer_Assertions;

package body SP.Memory_Tests is

    type Int_Access is access Integer;
    package Int_Ptr is new SP.Memory (T => Integer, T_Access => Int_Access);

    procedure Test_Count (Op : in out Trendy_Test.Operation'Class) is
    begin
        Op.Register;

        declare
            I : Int_Ptr.Arc;
            J : Int_Ptr.Arc;
        begin
            Assert_EQ (Op, Integer (I.Count), 0);
            Assert_EQ (Op, Integer (J.Count), 0);
            Assert (Op, not I.Is_Valid);
            Assert (Op, not J.Is_Valid);

            I := Int_Ptr.Make (new Integer'(5));
            Assert_EQ (Op, Integer (I.Count), 1);
            Assert_EQ (Op, Integer (J.Count), 0);
            Assert (Op, I.Is_Valid);
            Assert (Op, not J.Is_Valid);

            J := I;
            Assert_EQ (Op, Integer (I.Count), 2);
            Assert_EQ (Op, Integer (J.Count), 2);
            Assert (Op, I.Is_Valid);
            Assert (Op, J.Is_Valid);

            I.Reset;
            Assert_EQ (Op, Integer (I.Count), 0);
            Assert_EQ (Op, Integer (J.Count), 1);
            Assert (Op, not I.Is_Valid);
            Assert (Op, J.Is_Valid);

            J.Reset;
            Assert_EQ (Op, Integer (I.Count), 0);
            Assert_EQ (Op, Integer (J.Count), 0);
            Assert (Op, not I.Is_Valid);
            Assert (Op, not J.Is_Valid);
        end;
    end Test_Count;

    ---------------------------------------------------------------------------
    -- Test Registry
    ---------------------------------------------------------------------------
    function All_Tests return Trendy_Test.Test_Group is (
        1 => Test_Count'Access
        );

end SP.Memory_Tests;
