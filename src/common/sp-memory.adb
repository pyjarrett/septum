with Interfaces;

package body SP.Memory is
    use all type Interfaces.Integer_32;

    function Make (Allocated : T_Access) return Arc is
    begin
        return Arc' (Ada.Finalization.Controlled with
            Block => new Control_Block' (
                Value => Allocated,
                Count => Atomic.Signed_32.Init (0)));
    end Make;

    function Make_Null return Arc is
    begin
        return Self : Arc do
            null;
        end return;
    end Make_Null;

    function Get (Self : Arc) return Reference_Type is
    begin
        return (Element => Self.Block.Value);
    end Get;

    function Is_Valid (Self : Arc) return Boolean is
    begin
        return Self.Block /= null and then Self.Block.Value /= null;
    end Is_Valid;

    procedure Reset (Self : aliased in out Arc) is
    begin
        if Self.Block /= null then
            if Atomic.Signed_32.Add_Fetch (Self.Block.Count, -1) = 0 then
                Free (Self.Block.Value);
                Free (Self.Block);
            else
                Self.Block := null;
            end if;
        end if;
    end Reset;

    procedure Increment (Self : in out Arc) is
    begin
        if Self.Block /= null then
            Atomic.Signed_32.Add (Self.Block.Count, 1);
        end if;
    end Increment;

    overriding
    procedure Initialize (Self : in out Arc) is
    begin
        Increment (Self);
    end Initialize;

    overriding
    procedure Adjust (Self : in out Arc) is
    begin
        Increment (Self);
    end Adjust;

    overriding
    procedure Finalize (Self : in out Arc) is
    begin
        Reset (Self);
    end Finalize;

end SP.Memory;
