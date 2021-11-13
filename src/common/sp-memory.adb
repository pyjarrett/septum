package body SP.Memory is
    function Make (Allocated : T_Access) return Arc is
    begin
        return Arc' (Ada.Finalization.Controlled with
            Block => new Control_Block' (
                Value => Allocated,
                Count => Atomic_Integer.Init (1)));
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
        return Self.Block /= null and then Self.Block.Value /= null and then Atomic_Integer.Load (Self.Block.Count) > 0;
    end Is_Valid;

    procedure Reset (Self : aliased in out Arc) is
    begin
        if Self.Block /= null then
            if Atomic_Integer.Add_Fetch (Self.Block.Count, -1) = 0 then
                Free (Self.Block.Value);
                Free (Self.Block);
            else
                Self.Block := null;
            end if;
        end if;
    end Reset;

    function Count (Self : aliased in out Arc) return Reference_Count is
    begin
        if Self.Block /= null then
            return Atomic_Integer.Load (Self.Block.Count);
        else
            return 0;
        end if;
    end Count;

    procedure Increment (Self : in out Arc) is
    begin
        if Self.Block /= null then
            Atomic_Integer.Add (Self.Block.Count, 1);
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
