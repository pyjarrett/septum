package body SP.Memory is

    function Make (Allocated : T_Access) return RC is
    begin
        return Self : RC do
            Self.Target := new Backing' (Value => Allocated, Count => 1);
        end return;
    end Make;

    function Get (Self : RC) return Reference_Type is
    begin
        return (Element => Self.Target.Value);
    end Get;

    function Is_Valid (Self : RC) return Boolean is
    begin
        return Self.Target /= null and then Self.Target.Value /= null;
    end Is_Valid;

    procedure Reset (Self : in out RC) is
    begin
        if Self.Target /= null then
            Decrement (Self.Target.all);
            if Is_Zero (Self.Target.all) then
                Free (Self.Target);
            end if;
        end if;
        Self.Target := null;
    end Reset;

    overriding
    procedure Initialize (Self : in out RC) is
    begin
        Increment (Self.Target.all);
    end Initialize;

    overriding
    procedure Adjust (Self : in out RC) is
    begin
        Increment (Self.Target.all);
    end Adjust;

    overriding
    procedure Finalize (Self : in out RC) is
    begin
        Decrement (Self.Target.all);

        if Self.Is_Valid and then Is_Zero (Self.Target.all) then
            Free (Self.Target.Value);
            Free (Self.Target);
        end if;

    end Finalize;


    function Is_Zero (Self : Backing) return Boolean is
    begin
        return Self.Count = 0;
    end Is_Zero;

    procedure Increment (Self : in out Backing) is
    begin
        Self.Count := Self.Count + 1;
    end Increment;

    procedure Decrement (Self : in out Backing) is
    begin
        Self.Count := Self.Count - 1;
        if Self.Count = 0 then
            Free (Self.Value);
        end if;
    end Decrement;

end SP.Memory;
