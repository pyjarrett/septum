with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Atomic.Signed;

generic
    type T (<>) is private;
    type T_Access is access T;
package SP.Memory
    with Preelaborate
is

    -- Atomic reference counting pointer.
    type Arc is new Ada.Finalization.Controlled with private;

    type Reference_Type (Element : access T) is limited null record
        with Implicit_Dereference => Element;

    type Reference_Count is new Integer;
    package Atomic_Integer is new Atomic.Signed (Reference_Count);

    function Make (Allocated : T_Access) return Arc
        with Post => Is_Valid (Make'Result);

    function Make_Null return Arc
        with Post => not Is_Valid (Make_Null'Result);

    function Is_Valid (Self : Arc) return Boolean;

    function Get (Self : Arc) return Reference_Type
        with Pre => Is_Valid (Self);

    procedure Reset (Self : aliased in out Arc)
        with Post => not Is_Valid (Self);

    -- Debugging function to get number of reference counts.
    function Count (Self : aliased in out Arc) return Reference_Count;

    overriding
    procedure Initialize (Self : in out Arc);

    overriding
    procedure Adjust (Self : in out Arc);

    overriding
    procedure Finalize (Self : in out Arc)
        with Post => not Is_Valid (Self);

private

    -- The backing type which actually tracks the reference count, as well as
    -- tracking the value being pointed to.
    type Control_Block is limited record
        Value : T_Access := null;
        Count : aliased Atomic_Integer.Instance := Atomic_Integer.Init (0);
    end record;

    type Control_Block_Access is access Control_Block;

    type Arc is new Ada.Finalization.Controlled with record
        Block : Control_Block_Access := null;
    end record;

    procedure Free is new Ada.Unchecked_Deallocation (T, T_Access);
    procedure Free is new Ada.Unchecked_Deallocation (Control_Block, Control_Block_Access);

end SP.Memory;
