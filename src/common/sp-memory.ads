with Ada.Finalization;
with Ada.Unchecked_Deallocation;

generic
    type T (<>) is private;
package SP.Memory is

    -- A reference counting pointer.
    type RC is new Ada.Finalization.Controlled with private;
    type T_Access is access T;
    type Reference_Type (Element : access T) is limited null record
        with Implicit_Dereference => Element;

    function Make (Allocated : T_Access) return RC
        with Post => Is_Valid (Make'Result);
    function Is_Valid (Self : RC) return Boolean;

    function Get (Self : RC) return Reference_Type
        with Pre => Is_Valid (Self);

    procedure Reset (Self : in out RC)
        with Post => not Is_Valid (Self);

    overriding
    procedure Initialize (Self : in out RC);

    overriding
    procedure Adjust (Self : in out RC);

    overriding
    procedure Finalize (Self : in out RC)
        with Post => not Is_Valid (Self);

private

    -- The backing type which actually tracks the reference count, as well as
    -- tracking the value being pointed to.
    type Backing is limited record
        Value : T_Access;
        Count : Natural := 0;
    end record;

    type Backing_Access is access Backing;

    function Is_Zero (Self : Backing) return Boolean;
    procedure Increment (Self : in out Backing)
        with Post => not Is_Zero (Self);
    procedure Decrement (Self : in out Backing)
        with Pre => not Is_Zero (Self);

    type RC is new Ada.Finalization.Controlled with record
        Target : Backing_Access;
    end record;

    procedure Free is new Ada.Unchecked_Deallocation(T, T_Access);
    procedure Free is new Ada.Unchecked_Deallocation(Backing, Backing_Access);

end SP.Memory;