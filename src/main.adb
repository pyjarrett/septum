with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;

with SP.Interactive;

procedure Main is
begin
    SP.Interactive.Main;
exception
    when Err : others =>
        Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Err));
        Ada.Text_IO.Put_Line ("Exception traceback: " & GNAT.Traceback.Symbolic.Symbolic_Traceback (Err));
end Main;
