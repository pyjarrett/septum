with Ada.Strings.Maps;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

package body SP.Strings is
    function Split (S : Ada.Strings.Unbounded.Unbounded_String) return String_Vectors.Vector is
        --  Splits an unbounded string on spaces.
        Start  : Positive := 1;
        Finish : Natural  := 0;
    begin
        return Result : String_Vectors.Vector do
            while Start <= Ada.Strings.Unbounded.Length (S) loop
                Ada.Strings.Unbounded.Find_Token
                   (Source => S, Set => Ada.Strings.Maps.To_Set (" "), From => Start,
                    Test   => Ada.Strings.Outside, First => Start, Last => Finish);
                String_Vectors.Append
                   (Container => Result,
                    New_Item  =>
                       Ada.Strings.Unbounded.To_Unbounded_String
                          (Ada.Strings.Unbounded.Slice (S, Start, Finish)));
                Start := Finish + 1;
            end loop;
        end return;
    end Split;

    function Read_Lines (File_Name : String; Result : out String_Vectors.Vector) return Boolean is
        --  Reads all the lines from a file.
        File : Ada.Text_IO.File_Type;
        Line : Ada.Strings.Unbounded.Unbounded_String;
    begin
        String_Vectors.Clear (Result);
        Ada.Text_IO.Open (File => File, Mode => Ada.Text_IO.In_File, Name => File_Name);
        while not Ada.Text_IO.End_Of_File (File) loop
            Line := Ada.Strings.Unbounded.Text_IO.Get_Line (File);
            Result.Append (Line);
        end loop;

        Ada.Text_IO.Close (File);
        return True;
    exception
        when Ada.Text_IO.End_Error =>
            if Ada.Text_IO.Is_Open (File) then
                Ada.Text_IO.Close (File);
            end if;
            return True;
        when others =>
            Ada.Text_IO.Put_Line ("Unable to read contents of: " & File_Name);
            return False;
    end Read_Lines;

end SP.Strings;
