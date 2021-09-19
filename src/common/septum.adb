-------------------------------------------------------------------------------
-- Copyright 2021, The Septum Developers (see AUTHORS file)

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;

with SP.Interactive;

procedure Septum is
begin
    SP.Interactive.Main;
exception
    when Err : others =>
        Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Err));
        Ada.Text_IO.Put_Line ("Exception traceback: " & GNAT.Traceback.Symbolic.Symbolic_Traceback (Err));
end Septum;
