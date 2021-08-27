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

package body SP.Terminal is

    procedure Clear_Line is
    begin
        GNATCOLL.Terminal.Beginning_Of_Line(Stdout_Term_Info);
        GNATCOLL.Terminal.Clear_To_End_Of_Line(Stdout_Term_Info);
    end Clear_Line;

begin

    GNATCOLL.Terminal.Init_For_Stdout(Stdout_Term_Info);

end SP.Terminal;
