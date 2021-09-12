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

    function Colorize (S : String; Color : ANSI.Colors) return String is
    begin
        return ANSI.Foreground (Color)
            & S
            & ANSI.Foreground (ANSI.Default);
    end Colorize;

    function Colorize (US : Ada.Strings.Unbounded.Unbounded_String; Color : ANSI.Colors)
        return Ada.Strings.Unbounded.Unbounded_String
    is
        use all type Ada.Strings.Unbounded.Unbounded_String;
    begin
        return ANSI.Foreground (Color)
            & US
            & ANSI.Foreground (ANSI.Default);
    end Colorize;

end SP.Terminal;