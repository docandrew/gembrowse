-------------------------------------------------------------------------------
-- gembrowse-net.ads
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Gembrowse.net is

    procedure setup;
    procedure teardown;
    function fetchPage (url : Unbounded_String; page : out Unbounded_String) return Boolean;

end Gembrowse.net;
