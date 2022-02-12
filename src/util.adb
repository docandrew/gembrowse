-------------------------------------------------------------------------------
-- util.adb
--
-- Generic utility routines
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
package body Util is

    function max (l,r : Natural) return Natural is
    begin
        return (if l < r then r else l);
    end max;

    function min (l,r : Natural) return Natural is
    begin
        return (if l > r then r else l);
    end min;

end Util;
