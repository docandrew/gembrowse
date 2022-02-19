-------------------------------------------------------------------------------
-- gembrowse-file.adb
--
-- Handle local file access
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Gembrowse.File is
    
    ---------------------------------------------------------------------------
    -- Attempt to load a local file from disk and set the contents
    ---------------------------------------------------------------------------
    procedure loadLocalFile (url : String; contents : out Unbounded_String);

end Gembrowse.File;