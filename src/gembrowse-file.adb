-------------------------------------------------------------------------------
-- gembrowse-file.adb
--
-- Handle local file access
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Gembrowse.URL;

package body Gembrowse.File is
    
    ---------------------------------------------------------------------------
    -- loadLocalFile
    --
    -- The file: scheme RFC can have "authority" as part of a path. We'll
    -- assume local files and just concatenate host/path to get a path to the
    -- local file.
    --
    -- @TODO directory listings would be kind of cool w/
    -- GNAT.Directory_Operations
    ---------------------------------------------------------------------------
    procedure loadLocalFile (url : String; contents : out Unbounded_String) is
        input : File_Type;
        bounded : Gembrowse.URL.URLStrings.Bounded_String := 
            Gembrowse.URL.URLStrings.To_Bounded_String (url);
        parsed : Gembrowse.URL.URL;

        fullPath : Unbounded_String;

    begin
        contents := Null_Unbounded_String;

        Gembrowse.URL.parseURL (bounded, parsed);

        if parsed.scheme.To_String = "file" then

            fullPath := To_Unbounded_String (parsed.host.To_String & parsed.path.To_String);
            
            Put_Line (Standard_Error, "fullPath: " & fullPath.To_String);

            Open (input, Ada.Text_IO.In_File, fullPath.To_String);

            while not End_Of_File (input) loop
                contents.Append (Get_Line (input));
            end loop;

            Close (input);
        end if;

        Put_Line (Standard_Error, "file contents: " & contents.To_String);

    exception
        --@TODO set tooltip here for file not found or bad permissions
        when E : others =>
            contents := To_Unbounded_String ("Could not open file " & fullPath.To_String);
    end loadLocalFile;

end Gembrowse.File;