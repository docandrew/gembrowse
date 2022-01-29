-------------------------------------------------------------------------------
-- main.adb
--
-- Entry point for Gembrowse
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
-- with Ada.Text_IO; use Ada.Text_IO;
-- with GNAT.OS_Lib;
with Interfaces.C; use Interfaces.C;
-- with Interfaces.C.Strings; use Interfaces.C.Strings;

-- with tls; use tls;

with Console;
with Gembrowse.net;
with Gembrowse.UI;

procedure Main is
    -- type procPtr is access procedure with Convention => C;
    
    -- from libc, to ensure cleanup if we exit
    -- unexpectedly.
    -- function atexit (p : procPtr) return Interfaces.C.int
    --     with Import, Convention => C;
    
    -- procedure Cleanup with Convention => C;

    -- procedure Cleanup is
    -- begin
    --     Console.disableMouse;
    -- end Cleanup;

    -- ign : Interfaces.C.int;
begin
    -- ign := atexit (Cleanup'Access);

    Console.altBuffer;
    Gembrowse.net.setup;
    Gembrowse.UI.renderLoop;
    Gembrowse.net.teardown;

    Console.mainBuffer;
    Console.resetColor;
end Main;
