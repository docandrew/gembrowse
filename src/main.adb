-------------------------------------------------------------------------------
-- main.adb
--
-- Entry point for Gembrowse
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
-- with Ada.Text_IO; use Ada.Text_IO;
-- with GNAT.OS_Lib;
-- with Interfaces.C; use Interfaces.C;
-- with Interfaces.C.Strings; use Interfaces.C.Strings;

-- with tls; use tls;

with Console;
with Gembrowse.net;
with Gembrowse.UI;

procedure Main is
begin
    Console.altBuffer;
    Gembrowse.net.setup;
    Gembrowse.UI.renderLoop;
    Gembrowse.net.teardown;
    Console.mainBuffer;
    Console.resetColor;
end Main;
