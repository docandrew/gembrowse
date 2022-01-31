-------------------------------------------------------------------------------
-- main.adb
--
-- Entry point for Gembrowse
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with GNAT.Traceback.Symbolic;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;

with Console;
with Gembrowse.Net;
with Gembrowse.UI;

procedure Main is
begin
    Console.altBuffer;
    Gembrowse.net.setup;
    Gembrowse.UI.renderLoop;
    Gembrowse.net.teardown;

    Console.mainBuffer;
    Console.resetColor;

exception
    when E : others =>
        Console.normalMode;
        Console.disableMouse;
        Console.Error (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
        Console.Error ("Oops! Gembrowse had a fatal error and had to exit.");
end Main;
