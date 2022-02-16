-------------------------------------------------------------------------------
-- tests.adb
--
-- Unit Tests
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Assertions;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;

with Gembrowse.URL;
with Gembrowse.URL.Punycode;

procedure Tests is
begin
    Gembrowse.URL.Punycode.runTests;
    Gembrowse.URL.runTests;

    Put_Line ("All tests ran successfully.");
exception
    when E : Ada.Assertions.Assertion_Error =>
        Put_Line ("One or more tests failed: ");
        Put_Line (Ada.Exceptions.Exception_Message (E));
        Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Tests;
