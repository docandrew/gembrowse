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

with Console;

with Tests.URL;
with Tests.URL.Punycode;

procedure TestsMain is
    testCaseCount : Natural := 0;
begin
    Console.Heading ("Running Tests.URL.Punycode");
    Tests.URL.Punycode.runTests (testCaseCount);
    Console.Heading ("Done.");

    Console.Heading ("Running Tests.URL");
    Tests.URL.runTests (testCaseCount);
    Console.Heading ("Done.");

    Put_Line ("");
    Console.Heading ("Completed" & testCaseCount'Image & " tests. All tests ran successfully.");
exception
    when E : Ada.Assertions.Assertion_Error =>
        Console.Error ("One or more tests failed: ");
        Put_Line (Ada.Exceptions.Exception_Message (E));
        Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end TestsMain;
