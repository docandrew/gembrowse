-------------------------------------------------------------------------------
-- main.adb
--
-- Entry point for Gembrowse
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Command_Line.Environment;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Traceback.Symbolic;
with GNAT.OS_Lib;
with Interfaces.C; use Interfaces.C;

with Console;
with Gembrowse.Bookmarks;
with Gembrowse.Net;
with Gembrowse.UI;

procedure Main is

    ---------------------------------------------------------------------------
    -- getHomeFolder
    -- Search environment variables for user's home folder, for default
    -- bookmark location.
    ---------------------------------------------------------------------------
    function getHomeFolder return String is
    begin
        -- check environment variables for home directory
        for env in 1..Ada.Command_Line.Environment.Environment_Count loop
            -- Put_Line (Ada.Command_Line.Environment.Environment_Value (env));
            declare
                curEnv : String := Ada.Command_Line.Environment.Environment_Value (env);
            begin
                if Index (curEnv, "HOME=") = 1 then
                    return curEnv (6..curEnv'Last);
                end if;
            end;
        end loop;

        return "";
    end getHomeFolder;

    version : String := "0.0.0";

    userSpecifiedBookmarkFile : Boolean := False;
    home : String := getHomeFolder;
begin
    -- parse command-line arguments
    for arg in 1 .. Ada.Command_Line.Argument_Count loop
        if Ada.Command_Line.Argument (arg) = "-b" and arg < Ada.Command_Line.Argument_Count then
            declare
                bookmarkFilepath : String := Ada.Command_Line.Argument (arg + 1);
                bookmarkFile     : File_Type;
            begin
                -- limit file path length
                if bookmarkFilepath'Last > 4096 then
                    Put_Line ("Bookmark file path too long, needs to be < 4096 characters");
                    GNAT.OS_Lib.OS_Exit (1);
                end if;

                Gembrowse.Bookmarks.useBookmarkFile (Ada.Command_Line.Argument (arg + 1));
                userSpecifiedBookmarkFile := True;
            end;
        elsif Ada.Command_Line.Argument (arg) = "-h" or 
              Ada.Command_Line.Argument (arg) = "--help" or
              Ada.Command_Line.Argument (arg) = "-v" or
              Ada.Command_Line.Argument (arg) = "--version" then
            Put_Line ("Gembrowse " & version);
            Put_Line (" command-line options:");
            Put_Line (" -b <bookmark file>       Specify bookmark file (default is ~/.gembrowse_bookmarks.gmi)");
            GNAT.OS_Lib.OS_Exit (0);
        end if;
    end loop;

    if not userSpecifiedBookmarkFile and home /= "" then
        Gembrowse.Bookmarks.useBookmarkFile (home & "/.gembrowse_bookmarks.gmi");
    end if;

    Console.altBuffer;
    Gembrowse.net.setup;
    Gembrowse.UI.renderLoop;
    Gembrowse.net.teardown;

    Console.mainBuffer;
    Console.resetColor;

exception
    when E : Ada.IO_Exceptions.Use_Error =>
        Put_Line (Ada.Exceptions.Exception_Message (E));

    when E : others =>
        Console.normalMode;
        Console.disableMouse;
        Console.Error ("Oops! Gembrowse had a fatal error and had to exit. Error was:");
        Put_Line (Ada.Exceptions.Exception_Message (E));
        Put_Line (Ada.Exceptions.Exception_Name (E));
        Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Main;
