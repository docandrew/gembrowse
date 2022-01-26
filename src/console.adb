-------------------------------------------------------------------------------
-- console.adb
--
-- TUI primitives
--
-- Copyright Jon Andrew 2022
-------------------------------------------------------------------------------
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C;

with termios;

package body Console is

    procedure altBuffer is
    begin
        Put (ASCII.ESC & "[?1049h");
    end altBuffer;

    procedure mainBuffer is
    begin
        Put (ASCII.ESC & "[?1049l");
    end mainBuffer;

    -- procedure setTerm (buffer : Boolean; echo : Boolean) is
    --     old : aliased termios.termios;
    --     cur : aliased termios.termios;
    --     ign : Interfaces.C.int;

    --     use Interfaces.C;
    -- begin
    --     ign := termios.tcgetattr (0, old'Access);
    --     cur := old;

    --     if echo then
    --         cur.c_lflag := cur.c_lflag or termios.ECHO;
    --     else
    --         cur.c_lflag := cur.c_lflag and (not termios.ECHO);
    --     end if;

    --     if buffer then
    --         cur.c_lflag := cur.c_lflag or termios.ICANON;
    --     else
    --         cur.c_lflag := cur.c_lflag and (not termios.ICANON);
    --     end if;
    --     -- if not echo or not buffer then
    --     --     termios.cfmakeraw (cur'Access);
    --     -- end if;

    --     ign := termios.tcsetattr (0, 0, cur'Access);
    -- end setTerm;

    procedure setColor (r, g, b : Natural) is
    begin
        Put (ASCII.ESC & "[38;2;");
        Put (r, Width => 0); Put (";");
        Put (g, Width => 0); Put (";");
        Put (b, Width => 0); Put ("m");
    end setColor;

    procedure setBGColor (r, g, b : Natural) is
    begin
        Put (ASCII.ESC & "[48;2;");
        Put (r, Width => 0); Put (";");
        Put (g, Width => 0); Put (";");
        Put (b, Width => 0); Put ("m");
    end setBGColor;

    procedure setCursor (x, y : Natural) is
    begin
        Put (ASCII.ESC & "[");
        Put (y, Width => 0); Put(";");
        Put (x, Width => 0); Put("H");
    end setCursor;

    procedure resetColor is
    begin
        Put (ASCII.ESC & "[0m");
    end resetColor;

    procedure clear is
    begin
        Put (ASCII.ESC & "[");
        Put (2, Width => 0);
        Put ("J");
    end clear;

    procedure getCursor (x, y : out Natural) is
        chr : Character;
        ubs : Unbounded_String;

        -- need to disable stdin echo, store the terminal settings here
        -- ign : Interfaces.C.int;
        -- old : aliased termios.termios;
        -- cur : aliased termios.termios;
    begin
        -- ign := termios.tcgetattr (0, old'Access);
        -- cur := old;
        -- termios.cfmakeraw (cur'Access);
        -- ign := termios.tcsetattr (0, 0, cur'Access);

        Put (ASCII.ESC & "[");
        Put (6, Width => 0);
        Put ("n");

        -- response should be ESC [ <r> ; <c> R
        -- has to be done this way since no terminating newline.
        -- Get_Immediate is used to avoid echo of the response
        loop
            Get_Immediate (chr);
            Append (ubs, chr);
            exit when chr = 'R';
        end loop;
        
        -- ign := termios.tcsetattr (0, 0, old'Access);

        declare
            pos  : String  := To_String (ubs);
            brac : Natural := Index (pos, "[", pos'First);
            semi : Natural := Index (pos, ";", pos'First);
            arrr : Natural := Index (pos, "R", pos'First);

            row  : Natural := Natural'Value (pos(brac+1..semi-1));
            col  : Natural := Natural'Value (pos(semi+1..arrr-1));
        begin
            x := col;
            y := row;
        end;

    end getCursor;

    procedure termSize (width, height : out Natural) is
        oldX, oldY : Natural;
    begin
        -- save old cursor
        getCursor (oldX, oldY);
        setCursor (999, 999);
        getCursor (width, height);
        setCursor (oldX, oldY);
    end termSize;

    procedure Heading (s : String) is
    begin
        setColor (0,200,0);
        Put_Line (s);
        resetColor;
    end Heading;

    procedure Warn (s : String) is
    begin
        setColor (130,130,0);
        Put_Line (s);
        resetColor;
    end Warn;

    procedure Error (s : String) is
    begin
        setColor (200,0,0);
        Put_Line (s);
        resetColor;
    end Error;

end Console;
