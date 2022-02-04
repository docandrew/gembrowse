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
with System;

with termios;

package body Console is

    STDIN_FILENO  : constant := 0;
    STDOUT_FILENO : constant := 1;

    ---------------------------------------------------------------------------
    -- Need to use C's read() function for compatibility with raw terminal mode
    ---------------------------------------------------------------------------
    function read (fd  : Interfaces.C.int;
                   c   : System.Address;
                   len : Interfaces.C.size_t) return Interfaces.C.ptrdiff_t
        with Import, Convention => C, External_Name => "read";

    ---------------------------------------------------------------------------
    -- Need to use C's ioctl() to get terminal size. It's a varargs function
    -- but we only use it for one thing so just pass what we need.
    ---------------------------------------------------------------------------
    type winsize is record
        ws_row    : Interfaces.C.unsigned_short;
        ws_col    : Interfaces.C.unsigned_short;
        ws_xpixel : Interfaces.C.unsigned_short;
        ws_ypixel : Interfaces.C.unsigned_short;
    end record with Convention => C_Pass_By_Copy;

    TIOCGWINSZ : constant := 16#5413#;

    function ioctl (fd  : Interfaces.C.int;
                    req : Interfaces.C.unsigned_long;
                    arg : System.Address) return Interfaces.C.int
        with Import, Convention => C, External_Name => "ioctl";
    
    ---------------------------------------------------------------------------
    -- getch
    -- Return next character available from STDIN, or NUL if nothing available.
    ---------------------------------------------------------------------------
    function getch return Character is
        ret : Character;
        num : Interfaces.C.ptrdiff_t;

        use Interfaces.C;
    begin
        num := read (STDIN_FILENO, ret'Address, 1);
        
        if num < 1 then
            return ASCII.NUL;
        end if;

        return ret;
    end getch;

    ---------------------------------------------------------------------------
    -- altBuffer
    -- Instruct terminal to use alternate buffer, so previous history goes
    -- unmolested.
    ---------------------------------------------------------------------------
    procedure altBuffer is
    begin
        Put (ASCII.ESC & "[?1049h");
    end altBuffer;

    ---------------------------------------------------------------------------
    -- mainBuffer
    -- Revert to the terminal's primary buffer when leaving Gembrowse.
    ---------------------------------------------------------------------------
    procedure mainBuffer is
    begin
        Put (ASCII.ESC & "[?1049l");
    end mainBuffer;

    old : aliased termios.termios;

    ---------------------------------------------------------------------------
    -- rawMode
    -- Disable stdin echo and line buffering
    ---------------------------------------------------------------------------
    procedure rawMode is
        cur : aliased termios.termios;
        ign : Interfaces.C.int;

        use Interfaces.C;
    begin
        ign := termios.tcgetattr (STDIN_FILENO, old'Access);
        cur := old;

        termios.cfmakeraw (cur'Access);

        -- for non-blocking read()
        cur.c_cc(termios.VMIN)  := 0;
        cur.c_cc(termios.VTIME) := 0;

        ign := termios.tcsetattr (STDIN_FILENO, 0, cur'Access);
    end rawMode;

    ---------------------------------------------------------------------------
    -- Restore terminal mode
    ---------------------------------------------------------------------------
    procedure normalMode is
        ign : Interfaces.C.int;
    begin
        ign := termios.tcsetattr (0, 0, old'Access);
    end normalMode;

    ---------------------------------------------------------------------------
    -- setColor
    -- set 24-bit RGB Color for text foreground
    ---------------------------------------------------------------------------
    procedure setColor (r, g, b : Natural) is
    begin
        Put (ASCII.ESC & "[38;2;");
        Put (r, Width => 0); Put (";");
        Put (g, Width => 0); Put (";");
        Put (b, Width => 0); Put ("m");
        Flush;
    end setColor;

    procedure setColor (c : Colors.ThemeColor) is 
    begin
        setColor (c.r, c.g, c.b);
    end setColor;

    ---------------------------------------------------------------------------
    -- setBGColor
    -- set 24-bit RGB Color for text background
    ---------------------------------------------------------------------------
    procedure setBGColor (r, g, b : Natural) is
    begin
        Put (ASCII.ESC & "[48;2;");
        Put (r, Width => 0); Put (";");
        Put (g, Width => 0); Put (";");
        Put (b, Width => 0); Put ("m");
        -- Flush;
    end setBGColor;

    procedure setBGColor (c : Colors.ThemeColor) is
    begin
        setBGColor (c.r, c.g, c.b);
    end setBGColor;

    procedure resetColor is
    begin
        Put (ASCII.ESC & "[0m");
    end resetColor;

    procedure underlineOn is
    begin
        Put (ASCII.ESC & "[4m");
    end underlineOn;

    procedure underlineOff is
    begin
        Put (ASCII.ESC & "[24m");
    end underlineOff;

    procedure italicsOn is
    begin
        Put (ASCII.ESC & "[3m");
    end italicsOn;

    procedure italicsOff is
    begin
        Put (ASCII.ESC & "[23m");
    end italicsOff;

    procedure boldOn is
    begin
        Put (ASCII.ESC & "[1m");
    end boldOn;

    procedure boldOff is
    begin
        Put (ASCII.ESC & "[21m");
    end boldOff;

    procedure enableMouse is
    begin
        Put (ASCII.ESC & "[?1003h" &    -- any mouse event
             ASCII.ESC & "[?1006h");    -- additional mouse response
    end enableMouse;

    procedure disableMouse is
    begin
        Put (ASCII.ESC & "[?1000l");
    end disableMouse;

    procedure clear is
    begin
        Put (ASCII.ESC & "[");
        Put (2, Width => 0);
        Put ("J");
    end clear;

    procedure setCursor (x, y : Natural) is
    begin
        Put (ASCII.ESC & "[");
        Put (y, Width => 0); Put(";");
        Put (x, Width => 0); Put("H");
        -- Flush;
    end setCursor;

    ---------------------------------------------------------------------------
    -- getCursor
    -- 
    -- Need to ensure mouse disabled or we may get extra inputs to STDIN causing
    -- a race condition.
    ---------------------------------------------------------------------------
    procedure getCursor (x, y : out Natural) is
        chr : Character;
        ubs : Unbounded_String;
    begin
        Put (ASCII.ESC & "[");
        Put (6, Width => 0);
        Put ("n");

        -- response should be ESC [ <r> ; <c> R
        loop
            chr := getch;
            
            -- turn this into a blocking read
            if chr /= ASCII.NUL then
                Append (ubs, chr);
            end if;

            exit when chr = 'R';
        end loop;
        
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

    ---------------------------------------------------------------------------
    -- hideCursor
    ---------------------------------------------------------------------------
    procedure hideCursor is
    begin
        Put (ASCII.ESC & "[?25l");
    end hideCursor;

    ---------------------------------------------------------------------------
    -- showCursor
    ---------------------------------------------------------------------------
    procedure showCursor is
    begin
        Put (ASCII.ESC & "[?25h");
    end showCursor;

    ---------------------------------------------------------------------------
    -- termSize
    ---------------------------------------------------------------------------
    procedure termSize (width, height : out Natural) is
        -- oldX, oldY : Natural;
        ws : winsize;
        ret : Interfaces.C.int;

        use Interfaces.C;
    begin
        ret := ioctl (STDOUT_FILENO, TIOCGWINSZ, ws'Address);

        if ret = -1 or ws.ws_col = 0 then
            width  := 0;
            height := 0;
        else
            width  := Natural(ws.ws_col);
            height := Natural(ws.ws_row);
        end if;
    end termSize;

    procedure setTitle (s : String) is
    begin
        Put (ASCII.ESC & "]0;" & s & ASCII.ESC & "\");
    end setTitle;

    -- xterm-specific. Cause terminal to get shift, alt, ctrl info as esc sequence
    procedure modifyOtherKeys is
    begin
        Put (ASCII.ESC & "[>4;2m");
    end modifyOtherKeys;

    procedure unmodifyOtherKeys is
    begin
        Put (ASCII.ESC & "[>4m");
    end unmodifyOtherKeys;

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
