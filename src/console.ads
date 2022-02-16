-------------------------------------------------------------------------------
-- console.ads
--
-- TUI primitives
--
-- Copyright Jon Andrew 2022
-------------------------------------------------------------------------------
with Interfaces.C;

with Colors;

package Console is

    function getch return Character;

    procedure altBuffer;
    procedure mainBuffer;

    procedure rawMode;
    procedure normalMode;

    procedure setColor (r, g, b : Natural);
    procedure setColor (c : Colors.ThemeColor);
    procedure setBGColor (r, g, b: Natural);
    procedure setBGColor (c : Colors.ThemeColor);
    procedure resetColor;

    procedure underlineOn;
    procedure underlineOff;

    procedure boldOn;
    procedure boldOff;

    procedure italicsOn;
    procedure italicsOff;

    procedure enableMouse;
    procedure disableMouse;
    
    procedure setCursor (x, y : Natural);
    procedure getCursor (x, y : out Natural);
    procedure hideCursor;
    procedure showCursor;

    procedure termSize (width, height : out Natural);
    
    procedure clear;
    procedure eraseLine;

    procedure setTitle (s : String);

    procedure modifyOtherKeys;
    procedure unmodifyOtherKeys;

    procedure Heading (s : String);
    procedure Warn (s : String);
    procedure Error (s : String);

end Console;