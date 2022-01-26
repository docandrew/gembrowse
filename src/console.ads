-------------------------------------------------------------------------------
-- console.ads
--
-- TUI primitives
--
-- Copyright Jon Andrew 2022
-------------------------------------------------------------------------------
package Console is

    procedure altBuffer;
    procedure mainBuffer;

    procedure setColor (r, g, b : Natural);
    procedure setBGColor (r, g, b: Natural);
    procedure resetColor;

    
    procedure setCursor (x, y : Natural);
    procedure getCursor (x, y : out Natural);

    procedure termSize (width, height : out Natural);
    
    procedure clear;

    procedure Heading (s : String);
    procedure Warn (s : String);
    procedure Error (s : String);

end Console;