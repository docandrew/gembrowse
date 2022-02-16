-------------------------------------------------------------------------------
-- gembrowse-ui-keys.ads
--
-- Constants for key codes. We use our own keycode definitions here, these do
-- not correspond to any sort of official "scan code" or anything else. We map
-- ANSI terminal escape sequences to a keypress defined here.
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
package Gembrowse.UI.Keys is

    ----------------------------------------------------------------------------
    -- Non-printable control keys or single keypresses
    ----------------------------------------------------------------------------
    type Key_Codes is (
        KEY_NONE,
        KEY_F1,
        KEY_F2,
        KEY_F3,
        KEY_F4,
        KEY_F5,
        KEY_F6,
        KEY_F7,
        KEY_F8,
        KEY_F9,
        KEY_F10,
        KEY_F11,
        KEY_F12,
        KEY_ESC,
        KEY_ENTER,
        KEY_PRTSC,
        KEY_SCRLK,
        KEY_PAUSE,
        KEY_TAB,
        KEY_INS,
        KEY_BACKSPACE,
        KEY_DEL,
        KEY_HOME,
        KEY_END,
        KEY_PGUP,
        KEY_PGDN,
        KEY_WIN,
        KEY_UP,
        KEY_LEFT,
        KEY_DOWN,
        KEY_RIGHT
    );

    ----------------------------------------------------------------------------
    -- Key_Modifiers
    --
    -- Track whether shift, alt, ctrl are being held down
    ----------------------------------------------------------------------------
    type Key_Modifiers is record
        NUMLOCK : Boolean;
        SHIFT   : Boolean;
        ALT     : Boolean;
        CTRL    : Boolean;
    end record;

end Gembrowse.UI.Keys;
