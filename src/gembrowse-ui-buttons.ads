-------------------------------------------------------------------------------
-- gembrowse-ui-buttons.ads
--
-- Button drawing and input handling routines.
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Gembrowse.UI.State;

with Colors;
with Console;

package Gembrowse.UI.Buttons is

    ---------------------------------------------------------------------------
    -- Button
    --
    -- @param st : UIState
    -- @param x : x position of the button
    -- @param y : y position of the button
    -- @param label : label of the button
    -- @param tooltip : tooltip for button hover
    -- @param pad : padding if detection region of button should be larger than
    --  the button size itself
    -- @param fg : foreground color of text on button
    -- @param bg : background color of text on button
    -- @return True if button was pressed, False otherwise.
    -- @modifies st.tooltip if being hovered over.
    ---------------------------------------------------------------------------
    function Button (st : in out Gembrowse.UI.State.UIState;
                     x : Positive;
                     y : Positive;
                     label : String;
                     tooltip : String;
                     fg : Colors.ThemeColor := Colors.currentTheme.ui;
                     bg : Colors.ThemeColor := Colors.currentTheme.bg;
                     padl : Natural := 0;
                     padr : Natural := 0;
                     padt : Natural := 0;
                     padb : Natural := 0) return Boolean;

end Gembrowse.UI.Buttons;
