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
    -- @param pad : padding if detection region of button should be larger (or smaller) 
    --  than the button size itself. 
    --
    --  Sometimes with multi-byte emojis the "length"
    --   of the label is larger than the cells it occupies, so a negative pad will
    --   give more accurate mouse detection for the button. Other times, the
    --   detection of a button needs to have some "grace" around it to make it a
    --   more clickable target. A positive pad is an easy way to do that.
    --
    --  Using a tooltip is a good way to test the detection region of your
    --   button.
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
                     padl : Integer := 0;
                     padr : Integer := 0;
                     padt : Integer := 0;
                     padb : Integer := 0) return Boolean;

end Gembrowse.UI.Buttons;
