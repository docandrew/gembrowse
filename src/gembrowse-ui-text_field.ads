-------------------------------------------------------------------------------
-- gembrowse-ui-text_field.ads
--
-- Render and handle input for editable text fields
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Gembrowse.UI.State;

package Gembrowse.UI.Text_Field is

    ----------------------------------------------------------------------------
    -- Text_Field
    --
    -- Display a single-line text field
    -- @field Display_Length is the number of characters that this widget can display
    -- @field Max_Length is the max number of characters that this widget can hold
    -- @field Force_Select - if True, this text field will always steal the
    --   keyboard focus, select the entire text, and become active. This must
    --   _always_ be turned off immediately after this field is drawn.
    -- @return True if we hit Return key or lose focus from this field.
    ----------------------------------------------------------------------------
    function Text_Field (st              : in out Gembrowse.UI.State.UIState;
                         Text            : in out Ada.Strings.Unbounded.Unbounded_String;
                         x               : Natural;
                         y               : Natural;
                         Display_Length  : Natural := 20;
                         Max_Length      : Natural := 20;
                         Force_Select    : Boolean := False) return Boolean;

end Gembrowse.UI.Text_Field;