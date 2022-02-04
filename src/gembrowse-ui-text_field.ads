with Ada.Strings.Unbounded;
with Gembrowse.UI.State;

package Gembrowse.UI.Text_Field is

    ----------------------------------------------------------------------------
    -- Text_Field
    --
    -- Display a single-line text field
    -- @field Display_Length is the number of characters that this widget can display
    -- @field Max_Length is the max number of characters that this widget can hold
    -- @return True if we hit Return key or lose focus from this field.
    ----------------------------------------------------------------------------
    function Text_Field (st              : in out Gembrowse.UI.State.UIState;
                         Text            : in out Ada.Strings.Unbounded.Unbounded_String;
                         x               : Natural;
                         y               : Natural;
                         Display_Length  : Natural := 20;
                         Max_Length      : Natural := 20) return Boolean;

end Gembrowse.UI.Text_Field;