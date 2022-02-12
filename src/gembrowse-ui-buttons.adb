-------------------------------------------------------------------------------
-- gembrowse-ui-buttons.adb
--
-- Button drawing & input handling.
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gembrowse.UI.Input;
with Gembrowse.UI.State; use Gembrowse.UI.State;

package body Gembrowse.UI.Buttons is

    function Button (st : in out Gembrowse.UI.State.UIState;
                     x : Positive;
                     y : Positive;
                     label : String;
                     tooltip : String;
                     fg   : Colors.ThemeColor := Colors.currentTheme.ui;
                     bg   : Colors.ThemeColor := Colors.currentTheme.bg;
                     padl : Natural := 0;
                     padr : Natural := 0;
                     padt : Natural := 0;
                     padb : Natural := 0) return Boolean is
        
        id      : constant Gembrowse.UI.State.ID := Gembrowse.UI.State.Next_ID (st);
        scope   : constant Gembrowse.UI.State.Scope_ID := st.Curr_Scope;

        use Gembrowse.UI.Input;
    begin
        Console.setCursor (st.Scope_X_Offset + x, st.Scope_Y_Offset + y);
        Console.setBGColor (bg);
        Console.setColor (fg);

        Put (label);

        if Region_Hit (st, x - padl, y - padt, x + padr, y + padb) then
            st.Hot_Item  := id;
            st.Hot_Scope := scope;

            st.tooltip := To_Unbounded_String (tooltip);

            if st.Active_Item = NO_ITEM and st.Mouse_Down then
                st.Active_Item := id;
                st.Active_Scope := Scope;
            end if;
        end if;

        st.Last_Widget := id;
        st.Last_Scope := Scope;
        
        -- If button is hot and was active, but button no longer pressed, then it
        -- was clicked.
        if st.Mouse_Down = False and
            st.Hot_Item = id and st.Hot_Scope = Scope and
            st.Active_Item = id and st.Active_Scope = Scope then

            return True;
        end if;

        return False;
    end Button;

end Gembrowse.UI.Buttons;