-------------------------------------------------------------------------------
-- gembrowse-ui-scrollbars.adb
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;

with Colors;
with Console;

with Gembrowse.UI.Buttons; use Gembrowse.UI.Buttons;
with Gembrowse.UI.Input; use Gembrowse.UI.Input;
with Gembrowse.UI.Keys; use Gembrowse.UI.Keys;
with Gembrowse.UI.State; use Gembrowse.UI.State;

package body Gembrowse.UI.Scrollbars is

    ---------------------------------------------------------------------------
    -- Horizontal Scrollbar
    --
    -- Scrollbars have three components - 2 buttons and a slider.
    ---------------------------------------------------------------------------
    function Horizontal_Scrollbar (st  : in out Gembrowse.UI.State.UIState;
                                   x   : Natural;
                                   y   : Natural;
                                   Width : Positive;
                                   Min : Integer;
                                   Max : Integer;
                                   Val : in out Integer) return Boolean
    is
        id      : constant Gembrowse.UI.State.ID := Gembrowse.UI.State.Next_ID (st);
        scope   : constant Gembrowse.UI.State.Scope_ID := st.Curr_Scope;

        cellsPerVal : constant Float := (if Max = Min then 0.0 else Float(Width - 2) / Float(Max - Min));
        xpos : constant Positive := Integer(Float(Val - Min) * cellsPerVal) + 1;

        -- Slider between the two buttons.
        function Horizontal_Slider return Boolean is
        begin
            if Region_Hit (st, x, y, x + Width, y) then
                st.Hot_Item := id;
                st.Hot_Scope := scope;

                if st.Active_Item = NO_ITEM and st.Mouse_Down then
                    st.Active_Item := id;
                    st.Active_Scope := scope;
                end if;
            end if;

            -- if no widget has keyboard focus, take it
            if st.Kbd_Item = NO_ITEM then
                st.Kbd_Item := id;
                st.Kbd_Scope := Scope;
            end if;

            -- if we have keyboard focus, show it and update heartbeat
            if st.Kbd_Item = id and st.Kbd_Scope = Scope then
                --@TODO indicate keyboard focus if tabbed.
                st.Kbd_Heartbeat := True;
            end if;

            Console.setBGColor (Colors.currentTheme.bg);
            Console.setColor (Colors.currentTheme.ui);

            -- draw the gutter
            Console.setCursor (x + 1, y);
            for dx in 1 .. Width-1 loop
                Put ("░");
            end loop;

            -- draw the slider
            Console.setCursor (x + xpos, y);
            Put ("▓");

            -- Update slider position.
            if st.Active_Item = id and st.Active_Scope = Scope then
                Update: declare
                    -- mouse position relative to scrollbar
                    Mouse_Pos : Integer := Integer(st.Mouse_x) - Integer(x);
                    New_Val : Integer;
                begin
                    st.Tooltip := To_Unbounded_String ("Val:" & Val'Image & " xpos:" & xpos'Image & " max:" & Max'Image);

                    -- if Mouse_Pos < Min then
                    --     Mouse_Pos := Min;
                    -- end if;

                    -- if Mouse_Pos > Max then
                    --     Mouse_Pos := Max;
                    -- end if;

                    New_Val := Min + Integer(Float(Mouse_Pos) / cellsPerVal);

                    if New_Val < Min then New_Val := Min; end if;
                    if New_Val > Max then New_Val := Max; end if;

                    if Val /= New_Val then
                        Val := New_Val;
                        return True;
                    end if;
                end Update;
            end if;

            -- If we have keyboard focus, process keys
            HandleKeys: Declare
            begin    
                if st.Kbd_Item = id and st.Kbd_Scope = Scope then
                    --Ada.Text_IO.Put_Line("slider key: " & st.Kbd_Pressed'Image);

                    case st.Kbd_Pressed is
                        when KEY_TAB =>
                            -- Lose focus, next widget will snag it.
                            st.Kbd_Item := NO_ITEM;
                            st.Kbd_Scope := NO_SCOPE;

                            -- or make previous widget get focus if we're doing shift+tab.
                            if st.Kbd_Modifier.Shift then
                                st.Kbd_Item := st.Last_Widget;
                                st.Kbd_Scope := st.Last_Scope;
                            end if;

                            -- clear key
                            st.Kbd_Pressed := KEY_NONE;

                        when KEY_LEFT =>
                            if Val > Min then
                                Val := Val - 1;
                            end if;

                            st.Kbd_Pressed := KEY_NONE;
                            return True;

                        when KEY_RIGHT =>
                            if Val < Max then
                                Val := Val + 1;
                            end if;

                            st.Kbd_Pressed := KEY_NONE;
                            return True;

                        when others =>
                            --Ada.Text_IO.Put_Line("other: " & st.Kbd_Pressed'Image);
                            null;
                    end case;
                end if;
            end HandleKeys;

            st.Last_Widget := id;
            st.Last_Scope := Scope;

            return False;
        end Horizontal_Slider;
    begin
        -- draw left scroll button
        if Button (st, x, y, "⬅", "Scroll left (shortcut: left arrow)") then
            if Val > Min then
                Val := Val - 1;
                return True;
            end if;
        end if;

        -- draw right scroll button
        if Button (st, x + Width, y, "➡", "Scroll right (shortcut: right arrow)") then
            if Val < Max then
                Val := Val + 1;
                return True;
            end if;
        end if;

        -- Draw slider
        return Horizontal_Slider;
    end Horizontal_Scrollbar;

    ---------------------------------------------------------------------------
    -- Horizontal Scrollbar
    --
    -- Scrollbars have three components - 2 buttons and a slider.
    ---------------------------------------------------------------------------
    function Vertical_Scrollbar (st  : in out Gembrowse.UI.State.UIState;
                                   x   : Natural;
                                   y   : Natural;
                                   Height : Positive;
                                   Min : Integer;
                                   Max : Integer;
                                   Val : in out Integer) return Boolean
    is
        id      : constant Gembrowse.UI.State.ID := Gembrowse.UI.State.Next_ID (st);
        scope   : constant Gembrowse.UI.State.Scope_ID := st.Curr_Scope;

        cellsPerVal : constant Float    := (if Max = Min then 0.0 else Float(Height - 2) / Float(Max - Min));
        ypos        : constant Positive := Integer(Float(Val - Min) * cellsPerVal) + 1;

        -- Slider between the two buttons.
        function Vertical_Slider return Boolean is
        begin
            if Region_Hit (st, x, y, x, y + Height) then
                st.Hot_Item := id;
                st.Hot_Scope := scope;

                if st.Active_Item = NO_ITEM and st.Mouse_Down then
                    st.Active_Item := id;
                    st.Active_Scope := scope;
                end if;
            end if;

            -- if no widget has keyboard focus, take it
            if st.Kbd_Item = NO_ITEM then
                st.Kbd_Item := id;
                st.Kbd_Scope := Scope;
            end if;

            -- if we have keyboard focus, show it and update heartbeat
            if st.Kbd_Item = id and st.Kbd_Scope = Scope then
                --@TODO indicate keyboard focus if tabbed.
                st.Kbd_Heartbeat := True;
            end if;

            Console.setBGColor (Colors.currentTheme.bg);
            Console.setColor (Colors.currentTheme.ui);

            -- draw the gutter
            for y2 in (y + 1) .. (y + Height - 1) loop
                Console.setCursor (x, y2);
                Put ("░");
            end loop;

            -- draw the slider
            -- @TODO consider making this bigger or scaling it up as the
            -- cellsPerVal grows.
            Console.setCursor (x, y + ypos);
            Put ("▓");

            -- Update slider position.
            if st.Active_Item = id and st.Active_Scope = Scope then
                Update: declare
                    -- mouse position relative to scrollbar
                    Mouse_Pos : Integer := Integer(st.Mouse_y) - Integer(y);
                    New_Val : Integer;
                begin
                    st.Tooltip := To_Unbounded_String ("Val:" & Val'Image & " ypos:" & ypos'Image & " max:" & Max'Image);

                    -- if Mouse_Pos < Min then
                    --     Mouse_Pos := Min;
                    -- end if;

                    -- if Mouse_Pos > Max then
                    --     Mouse_Pos := Max;
                    -- end if;

                    New_Val := Min + Integer(Float(Mouse_Pos) / cellsPerVal);

                    if New_Val < Min then New_Val := Min; end if;
                    if New_Val > Max then New_Val := Max; end if;

                    if Val /= New_Val then
                        Val := New_Val;
                        return True;
                    end if;
                end Update;
            end if;

            -- If we have keyboard focus, process keys
            HandleKeys: Declare
            begin    
                if st.Kbd_Item = id and st.Kbd_Scope = Scope then
                    --Ada.Text_IO.Put_Line("slider key: " & st.Kbd_Pressed'Image);

                    case st.Kbd_Pressed is
                        when KEY_TAB =>
                            -- Lose focus, next widget will snag it.
                            st.Kbd_Item := NO_ITEM;
                            st.Kbd_Scope := NO_SCOPE;

                            -- or make previous widget get focus if we're doing shift+tab.
                            if st.Kbd_Modifier.Shift then
                                st.Kbd_Item := st.Last_Widget;
                                st.Kbd_Scope := st.Last_Scope;
                            end if;

                            -- clear key
                            st.Kbd_Pressed := KEY_NONE;

                        when KEY_UP =>
                            if Val > Min then
                                Val := Val - 1;
                            end if;

                            st.Kbd_Pressed := KEY_NONE;
                            return True;

                        when KEY_DOWN =>
                            if Val < Max then
                                Val := Val + 1;
                            end if;

                            st.Kbd_Pressed := KEY_NONE;
                            return True;

                        when others =>
                            --Ada.Text_IO.Put_Line("other: " & st.Kbd_Pressed'Image);
                            null;
                    end case;
                end if;
            end HandleKeys;

            st.Last_Widget := id;
            st.Last_Scope := Scope;

            return False;
        end Vertical_Slider;
    begin
        -- draw up scroll button
        if Button (st, x, y, "⬆", "Scroll up (shortcut: up arrow)") then
            if Val > Min then
                Val := Val - 1;
                return True;
            end if;
        end if;

        -- draw down scroll button
        if Button (st, x, y + Height, "⬇", "Scroll down (shortcut: down arrow)") then
            if Val < Max then
                Val := Val + 1;
                return True;
            end if;
        end if;

        -- Draw slider
        return Vertical_Slider;
    end Vertical_Scrollbar;

end Gembrowse.UI.Scrollbars;
