with Ada.Real_Time;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;

with Console;

with Gembrowse.UI.Keys;
with Gembrowse.UI.State;

package body Gembrowse.UI.Input is

    ---------------------------------------------------------------------------
    -- Detect whether the mouse is in a certain region.
    ---------------------------------------------------------------------------
    function Region_Hit (st: in out Gembrowse.UI.State.UIState; x1,y1,x2,y2 : Natural) return Boolean is
    begin
        return (st.Mouse_x >= x1 + st.Scope_X_Offset and 
                st.Mouse_x <= x2 + st.Scope_X_Offset and 
                st.Mouse_y >= y1 + st.Scope_Y_Offset and 
                st.Mouse_y <= y2 + st.Scope_Y_Offset);
    end Region_Hit;

    ---------------------------------------------------------------------------
    -- Handle_Inputs
    --
    -- Read stdin for VT100 control codes, handle appropriately. This just
    -- reads input and handles it immediately instead of queuing up input
    -- events for other components to handle. This is more straightforward, but
    -- less re-usable. May consider the input queue if breaking this out into
    -- a separate TUI library at some point.
    ---------------------------------------------------------------------------
    procedure Handle_Inputs (st : in out Gembrowse.UI.State.UIState) is
        chr : Character;

        use Gembrowse.UI.Keys;
        use Interfaces;

        -----------------------------------------------------------------------
        -- printable
        -- Handle normal printable character keypresses
        -----------------------------------------------------------------------
        procedure printable (c : Character) is
        begin
            st.Kbd_Text := To_Unbounded_String ("" & c);
        end printable;

        -----------------------------------------------------------------------
        -- isMouseSequence
        -- If escSequence is potentially a mouse input, return True. Otherwise,
        -- return False.
        -----------------------------------------------------------------------
        function isMouseSequence (escSequence : Unbounded_String) return Boolean is
        begin
            return (Length (escSequence) >= 8 and then 
                    (Element(escSequence, 1) = '[' and 
                     Element(escSequence, 2) = '<' and 
                     (Element(escSequence, Length(escSequence)) = 'm' or
                      Element(escSequence, Length(escSequence)) = 'M')));
        end isMouseSequence;

        -----------------------------------------------------------------------
        -- handleMouse
        -- Given an escape sequence representing a mouse input, handle it
        -- appropriately.
        -----------------------------------------------------------------------
        procedure handleMouse (escSequence : Unbounded_String) is
            data  : String  := To_String (escSequence);
            lt    : Natural := Index (data, "<", data'First);
            semi1 : Natural := Index (data, ";", lt);
            semi2 : Natural := Index (data, ";", semi1+1);

            buttonStr : String := data(lt+1..semi1-1);
            posxStr   : String := data(semi1+1..semi2-1);
            posyStr   : String := data(semi2+1..data'Last-1);

            button, posx, posy : Natural;

            mtype  : Character := Element (escSequence, Length (escSequence));

            badSeq : Boolean := False;

            -- ANSI escape codes for mouse button held down plus movement are
            -- button + 32. Since we register the clicks separately, just treat
            -- everything in the range 32-35 as a movement.
            MOUSE_MOVE_L  : constant := 32;
            MOUSE_MOVE_H  : constant := 35;

            LEFT_CLICK   : constant := 0;
            MIDDLE_CLICK : constant := 1;
            RIGHT_CLICK  : constant := 2;
            WHEEL_UP     : constant := 64;
            WHEEL_DN     : constant := 65;

            use Ada.Real_Time;
        begin
            -- Quick sanity check here, if we get a goofed up escSequence, QC our values before proceeding.
            for c of buttonStr loop
                if c not in '0'..'9' then
                    badSeq := True;
                    return;
                end if;
            end loop;

            for c of posxStr loop
                if c not in '0'..'9' then
                    badSeq := True;
                    return;
                end if;
            end loop;
            
            for c of posyStr loop
                if c not in '0'..'9' then
                    badSeq := True;
                    return;
                end if;
            end loop;
            
            button := Natural'Value (buttonStr);
            posx   := Natural'Value (posxStr);
            posy   := Natural'Value (posyStr);

            if button >= MOUSE_MOVE_L and button <= MOUSE_MOVE_H then
                st.Mouse_X := posx;
                st.Mouse_Y := posy;
                st.Hover_Start := Ada.Real_Time.Clock;
            elsif button = LEFT_CLICK then
                if mtype = 'M' then
                    st.Mouse_Down := True;

                    -- Measure interval between clicks to see if double-click occurred.
                    if Ada.Real_Time.Clock <= st.Last_Click + Ada.Real_Time.Milliseconds (500) then
                        st.Double_Click := True;
                        -- tooltip := To_Unbounded_String ("Double Click");
                    end if;

                    st.Last_Click := Ada.Real_Time.Clock;
                elsif mtype = 'm' then
                    st.Mouse_Down := False;
                    st.Word_Select := False;
                end if;
            elsif button = MIDDLE_CLICK then
                st.Mouse_Buttons.Button_1 := True;
            elsif button = RIGHT_CLICK then
                st.Mouse_Buttons.Button_2 := True;
            elsif button = WHEEL_UP then
                st.Mouse_Buttons.Button_4 := True;
            elsif button = WHEEL_DN then
                st.Mouse_Buttons.Button_5 := True;
            end if;
        end handleMouse;

        -----------------------------------------------------------------------
        -- escapeSequence
        -- If we got an escape sequence, process it here.
        -----------------------------------------------------------------------
        procedure escapeSequence (escSequence : Unbounded_String) is
            F1      : constant String := "OP";
            F2      : constant String := "OQ";
            F3      : constant String := "OR";
            F4      : constant String := "OS";
            F5      : constant String := "[15~";
            F6      : constant String := "[17~";
            F7      : constant String := "[18~";
            F8      : constant String := "[19~";
            F9      : constant String := "[20~";
            INS     : constant String := "[2~";
            HOME    : constant String := "[H";
            ENDK    : constant String := "[F";
            PGUP    : constant String := "[5~";
            PGDN    : constant String := "[6~";
            DEL     : constant String := "[3~";
            UP      : constant String := "[A";
            DOWN    : constant String := "[B";
            RIGHT   : constant String := "[C";
            LEFT    : constant String := "[D";

            SHIFT_HOME : constant String := "[1;2H";
            SHIFT_END  : constant String := "[1;2F";

            CTRL_RIGHT : constant String := "[1;5C";
            CTRL_LEFT  : constant String := "[1;5D";

            SHIFT_RIGHT : constant String := "[1;2C";
            SHIFT_LEFT : constant String := "[1;2D";

            CTRL_SHIFT_RIGHT : constant String := "[1;6C";
            CTRL_SHIFT_LEFT  : constant String := "[1;6D";
        begin
            if To_String (escSequence) = F1 then
                st.Kbd_Pressed := KEY_F1;
            elsif To_String (escSequence) = F2 then
                st.Kbd_Pressed := KEY_F2;
            elsif To_String (escSequence) = F3 then
                st.Kbd_Pressed := KEY_F3;
            elsif To_String (escSequence) = F4 then
                st.Kbd_Pressed := KEY_F4;
            elsif To_String (escSequence) = F5 then
                st.Kbd_Pressed := KEY_F5;
            elsif To_String (escSequence) = F6 then
                st.Kbd_Pressed := KEY_F6;
            elsif To_String (escSequence) = F7 then
                st.Kbd_Pressed := KEY_F7;
            elsif To_String (escSequence) = F8 then
                st.Kbd_Pressed := KEY_F8;
            elsif To_String (escSequence) = F9 then
                st.Kbd_Pressed := KEY_F9;
            elsif To_String (escSequence) = INS then
                st.Kbd_Pressed := KEY_INS;
            elsif To_String (escSequence) = HOME then
                st.Kbd_Pressed := KEY_HOME;
            elsif To_String (escSequence) = PGUP then
                st.Kbd_Pressed := KEY_PGUP;
            elsif To_String (escSequence) = PGDN then
                st.Kbd_Pressed := KEY_PGDN;
            elsif To_String (escSequence) = ENDK then
                st.Kbd_Pressed := KEY_END;
            elsif To_String (escSequence) = DEL then
                st.Kbd_Pressed := KEY_DEL;
            elsif To_String (escSequence) = UP then
                st.Kbd_Pressed := KEY_UP;
            elsif To_String (escSequence) = DOWN then
                st.Kbd_Pressed := KEY_DOWN;
            elsif To_String (escSequence) = LEFT then
                st.Kbd_Pressed := KEY_LEFT;
            elsif To_String (escSequence) = RIGHT then
                st.Kbd_Pressed := KEY_RIGHT;
            elsif To_String (escSequence) = SHIFT_HOME then
                st.Kbd_Pressed := KEY_HOME;
                st.Kbd_Modifier.SHIFT := True;
            elsif To_String (escSequence) = SHIFT_END then
                st.Kbd_Pressed := KEY_END;
                st.Kbd_Modifier.SHIFT := True;
            elsif To_String (escSequence) = CTRL_RIGHT then
                st.Kbd_Pressed := KEY_RIGHT;
                st.Kbd_Modifier.CTRL := True;
            elsif To_String (escSequence) = CTRL_LEFT then
                st.Kbd_Pressed := KEY_LEFT;
                st.Kbd_Modifier.CTRL := True;
            elsif To_String (escSequence) = SHIFT_LEFT then
                st.Kbd_Pressed := KEY_LEFT;
                st.Kbd_Modifier.SHIFT := True;
            elsif To_String (escSequence) = SHIFT_RIGHT then
                st.Kbd_Pressed := KEY_RIGHT;
                st.Kbd_Modifier.SHIFT := True;
            elsif To_String (escSequence) = CTRL_SHIFT_LEFT then
                st.Kbd_Pressed := KEY_LEFT;
                st.Kbd_Modifier.CTRL := True;
                st.Kbd_Modifier.SHIFT := True;
            elsif To_String (escSequence) = CTRL_SHIFT_RIGHT then
                st.Kbd_Pressed := KEY_RIGHT;
                st.Kbd_Modifier.CTRL := True;
                st.Kbd_Modifier.SHIFT := True;
            elsif isMouseSequence (escSequence) then
                -- st.tooltip := escSequence;
                handleMouse (escSequence);
            else
                null;   -- @TODO probably other useful sequences to handle.
                st.tooltip := escSequence;
            end if;
        end escapeSequence;

        -----------------------------------------------------------------------
        -- escape
        -- Get escape sequences. These can be responses to terminal queries,
        -- or special keypresses, or mouse inputs.
        -----------------------------------------------------------------------
        procedure escape is
            escSequence : Unbounded_String;
        begin
            -- see if more input is waiting (multi char escape sequence)
            loop
                chr := Console.getch;
                exit when chr = ASCII.NUL;
                Append (escSequence, chr);
            end loop;

            -- useful for getting new esc sequences
            -- tooltip := escSequence;

            -- if we hit ESC by itself, we'll have an empty escSequence
            if Length (escSequence) = 0 then
                -- if editingURL then
                --     selectNone;
                -- end if;
                st.Kbd_Pressed := KEY_ESC;
                return;
            end if;

            escapeSequence (escSequence);
        end escape;

        -----------------------------------------------------------------------
        -- control
        -- Handle control characters
        -----------------------------------------------------------------------
        procedure control (c : Character) is
            pos : Natural;

            CTRL_C   : constant := 3;
            CTRL_D   : constant := 4;
            CTRL_F   : constant := 6;
            CTRL_H   : constant := 8;
            CTRL_J   : constant := 10;
            CTRL_L   : constant := 12;
            CR       : constant := 13;
            CTRL_Q   : constant := 17;
            CTRL_S   : constant := 19;
            CTRL_T   : constant := 20;
            CTRL_W   : constant := 23;
            CTRL_B   : constant := 29;
            DEL      : constant := 127;
        begin
            -- CTRL + something
            pos := Character'Pos (c);
            
            -- copied from Chrome for most part
            case pos is
                when CTRL_C =>
                    st.Kbd_Text := To_Unbounded_String ("c");
                    st.Kbd_Modifier.CTRL := True;
                when CTRL_D =>
                    st.Kbd_Text := To_Unbounded_String ("d");
                    st.Kbd_Modifier.CTRL := True;
                when CTRL_F =>
                    st.Kbd_Text := To_Unbounded_String ("f");
                    st.Kbd_Modifier.CTRL := True;
                when CTRL_H | DEL =>
                    st.Kbd_Pressed := KEY_BACKSPACE;
                when CTRL_J | CR =>
                    st.Kbd_Pressed := KEY_ENTER;
                when CTRL_L =>
                    st.Kbd_Text := To_Unbounded_String ("l");
                    st.Kbd_Modifier.CTRL := True;
                when CTRL_Q => 
                    st.Done := True;
                    st.Kbd_Modifier.CTRL := True;
                when CTRL_S =>
                    st.Kbd_Text := To_Unbounded_String ("c");
                    st.Kbd_Modifier.CTRL := True;
                when CTRL_T =>
                    st.Kbd_Text := To_Unbounded_String ("t");
                    st.Kbd_Modifier.CTRL := True;
                when CTRL_W =>
                    st.Kbd_Text := To_Unbounded_String ("w");
                    st.Kbd_Modifier.CTRL := True;
                when CTRL_B =>
                    st.Kbd_Text := To_Unbounded_String ("b");
                    st.Kbd_Modifier.CTRL := True;
                when others =>
                    st.tooltip := To_Unbounded_String ("key: " & pos'Image);
            end case;
        end control;
    begin
        st.Kbd_Modifier := (others => FALSE);
        st.Kbd_Pressed  := KEY_NONE;
        st.Kbd_Text     := Ada.Strings.Unbounded.Null_Unbounded_String;

        -- returns NUL if no input available
        chr := Console.getch;

        case chr is
            when ASCII.NUL =>
                return;
            when ASCII.SOH..ASCII.SUB | ASCII.DEL =>
                control (chr);
            when ' '..'~' =>
                -- normal printable char
                printable (chr);
            when ASCII.ESC =>
                escape;
            when others =>
                null;
                -- declare
                --     pos : Natural := Character'Pos (chr);
                -- begin
                --     tooltip := To_Unbounded_String ("asdf: " & pos'Image);
                -- end;
        end case;
    end Handle_Inputs;

end Gembrowse.UI.Input;