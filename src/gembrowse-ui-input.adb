-------------------------------------------------------------------------------
-- gembrowse-ui-input.adb
--
-- Copyright 2022 Jon Andrew
--
-- ANSI/VT control code input handling. Gembrowse uses "normal" mode, not
-- "application" mode, so input control sequences are handled accordingly.
--
-- See https://invisible-island.net/xterm/ctlseqs/ctlseqs.html for more info.
--
-- This is not a comprehensive handling for control sequences. Only the ones
-- that Gembrowse cares about (more or less) are dealt with here.
--
-- This package uses a task running in the background, all stdin inputs are
-- read by that task and then placed in a ring buffer via the protected
-- InputQueue object. The main thread reads characters from that ring buffer
-- until either a polling period has elapsed, or no more input is available.
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Console;

with Gembrowse.UI.Keys; use Gembrowse.UI.Keys;
with Gembrowse.UI.State; use Gembrowse.UI.State;

package body Gembrowse.UI.Input is

    -- Keep track of what we were doing in the input stream, so if we start
    -- parsing an escape sequence mid-stream then break away to do something
    -- else, we can resume getting the sequence.
    type ParserState is (
        UNDEFINED,            -- got some kind of a weird control code we can't handle.
                              --  discard characters until input stops.
        NONE,
        CONTROL,
        PRINTABLE,
        ESCAPE,               -- read a single ESC, need to determine what kind of control sequence
        SS3,                  -- ESC O, expect a single character next
        CSI,                  -- ESC [, control sequence indicator
        EXPECT_TILDE,         -- Have a complete control code, expect terminating ~
        CSI_1,                -- ESC [ 1
        CSI_2,                -- ESC [ 2
        CSI_1_SEMI,           -- ESC [ 1 ;
        CSI_1_SEMI_2,         -- ESC [ 1 ; 2
        CSI_1_SEMI_5,         -- ESC [ 1 ; 5
        CSI_1_SEMI_6,         -- ESC [ 1 ; 6
        MOUSE_BUTTON,         -- ESC [ <                     Get mouse button as a number
        MOUSE_COORD_X,        -- ESC [ < num ;               Getting mouse x coordinate
        MOUSE_COORD_Y         -- ESC [ < num ; num ; num     Getting mouse y coordinate. We're done
                              --                              when we get the terminating m or M.
    );

    parseState : ParserState := NONE;

    -- some keys need to have a terminating ~ before we can say "it's been pressed"
    -- we'll keep track of what it is here.
    proposedKeypress : Key_Codes;
    
    -- As we parse a mouse code, keep track of these digits
    -- @NOTE if we ever switch to SGR pixel mode for the mouse, this will need to
    -- be at least 4 digits.
    package MouseNumbers is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 3);
    use MouseNumbers;

    -- extra state for the mouse button/coordinates as we parse.
    mouseNum : Bounded_String;
    mouseButton : Natural;
    mouseX : Natural;
    mouseY : Natural;

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
    -- Get_Inputs
    ---------------------------------------------------------------------------
    procedure Get_Inputs (st : in out Gembrowse.UI.State.UIState) is
        chr       : Character;
        moreInput : Boolean := False;

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

        use Gembrowse.UI.Keys;

        -----------------------------------------------------------------------
        -- printable
        -- Handle normal printable character keypresses
        -----------------------------------------------------------------------
        procedure printable (c : Character) is
        begin
            Append (st.Kbd_Text, c);
        end printable;

        -----------------------------------------------------------------------
        -- escape
        -- Process the next character in an escape sequence.
        -- These can be responses to terminal queries, or special keypresses,
        -- or mouse inputs. The initial escape character should have already
        -- been consumed from the input stream. This will look at subsequent
        -- characters and advance the parser state. When the parser reaches a
        -- terminal character, we'll either have a full escape sequence or
        -- something we don't recognize. If it's something we don't recognize,
        -- we put the parser into an UNDEFINED state so further inputs in the
        -- stream are discarded.
        -----------------------------------------------------------------------
        procedure escape (c : Character) is
        begin
            
            case parseState is
                when ESCAPE =>
                    -- read the next character to see what kind of control
                    -- sequence to expect. If no character is present, it
                    -- was a literal escape key by itself.
                    case c is
                        when ASCII.NUL =>
                            -- st.Tooltip := To_Unbounded_String ("ESC");
                            st.Kbd_Pressed := KEY_ESC;
                            parseState := NONE;
                        when 'O' =>
                            parseState := SS3;
                        when '[' =>
                            parseState := CSI;
                        when others =>
                            -- something we don't know how to handle
                            st.Error := UNKNOWN_CONTROL_SEQUENCE;
                            parseState := UNDEFINED;
                    end case;

                when EXPECT_TILDE =>
                    if c = '~' then
                        st.Kbd_Pressed := proposedKeypress;
                        parseState := NONE;
                    else
                        st.Error := WANTED_TILDE;
                        parseState := UNDEFINED;
                    end if;

                when SS3 =>
                    -- expect a single character afterwards. we only get a few
                    -- of these.
                    case c is
                        when 'P' => 
                            st.Kbd_Pressed := KEY_F1;
                            parseState := NONE;
                        when 'Q' =>
                            st.Kbd_Pressed := KEY_F2;
                            parseState := NONE;
                        when 'R' => 
                            st.Kbd_Pressed := KEY_F3;
                            parseState := NONE;
                        when 'S' =>
                            st.Kbd_Pressed := KEY_F4;
                            parseState := NONE;
                        when others =>
                            st.Error := UNKNOWN_SS3;
                            parseState := UNDEFINED;
                    end case;
                
                when CSI =>
                    case c is
                        -- check for single character codes first.
                        when 'A' =>
                            st.Kbd_Pressed := KEY_UP;
                            parseState := NONE;
                        when 'B' =>
                            st.Kbd_Pressed := KEY_DOWN;
                            parseState := NONE;
                        when 'C' =>
                            st.Kbd_Pressed := KEY_RIGHT;
                            parseState := NONE;
                        when 'D' =>
                            st.Kbd_Pressed := KEY_LEFT;
                            parseState := NONE;
                        when 'F' =>
                            st.Kbd_Pressed := KEY_END;
                            parseState := NONE;
                        when 'H' =>
                            st.Kbd_Pressed := KEY_HOME;
                            parseState := NONE;
                        when 'Z' =>
                            st.Kbd_Pressed := KEY_TAB;
                            st.Kbd_Modifier.Shift := True;
                            parseState := NONE;

                        -- these will all have more chars to follow.
                        when '1' =>
                            parseState := CSI_1;
                        when '2' =>
                            parseState := CSI_2;
                        when '<' =>
                            -- Looks like a mouse input
                            -- After this, we expect a number
                            -- representing either a button
                            -- or 32-35 for movements.
                            mouseNum := Null_Bounded_String;
                            mouseX := 0;
                            mouseY := 0;
                            mouseButton := Natural'Last;
                            
                            parseState := MOUSE_BUTTON;
                        when '3' =>
                            proposedKeypress := KEY_DEL;
                            parseState := EXPECT_TILDE;
                        when '5' =>
                            proposedKeypress := KEY_PGUP;
                            parseState := EXPECT_TILDE;
                        when '6' =>
                            proposedKeypress := KEY_PGDN;
                            parseState := EXPECT_TILDE;
                        when others =>
                            st.Error := UNKNOWN_CSI;
                            parseState := UNDEFINED;
                    end case;
                
                when CSI_1 =>
                    case c is
                        when ';' =>
                            -- some kind of shift or ctrl combo
                            parseState := CSI_1_SEMI;
                        when '5' =>
                            proposedKeypress := KEY_F5;
                            parseState := EXPECT_TILDE;
                        when '7' =>
                            proposedKeypress := KEY_F6;
                            parseState := EXPECT_TILDE;
                        when '8' =>
                            proposedKeypress := KEY_F7;
                            parseState := EXPECT_TILDE;
                        when '9' =>
                            proposedKeypress := KEY_F8;
                            parseState := EXPECT_TILDE;
                        when others =>
                            st.Error := UNKNOWN_CSI_1;
                            parseState := UNDEFINED;
                    end case;
                
                when CSI_2 =>
                    case c is
                        when '0' =>
                            proposedKeypress := KEY_F9;
                            parseState := EXPECT_TILDE;
                        when '1' =>
                            proposedKeypress := KEY_F10;
                            parseState := EXPECT_TILDE;
                        when '3' =>
                            proposedKeypress := KEY_F11;
                            parseState := EXPECT_TILDE;
                        when '4' =>
                            proposedKeypress := KEY_F12;
                            parseState := EXPECT_TILDE;
                        when '~' =>
                            st.Kbd_Pressed := KEY_INS;
                            parseState := NONE;
                        when others =>
                            st.Error := UNKNOWN_CSI;
                            parseState := UNDEFINED;
                    end case;

                when CSI_1_SEMI =>
                    case c is
                        when '2' =>
                            parseState := CSI_1_SEMI_2;
                        when '5' =>
                            parseState := CSI_1_SEMI_5;
                        when '6' =>
                            parseState := CSI_1_SEMI_6;
                        when others =>
                            st.Error := UNKNOWN_CSI;
                            parseState := UNDEFINED;
                    end case;

                when CSI_1_SEMI_2 =>
                    case c is
                        --@TODO guessing A and B are ctrl+up/down
                        when 'C' =>
                            st.Kbd_Pressed := KEY_RIGHT;
                            st.Kbd_Modifier.SHIFT := True;
                            parseState := NONE;
                        when 'D' =>
                            st.Kbd_Pressed := KEY_LEFT;
                            st.Kbd_Modifier.SHIFT := True;
                            parseState := NONE;
                        when 'F' =>
                            st.Kbd_Pressed := KEY_END;
                            st.Kbd_Modifier.SHIFT := True;
                            parseState := NONE;
                        when 'H' =>
                            st.Kbd_Pressed := KEY_HOME;
                            st.Kbd_Modifier.SHIFT := True;
                            parseState := NONE;
                        when others =>
                            st.Error := UNKNOWN_CSI;
                            parseState := UNDEFINED;
                    end case;
                
                when CSI_1_SEMI_5 =>
                    case c is
                        --@TODO guessing A and B are ctrl+up/down
                        when 'C' =>
                            st.Kbd_Pressed := KEY_RIGHT;
                            st.Kbd_Modifier.CTRL := True;
                            parseState := NONE;
                        when 'D' =>
                            st.Kbd_Pressed := KEY_LEFT;
                            st.Kbd_Modifier.CTRL := True;
                            parseState := NONE;
                        when others =>
                            st.Error := UNKNOWN_CSI;
                            parseState := UNDEFINED;
                    end case;

                when CSI_1_SEMI_6 =>
                    case c is
                        --@TODO guessing A and B are ctrl+shift+up/down
                        when 'C' =>
                            st.Kbd_Pressed := KEY_RIGHT;
                            st.Kbd_Modifier.SHIFT := True;
                            st.Kbd_Modifier.CTRL := True;
                            parseState := NONE;
                        when 'D' =>
                            st.Kbd_Pressed := KEY_LEFT;
                            st.Kbd_Modifier.SHIFT := True;
                            st.Kbd_Modifier.CTRL := True;
                            parseState := NONE;
                        when others =>
                            st.Error := UNKNOWN_CSI;
                            parseState := UNDEFINED;
                    end case;
                
                when MOUSE_BUTTON =>
                    case c is
                        when '0'..'9' =>
                            -- did we somehow get an extra digit?
                            if mouseNum.Length = MouseNumbers.Max_Length then
                                st.Error := MOUSE_COORD_TOO_BIG;
                                mouseNum := Null_Bounded_String;
                                parseState := UNDEFINED;
                            else
                                mouseNum.Append (c);
                            end if;
                        when ';' =>
                            if mouseNum.Length = 0 then
                                -- if we got ESC[<;
                                st.Error := MOUSE_COORD_TOO_SMALL;
                                mouseNum := Null_Bounded_String;
                                parseState := UNDEFINED;
                            else
                                -- OK, we got a button. Record it and reset
                                -- mouseNum for the X coordinate.
                                mouseButton := Natural'Value (mouseNum.To_String);
                                mouseNum := Null_Bounded_String;
                                parseState := MOUSE_COORD_X;
                            end if;
                        when others =>
                            st.Error := UNKNOWN_CSI;
                            parseState := UNDEFINED;
                    end case;

                when MOUSE_COORD_X =>
                    case c is
                        when '0'..'9' =>
                            -- did we somehow get an extra digit?
                            if mouseNum.Length = MouseNumbers.Max_Length then
                                st.Error := MOUSE_COORD_TOO_BIG;
                                mouseNum := Null_Bounded_String;
                                parseState := UNDEFINED;
                            else
                                mouseNum.Append (c);
                            end if;
                        when ';' =>
                            if mouseNum.Length = 0 then
                                -- if we got ESC[>;
                                st.Error := MOUSE_COORD_TOO_SMALL;
                                mouseNum := Null_Bounded_String;
                                parseState := UNDEFINED;
                            else
                                -- OK, we got a coord. Record it and reset
                                -- mouseNum for the Y coordinate.
                                mouseX := Natural'Value (mouseNum.To_String);
                                mouseNum := Null_Bounded_String;
                                parseState := MOUSE_COORD_Y;
                            end if;
                        when others =>
                            st.Error := UNKNOWN_CSI;
                            parseState := UNDEFINED;
                    end case;

                when MOUSE_COORD_Y =>
                    case c is
                        when '0'..'9' =>
                            -- did we somehow get an extra digit?
                            if mouseNum.Length = MouseNumbers.Max_Length then
                                st.Error := MOUSE_COORD_TOO_BIG;
                                mouseNum := Null_Bounded_String;
                                parseState := UNDEFINED;
                            else
                                mouseNum.Append (c);
                            end if;
                        when 'm' | 'M' =>
                            if mouseNum.Length = 0 then
                                -- if we got ESC[<x;;
                                st.Error := MOUSE_COORD_TOO_SMALL;
                                mouseNum := Null_Bounded_String;
                                parseState := UNDEFINED;
                            else
                                -- OK, we got the y coord. Record it and determine
                                -- button press/release
                                mouseY := Natural'Value (mouseNum.To_String);
                                mouseNum := Null_Bounded_String;

                                case c is
                                    -- we can complete the input now
                                    when 'm' | 'M' =>
                                        if mouseButton >= MOUSE_MOVE_L and mouseButton <= MOUSE_MOVE_H then
                                            st.Mouse_X := mouseX;
                                            st.Mouse_Y := mouseY;
                                        elsif mouseButton = LEFT_CLICK then
                                            if c = 'M' then
                                                st.Mouse_Down := True;

                                                -- measure interval between clicks for double-click
                                                if Ada.Real_Time.Clock <= st.Last_Click + Ada.Real_Time.Milliseconds (500) then
                                                    st.Double_Click := True;
                                                end if;

                                                st.Last_Click := Ada.Real_Time.Clock;
                                            else
                                                st.Mouse_Down := False;
                                                st.Word_Select := False;
                                            end if;
                                        elsif mouseButton = MIDDLE_CLICK then
                                            st.Mouse_Buttons.Button_1 := (c = 'M');
                                        elsif mouseButton = RIGHT_CLICK then
                                            st.Mouse_Buttons.Button_2 := (c = 'M');
                                        elsif mouseButton = WHEEL_UP then
                                            st.Mouse_Buttons.Button_4 := True;
                                        elsif mouseButton = WHEEL_DN then
                                            st.Mouse_Buttons.Button_5 := True;
                                        end if;

                                        parseState := NONE;
                                    when others =>
                                        st.Error := UNKNOWN_MOUSE_PRESS;
                                        parseState := UNDEFINED;
                                end case;
                            end if;
                        when others =>
                            st.Error := UNKNOWN_CSI;
                            parseState := UNDEFINED;
                    end case;

                when others =>
                    st.Error := UNKNOWN_CONTROL_SEQUENCE;
                    parseState := UNDEFINED;
            end case;
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
            TAB      : constant := 9;
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
                when TAB =>
                    st.Kbd_Pressed := KEY_TAB;
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
                    -- st.tooltip := To_Unbounded_String ("key: " & pos'Image);
                    null;
            end case;
        end control;

        use Ada.Real_Time;

        -- When this is called, we'll poll stdin for new inputs.
        now      : Ada.Real_Time.Time := Ada.Real_Time.Clock;
        donePoll : Ada.Real_Time.Time := now + Ada.Real_Time.Milliseconds (20);
        -- escSequence : Unbounded_String := Null_Unbounded_String;
    begin
        st.Kbd_Modifier := (others => FALSE);
        st.Kbd_Pressed  := KEY_NONE;
        st.Kbd_Text     := Ada.Strings.Unbounded.Null_Unbounded_String;
        parseState      := NONE;

        -- If we are in the middle of an escape sequence, finish it before exiting the loop.
        while Ada.Real_Time.Clock < donePoll or
              (parseState /= NONE and parseState /= UNDEFINED) loop
            
            chr := Console.getch;

            if parseState = NONE then
                case chr is
                    when ASCII.NUL =>
                        null;
                        -- early return? bother checking again here?
                        -- might be more responsive if we bug out early.
                    when ASCII.SOH..ASCII.SUB | ASCII.DEL =>
                        parseState := CONTROL;
                        control (chr);
                        parseState := NONE;
                    when ' '..'~' =>
                        parseState := PRINTABLE;
                        printable (chr);
                        parseState := NONE;
                    when ASCII.ESC =>
                        parseState := ESCAPE;

                        -- Already ate the first ESC char in the stream.
                        -- Pass the next char to escape()
                        chr := Console.getch;
                        escape (chr);
                    when others =>
                        null;
                end case;
            elsif parseState = UNDEFINED then
                case chr is
                    when ASCII.NUL =>
                        -- only leave the undefined state if input runs out, then reset.
                        parseState := NONE;
                        st.Error := NO_ERROR;
                    when others =>
                        -- silently discard until input stream empties out.
                        null;
                end case;
            else
                -- resume where we left off in the middle of the escape sequence.
                escape (chr);
            end if;
        end loop;
    end Get_Inputs;

end Gembrowse.UI.Input;
