with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C; use Interfaces.C;

with Colors;
with Console;

with Gembrowse.UI.Input; use Gembrowse.UI.Input;
with Gembrowse.UI.Keys;
with Gembrowse.UI.State;

package body Gembrowse.UI.Text_Field is

    ---------------------------------------------------------------------------
    -- Is_Word_End
    --
    -- For ctrl+click, ctrl+arrow and double-click whole-word expansion
    ---------------------------------------------------------------------------
    function Is_Word_End (Char : Character) return Boolean
    is
        use ASCII;
    begin
        return  Char = ' ' or Char = '.' or Char = '\' or Char = '_' or 
                Char = '/' or Char = CR or Char = LF or Char = HT;
    end Is_Word_End;

    ---------------------------------------------------------------------------
    -- Get_Selection_Direction
    --
    -- Determine which direction a selection is based on its start, end, and the
    --  current cursor position.
    ---------------------------------------------------------------------------
    function Get_Selection_Direction (st : Gembrowse.UI.State.UIState) 
        return Gembrowse.UI.State.Selection_Direction
    is
        use Gembrowse.UI.State;
    begin
        return Dir : Gembrowse.UI.State.Selection_Direction do
            if st.Selection_Start = st.Selection_End then
                Dir := NONE;
            elsif st.Cursor_Pos = st.Selection_Start then
                Dir := LEFT;
            else
                Dir := RIGHT;
            end if;
        end return;
    end Get_Selection_Direction;

    ---------------------------------------------------------------------------
    -- Text_Field_Navigate
    --
    -- Handle intra-field navigation
    -- Return True if the user hit "enter" in this field
    ---------------------------------------------------------------------------
    function Text_Field_Navigate (st   : in out Gembrowse.UI.State.UIState;
                                  Text : in out Ada.Strings.Unbounded.Unbounded_String)
                                  return Boolean
    is
        package UBS renames Ada.Strings.Unbounded;
        
        use Gembrowse.UI.Keys;
        use Gembrowse.UI.State;
        
        Selection_Dir : constant Selection_Direction := Get_Selection_Direction (st);

        -- Used for ctrl+arrow to skip a word
        procedure Skip_Word_Right (st   : in out Gembrowse.UI.State.UIState;
                                   Text : Ada.Strings.Unbounded.Unbounded_String)
        is
            Low     : constant Positive := st.Cursor_Pos;
            High    : constant Positive := UBS.Length (Text);
            Char    : Character;
        begin
            Find_Loop: for i in Low .. High loop
                Char := UBS.Element (Text, i);
                st.Cursor_Pos := i;
                exit Find_Loop when Is_Word_End (Char);
            end loop Find_Loop;

            --st.Cursor_Pos := st.Cursor_Pos + 1;
        end Skip_Word_Right;

        procedure Skip_Word_Left (st   : in out Gembrowse.UI.State.UIState;
                                  Text : Ada.Strings.Unbounded.Unbounded_String)
        is
            High    : constant Positive := st.Cursor_Pos - 1;
            Low     : constant Positive := 2;
            Char    : Character;
        begin
            Find_Loop: for i in reverse Low .. High loop
                Char := UBS.Element (Text, i);
                exit Find_Loop when Is_Word_End (Char);
                st.Cursor_Pos := i;
            end loop Find_Loop;
        end Skip_Word_Left;

    begin
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

                st.Kbd_Pressed := KEY_NONE;

                -- Clear cursor and selection
                st.Cursor_Pos := 1;
                st.Selection_Start := 1;
                st.Selection_End := 1;

            when KEY_ENTER =>

                -- Give up keyboard focus for faster entry in the next field.
                st.Kbd_Item := NO_ITEM;
                st.Kbd_Scope := NO_SCOPE;
                st.Kbd_Pressed := KEY_NONE;

                st.Cursor_Pos := 1;
                st.Selection_Start := 1;
                st.Selection_End := 1;

                return True;

            when KEY_LEFT =>
                if st.Cursor_Pos > 1 then

                    if st.Kbd_Modifier.CTRL then
                        Skip_Word_Left (st, Text);
                    end if;

                    if st.Kbd_Modifier.SHIFT then
                        if Selection_Dir = LEFT or Selection_Dir = NONE then
                            -- grow selection left
                            st.Cursor_Pos       := st.Cursor_Pos - 1;
                            st.Selection_Start  := st.Cursor_Pos;
                        else
                            -- shrink selection from right
                            st.Cursor_Pos       := st.Cursor_Pos - 1;
                            st.Selection_End    := st.Cursor_Pos;
                        end if;
                    else
                        -- no selection
                        st.Cursor_Pos       := st.Cursor_Pos - 1;
                        st.Selection_Start  := st.Cursor_Pos;
                        st.Selection_End    := st.Selection_Start;
                    end if;
                end if;
            
            when KEY_RIGHT =>
                if st.Cursor_Pos <= UBS.Length (Text) then
                    -- if this is a ctrl+click, then we advance the
                    -- cursor to the next word.
                    if st.Kbd_Modifier.CTRL then
                        Skip_Word_Right (st, Text);
                    end if;
                    
                    if st.Kbd_Modifier.SHIFT then
                        if Selection_Dir = RIGHT or Selection_Dir = NONE then
                            -- grow selection right
                            st.Cursor_Pos       := st.Cursor_Pos + 1;
                            st.Selection_End    := st.Cursor_Pos;
                        else
                            -- shrink selection from left
                            st.Cursor_Pos       := st.Cursor_Pos + 1;
                            st.Selection_Start  := st.Cursor_Pos;
                        end if;
                    else
                        -- no selection
                        st.Cursor_Pos       := st.Cursor_Pos + 1;
                        st.Selection_Start  := st.Cursor_Pos;
                        st.Selection_End    := st.Selection_Start;
                    end if;
                end if;

            when KEY_HOME =>
                
                if st.Kbd_Modifier.SHIFT then
                    if Selection_Dir = RIGHT then
                        st.Selection_End    := st.Selection_Start;
                        st.Cursor_Pos       := 1;
                        st.Selection_Start  := st.Cursor_Pos;
                    else
                        st.Selection_End    := st.Cursor_Pos;
                        st.Cursor_Pos       := 1;
                        st.Selection_Start  := st.Cursor_Pos;
                    end if;
                else
                    st.Cursor_Pos           := 1;
                    st.Selection_Start      := st.Cursor_Pos;
                    st.Selection_End        := st.Selection_Start;
                end if;

            when KEY_END =>

                if st.Kbd_Modifier.SHIFT then
                    if Selection_Dir = LEFT then
                        st.Selection_Start  := st.Selection_End;
                        st.Cursor_Pos       := UBS.Length (Text) + 1;
                        st.Selection_End    := st.Cursor_Pos;
                    else
                        st.Selection_Start  := st.Cursor_Pos;
                        st.Cursor_Pos       := UBS.Length (Text) + 1;
                        st.Selection_End    := st.Cursor_Pos;
                    end if;
                else
                    st.Cursor_Pos := UBS.Length (Text) + 1;
                    st.Selection_Start := st.Cursor_Pos;
                    st.Selection_End := st.Selection_Start;
                end if;

            when KEY_BACKSPACE =>
                if st.Selection_Start < st.Selection_End then
                    UBS.Delete (Text, st.Selection_Start, st.Selection_End - 1);
                    st.Cursor_Pos := st.Selection_Start;
                    st.Selection_End := st.Cursor_Pos;
                else
                    -- no selection, so just delete prev character if there
                    -- is one, and move cursor left.
                    if st.Cursor_Pos > 1 then
                        UBS.Delete (Text, st.Cursor_Pos - 1, st.Cursor_Pos - 1);
                        st.Cursor_Pos       := st.Cursor_Pos - 1;
                        st.Selection_Start  := st.Cursor_Pos;
                        st.Selection_End    := st.Selection_Start;
                    end if;
                end if;

            when KEY_DEL =>
                -- If there's a selection, nuke it and make cursor
                -- go to the start.
                if st.Selection_Start < st.Selection_End then
                    UBS.Delete (Text, st.Selection_Start, st.Selection_End - 1);
                    st.Cursor_Pos       := st.Selection_Start;
                    st.Selection_End    := st.Cursor_Pos;
                else
                    -- no selection, so just delete next character if there
                    -- is one.
                    if st.Cursor_Pos <= UBS.Length (Text) then
                        UBS.Delete (Text, st.Cursor_Pos, st.Cursor_Pos);
                        st.Selection_Start  := st.Cursor_Pos;
                        st.Selection_End    := st.Selection_Start;
                    end if;
                end if;

            when others =>
                --Ada.Text_IO.Put_Line("other: " & st.Kbd_Pressed'Image);
                null;
        end case;

        return False;
    end Text_Field_Navigate;

    ---------------------------------------------------------------------------
    -- Text_Field_Edit
    --
    -- Handle characters typed or pasted into this field.
    ---------------------------------------------------------------------------
    procedure Text_Field_Edit (st         : in out Gembrowse.UI.State.UIState;
                               Text       : in out Ada.Strings.Unbounded.Unbounded_String;
                               Max_Length : Natural)
    is
        package UBS renames Ada.Strings.Unbounded;

        Can_Fit     : Integer;
        Select_Len  : constant Natural := st.Selection_End - st.Selection_Start;
        New_Text    : UBS.Unbounded_String;
    begin
        -- determine room to insert/append
        if Max_Length = 0 then
            Can_Fit := UBS.Length (st.Kbd_Text);
        else
            -- If there's a selection that we're about to replace,
            --  then we can add the length of that selection to
            --  the space we have, since it will get wiped out
            --  prior to inserting the new text.
            Can_Fit := Max_Length + Select_Len - UBS.Length (Text);
            
            if Can_Fit < 0 then
                Can_Fit := 0;
            elsif Can_Fit > UBS.Length (st.Kbd_Text) then
                -- Figure out how many chars we can copy here.
                Can_Fit     := UBS.Length (st.Kbd_Text);
                New_Text    := UBS.Unbounded_Slice (Source    => st.Kbd_Text,
                                                    Low       => 1,
                                                    High      => Can_Fit);
            else
                New_Text    := st.Kbd_Text;
            end if;
        end if;

        if Can_Fit > 0 then
            -- If there's a selection, we need to erase it first.
            if st.Selection_Start < st.Selection_End then
                UBS.Delete (Text, st.Selection_Start, st.Selection_End - 1);
                st.Cursor_Pos       := st.Selection_Start;
                st.Selection_End    := st.Cursor_Pos;
            end if;

            if st.Cursor_Pos = UBS.Length (Text) + 1 then
                UBS.Append (Source   => Text,
                            New_Item => New_Text);
                st.Cursor_Pos       := st.Cursor_Pos + Can_Fit;
                st.Selection_Start  := st.Cursor_Pos;
                st.Selection_End    := st.Selection_Start;
            else
                UBS.Insert (Source   => Text,
                            Before   => st.Cursor_Pos,
                            New_Item => UBS.To_String (New_Text));
                st.Cursor_Pos       := st.Cursor_Pos + Can_Fit;
                st.Selection_Start  := st.Cursor_Pos;
                st.Selection_End    := st.Selection_Start; 
            end if;

        end if;

        st.Kbd_Text := UBS.Null_Unbounded_String;
    end Text_Field_Edit;

    -- Draw a single line, editable text field
    function Text_Field (st              : in out Gembrowse.UI.State.UIState;
                         Text            : in out Ada.Strings.Unbounded.Unbounded_String;
                         x               : Natural;
                         y               : Natural;
                         Display_Length  : Natural := 20;
                         Max_Length      : Natural := 20) return Boolean
    is
        package UBS renames Ada.Strings.Unbounded;
        use Gembrowse.UI.State;

        id      : constant Gembrowse.UI.State.ID := Gembrowse.UI.State.Next_ID (st);
        scope   : constant Gembrowse.UI.State.Scope_ID := st.Curr_Scope;
        
        -- Size (in pixels) of the field. @TODO: Need to scale this by ppi
        -- Field_Width     : constant Positive := SDL.Positive_Dimension(Gembrowse.UI.Settings.Font_Size) * 
        --                                                         SDL.Positive_Dimension(Display_Length);
        -- Field_Height    : constant Positive := 2 * SDL.Positive_Dimension(Gembrowse.UI.Settings.Font_Size);
        
        -- How far inside the field to start drawing characters
        Text_Draw_Offset : constant Positive := 1;
        
        Cursor_Click_Update : Boolean := False;
        Cursor_Drag_Update : Boolean := False;

        Hit_Enter : Boolean;

        -- w : Natural;
        -- h : Natural;
    begin

        -- Check for mouseover/click
        if Region_Hit(st, x, y, x + Display_Length, y) then
            st.Hot_Item := id;
            st.Hot_Scope := scope;

            -- Update our cursor to the little text dealie
            -- SDL.Inputs.Mice.Cursors.Set_Cursor(Gembrowse.UI.State.Text_Cursor);

            -- Clicking on a text field gives us keyboard focus, also 
            --  updates the cursor location.
            if st.Active_Item = NO_ITEM and st.Mouse_Down then
                st.Active_Item  := id;
                st.Active_Scope := scope;
            
                st.Kbd_Item     := id;
                st.Kbd_Scope    := Scope;

                -- Since we got here via a click, we can eventually set the cursor
                --  to the click location. We don't know the details of text rendering
                --  yet in this part of the function, so we'll make a point to
                --  update the cursor later.
                Cursor_Click_Update := True;

                st.Cursor_Pos       := UBS.Length(Text) + 1;
                st.Selection_Start  := st.Cursor_Pos;
                st.Selection_End    := st.Selection_Start;
            end if;

        end if;

        -- if no widget has keyboard focus, take it. 
        if st.Kbd_Item = NO_ITEM then
            st.Kbd_Item     := id;
            st.Kbd_Scope    := Scope;
            
            --We now own the cursor and selection within the UIState, so can
            -- set it to the end of the field.
            st.Cursor_Pos       := UBS.Length(Text) + 1;
            st.Selection_Start  := st.Cursor_Pos;
            st.Selection_End    := st.Selection_Start;
        end if;

        -- Render background first, paint over it later.
        -- if we have keyboard focus, show it and update heartbeat
        if st.Kbd_Item = id and st.Kbd_Scope = Scope then
            Console.setCursor (x, y);
            Console.setBGColor (Colors.currentTheme.editorLine);
            for i in 1 .. Display_Length loop
                Put (" ");
            end loop;

            st.Kbd_Heartbeat := True;
        else
            Console.setCursor (x, y);
            Console.setBGColor (Colors.currentTheme.bg);
            for i in 1 .. Display_Length loop
                Put (" ");
            end loop;
        end if;

        -- If we are still active, then update cursor based on mouse position
        if st.Active_Item = id and st.Active_Scope = Scope and st.Mouse_Down
            and not Cursor_Click_Update then
            Cursor_Drag_Update := True;
        end if;

        Draw_Chars : declare
            --@TODO Clip the number of characters by what we can actually display.
            -- take cursor index into account, maybe something like this:
            -- First_Draw_Index    : Natural;
            -- End_Draw_Index      : Natural;

            Text_x          : Positive := x + Text_Draw_Offset;
            Cursor_x        : Positive := x + Text_Draw_Offset;
            Text_BG_Color   : Colors.ThemeColor;
            Text_FG_Color   : Colors.ThemeColor;
            Selection_Dir   : Selection_Direction := Get_Selection_Direction (st);
        begin
            Console.setCursor (Text_x, y);

            if not (st.Kbd_Item = id and st.Kbd_Scope = scope) then
                -- if we aren't active, then just render the text in one fell swoop
                Console.setBGColor (Colors.currentTheme.editorLine);
                Console.setColor (Colors.currentTheme.ui);
                Put (To_String (Text));
            else
                if UBS.Length(Text) > 0 then
                    -- render character-by-character so we can apply
                    -- correct styles to selection and get accurate cursor
                    -- placement.
                    for i in 1 .. UBS.Length(Text) loop
                        
                        -- invert colors for selection
                        if i >= st.Selection_Start and i < st.Selection_End then
                            Console.setBGColor (Colors.currentTheme.ui);
                            Console.setColor (Colors.currentTheme.editorLine);
                        else
                            Console.setBGColor (Colors.currentTheme.editorLine);
                            Console.setColor (Colors.currentTheme.ui);
                        end if;

                        if st.Cursor_Pos = i and st.Blink_On then
                            Console.underlineOn;
                            Put (UBS.Element (Text, i));
                            Console.underlineOff;
                        else
                            Put (UBS.Element (Text, i));
                        end if;

                        -- Determine cursor position.
                        -- Note this was originally written for variable-width fonts
                        -- where we don't know the X position of the cursor ahead of time.
                        -- Probably overkill for a TUI where we can just do simple arithmetic
                        -- to determine the cursor draw position.
                        --
                        -- if we just clicked here, and we just rendered a character
                        -- whose width takes us past where we clicked, then update
                        -- the cursor position to that character. We need to do
                        -- this here, because otherwise we don't know the position
                        -- each character is drawn at.
                        if Cursor_Click_Update and Text_x >= st.Mouse_x then
                            --Ada.Text_IO.Put_Line ("click" & i'Image);
                            st.Cursor_Pos := i;
                            st.Selection_Start := st.Cursor_Pos;
                            st.Selection_End := st.Selection_Start;

                            Cursor_Click_Update := False;
                        elsif Cursor_Drag_Update and Text_x >= st.Mouse_x and not st.Word_Select then
                            --@TODO handle expansion of selection when st.Word_Select is on.
                            --Ada.Text_IO.Put_Line ("drag" & i'Image);
                            st.tooltip := To_Unbounded_String ("Dragging" & i'Image & st.Mouse_x'Image);
                            -- if dragging the mouse, set cursor and adjust the
                            -- selection based on direction.
                            if i > st.Cursor_Pos then
                                --Ada.Text_IO.Put_Line ("left");
                                -- moved cursor left
                                if Selection_Dir = LEFT then
                                    -- grow selection left
                                    st.Cursor_Pos := i;
                                    st.Selection_Start := st.Cursor_Pos;
                                else
                                    -- shrink selection from right
                                    st.Cursor_Pos := i;
                                    st.Selection_End := st.Cursor_Pos;
                                end if;
                            elsif i < st.Cursor_Pos then
                                --Ada.Text_IO.Put_Line ("right");
                                -- moved cursor right
                                if Selection_Dir = RIGHT then
                                    -- grow selection right
                                    st.Cursor_Pos := i;
                                    st.Selection_End := st.Cursor_Pos;
                                else
                                    -- shrink selection from left
                                    st.Cursor_Pos := i;
                                    st.Selection_Start := st.Cursor_Pos;
                                end if;
                            else
                                -- moved cursor none
                                null;
                            end if;

                            Cursor_Drag_Update := False;
                        end if;

                        -- Update the cursor x value when appropriate
                        if st.Cursor_Pos = i then
                            Cursor_x := Text_x;
                        end if;

                        Text_x := Text_x + 1;
                    end loop;

                    -- re-draw cursor position if at end.
                    if st.Cursor_Pos = UBS.Length (Text) + 1 then
                        if st.Blink_On then
                            Console.underlineOn;
                            Put (" ");
                            Console.underlineOff;
                        else
                            Put (" ");
                        end if;
                    end if;

                    -- If we haven't updated the Cursor by this point, it
                    --  was to the right of the text we just rendered.
                    if Cursor_Click_Update then
                        --Ada.Text_IO.Put_Line("click2 at end");
                        st.Cursor_Pos := UBS.Length (Text) + 1;
                        st.Selection_Start := st.Cursor_Pos;
                        st.Selection_End := st.Selection_Start;

                        Cursor_Click_Update := False;
                    end if;

                    if Cursor_Drag_Update and not st.Word_Select then
                        --Ada.Text_IO.Put_Line("drag2 at end");
                        st.Cursor_Pos := UBS.Length (Text) + 1;
                        st.Selection_End := st.Cursor_Pos;
                        
                        Cursor_Drag_Update := False;
                    end if;

                    -- If the cursor is at the end, we won't see it's "character"
                    --  during the above loop. Set it here.
                    if st.Cursor_Pos = UBS.Length (Text) + 1 then
                        Cursor_x := Text_x;
                    end if;
                else
                    -- no text, just draw a cursor
                    if st.Blink_On then
                        Console.underlineOn;
                        Put (" ");
                        Console.underlineOff;
                    end if;
                end if;

                -- Expand selection if double clicked.
                if st.Double_Click and UBS.Length (Text) > 0 then
                    Select_Word: declare
                        Char : Character;
                    begin
                        --Ada.Text_IO.Put_Line("double click");
                        -- st.Selection_Start := 1;
                        -- st.Selection_End := UBS.Length (Text);
                        if st.Cursor_Pos < UBS.Length (Text) + 1 then
                            Expand_Start: for i in reverse 1 .. st.Cursor_Pos loop
                                Char := UBS.Element (Text, i);

                                exit Expand_Start when Is_Word_End (Char);
                                st.Selection_Start := i;
                                --Ada.Text_IO.Put_Line("Expand start: " & i'Image);
                            end loop Expand_Start;
                        end if;

                        if st.Cursor_Pos > 1 then
                            Expand_End: for i in st.Cursor_Pos .. UBS.Length (Text) loop
                                Char := UBS.Element (Text, i);
                                
                                exit Expand_End when Is_Word_End (UBS.Element (Text, i));
                                st.Selection_End := i + 1;
                                --Ada.Text_IO.Put_Line(" expand end: " & i'Image);
                            end loop Expand_End;
                        end if;

                        --Ada.Text_IO.Put_Line(" setting cursor: " & st.Selection_End'Image);
                        st.Cursor_Pos := st.Selection_End;
                        st.Word_Select := True;
                    end Select_Word;
                end if;
            end if; -- end "if active"
        end Draw_Chars;

        if st.Kbd_Item = id and st.Kbd_Scope = Scope then
            Hit_Enter := Text_Field_Navigate (st, Text);
   
            if Hit_Enter then
                st.Last_Widget := id;
                st.Last_Scope := Scope;

                return True;
            end if;

            if UBS.Length (st.Kbd_Text) /= 0 then
                Text_Field_Edit (st, Text, Max_Length);
            end if;
        end if;

        st.Last_Widget := id;
        st.Last_Scope := Scope;

        return False;
    end Text_Field;

end Gembrowse.UI.Text_Field;
