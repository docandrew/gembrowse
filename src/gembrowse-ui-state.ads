-------------------------------------------------------------------------------
-- gembrowse-ui-state.ads
--
-- In the Immediate Mode GUI paradigm, a minimal amount of state must still be 
-- kept. Instead of duplicating application logic state and UI state and
-- periodically syncing them, in IMGUI we let the application state directly
-- influence the UI state and vice versa. However, there is still some UI state
-- that needs to be kept between rendering+input passes. That's kept here.
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Containers.Vectors;
with Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Colors;

with Gembrowse.UI.Keys;
with Gembrowse.UI.Types; use Gembrowse.UI.Types;

package Gembrowse.UI.State is

    Invalid_Scope_Exception : Exception;

    ----------------------------------------------------------------------------
    -- ID
    -- Unique Widget ID
    ----------------------------------------------------------------------------
    type ID is new Integer;

    NO_ITEM         : constant ID := 0;
    INVALID_ITEM    : constant ID := -1;

    ----------------------------------------------------------------------------
    -- Scopes. A scope is a nesting of GUI elements, perhaps when a set of
    --  elements should be hidden together, or when elements in a sub-window
    --  need information about their parent to know where to draw themselves.
    ----------------------------------------------------------------------------
    MAX_SCOPES      : constant := 256;
    type Scope_ID is range -1 .. MAX_SCOPES;

    NO_SCOPE        : constant Scope_ID := 0;
    INVALID_SCOPE   : constant Scope_ID := -1;

    ----------------------------------------------------------------------------
    -- Last_ID_List
    -- Keep track of last assigned IDs in each scope so we can resume once
    -- out of scope.
    ----------------------------------------------------------------------------
    type Last_ID_List is array (1..Scope_ID'Last) of ID;

    ----------------------------------------------------------------------------
    -- Selection_Direction
    --
    -- If moving the cursor left "grows" the selection, i.e. the cursor is to
    -- the left, vs moving the cursor right "grows" the selection
    ----------------------------------------------------------------------------
    type Selection_Direction is (LEFT, NONE, RIGHT);

    ----------------------------------------------------------------------------
    -- Mouse_Button_State
    -- If a button was pressed during this input/rendering round, the
    -- corresponding field will be True, otherwise False.
    ----------------------------------------------------------------------------
    type Mouse_Button_State is record
        Button_1 : Boolean;
        Button_2 : Boolean;
        Button_3 : Boolean;
        Button_4 : Boolean;
        Button_5 : Boolean;
    end record;

    ----------------------------------------------------------------------------
    -- Error codes that may appear
    ----------------------------------------------------------------------------
    type Error_Code is (
        NO_ERROR,
        UNKNOWN_CONTROL_SEQUENCE,
        UNKNOWN_SS3,
        UNKNOWN_CSI,
        UNKNOWN_CSI_1,
        WANTED_TILDE,
        MOUSE_COORD_TOO_BIG,
        MOUSE_COORD_TOO_SMALL,
        UNKNOWN_MOUSE_PRESS
    );

    ----------------------------------------------------------------------------
    -- UIState
    --
    -- Low-level state of the GUI for rendering and input handling
    --@TODO change tooltip to a regular string.
    --@field Renderer is the renderer for the app window
    --@field Mouse_x is the x-position of the mouse
    --@field Mouse_y is the y-position of the mouse
    --@field Mouse_Down is True if the left mouse button is being held down
    --@field Last_Click is the time the last click occurred, for measuring
    --  double clicks
    --@field Hot_Item is the ID of the widget we're hovering over
    --@field Hot_Scope is the Scope of the widget we're hovering over
    --@field Active_Item is the ID of the widget we've interacted with
    --@field Active_Scope is the Scope of the widget we've interacted with
    --@field Curr_Scope is the current scope we're in
    --@field Scope_X_Offset is set when widgets in this scope should be drawn
    --  (and inputs handled) relative to the scope, perhaps in a sub-window or
    --  widget group.
    --@field Scope_Y_Offset. See Scope_X_Offset.
    --@field Last_IDs is the last ID we generated for a widget in a given scope
    --@field Kbd_Item is the widget with keyboard focus
    --@field Kbd_Scope is the scope of the widget with keyboard focus
    --@field Kbd_Pressed is the key that was pressed
    --@field Kbd_Modifier is shift, ctrl, alt, etc.
    --@field Kbd_Heartbeat lets us know if the previously focused widget was
    --  drawn. Used when de-selecting hidden widgets.
    --@field Kbd_Text is text entered from the keyboard.
    --@field Cursor_Pos is the cursor position within the focused text field.
    --@field Selection_Start is the start position of selected text within the
    --  focused text field.
    --@field Selection_End is the end position of selected text within the 
    --  focused text field.
    --@field Word_Select is True if we just double-clicked to select a word,
    --  and future cursor drags with the mouse button held down should continue
    --  to select words.
    --@field First_Draw_Index is the first character to start rendering within
    --  a text field, if the text exceeds the display width of the text field.
    --@field Last_Blink is the time at which the cursor last switched from
    --  blinking to non-blinking.
    --@field Blink_On is True if the cursor is drawn, False otherwise.
    --@field Last_Widget is the ID of the last widget handled.
    --@field Last_Scope is the scope of the last widget handled.
    --@field Hover_Start is the time at which the mouse stopped moving last.
    --@field Tooltip is set by the hot widget when it has been hovered over,
    --  and then rendered later (so it's on top of other drawn widgets).
    --@field Theme is the currently used color theme.
    --@field Done is set to True if we are exiting the program.
    --@field Window_Width is the width of the window
    --@field Window_Height is the height of the window
    ---------------------------------------------------------------------------
    type UIState is record
        Mouse_x          : Screen_Coordinate;
        Mouse_y          : Screen_Coordinate;
        Mouse_Down       : Boolean := False;
        Double_Click     : Boolean := False;
        Last_Click       : Ada.Real_Time.Time;
        Mouse_Buttons    : Mouse_Button_State;

        Hot_Item         : ID;
        Hot_Scope        : Scope_ID;

        Active_Item      : ID;
        Active_Scope     : Scope_ID;

        Curr_Scope       : Scope_ID := NO_SCOPE;
        Scope_X_Offset   : Screen_Coordinate := 0;
        Scope_Y_Offset   : Screen_Coordinate := 0;

        Last_IDs         : Last_ID_List := (others => NO_ITEM);

        Kbd_Item         : ID := NO_ITEM;
        Kbd_Scope        : Scope_ID := NO_SCOPE;

        Kbd_Pressed      : Gembrowse.UI.Keys.Key_Codes;
        Kbd_Modifier     : Gembrowse.UI.Keys.Key_Modifiers;
        Kbd_Heartbeat    : Boolean := False;
        Kbd_Text         : Ada.Strings.Unbounded.Unbounded_String;

        Cursor_Pos       : Positive := 1;
        Selection_Start  : Positive := 1;
        Selection_End    : Positive := 1;
        Word_Select      : Boolean := False;
        First_Draw_Index : Positive := 1;

        --Selection_Dir   : Select_Direction;
        Last_Blink       : Ada.Real_Time.Time;
        Blink_On         : Boolean := True;

        Last_Widget      : ID;
        Last_Scope       : Scope_ID;

        Hover_Start     : Ada.Real_Time.Time;
        Tooltip         : Unbounded_String;

        Theme           : Colors.Theme;
        Done            : Boolean := False;

        Window_Width    : Screen_Coordinate;
        Window_Height   : Screen_Coordinate;

        Error           : Error_Code := NO_ERROR;
        Error_Msg       : Unbounded_String;
    end record
        with Dynamic_Predicate => (Cursor_Pos = Selection_Start or Cursor_Pos = Selection_End);

    -- NO_KEY : constant SDL.Events.Keyboards.Key_Codes := SDL.Events.Keyboards.Key_Codes(0);

    ---------------------------------------------------------------------------
    -- Enter_Scope
    -- Enter a new Widget scope
    --@field st is the UI state for the application
    --@field Scope_X_Offset should be the x-coordinate for the parent widget,
    --  if new widgets should be positioned relative to the parent. I.e. a
    --  sub-window or dialog box.
    --@field Scope_Y_Offset see Scope_X_Offset.
    ---------------------------------------------------------------------------
    procedure Enter_Scope (st             : in out UIState;
                           Scope_X_Offset : Natural := 0;
                           Scope_Y_Offset : Natural := 0);

    ---------------------------------------------------------------------------
    -- Exit_Scope
    -- Exit a Widget scope
    --@note if a Scope_X_Offset and/or Scope_Y_Offset was provided when this
    -- scope was first entered, they must be provided here.
    ---------------------------------------------------------------------------
    procedure Exit_Scope (st             : in out UIState;
                          Scope_X_Offset : Natural := 0;
                          Scope_Y_Offset : Natural := 0);

    -- Generate the next ID for the current scope
    function Next_ID (st : in out UIState) return ID;

end Gembrowse.UI.State;
