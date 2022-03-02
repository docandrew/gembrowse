-------------------------------------------------------------------------------
-- gembrowse-ui.adb
--
-- Copyright 2022 Jon Andrew
--
-- @TODO - make the UI elements a little fancier with being "Hot" vs "Active"
-- and highlight accordingly.
-- @TODO - figure out how to divide keyboard focus between address bar and page, etc.
-- @TODO - arrow keys for moving between tabs
-- @TODO - viewport / scrollbars work
-- @TODO - menu? plan is to just have separate pages for help, bookmarks,
--  etc. but need a discoverable way to open those tabs. Maybe we just have
--  New Tab show that stuff.
--
-- Invariants:
-- There will always be an active tab.
-------------------------------------------------------------------------------
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Interrupts; use Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with Ada.Real_Time;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;

with Colors;
with Console;
with Util;

with Gembrowse.Bookmarks;
with Gembrowse.File;
with Gembrowse.Net; use Gembrowse.Net;

with Gembrowse.UI.Buttons; use Gembrowse.UI.Buttons;
with Gembrowse.UI.Input; use Gembrowse.UI.Input;
with Gembrowse.UI.Keys;
with Gembrowse.UI.State; use Gembrowse.UI.State;
with Gembrowse.UI.Scrollbars; use Gembrowse.UI.Scrollbars;
with Gembrowse.UI.Text_Field; use Gembrowse.UI.Text_Field;

with Gembrowse.URL;

package body Gembrowse.UI is

    ADDRESS_BAR_ROW : constant := 4;
    PAGE_START      : constant := 6;

    activeTab   : Natural := 1;

    -- Length of time to render a frame.
    frameTime   : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (1);

    GUI_State : Gembrowse.UI.State.UIState;

    highlightAddressBar : Boolean := False;

    -- History
    package Histories is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Unbounded_String);

    ---------------------------------------------------------------------------
    -- PageState
    -- Internal representation of a Gemini page and associated metadata
    ---------------------------------------------------------------------------
    type PageState is record
        title        : Unbounded_String;
        url          : Unbounded_String;
        tlsStatus    : Boolean;
        header       : Unbounded_String;    -- server response header
        pageContents : Unbounded_String;
        bookmarked   : Boolean := False;
        scrollx      : Positive := 1;       -- (may disable this functionality)
        scrolly      : Positive := 1;       -- beginning of page we've scrolled to
        lines        : Natural := 1;        -- number of lines in document
        cols         : Natural := 1;        -- widest number of columns of document
        status       : Unbounded_String;    -- status bar at bottom
        history      : Histories.List;      -- per-tab history
        historyIdx   : Histories.Cursor;    -- where in the history chain we are
        cursorPos    : Positive := 1;       -- save the cursor position in the address bar.
    end record;

    package PageStates is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => PageState);
    tabs : PageStates.Vector;

    -- For testing. The actual scrollbar limits will be baked into the PageState.
    -- scrollx : Natural := 0;
    -- scrolly : Natural := 0;

    -- fwd declarations
    procedure clearPage (st : in out Gembrowse.UI.State.UIState);
    procedure switchTab (st : in out Gembrowse.UI.State.UIState; tabNum : Positive);
    procedure historyNew (st : in out Gembrowse.UI.State.UIState; url : Unbounded_String);

    -----------------------------------------------------------------------
    -- isWhite
    -- Return True if character is whitespace, False otherwise.
    -----------------------------------------------------------------------
    function isWhite (c : Character) return Boolean is
    begin
        return (c = ' ' or c = ASCII.HT or c = ASCII.LF or c = ASCII.CR or c = ASCII.LF);
    end isWhite;

    -----------------------------------------------------------------------
    -- getNextWordLength
    -- Get the length of the next word in an Unbounded_String 
    -- (to decide if we need to break the line or not). 
    -- @param i is the index into the Unbounded_String prior to the word
    -- whose length we're calculating.
    -----------------------------------------------------------------------
    function getNextWordLength (ubs : Unbounded_String; i : Natural) return Natural is
        len : Natural := 0;
    begin
        for x in i + 1 .. ubs.Length loop
            exit when isWhite (ubs.Element(x));

            len := len + 1;
        end loop;

        return len;
    end getNextWordLength;

    -----------------------------------------------------------------------
    -- getLines
    -- Return the number of lines in a document taking into account
    -- the viewport width and splitting lines on word breaks. We need this
    -- to know the vertical scrollbar size.
    -----------------------------------------------------------------------
    function getLines (ubs : Unbounded_String; w : Positive) return Natural is
        i : Natural := 1;
        lines : Natural := 0;
        c : Character := ASCII.NUL;
        nextWordLength : Natural;
        col : Positive := 1;
    begin
        loop
            exit when i > ubs.Length;
            c := ubs.Element(i);

            if c = ASCII.LF or c = ASCII.CR then
                lines := @ + 1;
                col   := 3;
            elsif c = ' ' or c = ASCII.HT then
                nextWordLength := getNextWordLength (ubs, i);

                if nextWordLength > w - 3 then
                    -- next word will need to be split
                    lines := @ + 1;
                    col   := 1;
                elsif col + nextWordLength > w - 3 then
                    lines := @ + 1;
                    col   := 1;
                else
                    col := @ + 1;
                end if;
            else
                -- normal char
                col := @ + 1;
            end if;

            i := i + 1;
        end loop;

        return lines;
    end getLines;

    ---------------------------------------------------------------------------
    -- newTab
    ---------------------------------------------------------------------------
    procedure newTab (st : in out Gembrowse.UI.State.UIState) is
        newTabContents : String := 
            "# Gembrowse - Gemini Browser by Jon Andrew" & ASCII.LF &
            "" & ASCII.LF &
            "Press Ctrl+q to exit" & ASCII.LF &
            ASCII.LF &
            "##  Features" & ASCII.LF &
            "" & ASCII.LF &
            "* LibreSSL" & ASCII.LF &
            "* Written in Ada" & ASCII.LF &
            "* Mouse and Keyboard Friendly" & ASCII.LF &
            ASCII.LF &
            "=> file://" & Gembrowse.Bookmarks.getBookmarkPath & " Bookmarks" & ASCII.LF &
            ASCII.LF &
            "###    Links" & ASCII.LF &
            ASCII.LF &
            "=> gemini://docandrew.com/gembrowse Homepage" & ASCII.LF &
            ASCII.LF &
            "> This is a quote" & ASCII.LF &
            "> This is another quote" & ASCII.LF;

        use PageStates;

        newState : PageState := (title         => To_Unbounded_String ("New Tab"),
                                 url           => To_Unbounded_String ("New Tab"),
                                 tlsStatus     => True,
                                 pageContents  => To_Unbounded_String (newTabContents),
                                 status        => To_Unbounded_String (""),
                                 others        => <>);
    begin
        newState.lines := getLines (newState.pageContents, st.Window_Width);
        tabs.Append (newState);
        
        highlightAddressBar := True;

        switchTab (st, tabs.Last_Index);

    end newTab;

    ---------------------------------------------------------------------------
    -- switchTab
    ---------------------------------------------------------------------------
    procedure switchTab (st : in out Gembrowse.UI.State.UIState; tabNum : Positive) is
    begin
        clearPage (st);

        -- Force a re-render
        st.Page_Dirty    := True;
        st.Address_Dirty := True;
        
        if activeTab <= Natural(tabs.Length) then
             -- save old cursor for current tab if we didn't just close it
            tabs(activeTab).cursorPos := st.Cursor_Pos;
        end if;

        st.Selection_Start := 1;                        -- deselect any selected text.
        st.Selection_End   := 1;
        st.Cursor_Pos      := tabs(tabNum).cursorPos;   -- restore cursor position for new tab

        activeTab := tabNum;
    end switchTab;

    ---------------------------------------------------------------------------
    -- helpTab
    -- Would like to have a page showing shortcuts, etc.
    ---------------------------------------------------------------------------
    procedure helpTab is
    begin
        null;
    end helpTab;

    ---------------------------------------------------------------------------
    -- closeTab
    -- Note that we always have to have a tab open, so if this closes the last
    -- tab, go ahead and create a new one.
    ---------------------------------------------------------------------------
    procedure closeTab (st : in out Gembrowse.UI.State.UIState; num : Positive) is
        wasLast : Boolean;
    begin
        wasLast := (num = tabs.Last_Index);

        tabs.Delete (num);

        if tabs.Is_Empty then
            newTab (st);
            return;
        end if;

        -- if we deleted the last tab, and it was active, we need to adjust the
        -- active tab.
        if num = activeTab and wasLast then
            switchTab (st, tabs.Last_Index);
        end if;
    end closeTab;

    ---------------------------------------------------------------------------
    -- scrollUp
    -- Scroll the current viewport up by the specified number of lines.
    ---------------------------------------------------------------------------
    procedure scrollUp (st : in out Gembrowse.UI.State.UIState; lines : Positive := 1) is
    begin
        if tabs(activeTab).scrolly > 0 + lines then
            tabs(activeTab).scrolly := @ - lines;
        else
            tabs(activeTab).scrolly := 1;
        end if;

        st.Page_Dirty := True;
    end scrollUp;

    ---------------------------------------------------------------------------
    -- scrollDown
    -- Scroll the current viewport down by the specified number of lines.
    ---------------------------------------------------------------------------
    procedure scrollDown (st : in out Gembrowse.UI.State.UIState; lines : Positive := 1) is
    begin
        if tabs(activeTab).scrolly < tabs(activeTab).lines - lines then
            tabs(activeTab).scrolly := @ + lines;
        else
            tabs(activeTab).scrolly := tabs(activeTab).lines;
        end if;

        st.Page_Dirty := True;
    end scrollDown;

    ---------------------------------------------------------------------------
    -- scrollLeft
    -- Scroll the current viewport left by the specified number of columns.
    ---------------------------------------------------------------------------
    procedure scrollLeft (columns : Positive := 1) is
    begin
        null;
    end scrollLeft;

    ---------------------------------------------------------------------------
    -- scrollRight
    -- Scroll the current viewport right by the specified number of columns.
    ---------------------------------------------------------------------------
    procedure scrollRight (columns : Positive := 1) is
    begin
        null;
    end scrollRight;

    ---------------------------------------------------------------------------
    -- box
    -- Render a double-bordered box around an area.
    ---------------------------------------------------------------------------
    procedure box (x1, y1, x2, y2 : Natural;
                   topLeft  : String;
                   topRight : String;
                   botLeft  : String;
                   botRight : String) is
    begin
        Console.setColor (Colors.currentTheme.ui);

        Console.setCursor (x1, y1);
        Put (topLeft);

        -- top horiz line
        for x in x1+1..x2-1 loop
            Put ("═");
        end loop;

        -- left vert line
        for y in y1+1..y2-1 loop
            Console.setCursor (x1, y);
            Put ("║");
        end loop;

        Console.setCursor (x2, y1);
        Put (topRight);

        -- right vert line
        for y in y1+1..y2-1 loop
            Console.setCursor (x2, y);
            Put ("║");
        end loop;

        Console.setCursor (x1, y2);
        Put (botLeft);

        -- bottom horiz line
        for x in x1+1..x2-1 loop
            Put ("═");
        end loop;

        Console.setCursor (x2, y2);
        Put (botRight);
    end box;

    ---------------------------------------------------------------------------
    -- erase contents of the current gemini page
    ---------------------------------------------------------------------------
    procedure clearPage (st : in out Gembrowse.UI.State.UIState) is
    begin
        -- erase lines
        Console.setBGColor (Colors.currentTheme.bg);
        Console.setColor (Colors.currentTheme.ui);
        Console.setCursor (1, PAGE_START);

        for i in PAGE_START .. st.Window_Height - 3 loop
            Console.eraseLine;
            Put (ASCII.LF);
        end loop;

        -- left vert border
        for y in 6 .. st.Window_Height - 3 loop
            Console.setCursor (1, y);
            Put ("║");
        end loop;

        -- right vert border
        for y in 6 .. st.Window_Height - 3 loop
            Console.setCursor (st.Window_Width, y);
            Put ("║");
        end loop;

    end clearPage;

    ---------------------------------------------------------------------------
    -- loadPage
    -- Given an address, load that page into the active tab.
    -- @TODO push previous page to history on successful load
    -- @TODO determine bookmark status
    ---------------------------------------------------------------------------
    procedure loadPage (st : in out Gembrowse.UI.State.UIState; url : in out Unbounded_String) is
        searchTerm : Unbounded_String;

        -- we don't want to mess with the actual URL, if a user had a long search
        -- term we should preserve their spaces in the url variable.
        actualURL : Unbounded_String := Null_Unbounded_String;
    begin
        if url.Length = 0 then
            return;
        end if;

        clearPage (st);

        -- Force re-render and scroll up to the top. Clear the selection
        st.Page_Dirty := True;
        
        st.Cursor_Pos := 1;
        st.Selection_End := 1;
        st.Selection_Start := 1;

        tabs(activeTab).scrolly := 1;

        -- Take a look at the URL.
        -- @TODO enforce length < 1024
        if Index (url, " ", 1) /= 0 then
        
            -- If it has spaces, it's a search term.
            searchTerm := url;
            Gembrowse.URL.percentEncode (searchTerm);
            actualURL := To_Unbounded_String ("gemini://geminispace.info/search?" & To_String (searchTerm));
        
        elsif Index (url, "file:", 1) = 1 then
        
            -- try to load local file
            -- @TODO need to set lines, cols for this too.
            actualURL := url;
            Gembrowse.File.loadLocalFile (To_String (url), tabs(activeTab).pageContents);
            tabs(activeTab).url := actualURL;
            tabs(activeTab).lines := getLines (tabs(activeTab).pageContents, st.Window_Width);
            return;

        elsif Index (url, "https://") = 1 or Index (url, "http://") = 1 then
            -- @TODO use xdg-open or something here to open the link in another
            -- browser. Need to give this some thought to make sure we identify
            -- malicious links etc. and not run any scripts or executables.
            null;

        -- @TODO support other link types
        elsif Index (url, "gemini://") /= 1 then

            -- If it starts with gemini://, great. If not, insert that (keep in mind 1024 char limit).
            -- Keep in mind this might be a relative URL.

            -- @TODO handle relative URLs. When we first load a page, need to
            -- parse the full URL and keep the authority with the page. Then
            -- do the relative URI algo to get a full URL from it + relative
            -- link.
            actualURL := To_Unbounded_String ("gemini://" & To_String (url));
        else
            actualURL := url;
        end if;

        tabs(activeTab).url := actualURL;
        GUI_State.tooltip   := To_Unbounded_String ("Loading " & To_String (actualURL));

        if actualURL.Length > 1024 then
            st.Tooltip := To_Unbounded_String ("URL is too long (max length for Gemini is 1024)");
            return;
        end if;

        if not Gembrowse.net.fetchPage (actualURL, 
                                        tabs(activeTab).pageContents,
                                        tabs(activeTab).header) then
            tabs(activeTab).status := To_Unbounded_String ("Error loading " & To_String (actualURL));
            GUI_State.tooltip := tabs(activeTab).status;
        else
            tabs(activeTab).lines := getLines (tabs(activeTab).pageContents, st.Window_Width);
            historyNew (st, actualURL);
        end if;
    end loadPage;

    ---------------------------------------------------------------------------
    -- historyBack
    -- Move back to the previous page in the history chain, if it exists.
    ---------------------------------------------------------------------------
    procedure historyBack (st : in out Gembrowse.UI.State.UIState) is
    begin
        null;
    end historyBack;

    ---------------------------------------------------------------------------
    -- historyForward
    -- Move forward to the next page in the history chain, if it exists.
    ---------------------------------------------------------------------------
    procedure historyForward (st : in out Gembrowse.UI.State.UIState) is
    begin
        null;
    end historyForward;

    ---------------------------------------------------------------------------
    -- historyNew
    -- If we're at the end of our history chain, clicking on a new link or
    -- entering a new URL will just add it to our chain. If we're not at the
    -- end, then we delete all the nodes after our current one and then
    -- append this new one.
    ---------------------------------------------------------------------------
    procedure historyNew (st : in out Gembrowse.UI.State.UIState; url : Unbounded_String) is
    begin
        null;
    end historyNew;

    ---------------------------------------------------------------------------
    -- renderTitle
    -- Render URL and Tabs
    ---------------------------------------------------------------------------
    procedure renderTitle (st : in out Gembrowse.UI.State.UIState) is

        w : Natural := st.Window_Width;
        h : Natural := st.Window_Height;

        procedure renderTabs is
            -- id      : constant Gembrowse.UI.State.ID := Gembrowse.UI.State.Next_ID (st);
            -- scope   : constant Gembrowse.UI.State.Scope_ID := st.Curr_Scope;

            -- tracker for current draw position
            tx : Natural := 2;

            -- keep track of bounds of tab being drawn so we can detect a mouse press
            tx1, ty1, tx2, ty2 : Natural;

            use Util;
        begin

            -- show prev tab button
            if Button (st, 1, 2, "◀", "Previous Tab") then
                if activeTab = tabs.First_Index then
                    switchTab (st, tabs.Last_Index);
                else
                    switchTab (st, activeTab - 1);
                end if;
            end if;

            Console.setCursor (1,3);
            Put ("╔");

            -- @TODO figure out what to do when number of tabs exceeds the
            -- display. Some options are to restrict creation of new tabs once
            -- we reach a limit (but what if we resize smaller?),
            -- put a scrollbar on the tabs, add a placeholder (...)
            -- @TODO make sure title is always at least a character
            for t in tabs.First_Index..tabs.Last_Index loop
                exit when tx >= w - 16;

                tx1 := tx;
                ty1 := 1;

                if t = activeTab then
                    Console.setCursor (tx, 1);
                    Put ("╔");
                    Console.setCursor (tx, 2);
                    Put ("║");
                    Console.setCursor (tx, 3);
                    Put ("╝");
                    tx := tx + 1;

                    for c in 1..min(Length (tabs(t).title), (max (8, Length (tabs(t).title)))) loop
                        Console.setCursor (tx, 1);
                        Put ("═");
                        Console.setCursor (tx, 2);
                        Put (Element(tabs(t).title, c));
                        Console.setCursor (tx, 3);
                        Put (" ");
                        tx := tx + 1;
                    end loop;

                    Console.setCursor (tx, 1);
                    Put ("╗");
                    Console.setCursor (tx, 2);
                    Put ("║");
                    Console.setCursor (tx, 3);
                    Put ("╚");
                    tx := tx + 1;
                else
                    Console.setCursor (tx, 1);
                    Put ("┌");
                    Console.setCursor (tx, 2);
                    Put ("│");
                    Console.setCursor (tx, 3);
                    Put ("╧");
                    tx := tx + 1;

                    for c in 1..min(Length (tabs(t).title), (max (8, Length (tabs(t).title)))) loop
                        Console.setCursor (tx, 1);
                        Put ("─");
                        Console.setCursor (tx, 2);
                        Put (Element(tabs(t).title, c));
                        Console.setCursor (tx, 3);
                        Put ("═");
                        tx := tx + 1;
                    end loop;

                    Console.setCursor (tx, 1);
                    Put ("┐");
                    Console.setCursor (tx, 2);
                    Put ("│");
                    Console.setCursor (tx, 3);
                    Put ("╧");
                    tx := tx + 1;
                end if;

                tx2 := tx - 1;
                ty2 := 3;

                if Region_Hit (st, tx1, ty1, tx2, ty2) then
                    -- Check for click on inactive tab,
                    -- tooltip for tab closure on active tab
                    if st.Mouse_Down and t /= activeTab then
                        switchTab (st, t);
                    elsif t = activeTab then
                        st.Tooltip := To_Unbounded_String ("Close tab: Ctrl+w");
                    end if;
                end if;
                
            end loop;

            tx1 := tx;
            ty1 := 1;

            -- Add a button for new tab
            Console.setCursor (tx, 1);
            Put ("┌");
            Console.setCursor (tx, 2);
            Put ("│");
            Console.setCursor (tx, 3);
            Put ("╧");
            tx := tx + 1;

            Console.setCursor (tx, 1);
            Put ("─");

            if Button (st, tx, 2, "+", "New Tab (shortcut: Ctrl+t)",
                Colors.currentTheme.ui, Colors.currentTheme.bg, 1, 1, 1, 1) then
                newTab (st);
            end if;

            Console.setCursor (tx, 3);
            Put ("═");
            tx := tx + 1;

            Console.setCursor (tx, 1);
            Put ("┐");
            Console.setCursor (tx, 2);
            Put ("│");
            Console.setCursor (tx, 3);
            Put ("╧");
            tx := tx + 1;

            -- tx2 := tx - 1;
            -- ty2 := 3;

            -- draw remainder of horizontal border
            for x in tx .. w-1 loop
                Console.setCursor (x, 1);
                Put (" ");
                Console.setCursor (x, 2);
                Put (" ");
                Console.setCursor (x, 3);
                Put ("═");
            end loop;
            Put ("╗");

            -- show next tab button
            if Button (st, w, 2, "▶", "Next Tab (shortcut: Shift+Tab)") then
                if activeTab = tabs.Last_Index then
                    switchTab (st, tabs.First_Index);
                else
                    switchTab (st, activeTab + 1);
                end if;
            end if;

            -- show exit button
            if Button (st, w, 1, "✖", "Exit Program (shortcut: Ctrl+q)") then
                st.Done := True;
            end if;
        end renderTabs;
    begin
        Console.setColor (Colors.currentTheme.ui);

        renderTabs;

        -- Draw vert border on left and right
        Console.setCursor (1, 4);
        Put ("║");

        Console.setCursor (w, 4);
        Put ("║");

        -- Draw bottom line
        Console.setCursor (1, 5);
        Put ("╠");

        for x in 2..w-1 loop
            Put ("═");
        end loop;

        Put ("╣");

        -- Render navigation buttons
        if Button (st, 3, 4, "⬅️", "Back", Colors.currentTheme.bg, Colors.currentTheme.ui, 0, -2) then
            historyBack (st);
        end if;
        
        if Button (st, 6, 4, "➡️", "Forward", Colors.currentTheme.bg, Colors.currentTheme.ui, 0, -2) then
            historyForward (st);
        end if;

        if Button (st, 9, 4, "🔄", "Reload Page (shortcut: F5)", Colors.currentTheme.bg, Colors.currentTheme.ui, 0, -2) then
            loadPage (st, tabs(activeTab).url);
        end if;
        
        Console.setBGColor (Colors.currentTheme.bg);
        Put (" ");

        -- if updated cert (no TOFU) or bad TLS version
        -- Console.setBGColor (currentTheme.warning.r, currentTheme.warning.g, currentTheme.warning.b);
        -- Put ("🔓");

        -- if self-signed cert
        -- Console.setBGColor (currentTheme.caution.r, currentTheme.caution.g, currentTheme.caution.b);
        -- Put ("🔒");
        
        -- if CA-signed cert
        if Button (st, 12, 4, "🔐", "Self-signed certificate (Trusted on First-Use)",
                   Colors.currentTheme.ui, Colors.currentTheme.note, 0, -2) then
            null;                   
        end if;

        if Gembrowse.UI.Text_Field.Text_Field (st, tabs(activeTab).url, 14, 4, w - 14, w - 14, highlightAddressBar) then
            loadPage (st, tabs(activeTab).url);
        end if;

        highlightAddressBar := False;

        -- if Bookmarked, say so.
        if tabs(activetab).bookmarked then
            if Button (st, w-2, 4, "🔖", "Remove bookmark (shortcut: Ctrl+d)",
                       Colors.currentTheme.ui, Colors.currentTheme.note, 0, 1) then
                --@TODO remove bookmark
                null;
            end if;
        else
            if Button (st, w-2, 4, "📑", "Add bookmark (shortcut: Ctrl+d)",
                       Colors.currentTheme.ui, Colors.currentTheme.note, 0, 1) then
                --@TODO add bookmark
                null;
            end if;
        end if;
    end renderTitle;

    ---------------------------------------------------------------------------
    -- renderPage
    -- Render current page contents.
    -- @TODO how do we 
    ---------------------------------------------------------------------------
    procedure renderPage (st : in out Gembrowse.UI.State.UIState) is
        preformatting : Boolean := False;

        type LineType is (Plain, Link, ExternalLink, FileLink, H1, H2, H3, Preformat, UnorderedList, Quote);

        -----------------------------------------------------------------------
        -- setStyle
        -- Given the type of line, set rendering style accordingly.
        -----------------------------------------------------------------------
        procedure setStyle (ln : LineType) is
        begin
            case ln is
                when Plain =>
                    Console.underlineOff;
                    -- Console.boldOff;
                    Console.italicsOff;
                    Console.setBGColor (Colors.currentTheme.bg);
                    Console.setColor (Colors.currentTheme.fg);
                when Link | ExternalLink | FileLink =>
                    --@TODO determine whether a link is visited or not
                    Console.underlineOn;
                    -- Console.boldOff;
                    Console.italicsOff;
                    Console.setBGColor (Colors.currentTheme.bg);
                    Console.setColor (Colors.currentTheme.unvisitedLink);
                when H1 =>
                    Console.underlineOff;
                    -- Console.boldOn;
                    Console.italicsOff;
                    Console.setBGColor (Colors.currentTheme.bg);
                    Console.setColor (Colors.currentTheme.h1);
                when H2 =>
                    Console.underlineOff;
                    -- Console.boldOn;
                    Console.italicsOff;
                    Console.setBGColor (Colors.currentTheme.bg);
                    Console.setColor (Colors.currentTheme.h2);
                when H3 =>
                    Console.underlineOff;
                    -- Console.boldOff;
                    Console.italicsOff;
                    Console.setBGColor (Colors.currentTheme.bg);
                    Console.setColor (Colors.currentTheme.h3);
                when Preformat =>
                    Console.underlineOff;
                    -- Console.boldOff;
                    Console.italicsOff;
                    Console.setBGColor (Colors.currentTheme.bgPreformat);
                    Console.setColor (Colors.currentTheme.fgPreformat);
                when UnorderedList =>
                    Console.underlineOff;
                    -- Console.boldOff;
                    Console.italicsOff;
                    Console.setBGColor (Colors.currentTheme.bg);
                    Console.setColor (Colors.currentTheme.fgList);
                when Quote =>
                    Console.underlineOff;
                    -- Console.boldOff;
                    Console.italicsOn;
                    Console.setBGColor (Colors.currentTheme.bgQuote);
                    Console.setColor (Colors.currentTheme.fgQuote);
            end case;
        end setStyle;

        -----------------------------------------------------------------------
        -- skipWhitespace
        -- advance a cursor while whitespace exists.
        -----------------------------------------------------------------------
        procedure skipWhitespace (ubs : Unbounded_String; i : in out Natural) is
            c : Character;
        begin
            loop
                c := Element (ubs, i);
                
                exit when i > Length (ubs);
                exit when not isWhite (c);

                i := i + 1;
            end loop;
        end skipWhitespace;

        -----------------------------------------------------------------------
        -- nextLineType
        -- Given the page contents and the current render index, look ahead and
        -- determine the next type of line.
        -----------------------------------------------------------------------
        function nextLineType (ubs : Unbounded_String; idx : Natural) return LineType is
            i : Natural := idx;
            chr1 : Character := ASCII.NUL;
            chr2 : Character := ASCII.NUL;
            chr3 : Character := ASCII.NUL;
        begin
            if i <= Length (ubs) then
                chr1 := Element (ubs, i);
            end if;

            i := i + 1;
            if i <= Length (ubs) then
                chr2 := Element (ubs, i);
            end if;

            i := i + 1;
            if i <= Length (ubs) then
                chr3 := Element (ubs, i);
            end if;

            -- Put ("[" & chr1 & chr2 & chr3 & "] ");

            if chr1 = '=' and chr2 = '>' then
                skipWhitespace (ubs, i);

                --@TODO need to do a legitimate URL parse here to determine the link type
                declare
                    restOfLink : Unbounded_String := ubs.Unbounded_Slice (i, ubs.Length);
                begin
                    -- Put_Line (Standard_Error, " restOfLink: " & restOfLink.To_String);

                    if restOfLink.Index(".") = 1 or
                       restOfLink.Index("/") = 1 or
                       restOfLink.Index("gemini") = 1 then
                        return Link;
                    elsif restOfLink.Index("file") = 1 then
                        return FileLink;
                    else
                        return ExternalLink;
                    end if;
                end;
            elsif chr1 = '#' and chr2 = '#' and chr3 = '#' then
                return H3;
            elsif chr1 = '#' and chr2 = '#' then
                return H2;
            elsif chr1 = '#' then
                return H1;
            elsif chr1 = '`' and chr2 = '`' and chr3 = '`' then
                return Preformat;
            elsif chr1 = '>' then
                return Quote;
            elsif chr1 = '*' and chr2 = ' ' then
                return UnorderedList;
            else
                return Plain;
            end if;
        end nextLineType;

        -----------------------------------------------------------------------
        -- parseLink
        -- Given a text/gemini document containing a link, determine the URL and
        -- user-friendly description. If no user-friendly description is
        -- provided, desc will contain the URL.
        -- @param idx - start of the line containing the link, will be set to
        --  the end of the link line after this procedure call.
        -----------------------------------------------------------------------
        procedure parseLink (ubs  : Unbounded_String;
                             idx  : in out Natural; 
                             url  : out Unbounded_String; 
                             desc : out Unbounded_String) is

            tmpURL  : Unbounded_String := Null_Unbounded_String;
            tmpDesc : Unbounded_String := Null_Unbounded_String;
        begin
            -- we've already skipped the initial whitespace (if any).
            -- expect the next sequence of chars to be the URL.
            loop
                exit when idx > ubs.Length;
                exit when isWhite (Element (ubs, idx));

                tmpURL.Append (Element (ubs, idx));
                idx := idx + 1;
            end loop;

            skipWhitespace (ubs, idx);

            loop
                exit when idx > ubs.Length;
                exit when Element (ubs, idx) = ASCII.LF or Element (ubs, idx) = ASCII.CR;

                tmpDesc.Append (Element (ubs, idx));
                idx := idx + 1;
            end loop;

            if tmpDesc.Length = 0 then
                tmpDesc := tmpURL;
            end if;

            url  := tmpURL;
            desc := tmpDesc;
        end parseLink;

        -----------------------------------------------------------------------
        -- skipFormatting
        -- We don't want to render the extra markup for stuff like headers,
        -- links, etc., so skip over those chars here. This advances the
        -- page index/cursor (@param i)
        -----------------------------------------------------------------------
        procedure skipFormatting (lt : LineType; i : in out Natural; vx : in out Natural) is
        begin
            -- Depending on line type, we'll skip over the formatting chars.
            case lt is 
                when Plain =>
                    null;
                when Link =>
                    -- =>[ws]URL[ws]User-Friendly-Name
                    i := i + 2;     -- skip =>
                    skipWhitespace (tabs(activeTab).pageContents, i);

                    Console.underlineOff;
                    Put ("🔗 ");
                    Console.underlineOn;
                    vx := 6;
                when ExternalLink =>
                    i := i + 2;     -- skip =>
                    skipWhitespace (tabs(activeTab).pageContents, i);

                    Console.underlineOff;
                    Put ("🌐 ");
                    Console.underlineOn;
                    vx := 6;
                when FileLink =>
                    i := i + 2;     -- skip =>
                    skipWhitespace (tabs(activeTab).pageContents, i);

                    Console.underlineOff;
                    Put ("📁 ");
                    Console.underlineOn;
                    vx := 6;
                when H1 =>
                    i := i + 1;     -- skip #
                    skipWhitespace (tabs(activeTab).pageContents, i);
                when H2 =>
                    i := i + 2;     -- skip ##
                    skipWhitespace (tabs(activeTab).pageContents, i);
                when H3 =>
                    i := i + 3;     -- skip ###
                    skipWhitespace (tabs(activeTab).pageContents, i);
                when UnorderedList =>
                    i := i + 2;     -- skip '* '
                    Put ("• ");
                    vx := 5;
                when Quote =>
                    i := i + 1;     -- skip >
                when Preformat =>
                    preformatting := not preformatting;
                    i := i + 3;     -- skip ```
                    -- @TODO: there can be a description after ```, we want
                    --  to not render that but make it a tooltip if hovered.
            end case;
        end skipFormatting;

        -----------------------------------------------------------------------
        -- viewport dimensions
        -----------------------------------------------------------------------
        VIEWPORT_STARTX : constant := 3;
        VIEWPORT_STARTY : constant := 7;
        
        w : constant Natural := st.Window_Width;
        h : constant Natural := st.Window_Height;

        VIEWPORT_ENDX : constant Natural := w - 4;
        VIEWPORT_ENDY : constant Natural := h - 4;

        -----------------------------------------------------------------------
        -- put
        -- Put the next character and advance the viewport and document 
        -- coordinates. If we're at the far right edge of the viewport, then
        -- advance to next line. Otherwise, just print the character.
        -- This does _not_ advance the index into the page itself.
        -----------------------------------------------------------------------
        procedure Put (c : Character; dx, dy, vx, vy : in out Natural) is
            w : Natural := st.Window_Width;
            h : Natural := st.Window_Height;
        begin
            if vx > VIEWPORT_ENDX then
                vx := 3;
                vy := vy + 1;
                Console.setCursor (vx, vy);
            end if;
            
            if vy >= VIEWPORT_ENDY then
                return;
            else
                Put (c);
                vx := vx + 1;
                dx := dx + 1;
            end if;
        end Put;

        -----------------------------------------------------------------------
        -- renderLine
        -- Determine style of the current line and render it appropriately.
        -----------------------------------------------------------------------
        procedure renderLine (ubs : Unbounded_String; i : in out Natural; dx, dy, vx, vy : in out Natural) is
            curLineType : LineType;
            c : Character := ASCII.NUL;
            nextWordLength : Natural;
        begin
            -- What kind of line is this?
            curLineType := nextLineType (tabs(activeTab).pageContents, i);
            setStyle (curLineType);
            skipFormatting (curLineType, i, vx);

            -- If the next line is a link, then instead of printing it
            -- character by character, we'll render a Button whose label is
            -- either the href or the description (if one is provided).
            if curLineType = Link or 
               curLineType = ExternalLink or
               curLineType = FileLink then

                renderLink: declare
                    href : Unbounded_String := Null_Unbounded_String;
                    desc : Unbounded_String := Null_Unbounded_String;
                begin
                    parseLink (tabs(activeTab).pageContents, i, href, desc);

                    -- clip button by display width (taking into account the
                    --  width of the little logo we use).
                    if desc.Length > w - 8 then
                        desc := Unbounded_Slice (desc, 1, w - 8);
                    end if;

                    if Button (st      => st,
                                x       => vx,
                                y       => vy,
                                label   => desc.To_String,
                                tooltip => href.To_String,
                                fg      => Colors.currentTheme.unvisitedLink,
                                bg      => Colors.currentTheme.bg) then
                        
                        -- Exit_Scope (st);
                        setStyle (Plain);

                        loadPage (st, href);
                        return;
                    end if;
                end renderLink;
            else
                -- Render until the end of the line.
                loop
                    exit when i > ubs.Length;

                    c := ubs.Element(i);
                    exit when c = ASCII.LF or c = ASCII.CR;

                    if c = ' ' or c = ASCII.HT then
                        -- At a space or tab. Determine how long the next word is. If
                        -- it's longer than the viewport width, then just go ahead and
                        -- keep rendering it and let the default character handling 
                        -- (which wraps at viewport edge) do its thing. If the word is shorter
                        -- than the viewport width, but its too long to render within the
                        -- viewport, then insert a manual line break.
                        nextWordLength := getNextWordLength (tabs(activeTab).pageContents, i);

                        if nextWordLength > w - 4 then
                            -- word is longer than the viewport, so just accept default Put behavior
                            -- (overflow to next line). Adjust the number of lines in the
                            -- document when we do this.
                            Put (c, dx, dy, vx, vy);
                        elsif nextWordLength + vx > w - 4 then
                            -- ignore the space and just start the word at the next line.
                            vy := vy + 1;
                            vx := VIEWPORT_STARTX;
                            Console.setCursor (vx, vy);
                        else
                            -- print the space (print tabs as spaces too for now).
                            Put (' ', dx, dy, vx, vy);
                        end if;
                    else
                        -- normal char
                        Put (c, dx, dy, vx, vy);
                    end if;

                    i := i + 1;
                end loop;
            end if;

            -- skip the LF
            i := i + 1;

            -- advance document coords
            dy := dy + 1;
            dx := 1;

            -- advance viewport coords to next line.
            vx := 3;
            vy := vy + 1;
            Console.setCursor (vx, vy);
        end renderLine;

        -----------------------------------------------------------------------
        -- invisibleLine
        -- advance document coordinates by one line
        -----------------------------------------------------------------------
        procedure invisibleLine (ubs : Unbounded_String; i : in out Natural; dx, dy : in out Natural) is
            c : Character := ASCII.NUL;
        begin
            loop
                exit when i > ubs.Length;
                c := ubs.Element(i);
                exit when c = ASCII.LF or c = ASCII.CR;

                i := i + 1;
            end loop;

            -- skip the LF
            i := i + 1;

            -- advance document coords to next line
            dy := dy + 1;
            dx := 1;
        end invisibleLine;

        -- Current viewport coordinates.
        vx : Natural := VIEWPORT_STARTX;
        vy : Natural := VIEWPORT_STARTY;

        -- Document coordinates. i.e. the 1st character would be at
        -- dx, dy = (1,1). If we get a LF, then dy := dy + 1; Each character we
        -- print on the same line makes dx := dx + 1;
        dx : Natural := 1;
        dy : Natural := 1;

        -- index into page contents
        i : Natural := 1;

        use Gembrowse.UI.Buttons;
    begin

        Console.setBGColor (Colors.currentTheme.bg);
        Console.setColor (Colors.currentTheme.ui);

        -- left vert border
        for y in 6 .. h - 3 loop
            Console.setCursor (1, y);
            Put ("║");
        end loop;

        -- right vert border
        for y in 6 .. h - 3 loop
            Console.setCursor (w, y);
            Put ("║");
        end loop;

        if Vertical_Scrollbar (st     => st,
                               x      => w - 1,
                               y      => 6,
                               Height => h - 9,
                               Min    => 1,
                               Max    => tabs(activeTab).lines,
                               Val    => tabs(activeTab).scrolly) then
            st.Page_Dirty := True;
        end if;

        -- not implemented yet, may never.
        -- if Horizontal_Scrollbar (st    => st,
        --                          x     => 2,
        --                          y     => h - 3,
        --                          Width => w - 4,
        --                          Min   => 1,
        --                          Max   => tabs(activeTab).lines,
        --                          Val   => tabs(activeTab).scrollx) then
        --     st.Page_Dirty := True;
        -- end if;

        -- No need to re-render the page if nothing changed on it.
        if st.Page_Dirty then
            clearPage (st);
        end if;

        -- add square between horiz and vert scrollbars to make it look a little nicer.
        -- Console.setCursor (w - 1, h - 3);
        -- Put ("█");
        
        if tabs(activeTab).pageContents.Length = 0 then
            return;
        end if;

        -- start rendering at these viewport coords
        Console.setCursor (vx, vy);

        -- Need to open up a new scope here since certain widgets may be hidden
        -- within the page itself.
        Enter_Scope (st);

        renderLoop: loop
            exit when i > tabs(activeTab).pageContents.Length;
            exit when vy >= VIEWPORT_ENDY;

            if dy >= tabs(activeTab).scrolly then
                renderLine (tabs(activeTab).pageContents, i, dx, dy, vx, vy);
            else
                invisibleLine (tabs(activeTab).pageContents, i, dx, dy);
            end if;

        end loop renderLoop;

        Exit_Scope (st);

        setStyle (Plain);

        -- At the very end of the page, put the line we're on
        Console.setCursor (2, VIEWPORT_ENDY + 1);
        Put ("[" & tabs(activeTab).scrolly'Image & " /" & tabs(activeTab).lines'Image & "]");

        -- Freshly-rendered page
        st.Page_Dirty := False;
    end renderPage;

    ---------------------------------------------------------------------------
    -- footer
    -- Render footer and status bar
    ---------------------------------------------------------------------------
    procedure footer (st : in out Gembrowse.UI.State.UIState) is
        w : Natural := st.Window_Width;
        h : Natural := st.Window_Height;

        FOOTER_LINE : constant Natural := h - 1;
    begin
        Console.setCursor (2, FOOTER_LINE);
        Console.eraseLine;

        box (1, h-2, w, h, "╠", "╣", "╚", "╝");

        Console.setCursor (2, FOOTER_LINE);
        Console.setBGColor (Colors.currentTheme.bg);
        Console.setColor (Colors.currentTheme.note);

        if Length (st.tooltip) > 0 then

            if st.tooltip.Length > w - 4 then
                Put (To_String (st.tooltip)(1 .. w - 4));
            else
                Put (To_String (st.tooltip));
            end if;
        else
            Put (To_String (tabs(activeTab).status));
        end if;

    end footer;

    ---------------------------------------------------------------------------
    -- Signal handler for SIGWINCH - when the window is resized we need to
    -- query window size and re-render.
    ---------------------------------------------------------------------------
    protected body Sigwinch_Handler is
        procedure handle is
        begin
            Console.disableMouse;
            Console.termSize (GUI_State.Window_Width, GUI_State.Window_Height);
            Console.enableMouse;
            Console.setBGColor (Colors.currentTheme.bg);
            Console.clear;
            
            -- We have to recalculate the number of lines in each document
            -- when this happens as well.
            for tab of tabs loop
                tab.lines := getLines (tab.pageContents, GUI_sTATE.Window_Width);
            end loop;
        end handle;
    end Sigwinch_Handler;

    ---------------------------------------------------------------------------
    -- renderCursor
    -- draw a little + under the mouse pointer. Maybe make this an option,
    -- the regular mouse pointer works fine. This might be nice if not in an
    -- xterm.
    ---------------------------------------------------------------------------
    procedure renderCursor (st : Gembrowse.UI.State.UIState) is
    begin
        Console.setCursor (st.Mouse_x, st.Mouse_y);
        Console.setColor (Colors.currentTheme.fgSelected);
        Put ("᛭");
    end renderCursor;

    ---------------------------------------------------------------------------
    -- start a rendering iteration
    ---------------------------------------------------------------------------
    procedure startRender (st : in out Gembrowse.UI.State.UIState) is
    begin
        st.Hot_Item := Gembrowse.UI.State.NO_ITEM;
        st.Last_IDs := (others => Gembrowse.UI.State.NO_ITEM);

        Gembrowse.UI.State.Enter_Scope (st);
    end startRender;

    ---------------------------------------------------------------------------
    -- finish a rendering iteration
    ---------------------------------------------------------------------------
    procedure finishRender (st : in out Gembrowse.UI.State.UIState) is
        use Ada.Real_Time;
        use Gembrowse.UI.State;
        Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
    begin
        -- Handle timing for cursor blink
        if Now > st.Last_Blink + Milliseconds (400) then
            st.Blink_On := not (st.Blink_On);
            st.Last_Blink := Now;
        end if;

        -- If mouse isn't down, clear the active item.
        -- If mouse is clicked with no widget active, mark no active item (-1) so we
        -- don't "click" on something if we release the button on top of another
        -- widget.
        if st.Mouse_Down = False then
            --Ada.Text_IO.Put_Line("Setting active to NO_ITEM");
            st.Active_Item := NO_ITEM;
        else
            if st.Active_Item = NO_ITEM then
                --Ada.Text_IO.Put_Line("Setting active to INVALID_ITEM");
                st.Active_Item := INVALID_ITEM;
            end if;
        end if;

        st.Double_Click := False;

        -- If we finished a round and the heartbeat wasn't updated because a
        -- widget wasn't drawn, then remove the focus. If we don't do this, the
        -- hidden widget will keep the focus forever.
        if st.Kbd_Heartbeat = False then
            st.Kbd_Item  := NO_ITEM;
            st.Kbd_Scope := NO_SCOPE;
        end if;

        -- Reset heartbeat, pressed key, button presses and any text inputs that took place
        st.Kbd_Heartbeat := False;
        st.Kbd_Pressed   := Gembrowse.UI.Keys.KEY_NONE;
        st.Kbd_Text := Ada.Strings.Unbounded.Null_Unbounded_String;
        st.Mouse_Buttons := (others => False);

        -- Reset tooltip if necessary. We don't want to switch the tooltip on and
        -- off if the hot item is going to remain.
        -- If we leave here with no hot item (meaning that the
        -- mouse left a widget if it was there previously), then we'll
        -- turn the tooltip off. If it remains hot, we'll leave it
        -- to whatever it was set to by the widget that may have set it last.
        if st.Hot_Item = NO_ITEM then
            -- @TODO turn this back on when done debugging
            -- null;
            st.Tooltip := Null_Unbounded_String;
        end if;

        Gembrowse.UI.State.Exit_Scope (st);

        -- when we finish, we should be back to the initial scope
        if st.Curr_Scope /= NO_SCOPE then
            raise Gembrowse.UI.State.Invalid_Scope_Exception with "Called finishRender with unclosed scopes";
        end if;
    end finishRender;

    ---------------------------------------------------------------------------
    -- Global_Inputs
    -- There are some input types (like mouse wheel, F5, Ctrl+t) that we'd
    -- like to handle outside of rendering a particular widget. We do that here.
    ---------------------------------------------------------------------------
    procedure Global_Inputs (st : in out Gembrowse.UI.State.UIState) is
        use Gembrowse.UI.Keys;
    begin
        -- mouse wheel
        if st.Mouse_Buttons.Button_4 then
            scrollUp (st, 5);
        elsif st.Mouse_Buttons.Button_5 then
            scrollDown (st, 5);
        end if;

        -- Ctrl key combos
        if Length (st.Kbd_Text) > 0 then
            -- Ctrl+t
            if st.Kbd_Modifier.CTRL and Element (st.Kbd_Text, 1) = 't' then
                newTab (st);

                -- consume the 't' so nobody else gets it.
                st.Kbd_Text := Null_Unbounded_String;
            end if;
        end if;

        if Length (st.Kbd_Text) > 0 then
            -- Ctrl+w
            if st.Kbd_Modifier.CTRL and Element (st.Kbd_Text, 1) = 'w' then
                closeTab (st, activeTab);

                st.Kbd_Text := Null_Unbounded_String;
            end if;
        end if;

        if Length (st.Kbd_Text) > 0 then
            -- Ctrl+l
            if st.Kbd_Modifier.CTRL and Element (st.Kbd_Text, 1) = 'l' then
                highlightAddressBar := True;
                -- consume the text so we don't input it again.
                st.Kbd_Text := Null_Unbounded_String;
            end if;
        end if;

        -- Shift+Tab - move to next tab
        if st.Kbd_Modifier.SHIFT and st.Kbd_Pressed = KEY_TAB then
            if activeTab = tabs.Last_Index then
                switchTab (st, tabs.First_Index);
            else
                switchTab (st, activeTab + 1);
            end if;
        end if;

        -- Ctrl+Shift+Tab - move to previous tab
        -- if st.Kbd_Modifier.CTRL and st.Kbd_Modifer.SHIFT and st.Kbd_Pressed = KEY_TAB then
        --     if activeTab = tabs.First_Index then
        --         activeTab := tabs.Last_Index;
        --     else
        --         activeTab := activeTab - 1;
        --     end if;
        -- end if;

        -- F5
        if st.Kbd_Pressed = KEY_F5 then
            loadPage (st, tabs(activeTab).url);
            st.Kbd_Pressed := KEY_NONE;
        end if;
    end Global_Inputs;

    ---------------------------------------------------------------------------
    -- renderLoop
    -- Main drawing and input handling loop
    ---------------------------------------------------------------------------
    procedure renderLoop is
        startTime  : Ada.Real_Time.Time;
        nextPeriod : Ada.Real_Time.Time;

        -- oldw, oldh : Natural;
        use Ada.Real_Time;
    begin

        Console.hideCursor;
        Console.rawMode;
        Console.enableMouse;

        Console.setTitle ("Gembrowse 💎");
        Console.setBGColor (Colors.currentTheme.bg);
        Console.clear;

        Console.termSize (GUI_State.Window_Width, GUI_State.Window_Height);

        newTab (GUI_State);

        loop
            startTime := Ada.Real_Time.Clock;
            nextPeriod := startTime + Ada.Real_Time.Milliseconds (33);     -- 30 fps

            Console.setBGColor (Colors.currentTheme.bg);

            Global_Inputs (GUI_State);

            startRender (GUI_State);
            renderTitle (GUI_State);
            renderPage (GUI_State);
            footer (GUI_State);
            finishRender (GUI_State);

            Get_Inputs (GUI_State);

            delay until nextPeriod;

            exit when GUI_State.Done;
        end loop;

        Gembrowse.UI.Input.killInputs := True;
        Console.disableMouse;
        Console.normalMode;
        Console.showCursor;
    end renderLoop;

end Gembrowse.UI;