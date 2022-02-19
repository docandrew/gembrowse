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
--  etc. but need a discoverable way to open those tabs.
--
-- Invariants:
-- There will always be an active tab.
-------------------------------------------------------------------------------
with Ada.Assertions; use Ada.Assertions;
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

    ---------------------------------------------------------------------------
    -- PageState
    -- Internal representation of a Gemini page and associated metadata
    ---------------------------------------------------------------------------
    type PageState is record
        title        : Unbounded_String;
        url          : Unbounded_String;
        tlsStatus    : Boolean;
        pageContents : Unbounded_String;
        bookmarked   : Boolean := False;
        startx       : Positive := 1;             -- beginning of page we've scrolled to
        starty       : Positive := 1;
        status       : Unbounded_String;          -- status bar at bottom
    end record;

    package PageStates is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => PageState);
    tabs : PageStates.Vector;

    -- For testing. The actual scrollbar limits will be baked into the PageState.
    scrollx : Natural := 0;
    scrolly : Natural := 0;

    -- fwd declaration
    procedure clearPage (st : in out Gembrowse.UI.State.UIState);
    procedure switchTab (st : in out Gembrowse.UI.State.UIState; tabNum : Positive);

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
            "###    Bookmarks" & ASCII.LF &
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
                                 status        => To_Unbounded_String ("✔️ Loaded 843b in .013s "),
                                 others        => <>);
    begin
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

        -- selectNone;
        -- addressBarState.Active := False;

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
    procedure scrollUp (lines : Positive := 1) is
    begin
        null;
    end scrollUp;

    ---------------------------------------------------------------------------
    -- scrollDown
    -- Scroll the current viewport down by the specified number of lines.
    ---------------------------------------------------------------------------
    procedure scrollDown (lines : Positive := 1) is
    begin
        null;
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
        Console.setCursor (1, PAGE_START);

        for i in PAGE_START .. st.Window_Height - 4 loop
            Console.eraseLine;
            Put (ASCII.LF);
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

        -- Take a look at the URL.
        -- @TODO enforce length < 1024
        if Index (url, " ", 1) /= 0 then
            -- If it has spaces, it's a search term.
            searchTerm := url;
            Gembrowse.URL.percentEncode (searchTerm);
            actualURL := To_Unbounded_String ("gemini://geminispace.info/search?" & To_String (searchTerm));
        elsif Index (url, "file:", 1) = 1 then
            -- try to load local file
            actualURL := url;
            Gembrowse.File.loadLocalFile (To_String (url), tabs(activeTab).pageContents);
            return;
        elsif Index (url, "gemini://") /= 1 then
            -- If it starts with gemini://, great. If not, insert that (keep in mind 1024 char limit).
            actualURL := To_Unbounded_String ("gemini://" & To_String (url));
        else
            actualURL := url;
        end if;

        GUI_State.tooltip := To_Unbounded_String ("Loading " & To_String (actualURL));

        if actualURL.Length > 1024 then
            st.Tooltip := To_Unbounded_String ("URL is too long (max length for Gemini is 1024)");
            return;
        end if;

        if not Gembrowse.net.fetchPage (actualURL, tabs(activeTab).pageContents) then
            tabs(activeTab).status := To_Unbounded_String ("Error loading " & To_String (actualURL));
        end if;
    end loadPage;

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
        if Button (st, 3, 4, "⬅️", "Back (shortcut: backspace)", Colors.currentTheme.bg, Colors.currentTheme.ui, 0, 2) then
            null;
        end if;
        
        if Button (st, 6, 4, "➡️", "Forward", Colors.currentTheme.bg, Colors.currentTheme.ui, 0, 2) then
            null;
        end if;

        if Button (st, 9, 4, "🔄", "Reload Page (shortcut: F5)", Colors.currentTheme.bg, Colors.currentTheme.ui, 0, 2) then
            null;
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
                   Colors.currentTheme.ui, Colors.currentTheme.note, 0, 2) then
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

        type LineType is (Plain, Link, H1, H2, H3, Preformat, UnorderedList, Quote);

        -----------------------------------------------------------------------
        -- isWhite
        -- Return True if character is whitespace, False otherwise.
        -----------------------------------------------------------------------
        function isWhite (c : Character) return Boolean is
        begin
            return (c = ' ' or c = ASCII.HT or c = ASCII.LF or c = ASCII.CR or c = ASCII.LF);
        end isWhite;

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
                when Link =>
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
        -- nextLineType
        -- Given the page contents and the current render index, look ahead and
        -- determine the next type of line.
        -----------------------------------------------------------------------
        function nextLineType (ubs : Unbounded_String; idx : Natural) return LineType is
            i : Natural := idx;
            chr1 : Character;
            chr2 : Character;
            chr3 : Character;
        begin
            -- assert (Element (ubs, i) = ASCII.CR or Element (ubs, i) = ASCII.LF);

            i := i + 1;
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
                return Link;
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
        -- Given a text/gemini line containing a link, determine the URL and
        -- user-friendly description. If no user-friendly description is
        -- provided, desc will contain the URL.
        -- @param idx - start of the line containing the link, will be set to
        --  the end of the link line after this procedure call.
        -----------------------------------------------------------------------
        procedure parseLink (line : Unbounded_String; idx : in out Natural; url : out Unbounded_String; desc : out Unbounded_String) is
            -- urlStart : Natural;
            -- urlEnd : Natural;
            -- descStart : Natural;
            -- descEnd : Natural;
        begin
            null;
            -- loop
            -- end loop;
            -- if Length (desc) = 0 then
            --    desc := url;
            -- end if;
        end parseLink;

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

        procedure skipFormatting (lt : LineType; i : in out Natural) is
        begin
            -- Depending on line type, we'll skip over the formatting chars.
            case lt is 
                when Plain =>
                    i := i + 1;     -- skip LF
                when Link =>
                    i := i + 3;     -- skip LF, =>
                    skipWhitespace (tabs(activeTab).pageContents, i);

                    Console.underlineOff;
                    Put ("🔗 ");
                    Console.underlineOn;
                    -- =>[ws]URL[ws]User-Friendly-Name
                    -- i := i + 1; -- skip LF
                    -- parseLink (tabs(activeTab).pageContents, i, url, desc);
                when H1 =>
                    i := i + 2;     -- skip LF and #
                    skipWhitespace (tabs(activeTab).pageContents, i);
                when H2 =>
                    i := i + 3;     -- skip LF and ##
                    skipWhitespace (tabs(activeTab).pageContents, i);
                when H3 =>
                    i := i + 4;     -- skip LF and ###
                    skipWhitespace (tabs(activeTab).pageContents, i);
                when UnorderedList =>
                    i := i + 3;     -- skip LF and '* '
                    Put ("• ");
                when Quote =>
                    i := i + 2;   -- skip LF and >
                when Preformat =>
                    preformatting := not preformatting;
                    i := i + 4; -- skip LF and ```
            end case;
        end skipFormatting;

        -- Current viewport coordinates.
        vx : Natural := 3;
        vy : Natural := 7;

        w : Natural := st.Window_Width;
        h : Natural := st.Window_Height;

        -- index into page contents and char at that index
        i : Natural := 0;
        c : Character;

        -- type of line being rendered
        curLineType : LineType;

        use Gembrowse.UI.Buttons;
    begin
        Console.setBGColor (Colors.currentTheme.bg);
        Console.setColor (Colors.currentTheme.ui);

        -- left vert border
        for y in 6..h-3 loop
            Console.setCursor (1, y);
            Put ("║");
        end loop;

        -- right vert border
        for y in 6..h-3 loop
            Console.setCursor (w, y);
            Put ("║");
        end loop;

        if Vertical_Scrollbar (st => st,
                               x => w - 1,
                               y => 6,
                               Height => h - 10,
                               Min => 0,
                               Max => 100,
                               Val => scrolly) then
            null;
        end if;

        -- horiz scrollbar
        if Horizontal_Scrollbar (st => st,
                                 x => 2,
                                 y => h-3,
                                 Width => w-4,
                                 Min => 0,
                                 Max => 100,
                                 Val => scrollx) then
            null;
        end if;

        -- make it look a little nicer.
        Console.setCursor (w-1, h-3);
        Put ("█");

        Console.setCursor (vx, vy);
        
        if Length(tabs(activeTab).pageContents) = 0 then
            return;
        end if;

        -- Need to open up a new scope here since certain widgets may be hidden
        -- within the page itself.
        Enter_Scope (st);

        -- Determine initial line type.
        curLineType := nextLineType (tabs(activeTab).pageContents, i);
        setStyle (curLineType);
        skipFormatting (curLineType, i);

        -- This is a bit inefficent. We process the entire page and then determine whether
        -- the material being rendered is within the current viewport.
        loop
            exit when i > Length (tabs(activeTab).pageContents);

            c := Element (tabs(activeTab).pageContents, i);

            if c = ASCII.LF or c = ASCII.CR then
                vx := 3;
                vy := vy + 1;
                Console.setCursor (vx, vy);

                curLineType := nextLineType (tabs(activeTab).pageContents, i);

                setStyle (curLineType);
                skipFormatting (curLineType, i);
            else
                -- if this character is outside of our current viewport, don't render it.
                -- @TODO sensible line breaks (get length of next word, see if it will exceed viewport)
                if vx > w - 2 or vy > h - 5 then
                    null;
                else
                    Put (c);
                end if;
                i := i + 1;
            end if;
        end loop;

        Exit_Scope (st);

        setStyle (Plain);
    end renderPage;

    ---------------------------------------------------------------------------
    -- footer
    -- Render footer and status bar
    ---------------------------------------------------------------------------
    procedure footer (st : in out Gembrowse.UI.State.UIState) is
        w : Natural := st.Window_Width;
        h : Natural := st.Window_Height;
    begin
        box (1, h-2, w, h, "╠", "╣", "╚", "╝");

        Console.setCursor (2, h-1);
        Console.setBGColor (Colors.currentTheme.bg);
        Console.setColor (Colors.currentTheme.note);
        if Length (st.tooltip) > 0 then
            Put (To_String (st.tooltip));

            for i in Length (st.tooltip) .. w - 4 loop
                Put (" ");
            end loop;
        else
            Put (To_String (tabs(activeTab).status));
            for i in Length (tabs(activeTab).status) .. w - 4 loop
                Put (" ");
            end loop;
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
            if scrolly > 0 then
                scrolly := scrolly - 10;
            end if;
        elsif st.Mouse_Buttons.Button_5 then
            if scrolly < 100 then
                scrolly := scrolly + 10;
            end if;
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
                -- consume the text so we don't input it.
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
        newTab (GUI_State);

        Console.hideCursor;
        Console.rawMode;
        Console.enableMouse;

        Console.setTitle ("Gembrowse 💎");
        Console.setBGColor (Colors.currentTheme.bg);
        Console.clear;

        Console.termSize (GUI_State.Window_Width, GUI_State.Window_Height);

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