-------------------------------------------------------------------------------
-- gembrowse-ui.adb
--
-- Copyright 2022 Jon Andrew
--
-- @TODO - make the UI elements a little fancier with being "Hot" vs "Active"
-- and highlight accordingly.
-- @TODO - arrow keys for moving between tabs
-- @TODO - viewport / scrollbars work
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

with Console;

package body Gembrowse.UI is

    mousex, mousey : Natural := 1;
    click       : Boolean := False;
    activeTab   : Natural := 1;

    quit : Boolean := False;

    -- Length of time to render a frame.
    frameTime   : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (1);

    -- Address bar selection
    editingURL  : Boolean := False;
    cursorPos   : Natural := 0;
    selectStart : Natural;
    selectEnd   : Natural;
    appending   : Boolean := False;

    showTooltip : Boolean := True;
    tooltip : Unbounded_String := To_Unbounded_String ("Welcome to Gembrowse - Copyright 2022 Jon Andrew");

    w,h : Natural := 0;

    -- debugMode : Boolean := True;

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

    ---------------------------------------------------------------------------
    -- Theming (color schemes)
    ---------------------------------------------------------------------------
    type ThemeColor is record
        r : Natural;
        g : Natural;
        b : Natural;
    end record;

    type Theme is record
        bg              : ThemeColor;       -- screen background
        fg              : ThemeColor;       -- normal text
        ui              : ThemeColor;       -- UI elements (borders, buttons)
        editorLine      : ThemeColor;       -- editable text fields
        bgSelected      : ThemeColor;       -- bg for highlighted text
        fgSelected      : ThemeColor;       -- fg for highlighted text
        h1              : ThemeColor;       -- gmi h1
        h2              : ThemeColor;       -- gmi h2
        h3              : ThemeColor;       -- gmi h3
        visitedLink     : ThemeColor;
        unvisitedLink   : ThemeColor;
        bgQuote         : ThemeColor;
        fgQuote         : ThemeColor;
        bgPreformat     : ThemeColor;
        fgPreformat     : ThemeColor;
        bgList          : ThemeColor;
        fgList          : ThemeColor;
        note            : ThemeColor;
        caution         : ThemeColor;
        warning         : ThemeColor;
    end record;

    ayuMirage : Theme := (
        bg              => (16#24#, 16#29#, 16#36#),
        fg              => (16#CC#, 16#CA#, 16#C2#),
        ui              => (16#70#, 16#7A#, 16#8C#),
        editorLine      => (16#1A#, 16#1F#, 16#29#),
        bgSelected      => (16#40#, 16#9F#, 16#FF#),
        fgSelected      => (16#CC#, 16#CA#, 16#C2#),
        h1              => (16#FF#, 16#DF#, 16#B3#),
        h2              => (16#FF#, 16#AD#, 16#66#),
        h3              => (16#F2#, 16#87#, 16#79#),
        visitedLink     => (16#DF#, 16#BF#, 16#FF#),
        unvisitedLink   => (16#73#, 16#D0#, 16#FF#),
        bgQuote         => (16#1A#, 16#1F#, 16#29#),
        fgQuote         => (16#B8#, 16#CF#, 16#E6#),
        bgPreformat     => (16#24#, 16#29#, 16#36#),
        fgPreformat     => (16#FF#, 16#D1#, 16#73#),
        bgList          => (16#24#, 16#29#, 16#36#),
        fgList          => (16#D5#, 16#FF#, 16#80#),
        note            => (16#95#, 16#E6#, 16#CB#),
        caution         => (16#FF#, 16#AD#, 16#66#),
        warning         => (16#F2#, 16#87#, 16#79#)
    );

    ayuLight : Theme := (
        bg              => (16#FC#, 16#FC#, 16#FC#),
        fg              => (16#5C#, 16#61#, 16#66#),
        ui              => (16#8A#, 16#91#, 16#99#),
        editorLine      => (16#F3#, 16#F4#, 16#F5#),
        bgSelected      => (16#03#, 16#5B#, 16#D6#),
        fgSelected      => (16#4C#, 16#61#, 16#66#),
        h1              => (16#E6#, 16#BA#, 16#7E#),
        h2              => (16#FA#, 16#8D#, 16#3E#),
        h3              => (16#F0#, 16#70#, 16#71#),
        visitedLink     => (16#A3#, 16#7A#, 16#CC#),
        unvisitedLink   => (16#39#, 16#9E#, 16#E6#),
        bgQuote         => (16#8A#, 16#91#, 16#99#),
        fgQuote         => (16#78#, 16#7B#, 16#80#),
        bgPreformat     => (16#FC#, 16#FC#, 16#FC#),
        fgPreformat     => (16#F2#, 16#AE#, 16#49#),
        bgList          => (16#FC#, 16#FC#, 16#FC#),
        fgList          => (16#86#, 16#B3#, 16#00#),
        note            => (16#FC#, 16#BF#, 16#99#),
        caution         => (16#FA#, 16#8D#, 16#3E#),
        warning         => (16#FF#, 16#73#, 16#83#)
    );

    currentTheme : Theme := ayuMirage;

    ---------------------------------------------------------------------------
    -- select entire address bar
    ---------------------------------------------------------------------------
    procedure selectAll is
    begin
        editingURL := True;
        cursorPos := 0;
        selectStart := 1;
        selectEnd := Length(tabs(activeTab).url);
    end selectAll;

    ---------------------------------------------------------------------------
    -- deselect entire address bar
    ---------------------------------------------------------------------------
    procedure selectNone is
    begin
        editingURL := False;
        cursorPos := 0;
        selectStart := 0;
        selectEnd := 0;
    end selectNone;

    ---------------------------------------------------------------------------
    -- cursorLeft
    -- If editing the address bar, move the cursor one place to the left
    ---------------------------------------------------------------------------
    procedure cursorLeft is
    begin
        if cursorPos = 1 then
            return;
        elsif cursorPos = 0 then
            -- selection
            cursorPos := selectStart;
            selectEnd := cursorPos;
            tooltip := To_Unbounded_String ("left A");
        else
            cursorPos := cursorPos - 1;
            selectStart := cursorPos;
            selectEnd := cursorPos;
            tooltip := To_Unbounded_String ("left B");
        end if;
    end cursorLeft;

    ---------------------------------------------------------------------------
    -- cursorRight
    -- If editing the address bar, move the cursor one place to the right
    ---------------------------------------------------------------------------
    procedure cursorRight is
    begin
        if cursorPos > Length(tabs(activeTab).url) + 1 then
            return;
        elsif cursorPos = 0 then
            -- selection
            cursorPos := selectEnd;
            selectStart := cursorPos;
            selectEnd := cursorPos;
            tooltip := To_Unbounded_String ("right A");
        else
            cursorPos := cursorPos + 1;
            selectStart := cursorPos;
            selectEnd := cursorPos;
            tooltip := To_Unbounded_String ("right B");
        end if;
    end cursorRight;

    ---------------------------------------------------------------------------
    -- loadPage
    -- Given an address, load that page into the active tab.
    -- @TODO push previous page to history
    -- @TODO determine bookmark status
    ---------------------------------------------------------------------------
    procedure loadPage (url : Unbounded_String) is
    begin
        showTooltip := True;
        tooltip := To_Unbounded_String ("Loading " & To_String (url));
        selectNone;
    end loadPage;

    ---------------------------------------------------------------------------
    -- newTab
    ---------------------------------------------------------------------------
    procedure newTab is
        newTabContents : String := 
            "# Gembrowse - Gemini Browser by Jon Andrew" & ASCII.LF &
            "" & ASCII.LF &
            "This is a normal line" & ASCII.LF &
            ASCII.LF &
            "##  Features" & ASCII.LF &
            "" & ASCII.LF &
            "* LibreSSL" & ASCII.LF &
            "* Written in Ada" & ASCII.LF &
            ASCII.LF &
            "###    Links" & ASCII.LF &
            ASCII.LF &
            "=> gemini://docandrew.com/gembrowse Homepage" & ASCII.LF &
            ASCII.LF &
            "> This is a quote" & ASCII.LF &
            "> This is another quote" & ASCII.LF;
    begin
        tabs.Append ((title         => To_Unbounded_String ("New Tab"),
                      url           => To_Unbounded_String ("New Tab"),
                      tlsStatus     => True,
                      pageContents  => To_Unbounded_String (newTabContents),
                      status        => To_Unbounded_String ("✔️ Loaded 843b in .013s "),
                      others        => <>));
        
        activeTab := tabs.Last_Index;

        -- highlight whole address bar without having to click
        selectAll;
    end newTab;

    ---------------------------------------------------------------------------
    -- closeTab
    -- Note that we always have to have a tab open, so if this closes the last
    -- tab, go ahead and create a new one.
    ---------------------------------------------------------------------------
    procedure closeTab (num : Positive) is
        wasLast : Boolean;
    begin
        wasLast := (num = tabs.Last_Index);

        tabs.Delete (num);

        selectNone;

        if tabs.Is_Empty then
            newTab;
            return;
        end if;

        -- if we deleted the last tab, and it was active, we need to adjust the
        -- active tab.
        if num = activeTab and wasLast then
            activeTab := tabs.Last_Index;
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
        Console.setColor (currentTheme.ui.r, currentTheme.ui.g, currentTheme.ui.b);

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
    -- Detect whether the mouse is in a certain region.
    ---------------------------------------------------------------------------
    function regionHit (x1,y1,x2,y2 : Natural) return Boolean is
    begin
        return (mousex >= x1 and mousex <= x2 and mousey >= y1 and mousey <= y2);
    end regionHit;

    ---------------------------------------------------------------------------
    -- renderTitle
    -- Render URL and Tabs
    ---------------------------------------------------------------------------
    procedure renderTitle is

        procedure renderTabs is
            -- moreTabs : Boolean := True;
            
            -- tracker for current draw position
            tx : Natural := 2;

            -- keep track of bounds of tab being drawn so we can detect a mouse press
            tx1, ty1, tx2, ty2 : Natural;

            function max (l,r : Natural) return Natural is
            begin
                return (if l < r then r else l);
            end max;

            function min (l,r : Natural) return Natural is
            begin
                return (if l > r then r else l);
            end min;
        begin

            -- show prev tab button
            Console.setCursor (1, 2);
            Put ("◀");

            -- Check for click in prev tab button
            if regionHit (1,2,1,2) then
                tooltip     := To_Unbounded_String ("Previous Tab");
                showTooltip := True;

                if click then
                    showTooltip := False;
                    activeTab := max (1, activeTab - 1);
                    selectNone;
                    click := False;
                end if;
            end if;

            Console.setCursor (1,3);
            Put ("╔");

            -- Console.setBGColor (currentTheme.ui.r, currentTheme.ui.g, currentTheme.ui.b);
            -- Console.setColor (currentTheme.)

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

                -- Check for click on inactive tab
                if click and regionHit (tx1, ty1, tx2, ty2) then
                    showTooltip := False;
                    activeTab := t;
                    selectNone;
                    click := False;
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
            Console.setCursor (tx, 2);
            Put ("+");
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

            tx2 := tx - 1;
            ty2 := 3;

            -- Check for click in "New Tab (+)"
            if regionHit (tx1, ty1, tx2, ty2) then
                tooltip := To_Unbounded_String ("New Tab (shortcut: Ctrl+t)");
                showTooltip := True;
                if click then
                    newTab;
                    click := False;
                end if;
            end if;

            -- draw remainder of horizontal border
            for x in tx..w-1 loop
                Console.setCursor (x, 1);
                Put (" ");
                Console.setCursor (x, 2);
                Put (" ");
                Console.setCursor (x, 3);
                Put ("═");
            end loop;
            Put ("╗");

            -- show next tab button
            Console.setCursor (w, 2);
            Put ("▶");

            -- Check for click in next tab button
            if regionHit (w,2,w,2) then
                tooltip := To_Unbounded_String ("Next Tab");
                showTooltip := True;
                if click then
                    activeTab := min (Natural (tabs.Length), activeTab + 1);
                    selectNone;
                    click := False;
                end if;
            end if;

            -- show exit button
            Console.setCursor (w, 1);
            Put ("✖");

            if regionHit (w,1,w,1) then
                tooltip := To_Unbounded_String ("Exit program (shortcut: Ctrl+q)");
                showTooltip := True;
                if click then
                    quit := True;
                end if;
            end if;

        end renderTabs;
    begin
        Console.setColor (currentTheme.ui.r, currentTheme.ui.g, currentTheme.ui.b);

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

        -- Render URL bar
        Console.setCursor (3,4);

        Console.setBGColor (currentTheme.ui.r, currentTheme.ui.g, currentTheme.ui.b);
        Console.setColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
        Put ("⬅️");
        
        -- check for hover/click on reload w/ tool-tip
        if regionHit (2, 4, 4, 4) then
            tooltip := To_Unbounded_String("Back (shortcut: backspace)");
            showTooltip := True;
            if click then
                -- reload
                null;
            end if;
        end if;

        Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
        Put (" ");

        Console.setBGColor (currentTheme.ui.r, currentTheme.ui.g, currentTheme.ui.b);
        Console.setColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
        Put ("➡️");

        -- check for hover/click on reload w/ tool-tip
        if regionHit (5, 4, 7, 4) then
            tooltip := To_Unbounded_String("Forward");
            showTooltip := True;
            if click then
                -- reload
                null;
            end if;
        end if;

        Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
        Console.setColor (currentTheme.fg.r, currentTheme.fg.g, currentTheme.fg.b);
        Put (" ");

        Console.setBGColor (currentTheme.ui.r, currentTheme.ui.g, currentTheme.ui.b);
        Put ("🔄");
        
        Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
        Console.setColor (currentTheme.fg.r, currentTheme.fg.g, currentTheme.fg.b);
        Put (" ");

        -- check for hover/click on reload w/ tool-tip
        if regionHit (8, 4, 10, 4) then
            tooltip := To_Unbounded_String("Reload Page (shortcut: F5)");
            showTooltip := True;
            if click then
                -- reload
                null;
            end if;
        end if;

        -- if updated cert (no TOFU) or bad TLS version
        -- Console.setBGColor (currentTheme.warning.r, currentTheme.warning.g, currentTheme.warning.b);
        -- Put ("🔓");

        -- if self-signed cert
        -- Console.setBGColor (currentTheme.caution.r, currentTheme.caution.g, currentTheme.caution.b);
        -- Put ("🔒");
        
        -- if CA-signed cert
        Console.setBGColor (currentTheme.note.r, currentTheme.note.g, currentTheme.note.b);
        Put ("🔐");

        -- check for hover/click on reload w/ tool-tip
        if regionHit (11, 4, 13, 4) then
            tooltip := To_Unbounded_String("Self-signed certificate (Trusted on First-Use)");
            showTooltip := True;
        end if;

        Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
        Console.setColor (currentTheme.fg.r, currentTheme.fg.g, currentTheme.fg.b);
        Put (" ");

        Console.setBGColor (currentTheme.editorLine.r, currentTheme.editorLine.g, currentTheme.editorLine.b);
        Console.setColor (currentTheme.note.r, currentTheme.note.g, currentTheme.note.b);

        -- check for click in address bar.
        -- single-click = highlight whole url
        -- click again = put cursor on a single position.
        -- click yet-again = highlight whole url
        if click and regionHit (14, 4, w - 2, 4) then
            appending  := False;
            editingURL := True;

            if cursorPos /= 0 then
                -- highlight whole bar.
                selectAll;
            else
                -- insert/append
                cursorPos := (if mousex - 14 <= 1 then 1 else mousex - 14);
                selectStart := cursorPos;
                selectEnd := cursorPos;

                if cursorPos > Length (tabs(activeTab).url) then
                    cursorPos := Length (tabs(activeTab).url);
                    appending := True;
                end if;
            end if;

            click := False;
        end if;

        if editingURL then
            for i in 1 .. Length (tabs(activeTab).url) loop
                if i >= selectStart and i <= selectEnd then
                    Console.setBGColor (currentTheme.bgSelected.r, currentTheme.bgSelected.g, currentTheme.bgSelected.b);
                    Console.setColor (currentTheme.fgSelected.r, currentTheme.fgSelected.g, currentTheme.fgSelected.b);
                    Put ("" & Element (tabs(activeTab).url, i));
                else
                    Console.setBGColor (currentTheme.editorLine.r, currentTheme.editorLine.g, currentTheme.editorLine.b);
                    Console.setColor (currentTheme.fgSelected.r, currentTheme.fgSelected.g, currentTheme.fgSelected.b);
                    Put ("" & Element (tabs(activeTab).url, i));
                end if;
            end loop;

            -- if appending, need to highlight space after URL
            if appending then
                Console.setBGColor (currentTheme.bgSelected.r, currentTheme.bgSelected.g, currentTheme.bgSelected.b);
                Console.setColor (currentTheme.fgSelected.r, currentTheme.fgSelected.g, currentTheme.fgSelected.b);
                Put (" ");
            end if;
        else
            Console.setBGColor (currentTheme.editorLine.r, currentTheme.editorLine.g, currentTheme.editorLine.b);
            Console.setColor (currentTheme.fg.r, currentTheme.fg.g, currentTheme.fg.b);
            Put (To_String (tabs(activeTab).url));
        end if;

        Console.setBGColor (currentTheme.editorLine.r, currentTheme.editorLine.g, currentTheme.editorLine.b);
        Console.setColor (currentTheme.note.r, currentTheme.note.g, currentTheme.note.b);

        -- fill rest of line so address bar looks nice
        if appending then
            for x in Length(tabs(activetab).url) .. w - 19 loop
                Put (" ");
            end loop;
        else
            for x in Length(tabs(activetab).url) .. w - 18 loop
                Put (" ");
            end loop;
        end if;

        -- if Bookmarked, say so.
        Console.setCursor (w-2, 4);
        if tabs(activetab).bookmarked then
            Put ("🔖");
        else
            Put ("📑");
        end if;

        -- check hover/click on bookmark button
        if regionHit (w-2, 4, w-1, 4) then
            showTooltip := True;
            if tabs(activetab).bookmarked then
                tooltip := To_Unbounded_String ("Remove bookmark (shortcut: Ctrl+d)");
            else
                tooltip := To_Unbounded_String ("Add bookmark (shortcut: Ctrl+d)");
            end if;
        end if;
    end renderTitle;

    ---------------------------------------------------------------------------
    -- renderPage
    -- Render current page contents
    ---------------------------------------------------------------------------
    procedure renderPage is
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
                    Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
                    Console.setColor (currentTheme.fg.r, currentTheme.fg.g, currentTheme.fg.b);
                when Link =>
                    --@TODO determine whether a link is visited or not
                    Console.underlineOn;
                    -- Console.boldOff;
                    Console.italicsOff;
                    Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
                    Console.setColor (currentTheme.unvisitedLink.r, currentTheme.unvisitedLink.g, currentTheme.unvisitedLink.b);
                when H1 =>
                    Console.underlineOff;
                    -- Console.boldOn;
                    Console.italicsOff;
                    Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
                    Console.setColor (currentTheme.h1.r, currentTheme.h1.g, currentTheme.h1.b);
                when H2 =>
                    Console.underlineOff;
                    -- Console.boldOn;
                    Console.italicsOff;
                    Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
                    Console.setColor (currentTheme.h2.r, currentTheme.h2.g, currentTheme.h2.b);
                when H3 =>
                    Console.underlineOff;
                    -- Console.boldOff;
                    Console.italicsOff;
                    Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
                    Console.setColor (currentTheme.h3.r, currentTheme.h3.g, currentTheme.h3.b);
                when Preformat =>
                    Console.underlineOff;
                    -- Console.boldOff;
                    Console.italicsOff;
                    Console.setBGColor (currentTheme.bgPreformat.r, currentTheme.bgPreformat.g, currentTheme.bgPreformat.b);
                    Console.setColor (currentTheme.fgPreformat.r, currentTheme.fgPreformat.g, currentTheme.fgPreformat.b);
                when UnorderedList =>
                    Console.underlineOff;
                    -- Console.boldOff;
                    Console.italicsOff;
                    Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
                    Console.setColor (currentTheme.fgList.r, currentTheme.fgList.g, currentTheme.fgList.b);
                when Quote =>
                    Console.underlineOff;
                    -- Console.boldOff;
                    Console.italicsOn;
                    Console.setBGColor (currentTheme.bgQuote.r, currentTheme.bgQuote.g, currentTheme.bgQuote.b);
                    Console.setColor (currentTheme.fgQuote.r, currentTheme.fgQuote.g, currentTheme.fgQuote.b);
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
            urlStart : Natural;
            urlEnd : Natural;
            descStart : Natural;
            descEnd : Natural;
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

        -- index into page contents and char at that index
        i : Natural := 0;
        c : Character;

        -- type of line being rendered
        curLineType : LineType;
    begin
        Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
        Console.setColor (currentTheme.ui.r, currentTheme.ui.g, currentTheme.ui.b);

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

        -- right scrollbar
        -- get position from current page
        Console.setCursor (w-1, 6);
        Put ("⬆");

        for y in 7..h-5 loop
            Console.setCursor (w-1, y);
            Put ("░");
        end loop;

        Console.setCursor (w-1, h-4);
        Put ("⬇");
        
        -- determine viewable area
        -- scrollbar position
        Console.setCursor (w-1, h-11);
        Put ("▓");

        -- horiz scrollbar
        Console.setCursor (2, h-3);
        Put ("⬅");

        for x in 2..w-4 loop
            -- Console.setCursor (x, h-3);
            Put ("░");
        end loop;

        Put ("➡");
        Put ("█");

        Console.setCursor (vx, vy);
        
        if Length(tabs(activeTab).pageContents) = 0 then
            return;
        end if;

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

        setStyle (Plain);
        -- Console.underlineOff;
        -- Console.boldOff;
        -- Console.italicsOff;
    end renderPage;

    ---------------------------------------------------------------------------
    -- renderFooter
    -- Render footer and status bar
    ---------------------------------------------------------------------------
    procedure renderFooter is
    begin
        box (1, h-2, w, h, "╠", "╣", "╚", "╝");

        Console.setCursor (2, h-1);
        Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
        Console.setColor (currentTheme.note.r, currentTheme.note.g, currentTheme.note.b);
        -- Put (To_String (status) & To_String (escSequence));
        if showTooltip then
            Put (To_String (tooltip));
            for i in Length (tooltip) .. w - 4 loop
                Put (" ");
            end loop;
        else
            Put (To_String (tabs(activeTab).status));
            for i in Length (tabs(activeTab).status) .. w - 4 loop
                Put (" ");
            end loop;
        end if;

    end renderFooter;

    ---------------------------------------------------------------------------
    -- Signal handler for SIGWINCH - when the window is resized we need to
    -- query window size and re-render.
    ---------------------------------------------------------------------------
    protected body Sigwinch_Handler is
        procedure handle is
        begin
            Console.disableMouse;
            Console.termSize (w, h);
            Console.enableMouse;
            Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
            Console.clear;
        end handle;
    end Sigwinch_Handler;

    ---------------------------------------------------------------------------
    -- handleInput
    --
    -- Read stdin for VT100 control codes, handle appropriately. This just
    -- reads input and handles it immediately instead of queuing up input
    -- events for other components to handle. This is more straightforward, but
    -- less re-usable. May consider the input queue if breaking this out into
    -- a separate TUI library at some point.
    ---------------------------------------------------------------------------
    procedure handleInput is
        chr      : Character;

        use Interfaces;

        -----------------------------------------------------------------------
        -- backspace
        -- Handle backspace depending on whether we're editing the address bar
        -- or not.
        -----------------------------------------------------------------------
        procedure backspace is
        begin
            tooltip := To_Unbounded_String ("BS");
            if editingURL then
                if Length(tabs(activeTab).url) = 0 then
                    return;
                elsif cursorPos > Length(tabs(activeTab).url) then
                    Delete (tabs(activeTab).url, Length(tabs(activeTab).url), Length(tabs(activeTab).url));
                    cursorLeft;
                elsif cursorPos = 0 and selectStart /= 0 and selectEnd /= 0 then
                    Delete (tabs(activeTab).url, selectStart, selectEnd);
                end if;
            else
                -- @TODO go back in history
                null;
            end if;
        end backspace;

        -----------------------------------------------------------------------
        -- delete
        -- Handle press of the delete key
        -----------------------------------------------------------------------
        procedure delete is
        begin
            tooltip := To_Unbounded_String ("DEL");
            null;
            --TODO handle delete
        end delete;

        -----------------------------------------------------------------------
        -- enter
        -- Handle Enter keypress.
        -----------------------------------------------------------------------
        procedure enter is
        begin
            if editingURL then
                loadPage (tabs(activeTab).url);
            end if;
        end enter;

        -----------------------------------------------------------------------
        -- printable
        -- Handle normal printable character keypresses
        -----------------------------------------------------------------------
        procedure printable (c : Character) is
        begin
            if editingURL then
                -- printable character, add it to our text
                if cursorPos > Length(tabs(activeTab).url) then
                    -- cursor at end
                    Append (tabs(activeTab).url, c);
                    cursorPos := cursorPos + 1;
                    appending := True;
                elsif cursorPos = 1 then
                    -- cursor at beginning
                    Insert (tabs(activeTab).url, 1, "" & c);
                elsif cursorPos /= 0 and selectStart = 0 and selectEnd = 0 then
                    -- cursor in middle of word
                    Insert (tabs(activeTab).url, cursorPos, "" & c);
                    cursorPos := cursorPos + 1;
                elsif cursorPos = 0 and selectStart /= 0 and selectEnd /= 0 then
                    -- whole word selected
                    -- @TODO right now we just allow selecting entire address, not a sub-string,
                    -- would like to allow dragging or shift+arrow to enable selective editing.
                    Delete (tabs(activeTab).url, selectStart, selectEnd);
                    Append (tabs(activeTab).url, c);
                    cursorPos := Length(tabs(activeTab).url) + 1;
                    selectStart := 0;
                    selectEnd := 0;
                    appending := True;
                else
                    -- not sure why we'd get here.
                    null;
                end if;
            end if;
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

            MOUSE_MOVE  : constant := 35;
            LEFT_CLICK  : constant := 0;
            RIGHT_CLICK : constant := 1;
            WHEEL_UP    : constant := 4;
            WHEEL_DN    : constant := 5;
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

            if button = MOUSE_MOVE then
                mousex := posx;
                mousey := posy;
            elsif button = LEFT_CLICK then
                if mtype = 'M' then
                    click := True;
                elsif mtype = 'm' then
                    click := False;
                end if;
            elsif button = WHEEL_UP then
                scrollUp (4);
            elsif button = WHEEL_DN then
                scrollDown (4);
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
            KEY_END : constant String := "[F";
            PGUP    : constant String := "[5~";
            PGDN    : constant String := "[6~";
            DEL     : constant String := "[3~";
            UP      : constant String := "[A";
            DOWN    : constant String := "[B";
            RIGHT   : constant String := "[C";
            LEFT    : constant String := "[D";
        begin
            if To_String (escSequence) = F1 then
                null;
            elsif To_String (escSequence) = F2 then
                null;
            elsif To_String (escSequence) = F3 then
                null;
            elsif To_String (escSequence) = F4 then
                null;
            elsif To_String (escSequence) = F5 then
                --loadPage (tabs(activeTab).url);
                null;
            elsif To_String (escSequence) = F6 then
                null;
            elsif To_String (escSequence) = F7 then
                null;
            elsif To_String (escSequence) = F8 then
                null;
            elsif To_String (escSequence) = F9 then
                null;
            elsif To_String (escSequence) = INS then
                null;
            elsif To_String (escSequence) = HOME then
                null;   -- @TODO scroll up to top
            elsif To_String (escSequence) = PGUP then
                if not editingURL then
                    scrollUp (20);
                end if;
            elsif To_String (escSequence) = PGDN then
                if not editingURL then
                    scrollDown (20);
                end if;
            elsif To_String (escSequence) = KEY_END then
                null;
            elsif To_String (escSequence) = DEL then
                delete;
            elsif To_String (escSequence) = UP then
                if not editingURL then
                    scrollUp;
                end if;
            elsif To_String (escSequence) = DOWN then
                if not editingURL then
                    scrollDown;
                end if;
            elsif To_String (escSequence) = LEFT then
                if editingURL then
                    cursorLeft;
                else
                    scrollLeft;
                end if;
            elsif To_String (escSequence) = RIGHT then
                if editingURL then
                    cursorRight;
                else
                    scrollRight;
                end if;
            elsif isMouseSequence (escSequence) then
                handleMouse (escSequence);
            else
                null;   -- @TODO probably other useful sequences to handle.
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

            -- if we hit ESC by itself, we'll have an empty escSequence
            if Length (escSequence) = 0 then
                if editingURL then
                    selectNone;
                end if;

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
                when CTRL_D => null;        --@TODO save bookmark
                when CTRL_F =>
                    --@TODO search function
                    -- probably want to change address bar to a special
                    -- find-in-page mode where editingURL is true, but "find"
                    -- is also true
                    null;
                when CTRL_H | DEL =>
                    backspace;
                when CTRL_J | CR =>
                    enter;
                when CTRL_L =>
                    selectAll;
                when CTRL_Q => quit := True;
                when CTRL_S => null;        --@TODO save document to disk
                when CTRL_T => newTab;
                when CTRL_W => closeTab (activeTab);
                when CTRL_B => null;        --@TODO show bookmarks
                when others =>
                    tooltip := To_Unbounded_String ("key: " & pos'Image);
            end case;
        end control;
    begin
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
    end handleInput;

    ---------------------------------------------------------------------------
    -- renderCursor
    -- draw a little + under the mouse pointer. Maybe make this an option,
    -- the regular mouse pointer works fine. This might be nice if not in an
    -- xterm.
    ---------------------------------------------------------------------------
    procedure renderCursor is
    begin
        Console.setCursor (mousex, mousey);
        Console.setColor (currentTheme.fgSelected.r, currentTheme.fgSelected.g, currentTheme.fgSelected.b);
        Put ("᛭");
    end renderCursor;

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
        newTab;

        Console.hideCursor;
        Console.rawMode;
        Console.enableMouse;

        Console.setTitle ("Gembrowse 💎");
        Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
        Console.clear;

        Console.termSize (w, h);

        loop
            -- showTooltip := False;

            --@TODO may need to adjust this or the input handler, it seems
            -- sometimes we miss mouse clicks
            startTime := Ada.Real_Time.Clock;
            nextPeriod := startTime + Ada.Real_Time.Milliseconds (33);     -- 30 fps

            Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);

            renderTitle;
            renderPage;
            renderFooter;
            -- renderCursor;

            handleInput;

            delay until nextPeriod;

            exit when quit;
        end loop;

        Console.disableMouse;
        Console.normalMode;
        Console.showCursor;
    end renderLoop;

end Gembrowse.UI;