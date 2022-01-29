-------------------------------------------------------------------------------
-- gembrowse-ui.adb
--
-- Copyright 2022 Jon Andrew
--
-- @TODO - something getting wonky here with setting italic and strikethrough
-------------------------------------------------------------------------------
with Ada.Interrupts; use Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with Ada.Real_Time;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Console;

package body Gembrowse.UI is

    mousex, mousey : Natural := 1;
    click       : Boolean := False;
    activeTab   : Natural := 1;

    -- Length of time to render a frame.
    frameTime   : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (1);

    -- tabsDirty   : Boolean := False;
    -- titleDirty  : Boolean := False;
    -- pageDirty   : Boolean := False;
    -- statusDirty : Boolean := False;

    status : Unbounded_String := To_Unbounded_String ("✔️ Loaded 843b in .013s ");

    w,h : Natural := 0;

    type Modifiers is record
        alt     : Boolean;
        ctrl    : Boolean;
        shift   : Boolean;
        caps    : Boolean;
    end record;

    type EventType is (Unknown, MouseMotion, MouseButtonDown, MouseButtonUp, KeyPress);

    type Event (kind : EventType := Unknown) is record
        kbmod : Modifiers := (others => False);

        case kind is
            when Unknown => null;
            when MouseMotion =>
                x, y : Natural;
            when MouseButtonDown | MouseButtonUp =>
                mx, my : Natural;
                button : Natural;
            when KeyPress =>
                key : Natural;
        end case;
    end record;

    package EventQueues is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => Event);
    events : EventQueues.Vector;

    type PageState is record
        title        : Unbounded_String;
        url          : Unbounded_String;
        tlsStatus    : Boolean;
        pageContents : Unbounded_String;
        startx       : Positive := 1;             -- beginning of page we've scrolled to
        starty       : Positive := 1;
    end record;

    package PageStates is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => PageState);
    tabs : PageStates.Vector;

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

    procedure newTab is
        newTabContents : String := 
            "# Gembrowse - Gemini Browser by Jon Andrew" & ASCII.LF &
            "" & ASCII.LF &
            "## Features" & ASCII.LF &
            "" & ASCII.LF &
            " * LibreSSL" & ASCII.LF &
            " * Written in Ada" & ASCII.LF;
    begin
        tabs.Append ((title         => To_Unbounded_String ("New Tab"),
                      url           => To_Unbounded_String ("New Tab"),
                      tlsStatus     => False, 
                      pageContents  => To_Unbounded_String (newTabContents),
                      others        => <>));
    end newTab;

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
            if click and regionHit (1,2,1,2) then
                -- status := To_Unbounded_String ("< Active tab: " & activeTab'Image);
                activeTab := max (1, activeTab - 1);
                click := False;
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
                    activeTab := t;
                    -- status := To_Unbounded_String ("Active tab: " & activeTab'Image);
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
            if click and regionHit (tx1, ty1, tx2, ty2) then
                newTab;
                click := False;
            end if;

            -- draw remainder of horizontal border
            for x in tx..w-1 loop
                Put ("═");
            end loop;
            Put ("╗");

            -- show next tab button
            Console.setCursor (w, 2);
            Put ("▶");

            -- Check for click in next tab button
            if click and regionHit (w,2,w,2) then
                activeTab := min (Natural (tabs.Length), activeTab + 1);
                -- status := To_Unbounded_String ("> Active tab: " & activeTab'Image);
                click := False;
            end if;

        end renderTabs;

    begin
        Console.setColor (currentTheme.ui.r, currentTheme.ui.g, currentTheme.ui.b);

        --@TODO ugly artifacts here from redrawing, just draw the parts not
        -- being drawn by renderTabs
        -- box (1,3,w,5,"╔","╗","╠","╣");

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
        
        -- if updated cert (no TOFU) or bad TLS version
        -- Console.setBGColor (currentTheme.warning.r, currentTheme.warning.g, currentTheme.warning.b);
        -- Put ("🔓");

        -- if self-signed cert
        -- Console.setBGColor (currentTheme.caution.r, currentTheme.caution.g, currentTheme.caution.b);
        -- Put ("🔒");
        
        -- if CA-signed cert
        Console.setBGColor (currentTheme.note.r, currentTheme.note.g, currentTheme.note.b);
        Put ("🔐");

        Console.setBGColor (currentTheme.editorLine.r, currentTheme.editorLine.g, currentTheme.editorLine.b);
        Console.setColor (currentTheme.note.r, currentTheme.note.g, currentTheme.note.b);
        --Put_Line (" gemini://localhost");
        Put_Line (To_String (tabs(activeTab).url));
    end renderTitle;

    ---------------------------------------------------------------------------
    -- renderPage
    -- Render current page contents
    ---------------------------------------------------------------------------
    procedure renderPage is
        -- viewportCursor : Natural;       -- where in UI we're rendering
        -- pageCursor : Natural;           -- where in document we're rendering

        procedure renderTextLine is
        begin
            null;
        end renderTextLine;

        procedure renderPreformattedLine is
        begin
            null;
        end renderPreformattedLine;

        procedure renderNextLine is
        begin
            null;
        end renderNextLine;
    begin
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

        -- go line-by-line
        -- renderNextLine;
        -- This is a bit inefficent. We process the entire page and then determine whether
        -- the material being rendered is within the current viewport.
    end renderPage;

    escSequence : Unbounded_String;

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
        Put (To_String (status) & To_String (escSequence));
        -- Put (To_String (status));
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
            Console.clear;
        end handle;
    end Sigwinch_Handler;

    quit : Boolean := False;

    ---------------------------------------------------------------------------
    -- enqueueInputEvents
    -- Read stdin for VT100 control codes, handle appropriately.
    ---------------------------------------------------------------------------
    procedure enqueueInputEvents is
        chr      : Character;
        isEsc    : Boolean := False;

        curEvent : Event;
    begin
        -- returns NUL if no input available
        chr := Console.getch;

        if chr = ASCII.NUL then
            return;
        -- @TODO
        -- elsif chr in ASCII.SOH..ASCII.26 then
        -- CTRL+x
        elsif chr = ASCII.ESC then
            escSequence := To_Unbounded_String ("");
            isEsc := True;
        else
            escSequence := To_Unbounded_String ("" & chr);
        end if;

        -- see if more input is waiting (multi char escape sequence)
        loop
            chr := Console.getch;
            
            exit when chr = ASCII.NUL;

            Append (escSequence, chr);
        end loop;

        -- if we hit ESC by itself, we'll have an empty escSequence
        if isEsc and Length (escSequence) = 0 then
            -- escape by itself
            Append (escSequence, " = ESC");
            return;
        end if;

        quit := (if Element (escSequence, 1) = 'q' then True else False);
        
        if isEsc then
            if To_String (escSequence) = "OP" then
                -- F1
                Append (escSequence, " = F1");
            elsif To_String (escSequence) = "OQ" then
                Append (escSequence, " = F2");
            elsif To_String (escSequence) = "OR" then
                Append (escSequence, " = F3");
            elsif To_String (escSequence) = "OS" then
                Append (escSequence, " = F4");
            elsif To_String (escSequence) = "[15~" then
                Append (escSequence, " = F5");
            elsif To_String (escSequence) = "[17~" then
                Append (escSequence, " = F6");
            elsif To_String (escSequence) = "[18~" then
                Append (escSequence, " = F7");
            elsif To_String (escSequence) = "[19~" then
                Append (escSequence, " = F8");
            elsif To_String (escSequence) = "[20~" then
                Append (escSequence, " = F9");
            elsif To_String (escSequence) = "[2~" then
                Append (escSequence, " = INS");
            elsif To_String (escSequence) = "[H" then
                Append (escSequence, " = HOME");
            elsif To_String (escSequence) = "[5~" then
                Append (escSequence, " = PGUP");
            elsif To_String (escSequence) = "[6~" then
                Append (escSequence, " = PGDN");
            elsif To_String (escSequence) = "[F" then
                Append (escSequence, " = END");
            elsif To_String (escSequence) = "[3~" then
                Append (escSequence, " = DEL");
            elsif To_String (escSequence) = "[A" then
                Append (escSequence, " = UP");
            elsif To_String (escSequence) = "[B" then
                Append (escSequence, " = DOWN");
            elsif To_String (escSequence) = "[C" then
                Append (escSequence, " = LEFT");
            elsif To_String (escSequence) = "[D" then
                Append (escSequence, " = RIGHT");
            else
                -- check mouse movements
                -- mouse stuff is format [<B;x;yM
                -- @TODO this is still pretty glitchy - I think sometimes events can come in "mid event" and goof this up.
                -- need to add some sanity checking to ensure only complete sequences are processed and handle errors.

                if Length (escSequence) >= 8 and then (Element(escSequence, 1) = '[' and Element(escSequence, 2) = '<' and 
                        (Element(escSequence, Length(escSequence)) = 'm' or Element(escSequence, Length(escSequence)) = 'M')) then

                    parseMouseData: declare
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
                    begin
                        Append (escSequence, " MOUSE");

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

                        if button = 35 then
                            -- mouse movement
                            curEvent := (kind => MouseMotion, x => posx, y => posy, others => <>);
                            Append (escSequence, " (" & curEvent.x'Image & "," & curEvent.y'Image & ")");
                            mousex := posx;
                            mousey := posy;
                        else
                            -- button press or release
                            if mtype = 'M' then
                                curEvent := (kind => MouseButtonDown, mx => posx, my => posy, others => <>);
                                Append (escSequence, " DN (" & curEvent.mx'Image & "," & curEvent.my'Image & ")");

                                if button = 0 then click := True; end if;

                            elsif mtype = 'm' then
                                curEvent := (kind => MouseButtonUp, mx => posx, my => posy, others => <>);
                                Append (escSequence, " UP (" & curEvent.mx'Image & "," & curEvent.my'Image & ")");

                                if button = 0 then click := False; end if;

                            end if;
                        end if;
                    end parseMouseData;
                end if;
            end if;
        end if;
    end enqueueInputEvents;

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

            startTime := Ada.Real_Time.Clock;
            nextPeriod := startTime + Ada.Real_Time.Milliseconds (33);     -- 30 fps

            Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
            -- Console.clear;  -- need to avoid this, just redraw dirty parts.
            renderTitle;
            renderPage;
            renderFooter;
            -- renderCursor;

            enqueueInputEvents;

            delay until nextPeriod;

            exit when quit;
        end loop;

        Console.disableMouse;
        Console.normalMode;
        Console.showCursor;
    end renderLoop;

end Gembrowse.UI;