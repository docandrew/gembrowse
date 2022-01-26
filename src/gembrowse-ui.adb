-------------------------------------------------------------------------------
-- gembrowse-ui.adb
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Interrupts; use Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Console;

package body Gembrowse.UI is

    w,h : Natural := 0;

    type ThemeColor is record
        r : Natural;
        g : Natural;
        b : Natural;
    end record;

    type Theme is record
        bg              : ThemeColor;
        fg              : ThemeColor;
        ui              : ThemeColor;
        bgSelected      : ThemeColor;
        fgSelected      : ThemeColor;
        h1              : ThemeColor;
        h2              : ThemeColor;
        h3              : ThemeColor;
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

    procedure renderTitle is

        procedure renderTabs is
            moreTabs : Boolean := True;
        begin
            -- Console.setBGColor (currentTheme.ui.r, currentTheme.ui.g, currentTheme.ui.b);
            -- Console.setColor (currentTheme.)
            Console.setCursor (2,1);

            -- draw top of active tab
            Put ("╔");
            for x in 1..5 loop
                Put ("═");
            end loop;

            Put ("╗");

            -- draw top of inactive tab
            Put ("┌");

            for x in 1..5 loop
                Put ("─");
            end loop;

            Put ("┐");

            -- draw middle of active tab
            Console.setCursor (2,2);
            Put ("║");

            -- Console.setBGColor (currentTheme.bgSelected.r, currentTheme.bgSelected.g, currentTheme.bgSelected.b);
            Console.setColor (currentTheme.h1.r, currentTheme.h1.g, currentTheme.h1.b);
            Put (" TAB ");
            -- Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
            Console.setColor (currentTheme.ui.r, currentTheme.ui.g, currentTheme.ui.b);

            Put ("║");

            -- draw middle of inactive tabs
            Put ("│");
            Put (" TB2 ");
            Put ("│");

            -- draw bottom of active tab
            Console.setCursor (2,3);
            Put ("╝     ╚");

            -- draw bottom of inactive tab
            Put ("╧═════╧");

        end renderTabs;

    begin
        Console.setColor (currentTheme.ui.r, currentTheme.ui.g, currentTheme.ui.b);
        box (1,3,w,5,"╔","╗","╠","╣");

        renderTabs;
        
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

        Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
        Console.setColor (currentTheme.note.r, currentTheme.note.g, currentTheme.note.b);
        Put_Line (" gemini://localhost");
    end renderTitle;

    procedure renderPage is
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
    end renderPage;

    escSequence : Unbounded_String;

    procedure renderFooter is
    begin
        box (1, h-2, w, h, "╠", "╣", "╚", "╝");

        Console.setCursor (2, h-1);
        Console.setBGColor (currentTheme.bg.r, currentTheme.bg.g, currentTheme.bg.b);
        Console.setColor (currentTheme.note.r, currentTheme.note.g, currentTheme.note.b);
        Put ("✔️ Loaded 843b in .013s " & To_String (escSequence));
    end renderFooter;

    -- Signal handler for SIGWINCH - when the window is resized we need to re-render.
    -- protected body Sigwinch_Handler is
    --     procedure handle is
    --     begin
    --         Console.termSize (w, h);
    --         renderTitle;
    --         renderPage;
    --         renderFooter;
    --     end handle;
    -- end Sigwinch_Handler;

    procedure renderLoop is
        hasInput : Boolean;
        chr  : Character;
        -- chr1 : Character;
        -- chr2 : Character;
        -- chr3 : Character;
        -- chr4 : Character;
        -- escSequence : Unbounded_String;
        -- nw : Natural := Natural'Last;
        -- nh : Natural := Natural'Last;
    begin
        loop
            -- Delete (escSequence, 1, Length (escSequence));

            Console.termSize (w, h);

            Console.setBGColor (16#1F#, 16#24#, 16#30#);
            Console.clear;
            renderTitle;
            renderPage;
            renderFooter;

            -- @TODO check for resize too
            -- block on input
            Get_Immediate (chr);

            if chr = 'q' then
                exit;
            elsif chr = ASCII.ESC then
                -- see if more input is waiting (multi char escape sequence)
                -- declare
                    
                -- begin
                escSequence := To_Unbounded_String ("");
                    loop
                        Get_Immediate (chr, hasInput);
                        
                        if hasInput then
                            Append (escSequence, chr);
                        end if;

                        exit when not hasInput;
                    end loop;

                    -- see what kind of escape sequence we got.
                    -- Put (To_String (escSequence));
                -- end;
            end if;

            -- if we get an escape here, it's probably an arrow key or something like that.
            -- we need to consume the full sequence.

            -- if chr = ASCII.ESC then
            --     Get_Immediate (chr1);

            --     if chr1 = '[' then              -- CSI
            --         Get_Immediate (chr2);

            --         if chr2 = '1' then

            --         elsif chr2 = '2' then

            --         end if;

            --     elsif chr1 = 'O' then           -- SS3
            --         Get_Immediate (chr2);

            --         if chr2 = 'P' then
            --             Put ("F1");
            --         elsif chr2 = 'Q' then
            --             Put ("F2") ;
            --         elsif chr2 = 'R' then
            --             Put ("F3");
            --         elsif chr2 = 'S' then
            --             Put ("F4");
            --         end if;
            --     end if;

            --     -- Get_Immediate (chr2);
                
            --     inp1 := chr1;
            --     inp2 := chr2;
            -- end if;
        end loop;

    end renderLoop;

end Gembrowse.UI;