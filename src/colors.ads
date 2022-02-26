-------------------------------------------------------------------------------
-- colors.ads
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
package Colors is

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

    gemBlue : Theme := (
        bg              => (16#16#, 16#19#, 16#25#),
        fg              => (16#40#, 16#6E#, 16#8E#),
        ui              => (16#8E#, 16#A8#, 16#C3#),
        editorLine      => (16#23#, 16#39#, 16#5B#),
        bgSelected      => (16#23#, 16#39#, 16#5B#),
        fgSelected      => (16#CB#, 16#F7#, 16#ED#),
        h1              => (16#CB#, 16#F7#, 16#ED#),
        h2              => (16#CB#, 16#F7#, 16#ED#),    -- make dimmer
        h3              => (16#CB#, 16#F7#, 16#ED#),
        visitedLink     => (16#8E#, 16#A8#, 16#C3#),
        unvisitedLink   => (16#CB#, 16#F7#, 16#ED#),
        bgQuote         => (16#16#, 16#19#, 16#25#),
        fgQuote         => (16#8E#, 16#A8#, 16#C3#),
        bgPreformat     => (16#16#, 16#19#, 16#25#),
        fgPreformat     => (16#40#, 16#6E#, 16#8E#),
        bgList          => (16#16#, 16#19#, 16#25#),
        fgList          => (16#40#, 16#6E#, 16#8E#),
        note            => (16#FC#, 16#FD#, 16#AF#),
        caution         => (16#EF#, 16#D7#, 16#80#),
        warning         => (16#DB#, 16#A1#, 16#59#)
    );

    vaporwave : Theme := (
        bg              => (16#30#, 16#03#, 16#50#),
        fg              => (16#94#, 16#16#, 16#7F#),
        ui              => (16#E9#, 16#34#, 16#79#),
        editorLine      => (16#30#, 16#03#, 16#50#),
        bgSelected      => (16#30#, 16#03#, 16#50#),
        fgSelected      => (16#F6#, 16#2E#, 16#97#),
        h1              => (16#15#, 16#3C#, 16#B4#),
        h2              => (16#15#, 16#3C#, 16#B4#),    -- make dimmer
        h3              => (16#15#, 16#3C#, 16#B4#),
        visitedLink     => (16#F9#, 16#AC#, 16#53#),
        unvisitedLink   => (16#F9#, 16#AC#, 16#53#),
        bgQuote         => (16#30#, 16#03#, 16#50#),
        fgQuote         => (16#E9#, 16#34#, 16#79#),
        bgPreformat     => (16#30#, 16#03#, 16#50#),
        fgPreformat     => (16#94#, 16#16#, 16#7F#),
        bgList          => (16#30#, 16#03#, 16#50#),
        fgList          => (16#F6#, 16#2E#, 16#97#),
        note            => (16#15#, 16#3C#, 16#B4#),
        caution         => (16#F6#, 16#2E#, 16#97#),
        warning         => (16#F9#, 16#AC#, 16#53#)
    );

    currentTheme : Theme := Colors.ayuMirage;
    -- currentTheme : Theme := vaporwave;
end Colors;
