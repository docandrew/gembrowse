-------------------------------------------------------------------------------
-- gembrowse-ui-scrollbars.ads
--
-- Rendering and input for scrollbars.
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Gembrowse.UI.State;

package Gembrowse.UI.Scrollbars is

    ---------------------------------------------------------------------------
    -- Horizontal_Scrollbar
    ---------------------------------------------------------------------------
    function Horizontal_Scrollbar (st    : in out Gembrowse.UI.State.UIState;
                                   x     : Natural;
                                   y     : Natural;
                                   Width : Positive;
                                   Min   : Integer;
                                   Max   : Integer;
                                   Val   : in out Integer) return Boolean;

    ---------------------------------------------------------------------------
    -- Vertical_Scrollbar
    ---------------------------------------------------------------------------
    function Vertical_Scrollbar (st  : in out Gembrowse.UI.State.UIState;
                                 x   : Natural;
                                 y   : Natural;
                                 Height : Positive;
                                 Min : Integer;
                                 Max : Integer;
                                 Val : in out Integer) return Boolean;

end Gembrowse.UI.Scrollbars;
