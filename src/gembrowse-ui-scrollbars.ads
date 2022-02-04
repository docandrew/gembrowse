
with Gembrowse.UI.State;

package Gembrowse.UI.Scrollbars is

    function Horizontal_Scrollbar (st    : in out Gembrowse.UI.State.UIState;
                                   x     : Natural;
                                   y     : Natural;
                                   Width : Positive;
                                   Min   : Integer;
                                   Max   : Integer;
                                   Val   : in out Integer) return Boolean;

end Gembrowse.UI.Scrollbars;
