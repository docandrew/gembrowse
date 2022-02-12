-------------------------------------------------------------------------------
-- gembrowse-ui-input.ads
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Gembrowse.UI.State;

package Gembrowse.UI.Input is

    ---------------------------------------------------------------------------
    -- killInputs
    -- Flag to kill the input queueing task.
    ---------------------------------------------------------------------------
    killInputs : Boolean := False;

    ---------------------------------------------------------------------------
    -- Region_Hit
    --
    -- Detect whether the mouse is in a certain region. Note that this takes the
    -- current scope offset into account. The coordinates given as parameters
    -- should be the _relative_ coordinates for the widget.
    ---------------------------------------------------------------------------
    function Region_Hit (st: in out Gembrowse.UI.State.UIState; x1,y1,x2,y2 : Natural) return Boolean;
    
    ---------------------------------------------------------------------------
    -- Get_Inputs
    --
    -- Reads stdin for either keypresses or ANSI escape codes representing
    -- other inputs, such as mouse movement or clicks, and update the UI State
    -- with those inputs.
    ---------------------------------------------------------------------------
    procedure Get_Inputs (st : in out Gembrowse.UI.State.UIState);

end Gembrowse.UI.Input;
