-------------------------------------------------------------------------------
-- gembrowse-ui.ads
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Gembrowse.UI is

    protected Sigwinch_Handler is
        procedure handle;
        pragma Interrupt_Handler (handle);
        pragma Attach_Handler (handle, 28);
    end Sigwinch_Handler;

    procedure renderLoop;

end Gembrowse.UI;