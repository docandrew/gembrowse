-------------------------------------------------------------------------------
-- gembrowse-ui.ads
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Gembrowse.UI is

    -- protected Sigwinch_Handler is
    --     procedure handle;
    --     pragma Interrupt_Handler (handle);
    --     pragma Attach_Handler (handle, 28);
    -- end Sigwinch_Handler;

    type PageState is record
        title        : Unbounded_String;
        tlsStatus    : Boolean;
        pageContents : Unbounded_String;
    end record;

    package PageStates is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => PageState);
    tabs : PageStates.Vector;

    procedure renderLoop;

end Gembrowse.UI;