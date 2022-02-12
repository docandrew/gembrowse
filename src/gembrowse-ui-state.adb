-------------------------------------------------------------------------------
-- gembrowse-ui-state.adb
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Gembrowse.UI.Keys;

package body Gembrowse.UI.State is

    ---------------------------------------------------------------------------
    -- Enter_Scope
    ---------------------------------------------------------------------------
    procedure Enter_Scope (st               : in out UIState;
                           Scope_X_Offset   : Natural := 0;
                           Scope_Y_Offset   : Natural := 0) is
    begin
        if st.Curr_Scope = Scope_ID'Last then
            raise Invalid_Scope_Exception with "Exceeded maximum of nested Enter_Scope calls";
        end if;

        st.Curr_Scope       := st.Curr_Scope + 1;
        st.Scope_X_Offset   := st.Scope_X_Offset + Scope_X_Offset;
        st.Scope_Y_Offset   := st.Scope_Y_Offset + Scope_Y_Offset;
    end Enter_Scope;

    ---------------------------------------------------------------------------
    -- Exit_Scope
    ---------------------------------------------------------------------------
    procedure Exit_Scope (st                : in out UIState;
                          Scope_X_Offset    : Natural := 0;
                          Scope_Y_Offset    : Natural := 0) is
    begin
        if st.Curr_Scope = NO_SCOPE then
            raise Invalid_Scope_Exception with "Called Exit_Scope without matching Enter_Scope";
        end if;

        st.Curr_Scope       := st.Curr_Scope - 1;
        st.Scope_X_Offset   := st.Scope_X_Offset - Scope_X_Offset;
        st.Scope_Y_Offset   := st.Scope_Y_Offset - Scope_Y_Offset;
    end Exit_Scope;

    ---------------------------------------------------------------------------
    -- Next_ID
    ---------------------------------------------------------------------------
    function Next_ID(st: in out UIState) return ID is
        this_ID : constant ID := st.Last_IDs(st.Curr_Scope) + 1;
    begin
        st.Last_IDs(st.Curr_Scope) := this_ID;
        return this_ID;
    end Next_ID;

end Gembrowse.UI.State;