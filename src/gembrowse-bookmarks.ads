-------------------------------------------------------------------------------
-- gembrowse-bookmarks.ads
--
-- Routines for handling bookmarks. Bookmarks in Gembrowse are just stored in
-- a .gmi file
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gembrowse.UI.State;

package Gembrowse.Bookmarks is

    ---------------------------------------------------------------------------
    -- useBookmarkFile
    --
    -- Specify the bookmark file that Gembrowse should use during this session.
    ---------------------------------------------------------------------------
    procedure useBookmarkFile (path : String);

    ---------------------------------------------------------------------------
    -- addBookmark
    --
    -- Add a link to the bookmark file.
    --
    -- @param st - GUI state. If there's a problem writing the bookmarks,
    --   st.Error_Msg will be set.
    -- @param name - Name of the bookmark link
    -- @param url - URL for the bookmark
    -- @param written - True if bookmark file could be written to, False
    --  otherwise.
    ---------------------------------------------------------------------------
    procedure addBookmark (st      : in out Gembrowse.UI.State.UIState;
                           name    : Unbounded_String;
                           url     : Unbounded_String;
                           written : out Boolean);

    ---------------------------------------------------------------------------
    -- isBookmarked
    --
    -- @return True if this url is already bookmarked.
    ---------------------------------------------------------------------------
    function isBookmarked (url : Unbounded_String) return Boolean;

    ---------------------------------------------------------------------------
    -- deleteBookmark
    --
    -- Remove this bookmark from the list.
    ---------------------------------------------------------------------------
    procedure deleteBookmark (st      : in out Gembrowse.UI.State.UIState;
                              url     : Unbounded_String;
                              written : out Boolean);

end Gembrowse.Bookmarks;
