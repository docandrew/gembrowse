-------------------------------------------------------------------------------
-- gembrowse-bookmarks.adb
--
-- Bookmark handling.
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.IO_Exceptions;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Gembrowse.Bookmarks is
    package BookmarkPaths is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 4096);
    use BookmarkPaths;

    bookmarkFilepath : Bounded_String;
    bookmarkFile : File_Type;
    
    -- The bookmarks themselves are just stored as a plain .gmi file.
    bookmarks : Unbounded_String;

    -- If for whatever reason we can't read from or write to the bookmarks file,
    -- we can still use the browser, but won't be able to save bookmarks.
    useBookmarks : Boolean := False;

    ---------------------------------------------------------------------------
    -- useBookmarkFile
    ---------------------------------------------------------------------------
    procedure useBookmarkFile (path : String) is
    begin
        bookmarkFilepath := To_Bounded_String (path);

        -- Attempt to read bookmarks. If file does not exist, we'll try to
        -- create it later on when we add a bookmark.
        bookmarkFile.Open (In_File, To_String (bookmarkFilepath));

        while not End_Of_File (bookmarkFile) loop
            Append (bookmarks, bookmarkFile.Get_Line);
        end loop;

        useBookmarks := True;
        bookmarkFile.Close;
    exception
        when E : Ada.IO_Exceptions.Name_Error =>
            -- file probably doesn't exist yet, see if we can create it.
            -- If not, then we'll just catch the exception back in main() and
            -- print the error message.
            bookmarkFile.Create (Out_File, To_String (bookmarkFilepath));

            useBookmarks := True;
            bookmarkFile.Close;
    end useBookmarkFile;

    ---------------------------------------------------------------------------
    -- getBookmarkPath
    ---------------------------------------------------------------------------
    function getBookmarkPath return String is
    begin
        return To_String(bookmarkFilepath);
    end getBookmarkPath;

    ---------------------------------------------------------------------------
    -- addBookmark
    ---------------------------------------------------------------------------
    procedure addBookmark (st      : in out Gembrowse.UI.State.UIState;
                           name    : Unbounded_String;
                           url     : Unbounded_String;
                           written : out Boolean) is
    begin
        -- @TODO allow specifying a section, which is just a gmi heading.
        -- Search for the section specified. We'll add it immediately after
        -- the title there. If we can't find the section, we'll add a new one
        -- to the end of the bookmarks file. For now, just append the link to
        -- the end.

        bookmarks.Append (ASCII.LF & "=> " & name & " " & url);

        -- Open file for write
        bookmarkFile.Open (Out_File, To_String (bookmarkFilepath));

        bookmarkFile.Put (To_String (bookmarks));

        bookmarkFile.Close;

        written := True;
    exception
        when E : others =>
            st.Error_Msg := To_Unbounded_String ("Unable to save bookmarks to " & To_String (bookmarkFilepath));
            written := False;
    end addBookmark;

    ---------------------------------------------------------------------------
    -- isBookmarked
    ---------------------------------------------------------------------------
    function isBookmarked (url : Unbounded_String) return Boolean is
    begin
        -- @TODO
        -- iterate line by line in bookmarks.
        -- if a line is a link, parse out the title and the url.
        -- compare the url to the one we're checking. For now, just do a
        -- raw string search. It's not as "correct" but should do the job.

        return bookmarks.Index (To_String (url)) /= 0;
    end isBookmarked;

    ---------------------------------------------------------------------------
    -- deleteBookmark
    ---------------------------------------------------------------------------
    procedure deleteBookmark (st      : in out Gembrowse.UI.State.UIState;
                              url     : Unbounded_String;
                              written : out Boolean) is
    begin
        written := False;
    end deleteBookmark;

end Gembrowse.Bookmarks;
