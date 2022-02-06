with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces.C;

package Gembrowse.URL with SPARK_Mode is

    -- Default ports for a given scheme if none is specified. These are only
    -- specified if there's both an IANA entry for the protocol's default port
    -- and a matching IANA URI scheme. For instance, there's an android scheme,
    -- but no android protocol or default port, so it won't be listed here.
    --
    -- For protocols with secure and insecure ports, like POP, IMAP, SMTP, this
    -- software defaults to the secure version.
    --
    -- POP scheme specifies Post office protocol v3

    -- This list is not exhaustive, and these numbers are provided for
    -- convenience for code which wishes to use the port field of the URL type
    -- when a port isn't otherwise explicitly given.
    --
    PORT_FTP        : constant := 21;
    PORT_GEMINI     : constant := 1965;
    PORT_GOPHER     : constant := 70;
    PORT_HTTP       : constant := 80;
    PORT_HTTPS      : constant := 443;
    PORT_IMAP       : constant := 993;
    PORT_POP        : constant := 995;
    PORT_RSYNC      : constant := 873;
    PORT_SFTP       : constant := 115;
    PORT_SMTP       : constant := 587;
    PORT_SSH        : constant := 22;
    PORT_TELNET     : constant := 23;

    -- IPv6 Address
    -- Note some URL libraries handle zone IDs, however RFC 

    -- Gemini specifies a maximum URL length of 1024.
    package URLStrings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 1024);

    -- type URL is record
    --     scheme   : URLStrings.Bounded_String;
    --     user     : URLStrings.Bounded_String;
    --     password : URLStrings.Bounded_String;
    --     port     : Interfaces.C.unsigned_short;

    -- function parseURL (urlstr : URLStrings.Bounded_String) return URL;
    procedure parseURL (s : URLStrings.Bounded_String);

end Gembrowse.URL;