-------------------------------------------------------------------------------
-- gembrowse-url.ads
--
-- URL parsing and transformation routines.
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces.C;

pragma Elaborate (Ada.Strings.Bounded);

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

    -- If a URL is malformed, this type is the reason why.
    type ParseError is (
        NONE,
        BAD_SCHEME,
        BAD_PCHAR,
        EMPTY_SEGMENT,
        BAD_SEGMENT_NZ,
        BAD_ABSOLUTE_PATH,
        BAD_USERINFO,
        BAD_IPLITERAL,
        BAD_IPVFUTURE,
        BAD_IPV4ADDRESS,
        BAD_PERCENT_ENCODING,
        BAD_PORT,
        BAD_HEXDIGIT
    );

    -- Gemini specifies a maximum URL length of 1024.
    package URLStrings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 1024);

    subtype URLIndex is Natural range 0..URLStrings.Max_Length;

    type URL is record
        scheme   : URLStrings.Bounded_String    := URLStrings.Null_Bounded_String;
        user     : URLStrings.Bounded_String    := URLStrings.Null_Bounded_String;
        password : URLStrings.Bounded_String    := URLStrings.Null_Bounded_String;
        host     : URLStrings.Bounded_String    := URLStrings.Null_Bounded_String;
        port     : Interfaces.C.unsigned_short  := 0;
        path     : URLStrings.Bounded_String    := URLStrings.Null_Bounded_String;
        query    : URLStrings.Bounded_String    := URLStrings.Null_Bounded_String;
        fragment : URLStrings.Bounded_String    := URLStrings.Null_Bounded_String;
        error    : ParseError                   := NONE;
        errorIdx : URLIndex                     := 0;
    end record;

    ---------------------------------------------------------------------------
    -- parseURL
    --
    -- Given an unbounded string containing a URL, parse it into its
    -- individual components.
    ---------------------------------------------------------------------------
    procedure parseURL (s : URLStrings.Bounded_String; u : out URL);

    ---------------------------------------------------------------------------
    -- percentEncode
    --
    -- percent-encode a string, so URI-reserved characters, control characters
    -- and non-ASCII UTF-8 code points are converted to %xx where xx is the
    -- hexadecimal value of that character.
    ---------------------------------------------------------------------------
    procedure percentEncode (s : in out Unbounded_String);

    ---------------------------------------------------------------------------
    -- runTests
    -- Execute Unit Tests for this package.
    ---------------------------------------------------------------------------
    procedure runTests;

end Gembrowse.URL;
