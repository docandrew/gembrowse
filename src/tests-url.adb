-------------------------------------------------------------------------------
-- tests-url.adb
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Gembrowse.URL; use Gembrowse.URL;

package body Tests.URL is

    ---------------------------------------------------------------------------
    -- runTests
    --
    -- Unit testing suite for URL parsing subprograms.
    --
    -- @TODO break this off into a separate package. Prolly gonna get pretty
    -- big.
    ---------------------------------------------------------------------------
    procedure runTests (count : in out Natural) is

        use Ada.Assertions;
        use Ada.Strings.Unbounded;
        use Gembrowse.URL;

        -----------------------------------------------------------------------
        -- testPercentEncode
        -----------------------------------------------------------------------
        procedure testPercentEncode is
            type PercentEncodeTestCase is record
                modified : Unbounded_String;
                expected : Unbounded_String;
            end record;
            
            type PercentEncodeTestCases is array (Natural range 1..2) of PercentEncodeTestCase;

            testCases : PercentEncodeTestCases := (
                (To_Unbounded_String ("Hello Günter"),                           To_Unbounded_String ("Hello%20G%C3%BCnter")),
                (To_Unbounded_String ("all % * special [] chars ' are encoded"), To_Unbounded_String ("all%20%25%20%2A%20special%20%5B%5D%20chars%20%27%20are%20encoded"))
            );
        begin
            
            for tc of testCases loop
                percentEncode (tc.modified);
                assert (tc.modified.To_String = tc.expected.To_String, "percentEncode() properly converts non-URL characters to %xx digits");
                count := count + 1;
            end loop;

        end testPercentEncode;

        -----------------------------------------------------------------------
        -- testParseURL
        -----------------------------------------------------------------------
        procedure testParseURL is
            type ParseURLTestCase is record
                text     : URLStrings.Bounded_String;
                expected : Gembrowse.URL.URL;
                testMsg  : Unbounded_String;
            end record;

            type ParseURLTestCases is array (Natural range <>) of ParseURLTestCase;

            testCases : ParseURLTestCases := (
                ---------------------------------------------------------------
                -- Scheme parsing
                ---------------------------------------------------------------
                (
                    text => URLStrings.To_Bounded_String ("gemini://asdf.com"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("gemini"),
                        host     => URLStrings.To_Bounded_String ("asdf.com"),
                        port     => PORT_GEMINI,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("scheme is parsed correctly")
                ),
                (
                    text => URLStrings.To_Bounded_String ("1bad://url.com"),
                    expected => (
                        scheme   => URLStrings.Null_Bounded_String,
                        error    => BAD_SCHEME,
                        errorIdx => 1,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("schemes must start with ALPHA")
                ),
                (
                    text => URLStrings.To_Bounded_String ("as%df://host.org"),
                    expected => (
                        scheme => URLStrings.Null_Bounded_String,
                        error => BAD_SCHEME,
                        errorIdx => 3,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("schemes must be only ALPHA / DIGIT / '+' / '-' / '.'")
                ),
                (
                    text => URLStrings.To_Bounded_String ("a1s+df-foo.bar://host.org"),
                    expected => (
                        scheme => URLStrings.To_Bounded_String ("a1s+df-foo.bar"),
                        error  => NONE,
                        host   => URLStrings.To_Bounded_String ("host.org"),
                        others => <>
                    ),
                    testMsg => To_Unbounded_String ("schemes can contain ALPHA / DIGIT / '+' / '-' / '.'")
                ),
                ---------------------------------------------------------------
                -- Userinfo parsing
                ---------------------------------------------------------------
                (
                    text => URLStrings.To_Bounded_String ("https://jim:beam@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("https"),
                        user     => URLStrings.To_Bounded_String ("jim"),
                        password => URLStrings.To_Bounded_String ("beam"),
                        host     => URLStrings.To_Bounded_String ("host.org"),
                        port     => PORT_HTTPS,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("User info with a single semicolon is parsed properly.")
                ),
                (
                    text => URLStrings.To_Bounded_String ("weird://ji:m:be:am@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("weird"),
                        user     => URLStrings.To_Bounded_String ("ji:m:be:am"),
                        host     => URLStrings.To_Bounded_String ("host.org"),
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("User info for strange schemes with multiple semicolons is parsed into the 'user' field.")
                ),
                (
                    text => URLStrings.To_Bounded_String ("http://steve:this%6Ehere@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("http"),
                        user     => URLStrings.To_Bounded_String ("steve"),
                        password => URLStrings.To_Bounded_String ("thisnhere"),
                        host     => URLStrings.To_Bounded_String ("host.org"),
                        port     => PORT_HTTP,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Embedded percent-encoded characters are OK in the userinfo (1)")
                ),
                (
                    text => URLStrings.To_Bounded_String ("http://steve:this%6ehere@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("http"),
                        user     => URLStrings.To_Bounded_String ("steve"),
                        password => URLStrings.To_Bounded_String ("thisnhere"),
                        host     => URLStrings.To_Bounded_String ("host.org"),
                        port     => PORT_HTTP,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Embedded percent-encoded characters are OK in the userinfo (2)")
                ),
                (
                    text => URLStrings.To_Bounded_String ("http://steve:pw%2here@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("http"),
                        error    => BAD_PERCENT_ENCODING,
                        errorIdx => 19,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Incorrect percent encoded characters are caught (1)")
                ),
                (
                    text => URLStrings.To_Bounded_String ("http://%1steve:pw@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("http"),
                        error    => BAD_PERCENT_ENCODING,
                        errorIdx => 11,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Incorrect percent encoded characters are caught (2)")
                ),
                (
                    text => URLStrings.To_Bounded_String ("http://steve:pw%A@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("http"),
                        error    => BAD_PERCENT_ENCODING,
                        errorIdx => 19,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Incorrect percent encoded characters are caught (3)")
                ),
                ---------------------------------------------------------------
                -- host/port parsing
                ---------------------------------------------------------------
                (
                    text => URLStrings.To_Bounded_String ("http://www.host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("http"),
                        host     => URLStrings.To_Bounded_String ("www.host.org"),
                        port     => PORT_HTTP,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Normal hosts are parsed")
                ),
                (
                    text => URLStrings.To_Bounded_String ("https://www.host.org:1443"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("https"),
                        host     => URLStrings.To_Bounded_String ("www.host.org"),
                        port     => 1443,
                        error    => NONE,
                        errorIdx => 0,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Ports are parsed")
                ),
                (
                    text => URLStrings.To_Bounded_String ("https://www.host.org:"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("https"),
                        host     => URLStrings.To_Bounded_String ("www.host.org"),
                        port     => 443,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Absent ports are OK")
                ),
                (
                    text => URLStrings.To_Bounded_String ("https://www.host.org:65536"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("https"),
                        host     => URLStrings.To_Bounded_String ("www.host.org"),
                        port     => 0,
                        error    => BAD_PORT,
                        errorIdx => 27,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Ports requiring more than 16-bit representation are caught")
                ),
                (
                    text => URLStrings.To_Bounded_String ("https://www.host.org:123456"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("https"),
                        host     => URLStrings.To_Bounded_String ("www.host.org"),
                        port     => 0,
                        error    => BAD_PORT,
                        errorIdx => 27,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Ports requiring more than 16-bit representation are caught (2)")
                ),
                ---------------------------------------------------------------
                -- From Snyk research on URL parser differences
                -- https://claroty.com/2022/01/10/blog-research-exploiting-url-parsing-confusion/
                ---------------------------------------------------------------
                (
                    text => URLStrings.To_Bounded_String ("foo.com"),
                    expected => (
                        error    => NO_SCHEME,
                        errorIdx => 8,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Scheme is required for a URL")
                ),
                (
                    text => URLStrings.To_Bounded_String ("https:///foo.com"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("https"),
                        path     => URLStrings.To_Bounded_String ("/foo.com"),
                        port     => PORT_HTTPS,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Extra slash indicates an empty authority with a path")
                ),
                (
                    -- Per RFC 3986, this doesn't represent an authority, or
                    -- any sort of other path that's recognized in a URL.
                    -- path-abempty and path-absolute must start with a '/' if
                    -- present, and path-rootless must start with a segment-nz.
                    -- segment-nz can't contain a backslash.
                    text => URLStrings.To_Bounded_String ("https:\\foo.com"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("https"),
                        error    => BAD_SEGMENT_NZ,
                        errorIdx => 7,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Backslashes are an incorrect way to start the authority")
                ),
                (
                    text => URLStrings.To_Bounded_String ("http://%66%6f%6f%2e%63%6f%6d"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("http"),
                        host     => URLStrings.To_Bounded_String ("foo.com"),
                        port     => PORT_HTTP,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Percent-encoded authority is decoded properly")
                ),
                (
                    text => URLStrings.To_Bounded_String ("jndi:ldap://bad.com:1389/a"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("jndi"),
                        path     => URLStrings.To_Bounded_String ("ldap://bad.com:1389/a"),
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("Log4Shell malicious URLs")
                ),
                ---------------------------------------------------------------
                -- Paths
                ---------------------------------------------------------------
                ---------------------------------------------------------------
                -- Queries
                ---------------------------------------------------------------
                (
                    text => URLStrings.To_Bounded_String ("https://www.namecheap.com/myaccount/login/?ReturnUrl=%2fdashboard"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("https"),
                        host     => URLStrings.To_Bounded_String ("www.namecheap.com"),
                        path     => URLStrings.To_Bounded_String ("/myaccount/login/"),
                        port     => 443,
                        query    => URLStrings.To_Bounded_String ("ReturnUrl=/dashboard"),
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("URL with query and pct-encoded char")
                ),
                ---------------------------------------------------------------
                -- Fragments
                ---------------------------------------------------------------
                (
                    text => URLStrings.To_Bounded_String ("https://datatracker.ietf.org/doc/html/rfc/3986/#section-5"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("https"),
                        host     => URLStrings.To_Bounded_String ("datatracker.ietf.org"),
                        path     => URLStrings.To_Bounded_String ("/doc/html/rfc/3986/"),
                        port     => 443,
                        fragment => URLStrings.To_Bounded_String ("section-5"),
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("URL with just a fragment")
                )
            );

        begin
            for tc of testCases loop
                declare
                    u : Gembrowse.URL.URL;
                    use Ada.Text_IO;
                    package ParseErrorIO is new Ada.Text_IO.Enumeration_IO (ParseError);
                begin
                    parseURL (tc.text, u);

                    Put_Line ("");
                    Put ("Input:     " & URLStrings.To_String (tc.text) & ASCII.LF &
                         "-----------------------------------------------------" & ASCII.LF &
                         "scheme:    " & URLStrings.To_String (u.scheme) & ASCII.LF &
                         "user:      " & URLStrings.To_String (u.user) & ASCII.LF &
                         "password:  " & URLStrings.To_String (u.password) & ASCII.LF &
                         "host:      " & URLStrings.To_String (u.host) & ASCII.LF &
                         "port:     "  & u.port'Image & ASCII.LF &
                         "path:      " & URLStrings.To_String (u.path) & ASCII.LF &
                         "query:     " & URLStrings.To_String (u.query) & ASCII.LF &
                         "fragment:  " & URLStrings.To_String (u.fragment) & ASCII.LF &
                         "error:     ");
                    ParseErrorIO.Put (u.error);
                    Put_Line ("");
                    Put_Line ("errorIdx: " & u.errorIdx'Image);

                    assert (u = tc.expected, To_String (tc.testMsg));
                    count := count + 1;
                end;
            end loop;
        end testParseURL;

    begin
        testPercentEncode;
        testParseURL;
    end runTests;

end Tests.URL;
