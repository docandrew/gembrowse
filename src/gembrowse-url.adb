-------------------------------------------------------------------------------
-- gembrowse-url.adb
--
-- URL parsing and transformation routines.
--
-- Copyright 2022 Jon Andrew
--
-- @TODO should consider whether to null all fields when an error is caught.
-- If a user of this package is lazy they might get into a situation where a
-- not-well-formed or malicious URL is _partially_ successful in parsing, but
-- if they don't check the error field they might not realize it.
-- Right now this will nullify whatever field is being parsed when the error is
-- found and then stop parsing.
-------------------------------------------------------------------------------
with Ada.Assertions;
with Ada.Text_IO;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.Unbounded;

-------------------------------------------------------------------------------
-- Some ABNF notes:
-- [ item ] indicates item is optional
-- *3 ( item ) indicates 0 to 3 items
-- 1* ( item ) indicates at least 1 item
-- 2*3 (item) indicates 2 or 3 items
-- ( item1 / item2 ) indicates item1 or item2
-------------------------------------------------------------------------------
package body Gembrowse.URL with SPARK_Mode is

    ParseException : exception;

    type SetMember is access function (c : Character) return Boolean;

    ---------------------------------------------------------------------------
    -- isAuthorityDelimiter
    --
    -- If a character marks the end of an authority, return True, otherwise
    -- return False. 
    -- "AuthorityDelimiter" is not a production of the RFC 3986 grammar, but
    -- an authority is terminated either by a '/' for a path-abempty, a '?'
    -- for optional query, or '#' for optional fragment.
    ---------------------------------------------------------------------------
    function isAuthorityDelimiter (c : Character) return Boolean is
    begin
        case c is
            when '/' | '?' | '#' => return True;
            when others => return False;
        end case;
    end isAuthorityDelimiter;

    ---------------------------------------------------------------------------
    -- isGenDelimiter
    ---------------------------------------------------------------------------
    function isGenDelimiter (c : Character) return Boolean is
    begin
        case c is
            when ':' | '/' | '?' | '#' | '[' | ']' | '@' =>
                return True;
            when others =>
                return False;
        end case;
    end isGenDelimiter;

    ---------------------------------------------------------------------------
    -- isSubDelimiter
    ---------------------------------------------------------------------------
    function isSubDelimiter (c : Character) return Boolean is
    begin
        case c is
            when '!' | '$' | '&' | ''' | '(' | ')' | '*' | '+' | ',' | ';' | '=' =>
                return True;
            when others =>
                return False;
        end case;
    end isSubDelimiter;

    ---------------------------------------------------------------------------
    -- isUnreserved
    ---------------------------------------------------------------------------
    function isUnreserved (c : Character) return Boolean is
    begin
        case c is
            when 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '_' | '~' =>
                return True;
            when others =>
                return False;
        end case;
    end isUnreserved;

    ---------------------------------------------------------------------------
    -- isReserved
    ---------------------------------------------------------------------------
    function isReserved (c : Character) return Boolean is
    begin
        return isGenDelimiter (c) or isSubDelimiter (c);
    end isReserved;

    ---------------------------------------------------------------------------
    -- isPchar
    -- pchar := unreserved / pct-encoded / sub-delims / ':' / '@'
    -- Note that this function does NOT test for pct-encoded. This must be
    -- done separately since it requires more than one character to determine
    -- that a pct-encoded string is valid.
    ---------------------------------------------------------------------------
    function isPchar (c : Character) return Boolean is
    begin
        return isUnreserved (c) or isSubDelimiter (c) or c = ':' or c = '@';
    end isPchar;

    ---------------------------------------------------------------------------
    -- isQuery
    -- query := *( pchar / '/' / '?')
    -- Note that this function does NOT test for pct-encoded query members.
    -- This must be done separately (see isPchar)
    ---------------------------------------------------------------------------
    function isQuery (c : Character) return Boolean is
    begin
        return isPchar (c) or c = '/' or c = '?';
    end isQuery;

    ---------------------------------------------------------------------------
    -- isFragment
    -- Same ABNF production as isQuery.
    -- fragment := *( pchar / '/' / '?')
    ---------------------------------------------------------------------------
    function isFragment (c : Character) return Boolean is
    begin
        return isQuery (c);
    end isFragment;

    ---------------------------------------------------------------------------
    -- isDigit
    -- @return True if c is '0'..'9'
    ---------------------------------------------------------------------------
    function isDigit (c : Character) return Boolean is
    begin
        return c in '0'..'9';
    end isDigit;

    ---------------------------------------------------------------------------
    -- isHexDigit
    -- @return True if c in '0'..'9' or c in 'a'..'f' or c in 'A'..'F'
    ---------------------------------------------------------------------------
    function isHexDigit (c : Character) return Boolean is
    begin
        return isDigit (c) or c in 'a'..'f' or c in 'A'..'F';
    end isHexDigit;

    ---------------------------------------------------------------------------
    -- isAlpha
    ---------------------------------------------------------------------------
    function isAlpha (c : Character) return Boolean is
    begin
        return c in 'a'..'z' or c in 'A'..'Z';
    end isAlpha;

    ---------------------------------------------------------------------------
    -- isAlphaDigit
    ---------------------------------------------------------------------------
    function isAlphaDigit (c : Character) return Boolean is
    begin
        return isAlpha (c) or isDigit (c);
    end isAlphaDigit;

    ---------------------------------------------------------------------------
    -- percentDecode
    ---------------------------------------------------------------------------
    function percentDecode (s : String) return Character with
        Pre => s'Last = 3 and
               s(s'First) = '%' and
               isHexDigit (s(s'First + 1)) and
               isHexDigit (s(s'First + 2))
    is
    begin
        return Character'Val (Natural'Value ("16#" & s((s'First + 1) .. (s'First + 2)) & "#"));
    end percentDecode;

    ---------------------------------------------------------------------------
    -- parseURL
    -- Given a URL with gemini://host/etc. we need to parse out the individual
    -- components so we know what host to connect to.
    --
    -- URLs are: scheme://authority/path?query#fragment
    --
    -- actual grammar is scheme ":" hier-part ["?" query] ["#" fragment]
    ---------------------------------------------------------------------------
    procedure parseURL (s : URLStrings.Bounded_String; u : out URL) is
        newURL : URL := (
                    scheme   => URLStrings.Null_Bounded_String,
                    user     => URLStrings.Null_Bounded_String,
                    password => URLStrings.Null_Bounded_String,
                    host     => URLStrings.Null_Bounded_String,
                    port     => 0,
                    path     => URLStrings.Null_Bounded_String,
                    query    => URLStrings.Null_Bounded_String,
                    fragment => URLStrings.Null_Bounded_String,
                    error    => NONE,
                    errorIdx => 0);
        
        -- Where in the URL we're parsing from.
        idx : URLIndex := 1;

        use URLStrings;

        -----------------------------------------------------------------------
        -- peekNext
        -- return next character in the stream.
        -----------------------------------------------------------------------
        function peekNext return Character is
        begin
            -- if we can formally verify this isn't a problem with internal
            -- checks we can get rid of this, but will be useful for testing.
            if idx > s.Length then
                raise ParseException;
            end if;

            return s.Element(idx);
        end peekNext;

        -----------------------------------------------------------------------
        -- lookahead
        -- Given a character, look ahead to see if it occurs in the string
        -- before a character occurs in the string which is a member of some
        -- set of delimiters, as determined by the before() parameter.
        -----------------------------------------------------------------------
        function lookahead (lookfor : Character; before : SetMember) return Natural is
        begin
            for i in idx .. s.Length loop
                if s.Element(i) = lookfor then
                    return i;
                elsif before (s.Element(i)) then
                    return 0;
                end if;
            end loop;

            return 0;
        end lookahead;

        -----------------------------------------------------------------------
        -- getNext
        -- return next character in the stream and advance the read index.
        -----------------------------------------------------------------------
        function getNext return Character is
            c : Character := peekNext;
        begin
            idx := idx + 1;
            return c;
        end getNext;

        -----------------------------------------------------------------------
        -- getNextPercentEncoded
        -- Return the value of the next percent-encoded character in the stream.
        -- If the next character in the stream is not percent-encoded or is an
        -- invalid percent-encoding, then valid will be False and the URI
        -- should be considered NOT well-formed.
        -----------------------------------------------------------------------
        procedure getNextPercentEncoded (pc : out Character; valid : out Boolean) is
            c : Character := peekNext;
            c1 : Character;
            c2 : Character;
        begin
            if c /= '%' then
                pc := ASCII.NUL;
                valid := False;
                return;
            end if;

            -- confirm there's enough characters in the stream remaining
            -- to attempt to parse this.
            if idx > s.Length - 2 then
                pc    := ASCII.NUL;
                valid := False;
            else
                c := getNext;
                c1 := getNext;
                c2 := getNext;

                if not isHexDigit (c1) or not isHexDigit (c2) then
                    pc    := ASCII.NUL;
                    valid := False;
                else
                    pc := percentDecode (c & c1 & c2);
                    valid := True;
                end if;
            end if;
        end getNextPercentEncoded;

        -----------------------------------------------------------------------
        -- getNextPChar
        -- return the next RFC 3986 pchar in the stream and advance the read
        -- index. If the next character in the stream is not a pchar, the
        -- valid out param will be False, and the URI should be considered
        -- NOT well-formed.
        -----------------------------------------------------------------------
        procedure getNextPChar (pc : out Character; valid : out Boolean) is
        begin
            if peekNext = '%' then
                getNextPercentEncoded (pc, valid);
                return;
            elsif not isPchar (peekNext) then
                pc := ASCII.NUL;
                valid := False;
            else
                pc := getNext;
                valid := True;
            end if;
        end getNextPChar;

        -----------------------------------------------------------------------
        -- parseScheme
        -- The scheme is mandatory for a URI, and ends when we encounter the
        --  first ":"
        --
        -- scheme ::= ALPHA *(ALPHA / DIGIT / "+" / "-" / ".")
        -----------------------------------------------------------------------
        procedure parseScheme is
            c : Character;

            -------------------------------------------------------------------
            -- isSchemeChar
            -------------------------------------------------------------------
            function isSchemeChar (c : Character) return Boolean is
            begin
                return isAlphaDigit (c) or 
                    c = '+' or
                    c = '-' or
                    c = '.';
            end isSchemeChar;

        begin
            if idx > s.Length then
                return;
            end if;

            -- first char of scheme needs to be a letter.
            if not isAlpha (peekNext) then
                newURL.error := BAD_SCHEME;
                newURL.errorIdx := 1;
                return;
            end if;

            newURL.scheme.Append (getNext);

            for i in 2..s.Length loop
                c := peekNext;

                if c = ':' then
                    c := getNext;       -- eat the ':'
                    return;
                else
                    -- at this point, we haven't gotten to the end of the scheme
                    -- yet (as defined by the ':' char) so any non-scheme chars
                    -- are an error.
                    if not isSchemeChar (c) then
                        newURL.error    := BAD_SCHEME;
                        newURL.errorIdx := i;
                        newURL.scheme   := URLStrings.Null_Bounded_String;
                        return;
                    else
                        newURL.scheme.Append (getNext);
                    end if;
                end if;
            end loop;
        end parseScheme;

        -----------------------------------------------------------------------
        -- parseSegment
        --
        -- segment := *pchar
        -----------------------------------------------------------------------
        procedure parseSegment is
            c     : Character;
            valid : Boolean;
        begin
            loop
                exit when idx > s.Length or newURL.error /= NONE;
                
                -- if the next character is not a pchar, then we are done
                -- with this segment.
                if isPchar (peekNext) or peekNext = '%' then
                    getNextPChar (c, valid);

                    if valid then
                        newURL.path.Append (c);
                    else
                        newURL.error    := BAD_PCHAR;
                        newURL.errorIdx := idx;
                        newURL.path     := URLStrings.Null_Bounded_String;
                    end if;
                else
                    return;
                end if;
            end loop;
        end parseSegment;

        ---------------------------------------------------------------------------
        -- parseSegmentNZ
        -- segment-nz := 1*pchar
        --
        -- Same as parseSegment, but if we've reached the end of the stream or
        -- there is not at least one pchar, then this is an error.
        ---------------------------------------------------------------------------
        procedure parseSegmentNZ is
        begin
            if newURL.error /= NONE then
                return;
            end if;

            if idx > s.Length then
                newURL.error    := BAD_SEGMENT_NZ;
                newURL.errorIdx := idx;
                newURL.path     := URLStrings.Null_Bounded_String;
                return;
            end if;

            if isPchar (peekNext) or peekNext = '%' then
                parseSegment;
            else
                newURL.error    := BAD_SEGMENT_NZ;
                newURL.errorIdx := idx;
                newURL.path     := URLStrings.Null_Bounded_String;
            end if;
        end parseSegmentNZ;

        -----------------------------------------------------------------------
        -- parsePathABEmpty
        --
        -- path-abempty := *("/" segment)   ; path begins with "/" or is empty.
        -----------------------------------------------------------------------
        procedure parsePathABEmpty is
        begin
            -- a path-abempty is optional, but if present must begin with a '/'
            while newURL.error = NONE and (idx <= s.Length and then peekNext = '/') loop
                newURL.path.Append (getNext);       -- consume+append the '/'
                parseSegment;
            end loop;
        end parsePathABEmpty;

        -----------------------------------------------------------------------
        -- parsePathAbsolute
        -- path-absolute = "/" [segment-nz *( "/" segment )]
        -----------------------------------------------------------------------
        procedure parsePathAbsolute is
        begin
            if newURL.error /= NONE or idx > s.Length then
                return;
            end if;

            -- path-absolute must start with a '/'
            if idx > s.Length or else peekNext /= '/' then
                newURL.error    := BAD_ABSOLUTE_PATH;
                newURL.errorIdx := idx;
                newURL.path     := URLStrings.Null_Bounded_String;
                return;
            end if;

            newURL.path.Append (getNext);       -- consume+append the '/'

            if idx > s.Length then
                return;
            end if;

            -- here there's an optional segment, which is at least one pchar.
            -- If it's not a pchar, then we're done parsing the path-absolute.
            -- (it was just "/")
            --
            -- If we have the segment after the initial '/' then we have
            -- an optional number of *( '/' segment), which is the same
            -- as a path-abempty.

            if isPchar (peekNext) or peekNext = '%' then
                parseSegment;
                parsePathABEmpty;
            end if;
        end parsePathAbsolute;

        -----------------------------------------------------------------------
        -- parsePathRootless
        -- path-rootless := segment-nz *("/" segment)
        -----------------------------------------------------------------------
        procedure parsePathRootless is
        begin
            if newURL.error /= NONE or idx > s.Length then
                return;
            end if;

            parseSegmentNZ;
            parsePathABEmpty;
        end parsePathRootless;

        -----------------------------------------------------------------------
        -- parseUserInfo
        -- userinfo := *(unreserved / pct-encoded / sub-delims / ":")
        -----------------------------------------------------------------------
        procedure parseUserInfo is
            userinfo : URLStrings.Bounded_String := Null_Bounded_String;

            -- if userinfo is pct-encoded.
            c     : Character;
            valid : Boolean;

            ignore : Character;

            -- Is there more than one semicolon in the userinfo?
            numSemis : Natural  := 0;
            semiIdx  : URLIndex := 0;   -- only valid if numSemis = 1
        begin
            if newURL.error /= NONE or idx > s.Length then
                return;
            end if;

            loop
                if isUnreserved (peekNext) or isSubDelimiter (peekNext) then
                    
                    userinfo.Append (getNext);

                elsif peekNext = ':' then
                   
                    numSemis := numSemis + 1;
                    userinfo.Append (getNext);

                    if numSemis = 1 then
                        -- record the semicolon index _after_ percent-encoded
                        -- chars have been decoded and appended to the
                        -- userinfo string, so we know where to split user:pass
                        -- if appropriate.
                        semiIdx := userinfo.Length;
                    else
                        semiIdx := 0;
                    end if;

                elsif peekNext = '%' then

                    getNextPercentEncoded (c, valid);
                    
                    if not valid then
                        newURL.error    := BAD_PERCENT_ENCODING;
                        newURL.errorIdx := idx;
                        newURL.user     := URLStrings.Null_Bounded_String;
                        newURL.password := URLStrings.Null_Bounded_String;
                        return;
                    else
                        userinfo.Append (c);
                    end if;

                elsif peekNext = '@' then
                    -- consume the @
                    ignore := getNext;
                    exit;
                else
                    -- invalid character in the userinfo.
                    newURL.error    := BAD_USERINFO;
                    newURL.errorIdx := idx;
                    newURL.user     := URLStrings.Null_Bounded_String;
                    newURL.password := URLStrings.Null_Bounded_String;
                    return;
                end if;
            end loop;

            -- now that we have the userinfo data, we can see if it's a common
            -- user:pass like we might be expecting. If so, split it by the semi
            -- for convenience. Otherwise, just set user field to the whole
            -- string.
            if numSemis = 1 then
                newURL.user     := URLStrings.Bounded_Slice (userinfo, 1, semiIdx - 1);
                newURL.password := URLStrings.Bounded_Slice (userinfo, semiIdx + 1, userinfo.Length);
            else
                newURL.user     := userinfo;
            end if;
        end parseUserInfo;

        -----------------------------------------------------------------------
        -- parseh16
        -- 16 bits of address represented in hexadecimal.
        --
        -- h16 ::= 1*4HEXDIG
        -----------------------------------------------------------------------
        procedure parseh16 is
        begin
            if idx > s.Length or newURL.error /= NONE then
                return;
            end if;

            -- grab up to 4 hex digits, or until we don't see a hex digit, or
            -- we run out of stream.
            for i in 1..4 loop
                exit when idx > s.Length or else not isHexDigit (peekNext);

                newURL.host.Append (getNext);
            end loop;
        end parseh16;

        -----------------------------------------------------------------------
        -- parseDecOctet
        -- dec-octet ::= DIGIT              ; 0-9
        --           ::= %x31-39 DIGIT      ; 10-99
        --           ::= "1" 2DIGIT         ; 100-199
        --           ::= "2" %x30-34 DIGIT  ; 200-249
        --           ::= "25" %x30-35       ; 250-255
        -----------------------------------------------------------------------
        procedure parseDecOctet is
            subtype NatDigit is Natural range 0..9;
            type ShiftRegister is array (Positive range 1..3) of NatDigit;
            shiftReg : ShiftRegister := (others => 0);

            c : Character;
        begin
            if idx > s.Length or newURL.error /= NONE then
                return;
            end if;

            -- need at least one digit.
            if not isDigit (peekNext) then
                newURL.error := BAD_IPV4ADDRESS;
                newURL.errorIdx := idx;
            end if;

            for i in 1..3 loop
                exit when idx > s.Length or else not isDigit (peekNext);

                c := getNext;
                shiftReg (1) := shiftReg (2);
                shiftReg (2) := shiftReg (3);
                shiftReg (3) := NatDigit'Value ("" & c);

                newURL.host.Append (c);
            end loop;

            if shiftReg(1) * 100 + shiftReg(2) * 10 + shiftReg(3) > 255 then
                newURL.error := BAD_IPV4ADDRESS;
                newURL.errorIdx := idx;
            end if;
        end parseDecOctet;

        -----------------------------------------------------------------------
        -- parseIPv4Address
        -- IPv4Address ::= dec-octet "." dec-octet "." dec-octet "." dec-octet
        --
        -- This should only be called when an IPv4 address is required, such
        -- as during the ls32 parse. See note in RFC3986 3.2.2 regarding the
        -- ambiguous syntax rules for IPv4 Address vs reg-name.
        -----------------------------------------------------------------------
        procedure parseIPv4Address is
        begin
            if idx > s.Length or newURL.error /= NONE then
                return;
            end if;

            parseDecOctet;

            for i in 1..3 loop
                if idx > s.Length or else peekNext /= '.' then
                    newURL.error := BAD_IPV4ADDRESS;
                    newURL.errorIdx := idx;
                    return;
                else
                    newURL.host.Append (getNext);   -- append+consume '.'
                    parseDecOctet;
                end if;
            end loop;
        end parseIPv4Address;

        -----------------------------------------------------------------------
        -- parsels32
        -- least-significant 32-bits of an IPv6 address
        --
        -- ls32 ::= (h16 ":" h16) / IPv4Address
        -----------------------------------------------------------------------
        procedure parsels32 is
        begin
            null;
        end parsels32;

        -----------------------------------------------------------------------
        -- parseIPv6Address
        -- IPv6Address ::=                            6( h16 ":" ) ls32
        --             ::=                       "::" 5( h16 ":" ) ls32
        --             ::= [               h16 ] "::" 4( h16 ":" ) ls32
        --             ::= [*1 ( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
        --             ::= [*2 ( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
        --             ::= [*3 ( h16 ":" ) h16 ] "::"    h16 ":"   ls32
        --             ::= [*4 ( h16 ":" ) h16 ] "::"              ls32
        --             ::= [*5 ( h16 ":" ) h16 ] "::"              h16
        --             ::= [*6 ( h16 ":" ) h16 ] "::"
        -----------------------------------------------------------------------
        procedure parseIPv6Address is
        begin
            if newURL.error /= NONE or idx > s.Length then
                return;
            end if;

            null;
        end parseIPv6Address;

        -----------------------------------------------------------------------
        -- parseHEXDIG 
        -----------------------------------------------------------------------
        procedure parseHEXDIG is
        begin
            if newURL.error /= NONE or idx > s.Length then
                return;
            end if;

            if not isHexDigit (peekNext) then
                newURL.error := BAD_HEXDIGIT;
                newURL.errorIdx := idx;
                return;
            end if;

            newURL.host.Append (getNext);
        end parseHEXDIG;

        -----------------------------------------------------------------------
        -- parseIPvFuture
        -- IPvFuture ::= "v" 1*HEXDIG "." 1*(unreserved / sub-delims / ":")
        -----------------------------------------------------------------------
        procedure parseIPvFuture is
        begin
            if newURL.error /= NONE or idx > s.Length then
                return;
            end if;

            if peekNext /= 'v' then
                newURL.error    := BAD_IPVFUTURE;
                newURL.errorIdx := idx;
                newURL.host     := URLStrings.Null_Bounded_String;
                return;
            end if;

            newURL.host.Append (getNext);      -- append+consume 'v'

            parseHEXDIG;

            if idx > s.Length or else peekNext /= '.' then
                newURL.error    := BAD_IPVFUTURE;
                newURL.errorIdx := idx;
                newURL.host     := URLStrings.Null_Bounded_String;
            else
                newURL.host.Append (getNext);  -- append+consume '.'

                -- needs to be at least one unreserved / sub-delims / ":"
                if idx <= s.Length and then (isUnreserved (peekNext) or isSubDelimiter (peekNext) or peekNext = ':') then
                    newURL.host.Append (getNext);
                else
                    newURL.error    := BAD_IPVFUTURE;
                    newURL.errorIdx := idx;
                    newURL.host     := URLStrings.Null_Bounded_String;
                end if;

                while idx <= s.Length and then (isUnreserved (peekNext) or isSubdelimiter (peekNext) or peekNext = ':') loop
                    newURL.host.Append (getNext);
                end loop;
            end if;
        end parseIPvFuture;

        -----------------------------------------------------------------------
        -- parseIPLiteral
        -- IP-literal ::= "[" ( IPv6address / IPvFuture ) "]"
        -----------------------------------------------------------------------
        procedure parseIPLiteral is
        begin
            if newURL.error /= NONE or idx > s.Length then
                return;
            end if;

            -- consume leading '['
            if getNext /= '[' then
                newURL.error    := BAD_IPLITERAL;
                newURL.errorIdx := idx;
                newURL.host     := URLStrings.Null_Bounded_String;
                return;
            end if;

            if peekNext = 'v' then
                parseIPvFuture;
            else
                parseIPv6Address;
            end if;

            -- consume trailing ']'
            if idx > s.Length or else getNext /= ']' then
                newURL.error    := BAD_IPLITERAL;
                newURL.errorIdx := idx;
                newURL.host     := URLStrings.Null_Bounded_String;
                return;
            end if;
        end parseIPLiteral;

        -----------------------------------------------------------------------
        -- parseRegName
        -- reg-name := *( unreserved / pct-encoded / sub-delims )
        -----------------------------------------------------------------------
        procedure parseRegName is
            pc    : Character;
            valid : Boolean;
        begin
            if newURL.error /= NONE or idx > s.Length then
                return;
            end if;

            while idx <= s.Length and then (isUnreserved (peekNext) or isSubDelimiter (peekNext) or peekNext = '%') loop
                if peekNext = '%' then
                    getNextPercentEncoded (pc, valid);

                    if not valid then
                        newURL.error    := BAD_PERCENT_ENCODING;
                        newURL.errorIdx := idx;
                        newURL.host     := URLStrings.Null_Bounded_String;
                    else
                        newURL.host.Append (pc);
                    end if;
                else
                    newURL.host.Append (getNext);
                end if;
            end loop;
        end parseRegName;

        -----------------------------------------------------------------------
        -- parseHost
        --
        -- host := IP-literal / IPv4Address / reg-name
        -----------------------------------------------------------------------
        procedure parseHost is
        begin
            if newURL.error /= NONE or idx > s.Length then
                return;
            end if;

            if peekNext = '[' then
                parseIPLiteral;
            else
                -- use this for both IPv4 Address and reg-name
                parseRegName;
            end if;
        end parseHost;

        -----------------------------------------------------------------------
        -- parsePort
        --
        -- port = *DIGIT (per RFC)
        -- port = *5(DIGIT) (per common sense and decency - what we do here.)
        --
        -- The rule here says many digits can be part of the port, however we
        -- understand a port to be in the range 0-65535. Maybe there's some
        -- weird scheme out there that uses "port" for something that we 
        -- don't strictly understand as a TCP/UDP port, but if so, I don't want
        -- to know about it. Abiding strictly by the RFC here seems like a
        -- recipe for mischief, i.e. giving a very large port number and
        -- causing overflow.
        -----------------------------------------------------------------------
        procedure parsePort is
            subtype NatDigit is Natural range 0..9;
            type ShiftRegister is array (Positive range 1..5) of NatDigit;
            shiftReg : ShiftRegister := (others => 0);
        begin
            if newURL.error /= NONE or idx > s.Length then
                return;
            end if;

            for i in 1..5 loop
                exit when idx > s.Length or else not isDigit (peekNext);

                shiftReg(1) := shiftReg(2);
                shiftReg(2) := shiftReg(3);
                shiftReg(3) := shiftReg(4);
                shiftReg(4) := shiftReg(5);
                shiftReg(5) := NatDigit'Value ("" & getNext);
            end loop;

            -- Perform validation. This parser considers it an error if the
            -- port range exceeds 65535. So if the first 5 digits we parse are
            -- more than 65535, or if we see a 6th digit, then flag
            -- the whole thing as an error.
            if shiftReg(1) * 10000 + 
               shiftReg(2) * 1000 +
               shiftReg(3) * 100 +
               shiftReg(4) * 10 +
               shiftReg(5) > 65535 or 
               (idx <= s.Length and then isDigit(peekNext)) then

                newURL.error    := BAD_PORT;
                newURL.errorIdx := idx;
                newURL.port     := 0;

            end if;
        end parsePort;

        -----------------------------------------------------------------------
        -- parseAuthority
        -- authority := [ userinfo "@" ] host [":" port]
        -----------------------------------------------------------------------
        procedure parseAuthority is
        begin
            if newURL.error /= NONE or idx > s.Length then
                return;
            end if;

            -- We won't know what's userinfo or not until we encounter a '@',
            -- so look ahead to see if that's something we need to parse first.
            if lookahead ('@', isAuthorityDelimiter'Access) /= 0 then
                parseUserInfo;
            end if;

            parseHost;

            if idx <= s.Length and then peekNext = ':' then
                parsePort;
            end if;
        end parseAuthority;

        -----------------------------------------------------------------------
        -- parseHierPart
        -- hier-part := "//" authority path-abempty
        --              / path-absolute
        --              / path-rootless
        --              / path-empty
        -----------------------------------------------------------------------
        procedure parseHierPart is
            c : Character;
        begin
            if newURL.error /= NONE or idx > s.Length then
                Ada.Text_IO.Put_Line ("A");
                return;
            end if;

            if peekNext = '/' then
                
                c := getNext;       -- consume first '/'

                if peekNext = '/' then
                    c := getNext;   -- consume second '/'
                    -- expect an authority then a path-abempty
                    parseAuthority;
                    parsePathABEmpty;
                else
                    -- had a single '/' so must be an absolute path.
                    parsePathAbsolute;
                end if;
            else
                -- path-rootless
                parsePathRootless;
            end if;
        end parseHierPart;

        -----------------------------------------------------------------------
        -- parseQuery
        -----------------------------------------------------------------------
        procedure parseQuery is
        begin
            null;
        end parseQuery;

        -----------------------------------------------------------------------
        -- parseFragment
        -----------------------------------------------------------------------
        procedure parseFragment is
        begin
            null;
        end parseFragment;

    -- actual meat of parseURL
    begin
        parseScheme;

        parseHierPart;

        if idx < s.Length and then peekNext = '?' then
            parseQuery;
        end if;

        if idx < s.Length and then peekNext = '#' then
            parseFragment;
        end if;

        u := newURL;
    end parseURL;

    ---------------------------------------------------------------------------
    -- Chars that need to be percent-encoded.
    -- includes all reserved chars and couple extras - % and space
    ---------------------------------------------------------------------------
    function mustEncode (c : Character) return Boolean is
    begin
        case c is
            when ':' | '/' | '?' | '#' | '[' | ']' | '@' | '!' | '$' | '&' | 
                 ''' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '%' | ' ' =>
                return True;
            when others =>
                return False;
        end case;
    end mustEncode;

    ---------------------------------------------------------------------------
    -- percentEncode
    ---------------------------------------------------------------------------
    procedure percentEncode (s : in out Unbounded_String) is
        news : Unbounded_String := Null_Unbounded_String;
        c : Character;

        package ASCII_IO is new Ada.Text_IO.Integer_IO (Natural);
    begin
        for i in 1..s.Length loop
            
            c := s.Element (i);

            if mustEncode (c) or Character'Pos(c) < 32 or Character'Pos(c) > 126 then
                declare
                    asciiVal : String(1..6);
                begin
                    -- This is going to be 16#nn#, we just want nn so take
                    -- the slice.
                    ASCII_IO.Put (asciiVal, Character'Pos(c), 16);
                    news.Append ("%" & asciiVal (4..5));
                end;
            else
                news.Append (c);
            end if;
        end loop;

        s := news;
    end percentEncode;

    ---------------------------------------------------------------------------
    -- runTests
    --
    -- Unit testing suite for URL parsing subprograms.
    ---------------------------------------------------------------------------
    procedure runTests with SPARK_Mode => Off is

        use Ada.Assertions;
        use Ada.Strings.Unbounded;

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
            end loop;

        end testPercentEncode;

        -----------------------------------------------------------------------
        -- testParseURL
        -----------------------------------------------------------------------
        procedure testParseURL is
            type ParseURLTestCase is record
                text     : URLStrings.Bounded_String;
                expected : URL;
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
                        error    => NONE,
                        -- port     => 0,
                        -- errorIdx => 0,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("1. scheme is parsed correctly")
                ),
                (
                    text => URLStrings.To_Bounded_String ("1bad://url.com"),
                    expected => (
                        scheme   => URLStrings.Null_Bounded_String,
                        error    => BAD_SCHEME,
                        errorIdx => 1,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("2. schemes must start with ALPHA")
                ),
                (
                    text => URLStrings.To_Bounded_String ("as%df://host.org"),
                    expected => (
                        scheme => URLStrings.Null_Bounded_String,
                        error => BAD_SCHEME,
                        errorIdx => 3,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("3. schemes must be only ALPHA / DIGIT / '+' / '-' / '.'")
                ),
                (
                    text => URLStrings.To_Bounded_String ("a1s+df-foo.bar://host.org"),
                    expected => (
                        scheme => URLStrings.To_Bounded_String ("a1s+df-foo.bar"),
                        error  => NONE,
                        host   => URLStrings.To_Bounded_String ("host.org"),
                        others => <>
                    ),
                    testMsg => To_Unbounded_String ("4. schemes can contain ALPHA / DIGIT / '+' / '-' / '.'")
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
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("5. User info with a single semicolon is parsed properly.")
                ),
                (
                    text => URLStrings.To_Bounded_String ("weird://ji:m:be:am@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("weird"),
                        user     => URLStrings.To_Bounded_String ("ji:m:be:am"),
                        host     => URLStrings.To_Bounded_String ("host.org"),
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("6. User info for strange schemes with multiple semicolons is parsed into the 'user' field.")
                ),
                (
                    text => URLStrings.To_Bounded_String ("http://steve:this%6Ehere@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("http"),
                        user     => URLStrings.To_Bounded_String ("steve"),
                        password => URLStrings.To_Bounded_String ("thisnhere"),
                        host     => URLStrings.To_Bounded_String ("host.org"),
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("7. Embedded percent-encoded characters are OK in the userinfo (1)")
                ),
                (
                    text => URLStrings.To_Bounded_String ("http://steve:this%6ehere@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("http"),
                        user     => URLStrings.To_Bounded_String ("steve"),
                        password => URLStrings.To_Bounded_String ("thisnhere"),
                        host     => URLStrings.To_Bounded_String ("host.org"),
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("8. Embedded percent-encoded characters are OK in the userinfo (2)")
                ),
                (
                    text => URLStrings.To_Bounded_String ("http://steve:pw%2here@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("http"),
                        error    => BAD_PERCENT_ENCODING,
                        errorIdx => 19,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("9. Incorrect percent encoded characters are caught (1)")
                ),
                (
                    text => URLStrings.To_Bounded_String ("http://%1steve:pw@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("http"),
                        error    => BAD_PERCENT_ENCODING,
                        errorIdx => 11,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("10. Incorrect percent encoded characters are caught (2)")
                ),
                (
                    text => URLStrings.To_Bounded_String ("http://steve:pw%A@host.org"),
                    expected => (
                        scheme   => URLStrings.To_Bounded_String ("http"),
                        error    => BAD_PERCENT_ENCODING,
                        errorIdx => 19,
                        others   => <>
                    ),
                    testMsg => To_Unbounded_String ("11. Incorrect percent encoded characters are caught (3)")
                )
            );

        begin
            for tc of testCases loop
                declare
                    u : URL;
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
                end;
            end loop;
        end testParseURL;

    begin
        testPercentEncode;
        testParseURL;
    end runTests;

end Gembrowse.URL;