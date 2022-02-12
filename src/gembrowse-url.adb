-------------------------------------------------------------------------------
-- gembrowse-url.adb
--
-- URL parsing and transformation routines.
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;

-------------------------------------------------------------------------------
-- Some ABNF notes:
-- [ item ] indicates item is optional
-- *3 ( item ) indicates 0 to 3 items
-- 1* ( item ) indicates at least 1 item
-- 2*3 (item) indicates 2 or 3 items
-- ( item1 / item2 ) indicates item1 or item2
-------------------------------------------------------------------------------
package body Gembrowse.URL with SPARK_Mode is

    -- procedure normalize (url : in out URLStrings.Bounded_String) is
    -- begin
    --     null;
    -- end normalize;

    -- function isGenDelimiter (c : Character) return Boolean is
    -- begin
    --     case c is
    --         when ':' | '/' | '?' | '#' | '[' | ']' | '@' =>
    --             return True;
    --         when others =>
    --             return False;
    --     end case;
    -- end isGenDelimiter;

    -- function isSubDelimiter (c : Character) return Boolean is
    -- begin
    --     case c is
    --         when '!' | '$' | '&' | ''' | '(' | ')' | '*' | '+' | ',' | ';' | '=' =>
    --             return True;
    --         when others =>
    --             return False;
    --     end case;
    -- end isSubDelimiter;

    -- function isUnreserved (c : Character) return Boolean is
    -- begin
    --     case c is
    --         when 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '_' | '~' =>
    --             return True;
    --         when others =>
    --             return False;
    --     end case;
    -- end isUnreserved;

    -- function isReserved (c : Character) return Boolean is
    -- begin
    --     return isGenDelimiter (c) or isSubDelimiter (c);
    -- end isReserved;

    -- ---------------------------------------------------------------------------
    -- -- isDigit
    -- -- @return True if c is '0'..'9'
    -- ---------------------------------------------------------------------------
    -- function isDigit (c : Character) return Boolean is
    -- begin
    --     return c in '0'..'9';
    -- end isDigit;

    -- ---------------------------------------------------------------------------
    -- -- isHexDigit
    -- -- @return True if c in '0'..'9' or c in 'a'..'f' or c in 'A'..'F'
    -- ---------------------------------------------------------------------------
    -- function isHexDigit (c : Character) return Boolean is
    -- begin
    --     return isDigit (c) or c in 'a'..'f' or c in 'A'..'F';
    -- end isHexDigit;


    -- function parseScheme (s : URLStrings.Bounded_String) return URLStrings.Bounded_String is
    -- begin
    --     null;
    -- end parseScheme;

    -- ---------------------------------------------------------------------------
    -- -- parseUserInfo
    -- -- userinfo = *( unreserved / pct-encoded / sub-delims / ":")
    -- -- @NOTE user:password is deprecated, and in any case, the userinfo field
    -- --  is scheme-specific. The entire userinfo string will be provided to
    -- --  the calling application.
    -- ---------------------------------------------------------------------------
    -- function parseUserInfo (s : URLStrings.Bounded_String) return URLStrings.Bounded_String is
    -- begin
    --     null;
    -- end parseUserInfo;

    -- ---------------------------------------------------------------------------
    -- -- parseHEXDIG 
    -- ---------------------------------------------------------------------------
    -- function parseHEXDIG (c : Character) return Unsigned_4 with
    --     Pre  => isHexDigit (c),
    --     Post => parseHEXDIG'Result >= 0 and parseHEXDIG'Result < 16 is
    -- begin
    --     null;
    -- end parseHEXDIG;

    -- ---------------------------------------------------------------------------
    -- -- parseh16
    -- -- 16 bits of address represented in hexadecimal.
    -- --
    -- -- h16 ::= 1*4HEXDIG
    -- ---------------------------------------------------------------------------
    -- function parseh16 (s : URLStrings.Bounded_String) return Unsigned_16 is
    -- begin
    --     null;
    -- end parseh16;

    -- ---------------------------------------------------------------------------
    -- -- parseDecOctet
    -- -- dec-octet ::= DIGIT              ; 0-9
    -- --           ::= %x31-39 DIGIT      ; 10-99
    -- --           ::= "1" 2DIGIT         ; 100-199
    -- --           ::= "2" %x30-34 DIGIT  ; 200-249
    -- --           ::= "25" %x30-35       ; 250-255
    -- ---------------------------------------------------------------------------
    -- function parseDecOctet (s : URLStrings.Bounded_String) return Unsigned_8 is
    -- begin
    --     null;
    -- end parseDecOctet;

    -- ---------------------------------------------------------------------------
    -- -- parseIPv4Address
    -- -- IPv4Address ::= dec-octet "." dec-octet "." dec-octet "." dec-octet "."
    -- ---------------------------------------------------------------------------
    -- function parseIPv4Address (s : URLStrings.Bounded_String) return Unsigned_32 is
    -- begin
    --     null;
    -- end parseIPv4Address;

    -- ---------------------------------------------------------------------------
    -- -- parsels32
    -- -- least-significant 32-bits of an IPv6 address
    -- --
    -- -- ls32 ::= (h16 ":" h16) / IPv4Address
    -- ---------------------------------------------------------------------------
    -- function parsels32 (s : URLStrings.Bounded_String) return Unsigned_32 is
    -- begin
    --     null;
    -- end parsels32;

    -- ---------------------------------------------------------------------------
    -- -- parseIPv6Address
    -- -- IPv6Address ::=                            6( h16 ":" ) ls32
    -- --             ::=                       "::" 5( h16 ":" ) ls32
    -- --             ::= [               h16 ] "::" 4( h16 ":" ) ls32
    -- --             ::= [*1 ( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
    -- --             ::= [*2 ( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
    -- --             ::= [*3 ( h16 ":" ) h16 ] "::"    h16 ":"   ls32
    -- --             ::= [*4 ( h16 ":" ) h16 ] "::"              ls32
    -- --             ::= [*5 ( h16 ":" ) h16 ] "::"              h16
    -- --             ::= [*6 ( h16 ":" ) h16 ] "::"
    -- ---------------------------------------------------------------------------
    -- function parseIPv6Address (s : URLStrings.Bounded_String) return IPv6Address is
    -- begin
    --     null;
    -- end parseIPv6Address;

    -- ---------------------------------------------------------------------------
    -- -- parseIPvFuture
    -- -- IPvFuture ::= "v" 1*HEXDIG "." 1*(unreserved / sub-delims / ":")
    -- ---------------------------------------------------------------------------
    -- function parseIPvFuture (s : URLStrings.Bounded_String) return URLStrings.Bounded_String is
    -- begin
    --     null;
    -- end parseIPvFuture;

    -- ---------------------------------------------------------------------------
    -- -- parseIPLiteral
    -- -- IP-literal ::= "[" ( IPv6address / IPvFuture ) "]"
    -- ---------------------------------------------------------------------------
    -- function parseIPLiteral (s : URLStrings.Bounded_String) return URLStrings.Bounded_String is
    -- begin
    --     null;
    -- end parseIPLiteral;

    -- ---------------------------------------------------------------------------
    -- -- parseLabel
    -- -- a single label can be no longer than 63 octets.
    -- -- label ::= let-dig [ [ ldh-str ] let-dig ]
    -- ---------------------------------------------------------------------------
    -- function parseLabel (s : URLStrings.Bounded_String) return URLStrings.Bounded_String is
    -- begin
    --     null;
    -- end parseLabel;

    -- ---------------------------------------------------------------------------
    -- -- parseSubdomain
    -- ---------------------------------------------------------------------------
    -- function parseSubdomain (s : URLStrings.Bounded_String) return URLStrings.Bounded_String is
    -- begin
    --     null;
    -- end parseSubdomain;

    -- ---------------------------------------------------------------------------
    -- -- parseDomain
    -- ---------------------------------------------------------------------------
    -- function parseDomain (s : URLStrings.Bounded_String) return URLStrings.Bounded_String is
    -- begin
    --     null;
    -- end parseDomain;

    -- ---------------------------------------------------------------------------
    -- -- hostname
    -- -- From RFC 3986:
    -- -- 'Such a name consists of a sequence of domain labels separated by ".",
    -- -- each domain label starting and ending with an alphanumeric character and
    -- -- possibly also containing "-" characters. The rightmost domain label of a
    -- -- fully-qualified domain name in DNS may be followed by a single "." and
    -- -- should be if it is necessary to distinguish between the complete domain
    -- -- name and some local domain.'
    -- --
    -- -- From RFC 1034:
    -- -- domain      ::= subdomain | ""
    -- -- subdomain   ::= label | subdomain "." label
    -- -- label       ::= letter [ [ ldh-str ] let-dig ]
    -- -- ldh-str     ::= let-dig-hyp | let-dig-hyp ldh-str
    -- -- let-dig-hyp ::= let-dig | "-"
    -- -- let-dig     ::= letter | digit
    -- -- letter      ::= A-Z | a-z
    -- -- digit       ::= 0-9
    -- --
    -- -- From RFC 1123:
    -- -- 'The restriction on the first character is relaxed to allow either a
    -- --  letter or a digit. Host software MUST support his more liberal syntax."
    -- ---------------------------------------------------------------------------
    -- function parseHostname (s : URLStrings.Bounded_String) return URLStrings.Bounded_String is
    -- begin
    --     null;
    -- end parseHostname;

    -- ---------------------------------------------------------------------------
    -- -- parseRegName
    -- -- reg-name ::= *( unreserved / pct-encoded / sub-delims )
    -- ---------------------------------------------------------------------------
    -- function parseRegName (s : URLStrings.Bounded_String) return URLStrings.Bounded_String is
    -- begin
    --     null;
    -- end parseRegName;

    -- ---------------------------------------------------------------------------
    -- -- parseHost
    -- -- host ::= IP-literal / IPv4address / reg-name
    -- ---------------------------------------------------------------------------
    -- function parseHost (s : URLStrings.Bounded_String) return URLStrings.Bounded_String is
    -- begin
    --     null;
    -- end parseHost;

    -- ---------------------------------------------------------------------------
    -- -- parseAuthority
    -- -- Authority is preceded by "//" and terminated by "/", "?", "#" or end of
    -- -- URL
    -- -- authority ::= [ userinfo "@" ] host [ ":" port ]
    -- ---------------------------------------------------------------------------
    -- function parseAuthority (s : URLStrings.Bounded_String) return URLStrings.Bounded_String is
    -- begin
    --     null;
    -- end parseAuthority;

    -- ---------------------------------------------------------------------------
    -- function parsePath (s : URLStrings.Bounded_String) return URLStrings.Bounded_String is
    -- begin
    --     null;
    -- end parsePath;

    -- -- URLs are: scheme://authority/path?query#fragment
    -- function parseURL (urlstr : URLStrings.Bounded_String) return URL is
        
    -- begin
    --     null;
    --     -- normalize (s);
    --     -- parseScheme (s);
    --     -- parseAuthority (s);
    --     -- parsePath (s);
    --     -- parseQuery (s);
    --     -- parseFragment (s);
    -- end parseURL;

    ---------------------------------------------------------------------------
    -- parseScheme
    -- The scheme is mandatory, and ends when we encounter the first ":"
    --
    --  scheme ::= ALPHA *(ALPHA / DIGIT / "+" / "-" / ".")
    -- ---------------------------------------------------------------------------    
    procedure parseScheme (s : URLStrings.Bounded_String; u : in out URL; endScheme : out URLIndex) is
    begin
        null;
    end parseScheme;

    procedure parseURL (s : URLStrings.Bounded_String; u : out URL) is
        newURL : URL := (
                    scheme   => URLStrings.Null_Bounded_String,
                    user     => URLStrings.Null_Bounded_String,
                    password => URLStrings.Null_Bounded_String,
                    host     => URLStrings.Null_Bounded_String,
                    port     => 0,
                    path     => URLStrings.Null_Bounded_String,
                    query    => URLStrings.Null_Bounded_String);
        
        sliceIndex : URLIndex := 0;
    begin
        -- parseScheme (s, u, sliceIndex);
        null;
    end parseURL;

    ---------------------------------------------------------------------------
    -- Reserved characters in URIs
    ---------------------------------------------------------------------------
    function isReserved (c : Character) return Boolean is
    begin
        case c is
            when ':' | '/' | '?' | '#' | '[' | ']' | '@' | '!' | '$' | '&' | 
                 ''' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '%' | ' ' =>
                return True;
            when others =>
                return False;
        end case;
    end isReserved;

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

            if isReserved (c) or Character'Pos(c) < 32 or Character'Pos(c) > 126 then
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

end Gembrowse.URL;