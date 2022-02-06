with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Wide_Wide_Bounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Text_IO; use Ada.Text_IO;

package body Gembrowse.URL.IDNA with SPARK_Mode is

    -- bootstring parameters
    base         : constant := 36;
    tmin         : constant := 1;
    tmax         : constant := 26;
    skew         : constant := 38;
    damp         : constant := 700;
    initial_bias : constant := 72;
    initial_n    : constant := 16#80#;
    delimiter    : constant := 16#2D#;
    delimitChar  : constant Wide_Wide_Character := '-';

    maxint : constant Unsigned_32 := Unsigned_32'Last;

    ----------------------------------------------------------------------------
    -- Return True if character is a basic code point.
    ----------------------------------------------------------------------------
    function basic (c : Wide_Wide_Character) return Boolean is
    begin
        return Wide_Wide_Character'Pos (c) < 16#80#;
    end basic;

    ----------------------------------------------------------------------------
    -- Return True if character is a delimiter.
    ----------------------------------------------------------------------------
    function delim (c : Wide_Wide_Character) return Boolean is
    begin
        return Wide_Wide_Character'Pos (c) = delimiter;
    end delim;

    ----------------------------------------------------------------------------
    -- decodeDigit
    -- return numeric value of a basic code point in range 0 to base-1, or base
    -- if cp does not represent a value.
    ----------------------------------------------------------------------------
    function decodeDigit (c : Wide_Wide_Character) return Unsigned_32 is
        cp : Unsigned_32 := Wide_Wide_Character'Pos(c);
    begin
        -- if cp - 48 > 10 then
        --     return cp - 22;
        -- else
        --     if cp - 65 < 26 then
        --         return cp - 65;
        --     else
        --         if cp - 97 < 26 then
        --             return cp - 97;
        --         else
        --             return base;
        --         end if;
        --     end if;
        -- end if;
        case c is
            when 'A'..'Z' =>
                return cp - Wide_Wide_Character'Pos('A');
            when 'a'..'z' =>
                return cp - Wide_Wide_Character'Pos('a');
            when '0'..'9' =>
                return cp - Wide_Wide_Character'Pos('0') + 26;
            when others =>
                return base;
        end case;
    end decodeDigit;

    ----------------------------------------------------------------------------
    -- encodeDigit
    -- Always assumes lowercase.
    ----------------------------------------------------------------------------
    function encodeDigit (d : Unsigned_32) return Wide_Wide_Character with
        Pre => d < 36
    is
        -- dp : Unsigned_32 := (if d < 26 then 1 else 0);
    begin
        -- return d + 22 + 75 * dp;
        if d >= 0 and d <= 25 then
            return Wide_Wide_Character'Val (d + Wide_Wide_Character'Pos ('a'));
        elsif d > 25 and d <= 35 then
            return Wide_Wide_Character'Val (d + Wide_Wide_Character'Pos ('0') - 26);
        else
            return Wide_Wide_Character'Val (0);
        end if;
    end encodeDigit;

    ----------------------------------------------------------------------------
    -- flagged
    -- Return True if basic code point is flagged (uppercase)
    ----------------------------------------------------------------------------
    function flagged (bcp : Wide_Wide_Character) return Boolean with 
        Pre => bcp in 'a'..'z' or bcp in 'A'..'Z'
    is
    begin
        case bcp is
            when 'a'..'z' => return False;
            when 'A'..'Z' => return True;
            when others => return False;    -- should never get here.
        end case;
    end flagged;

    ----------------------------------------------------------------------------
    -- encodeBasic
    -- Force a basic code point to lowercase.
    ----------------------------------------------------------------------------
    function encodeBasic (bcp : Wide_Wide_Character) return Wide_Wide_Character with
        Pre => bcp in 'a'..'z' or bcp in 'A'..'Z'
    is
    begin
        case bcp is 
            when 'A'..'Z' => return Wide_Wide_Character'Val (Wide_Wide_Character'Pos (bcp) + 32);
            when 'a'..'z' => return bcp;
            when others => return bcp;
        end case;
    end encodeBasic;

    ----------------------------------------------------------------------------
    -- bias adaptation function
    ----------------------------------------------------------------------------
    function adapt (inDelta, numpoints : Unsigned_32; firstTime : Boolean) return Unsigned_32 is
        k     : Unsigned_32 := 0;
        delt : Unsigned_32 := inDelta;
    begin
        delt := (if firstTime then delt / damp else delt / 2);
        delt := delt + delt / numpoints;

        loop
            exit when delt <= ((base - tmin) * tmax) / 2;
            
            delt := delt / (base - tmin);
            k := k + base;
        end loop;

        return k + (base - tmin + 1) * delt / (delt + skew);
    end adapt;

    ----------------------------------------------------------------------------
    -- Convenience method for getting the numeric value of the character at a
    -- specific index in a Bounded_Wide_Wide_String
    ----------------------------------------------------------------------------
    function ElementAt (bwws : CodePoints.Bounded_Wide_Wide_String; idx : Positive) return Unsigned_32 is
        use CodePoints;
    begin
        return Unsigned_32(Wide_Wide_Character'Pos (Element (bwws, idx)));
    end ElementAt;

    ----------------------------------------------------------------------------
    -- encode
    ----------------------------------------------------------------------------
    function encode (input  : CodePoints.Bounded_Wide_Wide_String;
                     output : out CodePoints.Bounded_Wide_Wide_String) return PunycodeStatus is
        use CodePoints;

        n      : Unsigned_32 := initial_n;
        delt   : Unsigned_32 := 0;
        
        -- number of code points that have been handled
        h      : Unsigned_32;

        -- number of basic code points
        b      : Unsigned_32;

        -- number of chars that have been output
        outlen : Unsigned_32 := 0;

        -- maximum number of chars we can output
        maxlen : Unsigned_32 := Unsigned_32(CodePoints.Max_Length);
        
        bias   : Unsigned_32 := initial_bias;
        -- j      : Unsigned_32;
        m      : Unsigned_32;
        q      : Unsigned_32;
        k      : Unsigned_32;
        t      : Unsigned_32;

    begin
        output := CodePoints.Null_Bounded_Wide_Wide_String;

        -- handle basic code points, just append these to our output string.
        for j in 1..Length (input) loop
            if basic (Element (input, j)) then
                Append (output, Element (input, j));
                outlen := outlen + 1;
            end if;
        end loop;

        h := outlen;
        b := outlen;

        -- use a dash to separate the basic chars from the encoded unicode stuff
        if b > 0 then
            Append (output, '-');
        end if;

        -- main encoding loop
        loop
            exit when h >= Unsigned_32 (Length (input));
            -- all non-basic code points < n have been handled, find next larger one
            m := maxint;
            for j in 1..Length (input) loop
                if ElementAt (input, j) >= n and
                   ElementAt (input, j) < m then
                   m := ElementAt (input, j);
                end if;
            end loop;

            -- increase delta enough to advance decoder's <n,i> state to <m,0>
            -- but guard against overflow
            if m - n > (maxint - delt) / (h + 1) then
                return Overflow;
            end if;

            delt := delt + (m - n) * (h + 1);
            n := m;

            -- no need to check whether ElementAt (input, j) is basic
            for j in 1..Length (input) loop
                if ElementAt (input, j) < n then
                    if delt = Unsigned_32'Last then
                        return Overflow;
                    else
                        delt := delt + 1;
                    end if;
                end if;

                if ElementAt (input, j) = n then
                    -- represent delta as a generalized variable-length integer
                    q := delt;
                    k := base;

                    loop
                        if outlen >= maxlen then
                            return BigOutput;
                        end if;

                        if k <= bias then
                            t := tmin;
                        else
                            if k >= bias + tmax then
                                t := tmax;
                            else
                                t := k - bias;
                            end if;
                        end if;

                        exit when q < t;

                        Append (output, encodeDigit (t + (q - t) mod (base - t)));
                        outlen := outlen + 1;

                        q := (q - t) / (base - t);

                        k := k + base;
                    end loop;

                    Append (output, encodeDigit (q));
                    bias := adapt (delt, h + 1, h = b);
                    delt := 0;
                    h := h + 1;
                end if;
            end loop;

            delt := delt + 1;
            n := n + 1;

        end loop;

        return Success;
    end encode;

    ---------------------------------------------------------------------------
    -- To_Ascii
    ---------------------------------------------------------------------------
    function To_Ascii (input  : LabelStrings.Bounded_String;
                       output : out LabelStrings.Bounded_String) return Boolean is
        use CodePoints;
        use LabelStrings;

        in32  : CodePoints.Bounded_Wide_Wide_String;
        out32 : CodePoints.Bounded_Wide_Wide_String;
    begin

        in32 := CodePoints.To_Bounded_Wide_Wide_String (
            Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode (
                LabelStrings.To_String (input)));

        if (encode (in32, out32) /= Success) then
            output := LabelStrings.Null_Bounded_String;
            return False;
        else
            output := LabelStrings.To_Bounded_String (
                Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (
                    CodePoints.To_Wide_Wide_String (out32)));
            return True;
        end if;
    end To_Ascii;                       

    ----------------------------------------------------------------------------
    -- decode
    ----------------------------------------------------------------------------
    function decode (input : CodePoints.Bounded_Wide_Wide_String;
                     output : out CodePoints.Bounded_Wide_Wide_String) return PunycodeStatus is
    begin
        return BadInput;
    end decode;

    ---------------------------------------------------------------------------
    -- To_Unicode
    ---------------------------------------------------------------------------
    function To_Unicode (input  : LabelStrings.Bounded_String;
                         output : out LabelStrings.Bounded_String) return Boolean is
        use LabelStrings;
    begin

        output := LabelStrings.Null_Bounded_String;
        return False;
    end To_Unicode;

    ----------------------------------------------------------------------------
    -- runTests
    ----------------------------------------------------------------------------
    procedure runTests is
        use LabelStrings;

        type TestCase is record
            u_label : Bounded_String;
            a_label : Bounded_String;
        end record;

        type TestCaseArr is array (1..26) of TestCase;

        tcIdx : Positive := 1;

        testCases : TestCaseArr := (
            (Null_Bounded_String,                        Null_Bounded_String),
            (To_Bounded_String ("a"),                    To_Bounded_String ("a-")),
            (To_Bounded_String ("A"),                    To_Bounded_String ("A-")),
            (To_Bounded_String ("3"),                    To_Bounded_String ("3-")),
            (To_Bounded_String ("-"),                    To_Bounded_String ("--")),
            (To_Bounded_String ("--"),                   To_Bounded_String ("---")),
            (To_Bounded_String ("London"),               To_Bounded_String ("London-")),
            (To_Bounded_String ("Lloyd-Atkinson"),       To_Bounded_String ("Lloyd-Atkinson-")),
            (To_Bounded_String ("This has spaces"),      To_Bounded_String ("This has spaces-")),
            (To_Bounded_String ("-> $1.00 <-"),          To_Bounded_String ("-> $1.00 <--")),
            (To_Bounded_String ("а"),                    To_Bounded_String ("80a")),
            (To_Bounded_String ("ü"),                    To_Bounded_String ("tda")),
            (To_Bounded_String ("例"),                    To_Bounded_String ("fsq")),
            (To_Bounded_String ("😉"),                   To_Bounded_String ("n28h")),
            (To_Bounded_String ("αβγ"),                   To_Bounded_String ("mxacd")),
            (To_Bounded_String ("München"),               To_Bounded_String ("Mnchen-3ya")),
            (To_Bounded_String ("Mnchen-3ya"),            To_Bounded_String ("Mnchen-3ya-")),
            (To_Bounded_String ("München-Ost"),           To_Bounded_String ("Mnchen-Ost-9db")),
            (To_Bounded_String ("Bahnhof München-Ost"),   To_Bounded_String ("Bahnhof Mnchen-Ost-u6b")),
            (To_Bounded_String ("abæcdöef"),              To_Bounded_String ("abcdef-qua4k")),
            (To_Bounded_String ("правда"),                To_Bounded_String ("80aafi6cg")),
            (To_Bounded_String ("ยจฆฟคฏข"),               To_Bounded_String ("22cdfh1b8fsa")),
            (To_Bounded_String ("도메인"),                 To_Bounded_String ("hq1bm8jm9l")),
            (To_Bounded_String ("ドメイン名例"),            To_Bounded_String ("eckwd4c7cu47r2wf")),
            (To_Bounded_String ("MajiでKoiする5秒前"),      To_Bounded_String ("MajiKoi5-783gue6qz075azm5e")),
            (To_Bounded_String ("「bücher」"),             To_Bounded_String ("bcher-kva8445foa"))
        );
    begin
        assert (decodeDigit ('A') = 0   , "decodeDigit functions properly (1)");
        assert (decodeDigit ('Z') = 25  , "decodeDigit functions properly (2)");
        assert (decodeDigit ('a') = 0   , "decodeDigit functions properly (3)");
        assert (decodeDigit ('z') = 25  , "decodeDigit functions properly (4)");
        assert (decodeDigit ('0') = 26  , "decodeDigit functions properly (5)");
        assert (decodeDigit ('9') = 35  , "decodeDigit functions properly (6)");
        assert (decodeDigit ('~') = 36  , "decodeDigit functions properly (7)");

        assert (encodeDigit (0)  = 'a'  , "encodeDigit functions properly (1)");
        assert (encodeDigit (25) = 'z'  , "encodeDigit functions properly (2)");
        assert (encodeDigit (26) = '0'  , "encodeDigit functions properly (3)");
        assert (encodeDigit (35) = '9'  , "encodeDigit functions properly (4)");

        assert (flagged ('a') = False   , "flagged functions properly (1)");
        assert (flagged ('A') = True    , "flagged functions properly (2)");

        assert (encodeBasic ('A') = 'a' , "encodeBasic functions properly (1)");
        assert (encodeBasic ('a') = 'a' , "encodeBasic functions properly (2)");

        -- Test To_ASCII
        for tc of testCases loop
            declare
                unicode : Bounded_String := tc.u_label;
                ascii   : Bounded_String;
                succ : Boolean := To_Ascii (unicode, ascii);
            begin
                assert (ascii = tc.a_label, "To_Ascii functions properly" & tcIdx'Image);
                -- if succ then
                --     Put_Line ("unicode: " & To_String (unicode) & " ascii: " & To_String (ascii));
                -- else
                --     Put_Line ("Failed conversion.");
                -- end if;
            end;

            tcIdx := tcIdx + 1;
        end loop;
    end runTests;

end Gembrowse.URL.IDNA;
