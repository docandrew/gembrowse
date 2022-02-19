-------------------------------------------------------------------------------
-- tests-url-punycode.adb
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Bounded;
with Ada.Text_IO; use Ada.Text_IO;

with Gembrowse.URL; use Gembrowse.URL;
with Gembrowse.URL.Hostnames; use Gembrowse.URL.Hostnames;
with Gembrowse.URL.Punycode; use Gembrowse.URL.Punycode;

package body Tests.URL.Punycode is

    ----------------------------------------------------------------------------
    -- runTests
    ----------------------------------------------------------------------------
    procedure runTests (count : in out Natural) is
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
        -- assert (decodeDigit ('A') = 0   , "decodeDigit functions properly (1)");
        -- assert (decodeDigit ('Z') = 25  , "decodeDigit functions properly (2)");
        -- assert (decodeDigit ('a') = 0   , "decodeDigit functions properly (3)");
        -- assert (decodeDigit ('z') = 25  , "decodeDigit functions properly (4)");
        -- assert (decodeDigit ('0') = 26  , "decodeDigit functions properly (5)");
        -- assert (decodeDigit ('9') = 35  , "decodeDigit functions properly (6)");
        -- assert (decodeDigit ('~') = 36  , "decodeDigit functions properly (7)");

        -- assert (encodeDigit (0)  = 'a'  , "encodeDigit functions properly (1)");
        -- assert (encodeDigit (25) = 'z'  , "encodeDigit functions properly (2)");
        -- assert (encodeDigit (26) = '0'  , "encodeDigit functions properly (3)");
        -- assert (encodeDigit (35) = '9'  , "encodeDigit functions properly (4)");

        -- assert (flagged ('a') = False   , "flagged functions properly (1)");
        -- assert (flagged ('A') = True    , "flagged functions properly (2)");

        -- assert (encodeBasic ('A') = 'a' , "encodeBasic functions properly (1)");
        -- assert (encodeBasic ('a') = 'a' , "encodeBasic functions properly (2)");

        -- Test To_ASCII
        for tc of testCases loop
            declare
                unicode : Bounded_String := tc.u_label;
                ascii   : Bounded_String;
                succ    : Boolean;
            begin
                To_Ascii (unicode, ascii, succ);
                assert (ascii = tc.a_label, "To_Ascii functions properly" & tcIdx'Image);
                count := count + 1;
            end;

            tcIdx := tcIdx + 1;
        end loop;

        tcIdx := 1;

        -- Test To_Unicode
        for tc of testCases loop
            declare
                unicode : Bounded_String;
                ascii   : Bounded_String := tc.a_label;
                succ    : Boolean;
            begin
                To_Unicode (ascii, unicode, succ);
                assert (unicode = tc.u_label, "To_Unicode functions properly" & tcIdx'Image);
                count := count + 1;
            end;

            tcIdx := tcIdx + 1;
        end loop;
    end runTests;

end Tests.URL.Punycode;
