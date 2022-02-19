-------------------------------------------------------------------------------
-- gembrowse-url-punycode.ads
--
-- Internationalized Domain Names in Applications (IDNA)
--
-- Punycode encoding/decoding for Unicode domain labels.
--
-- Implementation translated from punycode.c in RFC 3492
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Wide_Wide_Bounded;

with Interfaces; use Interfaces;

with Gembrowse.URL.Hostnames; use Gembrowse.URL.Hostnames;

package Gembrowse.URL.Punycode with SPARK_Mode is

    -- Easiest just to convert our UTF-8 code points as single UTF-32 chars.
    package CodePoints is new Ada.Strings.Wide_Wide_Bounded.Generic_Bounded_Length (Max => LabelStrings.Max_Length);

    -- ACE - ASCII compatible encoding
    ACEPrefix : constant String := "xn--";

    type PunycodeStatus is (
        Success,
        BadInput,       -- input is invalid
        BigOutput,      -- output would exceed space provided
        Overflow        -- input needs wider integers to process
    );

    ---------------------------------------------------------------------------
    -- To_Ascii
    -- Convert Unicode to Punycode. Input is a UTF-8 bounded string. Returns
    -- False if conversion not possible, due either to invalid input or the
    -- resulting Punycode exceeding the maximum length of a DNS hostname label.
    ---------------------------------------------------------------------------
    procedure To_Ascii (input  : LabelStrings.Bounded_String;
                        output : out LabelStrings.Bounded_String;
                        result : out Boolean);

    ---------------------------------------------------------------------------
    -- To_Unicode
    -- Convert Punycode to Unicode. Input is a UTF-8 bounded string. Returns
    -- False if conversion not possible, due either to invalid input or the
    -- resulting Punycode exceeding the maximum length of a DNS hostname label.
    ---------------------------------------------------------------------------
    procedure To_Unicode (input  : LabelStrings.Bounded_String;
                          output : out LabelStrings.Bounded_String;
                          result : out Boolean);

end Gembrowse.URL.Punycode;
