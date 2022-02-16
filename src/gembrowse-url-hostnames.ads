-------------------------------------------------------------------------------
-- gembrowse-url-hostnames.ads
--
-- Types and constants for dealing with hostnames and labels.
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;

package Gembrowse.URL.Hostnames is

    MAX_LABEL_LENGTH : constant := 63;
    MAX_FQDN_LENGTH  : constant := 255;

    -- Per RFC 1123
    package HostnameStrings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 255);
    package LabelStrings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 63);

end Gembrowse.URL.Hostnames;
