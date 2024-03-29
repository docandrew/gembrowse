-------------------------------------------------------------------------------
-- gembrowse-net.ads
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Gembrowse.net is

    ---------------------------------------------------------------------------
    -- Gemini status codes
    ---------------------------------------------------------------------------
    type StatusCode is new Positive range 10..99;

    INPUT                    : constant StatusCode := 10;
    SENSITIVE_INPUT          : constant StatusCode := 11;
    SUCCESS                  : constant StatusCode := 20;
    TEMP_REDIRECT            : constant StatusCode := 30;
    PERM_REDIRECT            : constant StatusCode := 31;
    TEMP_FAILURE             : constant StatusCode := 40;
    SERVER_UNAVAIL           : constant StatusCode := 41;
    CGI_ERROR                : constant StatusCode := 42;
    PROXY_ERROR              : constant StatusCode := 43;
    SLOW_DOWN                : constant StatusCode := 44;
    PERM_FAILURE             : constant StatusCode := 50;
    NOT_FOUND                : constant StatusCode := 51;
    GONE                     : constant StatusCode := 52;
    PROXY_REFUSED            : constant StatusCode := 53;
    BAD_REQUEST              : constant StatusCode := 59;
    CLIENT_CERT_REQUIRED     : constant StatusCode := 60;
    CLIENT_CERT_UNAUTHORIZED : constant StatusCode := 61;
    CLIENT_CERT_INVALID      : constant StatusCode := 62;

    ---------------------------------------------------------------------------
    -- Set up global libtls structs
    ---------------------------------------------------------------------------
    procedure setup;

    ---------------------------------------------------------------------------
    -- Free memory used by global libtls structs
    ---------------------------------------------------------------------------
    procedure teardown;

    ---------------------------------------------------------------------------
    -- fetchPage
    -- Connects to the host specified by the urlstr, sends it the Gemini
    -- request for that URL, and reads the response from the server.
    -- We copy the response from this procedure's internal read buffer into the
    -- @param page.
    ---------------------------------------------------------------------------
    function fetchPage (urlstr   : Unbounded_String; 
                        page     : out Unbounded_String;
                        header   : out Unbounded_String) return Boolean;

end Gembrowse.net;
