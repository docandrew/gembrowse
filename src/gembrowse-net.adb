-------------------------------------------------------------------------------
-- gembrowse-net.adb
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib;

with tls; use tls;

with Console;
with Gembrowse.URL;

package body Gembrowse.Net is
    
    tlsConfig : access tls_config;

    ---------------------------------------------------------------------------
    -- setup
    --
    -- Configure root CA store and initialize libtls structs. We'll reuse
    -- the tlsConfig structure throughout the browser lifecycle.
    ---------------------------------------------------------------------------
    procedure setup is
        rootCAPath : constant String := "/etc/ssl/certs";
        rootCAPathPtr : Interfaces.C.Strings.chars_ptr := New_String (rootCAPath);
    begin
        if tls_init /= 0 then
            Console.Error ("Fatal: tls_init");
            GNAT.OS_Lib.OS_Exit (1);
        end if;

        tlsConfig := tls_config_new;

        if tlsConfig = null then
            Console.Error ("Fatal: tls_config_new");
            GNAT.OS_Lib.OS_Exit (2);
        end if;

        -- disable certificate validation
        tls_config_insecure_noverifycert (tlsConfig);
        tls_config_insecure_noverifyname (tlsConfig);

        -- Set to use TLSv1.2 and 1.3
        if tls_config_set_protocols(tlsConfig, TLS_PROTOCOLS_DEFAULT) /= 0 then
            Console.Error ("Fatal: tls_config_set_protocols");
            GNAT.OS_Lib.OS_Exit (3);
        end if;

        -- set root CA folder
        if tls_config_set_ca_path (tlsConfig, rootCAPathPtr) /= 0 then
            Console.Error ("Fatal: error setting root CA path (" & rootCAPath & ")");
            GNAT.OS_Lib.OS_Exit (4);
        end if;

        Free (rootCAPathPtr);
    end setup;

    ---------------------------------------------------------------------------
    -- teardown
    -- free libtls structs
    ---------------------------------------------------------------------------
    procedure teardown is
    begin
        tls_config_free (tlsConfig);
    end teardown;

    ---------------------------------------------------------------------------
    -- fetchPage
    ---------------------------------------------------------------------------
    function fetchPage (urlstr : Unbounded_String;
                        page   : out Unbounded_String) return Boolean is

        use Gembrowse.URL;

        CRLF : constant String := ASCII.CR & ASCII.LF;

        fixedURL   : constant String := urlstr.To_String;
        boundedURL : URLStrings.Bounded_String := URLStrings.To_Bounded_String (fixedURL);
        parsedURL  : Gembrowse.URL.URL;

        -- fqdn : constant String := To_String (parsedURL.host);
        fqdnPtr : Interfaces.C.Strings.chars_ptr; --:= New_String (fqdn);
        portPtr : Interfaces.C.Strings.chars_ptr; --:= New_String (parsedURL.port'Image);

        writeLen : Interfaces.C.long;
        sendBuf : constant String := urlstr.To_String & CRLF;

        readLen : Interfaces.C.long;
        bytesRead : Interfaces.C.long := 0;

        type Buffer is array (Natural range 1..32768) of Character;
        recvBuf : Buffer := (others => ASCII.NUL);

        tlsContext : access tls.tls;
    begin
        page      := Null_Unbounded_String;
        Gembrowse.URL.parseURL (boundedURL, parsedURL);

        if parsedURL.error /= NONE then
            return False;
        end if;

        Put_Line (Standard_Error, "Attempting to connect to " & To_String (urlstr));

        fqdnPtr := New_String (URLStrings.To_String (parsedURL.host));
        portPtr := New_String (parsedURL.port'Image);

        Put_Line (Standard_Error, "Connecting to " & URLStrings.To_String (parsedURL.host) & " port" & parsedURL.port'Image);

        -- init client context
        tlsContext := tls_client;

        if tlsContext = null then
            Put_Line (Standard_Error, "Fatal: tls_client");
            Free (portPtr);
            Free (fqdnPtr);
            return False;
        end if;

        -- apply config to context
        if tls_configure (tlsContext, tlsConfig) /= 0 then
            -- Console.Error ("Fatal: tls_configure (" & Value (tls_error (tlsContext)) & ")");
            Put_Line (Standard_Error, "Fatal: tls_configure (" & Value (tls_error (tlsContext)) & ")");
            Free (portPtr);
            Free (fqdnPtr);
            return False;
        end if;

        -- Connect to server
        if tls_connect (tlsContext, fqdnPtr, portPtr) /= 0 then
            Put_Line (Standard_Error, "Fatal: tls_connect (" & Value (tls_error (tlsContext)) & ")");
            Free (portPtr);
            Free (fqdnPtr);
            return False;
        end if;

        -- Confirm handshake success
        if tls_handshake (tlsContext) /= 0 then
            Put_Line (Standard_Error, "Fatal: tls_handshake (" & Value (tls_error (tlsContext)) & ")");
            Free (portPtr);
            Free (fqdnPtr);
            return False;
        end if;

        -- send request
        -- @TODO wrap this in a loop in case everything didn't get sent.
        writeLen := tls_write (tlsContext, sendBuf(1)'Address, sendBuf'Length);

        if writeLen < 0 then
            Put_Line (Standard_Error, "Fatal: tls_write (" & Value (tls_error (tlsContext)) & ")");
            Free (portPtr);
            Free (fqdnPtr);
            return False;
        end if;

        -- receive page. We'll copy the buffer each time.
        -- It seems that some servers will send the response such that
        -- the first tls_read will only get the header, and the second will get
        -- the body, other times we get everything on the first tls_read. Here
        -- we'll just keep reading while it has something to send.
        readLoop: loop
            Put_Line (Standard_Error, "attempt tls_read");
            readLen := tls_read (tlsContext, recvBuf(1)'Address, recvBuf'Length);

            if readLen = TLS_WANT_POLLIN or readLen = TLS_WANT_POLLOUT then
                -- Put_Line (Standard_Error, "TLS_WANT");
                null;   -- continue
            elsif readLen = -1 then
                -- Put_Line (Standard_Error, "read -1");
                exit readLoop;
            elsif readLen = 0 then
                -- Put_Line (Standard_Error, "read 0");
                exit readLoop;
            else
                -- Put_Line (Standard_Error, "read" & readLen'Image);
                bytesRead := bytesRead + readLen;

                for i in 1 .. Integer(readLen) loop
                    Put (Standard_Error, recvBuf (i));
                    Append (page, recvBuf (i));
                end loop;
            end if;
        end loop readLoop;

        -- Put_Line (Standard_Error, "Received" & bytesRead'Image & " bytes");
        -- Put_Line (Standard_Error, "Received " & To_String (page));

        -- cleanup

        -- Some servers have an issue with the tls_close without EOF notify (?)
        -- We'll ignore it.
        if tls_close (tlsContext) /= 0 then
            Put_Line (Standard_Error, "Warning: tls_close (" & Value (tls_error (tlsContext)) & ")");
        end if;

        tls_free (tlsContext);
        
        Free (portPtr);
        Free (fqdnPtr);

        return True;
    end fetchPage;

end Gembrowse.net;
