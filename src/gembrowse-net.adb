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

package body Gembrowse.Net is
    
    tlsConfig : access tls_config;

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

    procedure teardown is
    begin
        tls_config_free (tlsConfig);
    end teardown;

    function fetchPage (url : Unbounded_String; page : out Unbounded_String) return Boolean is
        CRLF : constant String := ASCII.CR & ASCII.LF;

        fqdn : constant String := "localhost";
        fqdnPtr : Interfaces.C.Strings.chars_ptr := New_String (fqdn);

        -- url : constant String := "gemini://localhost/";

        port : constant String := "1965";
        portPtr : Interfaces.C.Strings.chars_ptr := New_String (port);

        writeLen : Interfaces.C.long;
        sendBuf : constant String := To_String (url) & CRLF;

        readLen : Interfaces.C.long;    
        type Buffer is array (Natural range 1..32768) of Character;
        recvBuf : Buffer := (others => ASCII.NUL);

        tlsContext : access tls.tls;
    begin

        -- init client context
        tlsContext := tls_client;

        if tlsContext = null then
            Console.Error ("Fatal: tls_client");
            -- GNAT.OS_Lib.OS_Exit (5);
            return False;
        end if;

        -- apply config to context
        if tls_configure (tlsContext, tlsConfig) /= 0 then
            Console.Error ("Fatal: tls_configure (" & Value (tls_error (tlsContext)) & ")");
            -- GNAT.OS_Lib.OS_Exit (6);
            return False;
        end if;

        -- Connect to server
        if tls_connect (tlsContext, fqdnPtr, portPtr) /= 0 then
            Console.Error ("Fatal: tls_connect (" & Value (tls_error (tlsContext)) & ")");
            -- GNAT.OS_Lib.OS_Exit (7);
            return False;
        end if;

        -- Confirm handshake success
        if tls_handshake (tlsContext) /= 0 then
            Console.Error ("Fatal: tls_handshake (" & Value (tls_error (tlsContext)) & ")");
            -- GNAT.OS_Lib.OS_Exit (8);
            return False;
        end if;

        -- send request
        writeLen := tls_write (tlsContext, sendBuf(1)'Address, sendBuf'Length);

        if writeLen < 0 then
            Console.Error ("Fatal: tls_write (" & Value (tls_error (tlsContext)) & ")");
            -- GNAT.OS_Lib.OS_Exit (9);
            return False;
        else
            Console.Heading ("Wrote" & writeLen'Image & " bytes");
        end if;

        -- receive header
        readLen := tls_read (tlsContext, recvBuf(1)'Address, recvBuf'Length);

        -- Put_Line ("Received" & readLen'Image & " bytes");

        -- if readLen < 0 then
        --     Console.Error ("Fatal: tls_read (" & Value (tls_error (tlsContext)) & ")");
        -- end if;

        for i in 1..Integer(readLen) loop
            Put (recvBuf(i));
        end loop;

        -- receive body
        readLen := tls_read (tlsContext, recvBuf(1)'Address, recvBuf'Length);
        
        -- Put_Line ("Received" & readLen'Image & " bytes");

        -- if readLen < 0 then
        --     Console.Error ("Fatal: tls_read (" & Value (tls_error (tlsContext)) & ")");
        -- end if;
        page := Null_Unbounded_String;

        for i in 1..Integer(readLen) loop
            -- Put (recvBuf(i));
            Append (page, recvBuf(i));
        end loop;

        -- cleanup

        if tls_close (tlsContext) /= 0 then
            -- Console.Error ("Fatal: tls_close (" & Value (tls_error (tlsContext)) & ")");
            return False;
        end if;

        tls_free (tlsContext);
        
        Free (portPtr);
        Free (fqdnPtr);

        return True;
    end fetchPage;

end Gembrowse.net;
