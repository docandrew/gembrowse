pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
-- limited with x86_64_linux_gnu_bits_termios_struct_h;
-- with x86_64_linux_gnu_bits_termios_h;

package termios is

   B0 : constant := 8#000000#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:33
   B50 : constant := 8#000001#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:34
   B75 : constant := 8#000002#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:35
   B110 : constant := 8#000003#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:36
   B134 : constant := 8#000004#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:37
   B150 : constant := 8#000005#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:38
   B200 : constant := 8#000006#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:39
   B300 : constant := 8#000007#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:40
   B600 : constant := 8#000010#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:41
   B1200 : constant := 8#000011#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:42
   B1800 : constant := 8#000012#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:43
   B2400 : constant := 8#000013#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:44
   B4800 : constant := 8#000014#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:45
   B9600 : constant := 8#000015#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:46
   B19200 : constant := 8#000016#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:47
   B38400 : constant := 8#000017#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:48
   --  unsupported macro: EXTA B19200
   --  unsupported macro: EXTB B38400

   TIOCSER_TEMT : constant := 16#01#;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:60

   TCOOFF : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:64
   TCOON : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:65
   TCIOFF : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:66
   TCION : constant := 3;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:67

   TCIFLUSH : constant := 0;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:70
   TCOFLUSH : constant := 1;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:71
   TCIOFLUSH : constant := 2;  --  /usr/include/x86_64-linux-gnu/bits/termios.h:72

   ISIG   : constant := 1;
   ICANON : constant := 2;
   XCASE  : constant := 4;
   ECHO   : constant := 16;

   subtype cc_t is unsigned_char;  -- /usr/include/x86_64-linux-gnu/bits/termios.h:23

   subtype speed_t is unsigned;  -- /usr/include/x86_64-linux-gnu/bits/termios.h:24

   subtype tcflag_t is unsigned;  -- /usr/include/x86_64-linux-gnu/bits/termios.h:25

   NCCS : constant := 32;  --  /usr/include/x86_64-linux-gnu/bits/termios-struct.h:23

   type anon860_array862 is array (0 .. 31) of aliased cc_t;
   type termios is record
      c_iflag : aliased tcflag_t;  -- /usr/include/x86_64-linux-gnu/bits/termios-struct.h:26
      c_oflag : aliased tcflag_t;  -- /usr/include/x86_64-linux-gnu/bits/termios-struct.h:27
      c_cflag : aliased tcflag_t;  -- /usr/include/x86_64-linux-gnu/bits/termios-struct.h:28
      c_lflag : aliased tcflag_t;  -- /usr/include/x86_64-linux-gnu/bits/termios-struct.h:29
      c_line : aliased cc_t;  -- /usr/include/x86_64-linux-gnu/bits/termios-struct.h:30
      c_cc : aliased anon860_array862;  -- /usr/include/x86_64-linux-gnu/bits/termios-struct.h:31
      c_ispeed : aliased speed_t;  -- /usr/include/x86_64-linux-gnu/bits/termios-struct.h:32
      c_ospeed : aliased speed_t;  -- /usr/include/x86_64-linux-gnu/bits/termios-struct.h:33
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/termios-struct.h:24

   --  arg-macro: function CCEQ (val, c)
   --    return (c) = (val)  and then  (val) /= _POSIX_VDISABLE;
   subtype pid_t is x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- termios.h:30

   function cfgetospeed (uu_termios_p : access constant termios) return speed_t  -- termios.h:48
   with Import => True, 
        Convention => C, 
        External_Name => "cfgetospeed";

   function cfgetispeed (uu_termios_p : access constant termios) return speed_t  -- termios.h:51
   with Import => True, 
        Convention => C, 
        External_Name => "cfgetispeed";

   function cfsetospeed (uu_termios_p : access termios; uu_speed : speed_t) return int  -- termios.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "cfsetospeed";

   function cfsetispeed (uu_termios_p : access termios; uu_speed : speed_t) return int  -- termios.h:57
   with Import => True, 
        Convention => C, 
        External_Name => "cfsetispeed";

   function cfsetspeed (uu_termios_p : access termios; uu_speed : speed_t) return int  -- termios.h:61
   with Import => True, 
        Convention => C, 
        External_Name => "cfsetspeed";

   function tcgetattr (uu_fd : int; uu_termios_p : access termios) return int  -- termios.h:66
   with Import => True, 
        Convention => C, 
        External_Name => "tcgetattr";

   function tcsetattr
     (uu_fd : int;
      uu_optional_actions : int;
      uu_termios_p : access constant termios) return int  -- termios.h:70
   with Import => True, 
        Convention => C, 
        External_Name => "tcsetattr";

   procedure cfmakeraw (uu_termios_p : access termios)  -- termios.h:76
   with Import => True, 
        Convention => C, 
        External_Name => "cfmakeraw";

   function tcsendbreak (uu_fd : int; uu_duration : int) return int  -- termios.h:80
   with Import => True, 
        Convention => C, 
        External_Name => "tcsendbreak";

   function tcdrain (uu_fd : int) return int  -- termios.h:86
   with Import => True, 
        Convention => C, 
        External_Name => "tcdrain";

   function tcflush (uu_fd : int; uu_queue_selector : int) return int  -- termios.h:90
   with Import => True, 
        Convention => C, 
        External_Name => "tcflush";

   function tcflow (uu_fd : int; uu_action : int) return int  -- termios.h:94
   with Import => True, 
        Convention => C, 
        External_Name => "tcflow";

   function tcgetsid (uu_fd : int) return x86_64_linux_gnu_bits_types_h.uu_pid_t  -- termios.h:99
   with Import => True, 
        Convention => C, 
        External_Name => "tcgetsid";

end termios;
