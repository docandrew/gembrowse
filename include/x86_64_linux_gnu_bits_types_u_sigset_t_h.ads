pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package x86_64_linux_gnu_bits_types_u_sigset_t_h is

   type uu_sigset_t_array905 is array (0 .. 15) of aliased unsigned_long;
   type uu_sigset_t is record
      uu_val : aliased uu_sigset_t_array905;  -- /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h:7
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h:8

end x86_64_linux_gnu_bits_types_u_sigset_t_h;
