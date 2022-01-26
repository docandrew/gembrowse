pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;

package x86_64_linux_gnu_bits_types_struct_timeval_h is

   type timeval is record
      tv_sec : aliased x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h:10
      tv_usec : aliased x86_64_linux_gnu_bits_types_h.uu_suseconds_t;  -- /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h:11
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h:8

end x86_64_linux_gnu_bits_types_struct_timeval_h;
