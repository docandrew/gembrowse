pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;

package x86_64_linux_gnu_bits_types_struct_timespec_h is

   type timespec is record
      tv_sec : aliased x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h:12
      tv_nsec : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h:16
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h:10

end x86_64_linux_gnu_bits_types_struct_timespec_h;
