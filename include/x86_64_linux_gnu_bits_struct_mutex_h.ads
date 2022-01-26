pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_thread_shared_types_h;

package x86_64_linux_gnu_bits_struct_mutex_h is

   type uu_pthread_mutex_s is record
      uu_lock : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/struct_mutex.h:24
      uu_count : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/struct_mutex.h:25
      uu_owner : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/struct_mutex.h:26
      uu_nusers : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/struct_mutex.h:28
      uu_kind : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/struct_mutex.h:32
      uu_spins : aliased short;  -- /usr/include/x86_64-linux-gnu/bits/struct_mutex.h:34
      uu_elision : aliased short;  -- /usr/include/x86_64-linux-gnu/bits/struct_mutex.h:35
      uu_list : aliased x86_64_linux_gnu_bits_thread_shared_types_h.uu_pthread_list_t;  -- /usr/include/x86_64-linux-gnu/bits/struct_mutex.h:36
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/struct_mutex.h:22

end x86_64_linux_gnu_bits_struct_mutex_h;
