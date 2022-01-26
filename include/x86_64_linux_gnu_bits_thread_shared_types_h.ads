pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package x86_64_linux_gnu_bits_thread_shared_types_h is

   type uu_pthread_internal_list;
   type uu_pthread_internal_list is record
      uu_prev : access uu_pthread_internal_list;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:51
      uu_next : access uu_pthread_internal_list;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:52
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:49

   subtype uu_pthread_list_t is uu_pthread_internal_list;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:53

   type uu_pthread_internal_slist;
   type uu_pthread_internal_slist is record
      uu_next : access uu_pthread_internal_slist;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:57
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:55

   subtype uu_pthread_slist_t is uu_pthread_internal_slist;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:58

   type anon950_struct952 is record
      uu_low : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:99
      uu_high : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:100
   end record
   with Convention => C_Pass_By_Copy;
   type anon950_union951 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_wseq : aliased Extensions.unsigned_long_long;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:96
         when others =>
            uu_wseq32 : aliased anon950_struct952;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:101
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type anon950_struct954 is record
      uu_low : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:108
      uu_high : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:109
   end record
   with Convention => C_Pass_By_Copy;
   type anon950_union953 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_g1_start : aliased Extensions.unsigned_long_long;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:105
         when others =>
            uu_g1_start32 : aliased anon950_struct954;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:110
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type anon950_array955 is array (0 .. 1) of aliased unsigned;
   type uu_pthread_cond_s is record
      anon2094 : aliased anon950_union951;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:102
      anon2101 : aliased anon950_union953;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:111
      uu_g_refs : aliased anon950_array955;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:112
      uu_g_size : aliased anon950_array955;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:113
      uu_g1_orig_size : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:114
      uu_wrefs : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:115
      uu_g_signals : aliased anon950_array955;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:116
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h:92

end x86_64_linux_gnu_bits_thread_shared_types_h;
