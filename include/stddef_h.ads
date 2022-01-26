pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stddef_h is

   --  unsupported macro: NULL ((void *)0)
   --  arg-macro: procedure offsetof (TYPE, MEMBER)
   --    __builtin_offsetof (TYPE, MEMBER)
   subtype ptrdiff_t is long;  -- /home/doc/GNAT/2020/lib/gcc/x86_64-pc-linux-gnu/9.3.1/include/stddef.h:143

   subtype size_t is unsigned_long;  -- /home/doc/GNAT/2020/lib/gcc/x86_64-pc-linux-gnu/9.3.1/include/stddef.h:209

   subtype wchar_t is int;  -- /home/doc/GNAT/2020/lib/gcc/x86_64-pc-linux-gnu/9.3.1/include/stddef.h:321

   type max_align_t is record
      uu_max_align_ll : aliased Long_Long_Integer;  -- /home/doc/GNAT/2020/lib/gcc/x86_64-pc-linux-gnu/9.3.1/include/stddef.h:416
      uu_max_align_ld : aliased long_double;  -- /home/doc/GNAT/2020/lib/gcc/x86_64-pc-linux-gnu/9.3.1/include/stddef.h:417
   end record
   with Convention => C_Pass_By_Copy;  -- /home/doc/GNAT/2020/lib/gcc/x86_64-pc-linux-gnu/9.3.1/include/stddef.h:426

end stddef_h;
