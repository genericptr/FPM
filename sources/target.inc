{
  Free Pascal target is an Intel 80x86 or compatible (16 and 32 bit).
  CPU87 Free Pascal target is an Intel 80x86 or compatible (16 and 32 bit).
  CPU386  Free Pascal target is an Intel 80386 or later.
  CPUI386 Free Pascal target is an Intel 80386 or later.
  CPU68K  Free Pascal target is a Motorola 680x0 or compatible.
  CPUM68K Free Pascal target is a Motorola 680x0 or compatible.
  CPUM68020 Free Pascal target is a Motorola 68020 or later.
  CPU68 Free Pascal target is a Motorola 680x0 or compatible.
  CPUSPARC32  Free Pascal target is a SPARC v7 or compatible.
  CPUSPARC  Free Pascal target is a SPARC v7 or compatible.
  CPUALPHA  Free Pascal target is an Alpha AXP or compatible.
  CPUPOWERPC  Free Pascal target is a 32-bit or 64-bit PowerPC or compatible.
  CPUPOWERPC32  Free Pascal target is a 32-bit PowerPC or compatible.
  CPUPOWERPC64  Free Pascal target is a 64-bit PowerPC or compatible.
  CPUX86_64 Free Pascal target is a AMD64 or Intel 64-bit processor.
  CPUAMD64  Free Pascal target is a AMD64 or Intel 64-bit processor.
  CPUX64  Free Pascal target is a AMD64 or Intel 64-bit processor
  CPUIA64 Free Pascal target is a Intel itanium 64-bit processor.
  CPUARM  Free Pascal target is an ARM 32-bit processor.
  CPUAVR  Free Pascal target is an AVR 16-bit processor.
  CPU16 Free Pascal target is a 16-bit CPU.
  CPU32 Free Pascal target is a 32-bit CPU.
  CPU64 Free Pascal target is a 64-bit CPU.
}

{$if defined(CPUPOWERPC32)}
  {$define TARGET_CPU_PPC}
  {$undef TARGET_CPU_PPC64}
  {$undef TARGET_CPU_X86}
  {$undef TARGET_CPU_X86_64}
  {$undef TARGET_CPU_ARM}
  {$define TARGET_OS_MAC}
  {$undef TARGET_OS_IPHONE}
  {$undef TARGET_IPHONE_SIMULATOR}
  {$undef TARGET_RT_64_BIT}
{$elseif defined(CPUPOWERPC64)}
  {$undef TARGET_CPU_PPC}
  {$define TARGET_CPU_PPC64}
  {$undef TARGET_CPU_X86}
  {$undef TARGET_CPU_X86_64}
  {$undef TARGET_CPU_ARM}
  {$define TARGET_OS_MAC}
  {$undef TARGET_OS_IPHONE}
  {$undef TARGET_IPHONE_SIMULATOR}
  {$define TARGET_RT_64_BIT}
{$elseif defined(CPUI386)}
  {$undef TARGET_CPU_PPC}
  {$undef TARGET_CPU_PPC64}
  {$define TARGET_CPU_X86}
  {$undef TARGET_CPU_X86_64}
  {$undef TARGET_CPU_ARM}
  {$undef TARGET_RT_64_BIT}
  {$if defined(IPHONESIM)}
    {$undef TARGET_OS_MAC}
    {$define TARGET_OS_IPHONE}
    {$define TARGET_IPHONE_SIMULATOR}
  {$else}
    {$define TARGET_OS_MAC}
    {$undef TARGET_OS_IPHONE}
    {$undef TARGET_IPHONE_SIMULATOR}
  {$endif}
{$elseif defined(CPUX86_64)}
  {$undef TARGET_CPU_PPC}
  {$undef TARGET_CPU_PPC64}
  {$undef TARGET_CPU_X86}
  {$define TARGET_CPU_X86_64}
  {$undef TARGET_CPU_ARM}
  {$define TARGET_OS_MAC}
  {$undef TARGET_OS_IPHONE}
  {$undef TARGET_IPHONE_SIMULATOR}
  {$define TARGET_RT_64_BIT}
{$elseif defined(CPUARM)}
  {$undef TARGET_CPU_PPC}
  {$undef TARGET_CPU_PPC64}
  {$undef TARGET_CPU_X86}
  {$undef TARGET_CPU_X86_64}
  {$define TARGET_CPU_ARM}
  {$undef TARGET_OS_MAC}
  {$define TARGET_OS_IPHONE}
  {$undef TARGET_IPHONE_SIMULATOR}
  {$undef TARGET_RT_64_BIT}
{$endif}