#include "stdafx.h"
#ifdef _MSC_VER
#include <windows.h>
#include <process.h>
#include <io.h>
#include <psapi.h>
#else
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

pair<size_t,size_t> process_mem_usage()
{
   size_t vm_usage     = 0u;
   size_t resident_set = 0u;

#ifdef _MSC_VER
    PROCESS_MEMORY_COUNTERS_EX pmc;
    GetProcessMemoryInfo(
        GetCurrentProcess(),
        (PROCESS_MEMORY_COUNTERS*)&pmc,
        sizeof(pmc) );
    vm_usage = pmc.PrivateUsage;
    resident_set = pmc.WorkingSetSize;
#else
   ifstream stat_stream( "/proc/self/stat" );

   // dummy
   string pid, comm, state, ppid, pgrp, session, tty_nr;
   string tpgid, flags, minflt, cminflt, majflt, cmajflt;
   string utime, stime, cutime, cstime, priority, nice;
   string o, itrealvalue, starttime;

   // the two fields we want
   unsigned long vsize;
   long rss;

   // don't care about the rest
   stat_stream >> pid >> comm >> state >> ppid >> pgrp >> session >> tty_nr
               >> tpgid >> flags >> minflt >> cminflt >> majflt >> cmajflt
               >> utime >> stime >> cutime >> cstime >> priority >> nice
               >> o >> itrealvalue >> starttime >> vsize >> rss;

   // in case x86-64 is configured to use 2MB pages
   long page_size = sysconf( _SC_PAGE_SIZE );
   vm_usage     = vsize;
   resident_set = rss * page_size;
#endif
   return make_pair(vm_usage,resident_set);
}
