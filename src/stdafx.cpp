#ifdef _MSC_VER
#pragma warning( push, 0 )
#endif

#include "stdafx.h"

#ifdef _MSC_VER
#pragma warning( pop )
#endif

auto NOINLINE fail_assertion( 
    char const* const file,
    int const line_no,
    char const* const msg )
    -> void
{
    cerr << file << ':' << line_no << " Assertion failed:" << endl;
    cerr << msg << endl;
    TRAPFUNCTION;
}
