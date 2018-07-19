#pragma once
#ifdef _MSC_VER
#   pragma warning( disable : 4350 )
#   define _CRT_SECURE_NO_WARNINGS
#   define _SCL_SECURE_NO_WARNINGS
#   define WIN32_LEAN_AND_MEAN
#   ifdef NDEBUG
#       define _SECURE_SCL 0
#       define _ITERATOR_DEBUG_LEVEL 0
#   endif
#endif

//#define PROFILE
#ifndef PROFILE
#   define PROFILE_NOINLINE
#else
#   define PROFILE_NOINLINE BOOST_NOINLINE
#endif

//#include <atomic>
//#include <thread>
//#include <mutex>

#include <algorithm>
#include <array>
#include <bitset>
#include <chrono>
#include <deque>
#include <exception>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <limits>
//#include <list>
//#include <forward_list>
//#include <map>
#include <memory>
#include <numeric>
#include <new>
//#include <set>
//#include <queue>
#include <sstream>
#include <stdexcept>
#include <streambuf>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <cinttypes>

#define BOOST_RESULT_OF_USE_DECLTYPE
#define BOOST_MOVE_USE_STANDARD_LIBRARY_MOVE
#define BOOST_NO_RTTI
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/cxx11/any_of.hpp>
#include <boost/algorithm/cxx11/all_of.hpp>
#include <boost/format.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/functional/hash.hpp>
#include <boost/functional/hash/extensions.hpp>
#include <boost/iterator.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/iterator/zip_iterator.hpp>
#include <boost/program_options.hpp>
#include <boost/range.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/adaptor/indexed.hpp>
#include <boost/range/adaptor/indirected.hpp>
#include <boost/range/adaptor/map.hpp>
#include <boost/range/iterator.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/range/algorithm_ext.hpp>
#include <boost/range/join.hpp>
#include <boost/utility.hpp>
#include <boost/utility/string_ref.hpp>
#include <boost/process/system.hpp>
#include <boost/process/io.hpp>
#include <boost/process/pipe.hpp>

using namespace std;
namespace bt = boost;
namespace fs = boost::filesystem;
namespace rg = boost::range;
namespace ad = boost::adaptors;
namespace ag = boost::algorithm;

#ifdef _MSC_VER
#   define TRAPFUNCTION __debugbreak()
#   define NOINLINE __declspec(noinline)
#   ifndef NDEBUG
#       define MYFORCEINLINE
#   else
#       define MYFORCEINLINE __forceinline
#   endif
#   define SPINPAUSE _mm_pause()
#   define PREFETCH(p) _mm_prefetch( (char const*)p, _MM_HINT_T0 )
#   define ATTRIBUTE_USED
#else
#   define TRAPFUNCTION __builtin_trap()
#   define NOINLINE __attribute__((noinline))
#   ifndef NDEBUG
#       define MYFORCEINLINE
#   else
#       define MYFORCEINLINE __attribute__((always_inline))
#   endif
#   define SPINPAUSE _mm_pause()
#   define PREFETCH(p) _mm_prefetch( (char const*)p, _MM_HINT_T0 )
#   define ATTRIBUTE_USED __attribute__((used))
#endif

auto NOINLINE fail_assertion( 
    char const* const file,
    int const line_no,
    char const* const msg )
    -> void;

#ifndef NASSERT
#   define my_assert( expression ) \
            ((void)( !!(expression) || ( \
                fail_assertion( __FILE__, __LINE__, "" ),\
                true ) ))
#else
#   define my_assert( expression ) ( true )
#endif

#define die() my_assert( 0 ); abort();
#define die_exp() ( my_assert( 0 ), abort(), 0 )
