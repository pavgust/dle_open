#include "stdafx.h"
#ifdef _MSC_VER
#   pragma warning( push )
#   pragma warning( disable : 4065 4018 4702 4701 )
#endif
#include "gen/dleparser.cc"
#ifdef _MSC_VER
#   pragma warning( pop )
#endif

#include "main.h"

input_environment ienv;


extern "C"
auto find_file(
    char const* const filepath_str )
    -> char const* const
{
    fs::path filepath = filepath_str;
    static string str;
    bool const include_working_dir = true;
    if( filepath.is_absolute() ) {
        str = filepath.string();
        return str.c_str();
    }
    else {
        if( fs::exists( filepath ) ) {
            str = filepath.string();
            return str.c_str();
        }
        for( auto const& include_path : g_opt.include_directories ) {
            auto const f = include_path / filepath;
            if( fs::exists( f ) ) {
                str = f.string();
                return str.c_str();
            }
        }
    }
    str = filepath.string();
    return str.c_str();
    //cerr << bt::format( 
    //    "Could not find %1%.\n" )
    //    % filepath;
    //exit( 1 );
}

struct unlinker
{
	vector<fs::path> paths;
	~unlinker() 
	{
		for( auto const& x : paths ) {
			fs::remove( x );
		}
	}
};

unlinker g_unlinker;

auto open_file(
    fs::path const& filepath,
    bool const include_working_dir,
	bool const preprocess )
    -> FILE*
{
    FILE* in = nullptr;
    if( filepath.is_absolute() ) {
        in = fopen( filepath.string().c_str(), "rb" );
        if( in ) {
            g_opt.read_files.push_back( filepath );
        }
    }
    else {
        if( include_working_dir ) {
            in = fopen( filepath.string().c_str(), "rb" );
            if( in ) {
                g_opt.read_files.push_back( filepath );
            }
        }
        
        if( !in ) {
            for( auto const& include_path : g_opt.include_directories ) {
                in = fopen(
                    ( include_path / filepath ).string().c_str(), "rb" );
                if( in ) {
                    g_opt.read_files.push_back( include_path / filepath );
                    break;
                }
            }
        }
    }

    if( !in ) {
        cerr << bt::format( 
            "Error opening %1%.\n" )
            % filepath;
        exit( 1 );
    }

	if( preprocess ) {
		string exe = "gcc -E";
		for( auto const& x : g_opt.include_directories ) {
			exe += " -I" + x.string();
		}
		exe += " -";
		VERBOSE_PRINT( 1 ) 
			cout << bt::format( "Preprocessing %s with \"%s\"\n" )
				% filepath % exe;

		auto const ppfilename = ( boost::filesystem::temp_directory_path() / boost::filesystem::unique_path() );
		g_unlinker.paths.push_back( ppfilename );
		bt::process::pstream err;
		int retcode;
		try {
			retcode = bt::process::system( 
				exe, 
				bt::process::std_in < in, 
				bt::process::std_out > ppfilename,
				bt::process::std_err > stderr );
		}
		catch( bt::process::process_error const& e ) {
			cerr << e.what() << endl;
			exit( 1 );
		}
		if( retcode ) {
			cerr << "Preprocessing faied.\n";
			exit( 1 );
		}

		fclose( in );
		in = fopen( ppfilename.string().c_str(), "rb" );
	}

    return in;
}

auto read_file(
    fs::path const& filepath,
    bool const include_working_dir )
    -> void
{
    yyscan_t scanner;
    dlelex_init( &scanner );

    FILE* in = open_file( filepath, include_working_dir, true );
    dleset_in( in, scanner );
    dleset_debug( g_opt.yy_debug, scanner );
    if( g_opt.yy_debug ) {
        dledebug = 1;
    }
    ienv.paths.push_back( filepath );
    if( dleparse( scanner ) ) {
        cerr << "Syntax error." << endl;
        exit( -1 );
    }
    fclose( in );
    dlelex_destroy ( scanner );

    ienv.paths.pop_back();
}

auto construct( YYLTYPE const& loc ) -> rref<location_tc>
{
	my_assert( !ienv.paths.empty() );
    return construct<location_tc>(
        ienv.paths.back().filename().string(),
        (uint32_t)loc.first_line, 
        (uint32_t)loc.first_column, 
        (uint32_t)loc.last_line, 
        (uint32_t)loc.last_column );
}
