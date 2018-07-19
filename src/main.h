#pragma once
#include "stdafx.h"

struct options
{
    fs::path exec_path;
    vector<fs::path> read_files; 
    fs::path bc_include_directory;
    vector<fs::path> include_directories;
    fs::path bc_output_path;
    fs::path llvm_output_path;
    fs::path output_directory;
    string log_file_name;

    unordered_map<string,string> defines;
    
    bool compile_only{false};

    int opt_level{0};
    int inline_level{0};
    bool no_check{false};
    bool no_term_unification{false};
    bool no_term_offload{false};

    bool debug_info{false};
    enum counter_type { NO_COUNTER=0, INSERTIONS, ALL } counter{NO_COUNTER};

    int verbose{0};
    int warning{1};

    bool print_all_timings{false};

    bool print_before_opt{false};
    bool print_after_opt{false};

    bool trace{false};
    bool yy_debug{false};
};

extern options g_opt;

struct input_environment
{
    deque<fs::path> paths;
};

extern input_environment ienv;

extern "C"
auto find_file(
    char const* const filepath_str )
    ->  char const* const;

auto open_file(
    fs::path const& filepath,
    bool const include_working_dir = false,
	bool const preprocess = false )
    -> FILE*;

auto read_file(
    fs::path const& filepath,
    bool const include_working_dir = false )
    -> void;

