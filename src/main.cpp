#include "stdafx.h"
#include "utility.h"

#include "main.h"
#include "table.h"
//*****************************************************************************
// Driver
//*****************************************************************************
auto init()
{
	init_types();
	env.file_names.insert(env.file_names.end(),
	{ rref_str("/dev/stdin"),
		rref_str("/dev/stdout"),
		rref_str("/dev/stderr") });
}

auto qualify_output_path(fs::path& name)
{
	if (!name.empty() && !name.is_absolute()) {
		name = g_opt.output_directory / name;
	}
}

auto find_macro(string_ref const& x) -> string_ref
{
	auto const i = g_opt.defines.find(string(x));
	if (i == g_opt.defines.end()) {
		cerr << "Unknown define: " << x << endl;
		exit(1);
	}
	return i->second;
}

options g_opt;

auto main(
	int argc,
	char* argv[])
	-> int
{
	g_opt.exec_path = argv[0];

	namespace po = boost::program_options;
	po::options_description optdesc(
		"Usage: rlog [option...] <input-file>");
	optdesc.add_options()
		("help,h", "produce help message")
		("config-file,c",
			po::value<string>()->default_value(""),
			"config file")
			("bc-include,b",
				po::value<string>()->default_value(""),
				"include path for *.bc files")
				("input-file",
					po::value<string>(),
					"input file")
					("include,I",
						po::value<vector<string>>()->default_value(vector<string>(), ""),
						"include paths")
						("output,o",
							po::value<string>()->default_value(""),
							"bitcode output file name")
							("output-llvm,l",
								po::value<string>()->default_value(""),
								"llvm output file name")
								("output-dir",
									po::value<string>()->default_value(""),
									"output directory")
									("compile,C", "compile only")
		("optimize,O",
			po::value<int>()->default_value(0),
			"optimization level")
			("inline,i",
				po::value<int>()->default_value(0),
				"inline level")
				("define,D",
					po::value<vector<string>>()->default_value(vector<string>(), ""),
					"defines")
					("no-check,n", "no run-time assertions")
		("no-unification", "turn off term unification")
		("no-term-offload", "turn off term offload")
		("debug,g", "enable debug information")
		("counter",
			po::value<int>()->default_value(0),
			"counter level (0=no counter, 1=insertions, 2=all)")
			("log-name",
				po::value<string>()->default_value("program"),
				"log name")
				("verbose,v",
					po::value<int>()->implicit_value(1)->default_value(0),
					"verbosity level")
					("print-timings", "print all timings")
		("print-before,p", "print program before optimization")
		("print-after,P", "print program after optimization")
		("trace", "enable trace")
		("yydebug", "enable parser debug mode")
		;

	po::positional_options_description posoptdesc;
	posoptdesc.add("input-file", -1);

	string input_file_name;

	try {
		po::variables_map vm;
		store(po::command_line_parser(argc, argv).
			options(optdesc).positional(posoptdesc).run(), vm);

		notify(vm);

		auto const& config_file_name = vm["config-file"].as<string>();
		if (!config_file_name.empty()) {
			ifstream configfile(config_file_name);
			if (configfile.good()) {
				store(po::parse_config_file(configfile, optdesc), vm);
			}
			else {
				cerr << bt::format(
					"Error opening config file '%1%'.\n")
					% config_file_name;
				exit(1);
			}
		}

		notify(vm);

		if (vm.count("help")) {
			cout << optdesc;
			exit(0);
		}

		if (vm.count("input-file") == 0) {
			cerr << "No input specified." << endl;
			exit(1);
		}

		g_opt.bc_include_directory = fs::path{ vm["bc-include"].as<string>() };
		input_file_name = vm["input-file"].as<string>();
		rg::transform(
			vm["include"].as<vector<string>>(),
			back_inserter(g_opt.include_directories),
			[](auto const& x) { return fs::path{ x }; });
		g_opt.bc_output_path = fs::path{ vm["output"].as<string>() };
		g_opt.llvm_output_path = fs::path{ vm["output-llvm"].as<string>() };
		g_opt.output_directory = fs::path{ vm["output-dir"].as<string>() };
		g_opt.compile_only = vm.count("compile") > 0;
		g_opt.opt_level = vm["optimize"].as<int>();
		g_opt.inline_level = vm["inline"].as<int>();
		g_opt.no_check = vm.count("no-check") > 0;
		g_opt.no_term_unification = vm.count("no-unification") > 0;
		g_opt.no_term_offload = vm.count("no-term-offload") > 0;
		g_opt.debug_info = vm.count("debug") > 0;
		g_opt.counter = (options::counter_type)vm["counter"].as<int>();
		g_opt.log_file_name = vm["log-name"].as<string>();
		g_opt.verbose = vm["verbose"].as<int>();
		g_opt.print_all_timings = vm.count("print-timings") > 0;
		g_opt.print_before_opt = vm.count("print-before") > 0;
		g_opt.print_after_opt = vm.count("print-after") > 0;
		g_opt.trace = vm.count("trace") > 0;
		g_opt.yy_debug = vm.count("yydebug");

		for (auto const& str : vm["define"].as<vector<string>>()) {
			auto const equals = str.find_first_of("=");
			if (equals == string::npos) {
				cerr << "Invalid define: " << str << endl;
				exit(1);
			}
			g_opt.defines[str.substr(0, equals)] = str.substr(equals + 1);
		}

		qualify_output_path(g_opt.bc_output_path);
		qualify_output_path(g_opt.llvm_output_path);
	}
	catch (po::unknown_option const& e) {
		cerr << e.what() << endl;
		cout << optdesc;
		exit(-1);
	}
	catch (po::error const& e) {
		cerr << e.what() << endl;
		exit(-1);
	}
	init();
	read_file(input_file_name, true);

	expand_inlines();
	compute_dep_graph();
	process_indices();
	if (g_opt.verbose >= 2) {
		print_indices(root_structure());
	}
	compile_rules();

	if (!g_opt.compile_only) {
		codegen2::codegen();
	}
}

