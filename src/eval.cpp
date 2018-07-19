#include "stdafx.h"
#include "utility.h"

#include "main.h"
#include "ds.h"
#include "gen/datatypes.hh"
#include "eval.h"

//#define TRACE( x ) x
#define TRACE( x )

template< typename T >
struct fake_ref {};
using builtin_op_tc = t_builtin_op< fake_ref >;
tagged_object_t const invalid_object_idx = (tagged_object_t)-1;

struct array_view
{
    size_t const size;
    uint32_t const* p;
};

struct join_location
{
    uint32_t 
        rule_first_line, rule_first_column,
        rule_last_line, rule_last_column,
        literal_first_line, literal_first_column,
        literal_last_line, literal_last_column;
    uint32_t join_no;
    string_ref pidx_name;
};

struct eval_environment
{
    char const* root;
    dynamic_vector index;
    stable_vector string_pool;
    term_set<string_term_traits> string_set;
    vector<void (*)( array_view const& )> functions;
    vector<string> sources;
    vector<join_location> join_locations;
};
eval_environment eenv;


auto inline get_tagged_id(
    dconstr_tag_t const constr,
    object_t const idx )
    -> tagged_object_t
{
    my_assert( constr < 32 && (idx >> 27) == 0 );
    return (constr << 27) + idx;
}

auto inline get_dconstr_tag(
    tagged_object_t const id )
    -> dconstr_tag_t
{
    return id >> 27;
}

auto inline get_object_idx(
    tagged_object_t const id )
    -> object_t
{
    return (~(0b11111 << 27))&id;
}

auto print_uint32( array_view const& arg ) -> void
{
    my_assert( arg.size == 1 );
    cout << arg.p[0];
}

auto print_string( array_view const& arg ) -> void
{
    my_assert( arg.size == 1 );
    auto const str = eenv.string_set.get( arg.p[0] );
    cout << str;
}

auto read_array(
    uint32_t const*& p )
{
    auto const size = *p++;
    auto const p0 = p;
    p += size;
    return array_view{ size, p0 };
}

auto read_string(
    uint32_t const*& p )
{
    size_t const sz = *p++;
    char const* const str = reinterpret_cast<char const*>( p );
    size_t const words = (sz+3)>>2;
    p += words;
    return string_ref( str, sz );
}

auto open_file(
    fs::path const& file_path )
    -> FILE*;

auto eval_block(
    size_t const begin,
    tagged_object_t const arg_vars[],
    tagged_object_t vars[],
    char* index_buf )
    -> tagged_object_t;

auto read_file(
    tagged_object_t const arg_vars[],
    tagged_object_t vars[],
    char* index_buf,
    size_t const descr,
    array_view const& idxs,
    array_view const& types,
    size_t const cont )
{
    auto const file_name = eenv.sources[descr];
    auto* in = open_file( file_name );
    if( g_opt.verbose >= 1 ) {
        cout << bt::format( "Reading %s..." ) % file_name;
    }

    char line[1024*128];
    size_t lines = 0u;
    while( fgets( line, sizeof(line), in ) ) {
        auto const len = strnlen( line, sizeof(line) );
        if( len == 0 || line[len-1] != '\n' ) {
            cerr << bt::format(
                "Error reading %1% at position %2%.\n" )
                % file_name
                % ftell( in );
            exit( 1 );
        }

        auto str = string_ref( line, len );
        for( unsigned i = 0; i < idxs.size; ++i ) {
            auto const field_len = str.find_first_of( "\n\t" );
            if( field_len == str.npos ) {
                cerr << bt::format(
                    "Unexpected end of line reading line '%s' of "
                    "file '%s'.\n" )
                    % line
                    % file_name;
                exit( 1 );
            }

            if( types.p[i] == 0 ) {
                char buf[13];
                if( !(field_len + 1 < sizeof( buf ) ) ) {
                    cerr << bt::format(
                        "Unexpected uint32 field '%s' of "
                        "file '%s'.\n" )
                        % line
                        % file_name;
                    exit( 1 );
                }
                memcpy( buf, str.data(), field_len );
                buf[field_len] = 0;
                vars[idxs.p[i]] = atoi( buf );

            }
            else if( types.p[i] == 1 ) {
                vars[idxs.p[i]] = eenv.string_set.insert(
                    str.substr( 0, field_len ) );
            }
            else {
                die();
            }
            str = str.substr( field_len + 1 );
        }
        my_assert( str.empty() );
        eval_block( cont, arg_vars, vars, index_buf );
        ++lines;
    }

    if( g_opt.verbose >= 1 ) {
        cout << bt::format(
            " %d facts.\n" )
            % lines;
    }
    fclose( in );
}

auto eval(
    size_t const begin,
    tagged_object_t arg_vars[] )
    -> tagged_object_t;

auto eval_block(
    size_t const begin,
    tagged_object_t const arg_vars[],
    tagged_object_t vars[],
    char* index_buf )
    -> tagged_object_t
{
    tagged_object_t new_arg_vars[max_args];
    uint32_t const* p = reinterpret_cast<uint32_t const*>( eenv.root + begin );
    size_t const block_sz = *p++;
    uint32_t const* const end = reinterpret_cast<uint32_t const*>( 
        eenv.root + begin + block_sz );
    size_t join_info = (size_t)-1;
    while( p != end ) {
        auto const itype = *p++;
        switch( itype ) {
        case instruction_tc::CALL_VOID:
        {
            size_t const cont = *p++;
            auto const args = read_array( p );
            for( size_t i = 0; i < args.size; ++i ) {
                new_arg_vars[i] = vars[args.p[i]];
            }
            eval( cont, new_arg_vars );
        }
        break;
        case instruction_tc::CALL:
        {
            size_t const cont = *p++;
            size_t const ret = *p++;
            auto const args = read_array( p );
            for( size_t i = 0; i < args.size; ++i ) {
                new_arg_vars[i] = vars[args.p[i]];
            }
            vars[ret] = eval( cont, new_arg_vars );
        }
        break;
        case instruction_tc::CALL_EXTERNAL:
        {
            size_t const fno = *p++;
            auto const args = read_array( p );
            for( size_t i = 0; i < args.size; ++i ) {
                new_arg_vars[i] = vars[args.p[i]];
            }
            (*eenv.functions[fno])( array_view{ args.size, new_arg_vars } );
        }
        break;
        case instruction_tc::EVAL:
        {
            size_t const cont = *p++;
            eval_block( cont, arg_vars, vars, index_buf );
        }
        break;
        case instruction_tc::TRY_EVAL:
        {
            size_t const cont = *p++;
            auto const ret = eval_block( cont, arg_vars, vars, index_buf );
            if( ret != invalid_object_idx ) {
                return ret;
            }
        }
        break;
        case instruction_tc::UNIFY_F_ARG:
        {
            size_t const idx = *p++;
            size_t const arg_idx = *p++;
            vars[idx] = arg_vars[arg_idx];
        }
        break;
        case instruction_tc::UNIFY_BB:
        {
            size_t const var1idx = *p++;
            size_t const var2idx = *p++;
            if( vars[var1idx] != vars[var2idx] ) {
                return invalid_object_idx;
            }
        }
        break;
        case instruction_tc::NOT_ZERO:
        {
            size_t const varidx = *p++;
            if( vars[varidx] == 0 ) {
                return invalid_object_idx;
            }
        }
        break;
        case instruction_tc::NOT_UNIFY_BB:
        {
            size_t const var1idx = *p++;
            size_t const var2idx = *p++;
            if( vars[var1idx] == vars[var2idx] ) {
                return invalid_object_idx;
            }
        }
        break;
        case instruction_tc::UNIFY_FB:
        {
            size_t const var1idx = *p++;
            size_t const var2idx = *p++;
            vars[var1idx] = vars[var2idx];
        }
        break;
        case instruction_tc::UNIFY_B_CONSTANT:
        {
            size_t const varidx = *p++;
            size_t const value = *p++;
            if( vars[varidx] != value ) {
                return invalid_object_idx;
            }
        }
        break;
        case instruction_tc::UNIFY_F_CONSTANT:
        {
            size_t const varidx = *p++;
            size_t const value = *p++;
            vars[varidx] = value;
        }
        break;
        case instruction_tc::UNIFY_F_ARITHMETIC:
        {
            size_t const varidx = *p++;
            uint32_t const op = *p++;
            auto const idxs = read_array( p );
            switch( op ) {
            case builtin_op_tc::ADD:
            {
                vars[varidx] = vars[idxs.p[0]] + vars[idxs.p[1]];
            }
            break;
            case builtin_op_tc::SUB:
            {
                vars[varidx] = vars[idxs.p[0]] - vars[idxs.p[1]];
            }
            break;
            case builtin_op_tc::MULTIPLY:
            {
                vars[varidx] = vars[idxs.p[0]] * vars[idxs.p[1]];
            }
            break;
            case builtin_op_tc::DIVIDE:
            {
                vars[varidx] = vars[idxs.p[0]] / vars[idxs.p[1]];
            }
            break;
            case builtin_op_tc::LESS_THAN:
            {
                vars[varidx] = vars[idxs.p[0]] < vars[idxs.p[1]];
            }
            break;
            case builtin_op_tc::LESS_THAN_EQ:
            {
                vars[varidx] = vars[idxs.p[0]] <= vars[idxs.p[1]];
            }
            break;
            case builtin_op_tc::EQUALS:
            {
                vars[varidx] = vars[idxs.p[0]] == vars[idxs.p[1]];
            }
            break;
            case builtin_op_tc::NEQUALS:
            {
                vars[varidx] = vars[idxs.p[0]] != vars[idxs.p[1]];
            }
            break;
            case builtin_op_tc::AND_:
            {
                vars[varidx] = vars[idxs.p[0]] && vars[idxs.p[1]];
            }
            break;
            case builtin_op_tc::OR_:
            {
                vars[varidx] = vars[idxs.p[0]] || vars[idxs.p[1]];
            }
            break;
            default:
                die();
            }
        }
        break;
        case instruction_tc::BR_IF:
        {
            size_t const idx = *p++;
            size_t const condidx = *p++;
            size_t const trueblk = *p++;
            if( vars[condidx] != 0 ) {
                vars[idx] = eval_block( trueblk, arg_vars, vars, index_buf );
            }
        }
        break;
        case instruction_tc::BR_IF_ELSE:
        {
            size_t const idx = *p++;
            size_t const condidx = *p++;
            size_t const trueblk = *p++;
            size_t const falseblk = *p++;
            vars[idx] = vars[condidx] != 0 
                ? eval_block( trueblk, arg_vars, vars, index_buf )
                : eval_block( falseblk, arg_vars, vars, index_buf );
        }
        break;
        case instruction_tc::FRAME_RETURN:
        {
            size_t const idx = *p++;
            return vars[idx];
        }
        break;
        case instruction_tc::PACK_COMPOUND:
        {
            size_t const offset = *p++;
            uint32_t const dconstr_tag = *p++;
            size_t const to_idx = *p++;
            auto const idxs = read_array( p );

            auto& tset = *reinterpret_cast<indexed_term_set*>(
                eenv.index.data() + offset );

            object_t const obj_idx = tset.insert( 
                make_pair( vars, idxs.p ) );
            vars[to_idx] = get_tagged_id( dconstr_tag, obj_idx );
        }
        break;
        case instruction_tc::UNPACK_COMPOUND:
        {
            size_t const offset = *p++;
            uint32_t const dconstr_tag = *p++;
            size_t const from_idx = *p++;
            auto const idxs = read_array( p );

            tagged_object_t const obj_id = vars[from_idx];
            if( get_dconstr_tag( obj_id ) != dconstr_tag ) {
                return invalid_object_idx;
            }

            auto& tset = *reinterpret_cast<indexed_term_set*>(
                eenv.index.data() + offset );
            copy_components(
                tset,
                get_object_idx( obj_id ),
                make_pair( vars, idxs.p ) );
        }
        break;
        case instruction_tc::UNPACK_COMPOUND_ELSE:
        {
            size_t const offset = *p++;
            uint32_t const dconstr_tag = *p++;
            size_t const from_idx = *p++;
            auto const idxs = read_array( p );
            size_t const cont = *p++;

            tagged_object_t const obj_id = vars[from_idx];
            if( get_dconstr_tag( obj_id ) != dconstr_tag ) {
                return eval_block( cont, arg_vars, vars, index_buf );
            }

            auto& tset = *reinterpret_cast<indexed_term_set*>(
                eenv.index.data() + offset );
            copy_components(
                tset,
                get_object_idx( obj_id ),
                make_pair( vars, idxs.p ) );
        }
        break;

        case instruction_tc::WORKLIST_FIXPOINT:
        {
            auto const offsets = read_array( p );
            size_t const cont = *p++;
            for( ;; ) {
                size_t total = 0;
                for( size_t i = 0; i < offsets.size; ++i ) {
                    auto& old_wl = *reinterpret_cast<dynamic_vector*>( 
                        eenv.index.data() + offsets.p[i] );
                    auto& new_wl = *reinterpret_cast<dynamic_vector*>( 
                        eenv.index.data() + offsets.p[i] + sizeof( dynamic_vector ) );
                    total += new_wl.offset;
                    old_wl.swap( new_wl );
                    new_wl.clear();
                }

                if( total == 0 ) {
                    break;
                }
                eval_block( cont, arg_vars, vars, index_buf );
            }
        }
        break;
        case instruction_tc::SCAN_SOURCE:
        {
            size_t const descr = *p++;
            auto const idxs = read_array( p );
            auto const types = read_array( p );
            size_t const cont = *p++;

            read_file( arg_vars, vars, index_buf, descr, idxs, types, cont );
        }
        break;
        case instruction_tc::SCAN_ARRAY_MAP:
        {
            size_t const offset = *p++;
            size_t const idx = *p++;
            size_t const cont = *p++;

            auto const& amap = *reinterpret_cast<array_map const*>( 
                index_buf + offset );
            for( size_t i = 0; i < amap.size; ++i ) {
                char* const ibuf = (char*)amap[i];
                vars[idx] = i;
                eval_block( cont, arg_vars, vars, ibuf );
            }
        }
        break;
        case instruction_tc::LOOKUP_ARRAY_MAP:
        {
            size_t const offset = *p++;
            size_t const idx = *p++;
            size_t const cont = *p++;

            auto const& amap = *reinterpret_cast<array_map const*>( 
                index_buf + offset );
            size_t const i = vars[idx];
            if( i < amap.size ) {
                char* const ibuf = (char*)amap[i];
                eval_block( cont, arg_vars, vars, ibuf );
            }
        }
        break;
        case instruction_tc::LOOKUP_INSERT_ARRAY_MAP:
        {
            size_t const offset = *p++;
            size_t const idx = *p++;
            size_t const init = *p++;
            size_t const cont = *p++;

            auto& amap = *reinterpret_cast<array_map*>( 
                index_buf + offset );
            size_t const i = vars[idx];
            while( i >= amap.size ) {
                char* const ibuf = (char*)amap.alloc();
                eval_block( init, arg_vars, vars, ibuf );
            }
            char* const ibuf = (char*)amap[i];
            eval_block( cont, arg_vars, vars, ibuf );
        }
        break;
        case instruction_tc::SCAN_HASH_MAP:
        {
            size_t const offset = *p++;
            auto const idxs = read_array( p );
            size_t const cont = *p++;

            typedef indexed_map_traits::node_type node_type;
            auto const& tset = *reinterpret_cast<indexed_hash_map const*>( 
                index_buf + offset );
            size_t const elem_sz = tset.traits.size();
            for( size_t k = 0; k < tset.vec.size; ++k ) {
                node_type const* node = (node_type*)tset.vec[k];
                auto const* elems = tset.traits.get_values( *node );
                for( size_t i = 0; i < idxs.size; ++i ) {
                    vars[idxs.p[i]] = elems[i];
                }
                char* const ibuf = tset.traits.get_mapped( *node );
                eval_block( cont, arg_vars, vars, ibuf );
            }
        }
        break;
        case instruction_tc::LOOKUP_HASH_MAP:
        {
            size_t const offset = *p++;
            auto const idxs = read_array( p );
            size_t const cont = *p++;

            typedef indexed_map_traits::node_type node_type;
            auto& tset = *reinterpret_cast<indexed_hash_map*>( 
                index_buf + offset );

            auto const obj_id = tset.find( 
                make_pair( vars, idxs.p ) );
            if( obj_id != indexed_term_traits::invalid_idx ) {
                auto* node = get_node( tset, obj_id );
                char* const ibuf = tset.traits.get_mapped( *node );
                eval_block( cont, arg_vars, vars, ibuf );
            }
        }
        break;
        case instruction_tc::LOOKUP_INSERT_HASH_MAP:
        {
            size_t const offset = *p++;
            auto const idxs = read_array( p );
            size_t const init = *p++;
            size_t const cont = *p++;

            typedef indexed_map_traits::node_type node_type;
            auto& tset = *reinterpret_cast<indexed_hash_map*>( 
                index_buf + offset );

            auto const next_obj_id = tset.next_idx;
            auto const obj_id = tset.insert(
                make_pair( vars, idxs.p ) );
            auto* node = get_node( tset, obj_id );
            char* const ibuf = tset.traits.get_mapped( *node );
            if( obj_id == next_obj_id ) {
                eval_block( init, arg_vars, vars, ibuf );
            }
            eval_block( cont, arg_vars, vars, ibuf );
        }
        break;
        case instruction_tc::SCAN_BOOLEAN:
        {
            size_t const offset = *p++;
            size_t const cont = *p++;

            auto const* elem = reinterpret_cast<tagged_object_t const*>( 
                index_buf + offset );
            if( *elem != (tagged_object_t)-1 ) {
                eval_block( cont, arg_vars, vars, eenv.index.data() );
            }
        }
        break;
        case instruction_tc::UNIQUE_INSERT_BOOLEAN:
        {
            size_t const offset = *p++;
            size_t const cont = *p++;

            auto* elem = reinterpret_cast<tagged_object_t*>( 
                index_buf + offset );
            if( *elem == (tagged_object_t)-1 ) {
                *elem = 0;
                eval_block( cont, arg_vars, vars, eenv.index.data() );
            }
        }
        break;
        case instruction_tc::HAS_VALUE:
        {
            size_t const offset = *p++;
            size_t const idx = *p++;
            size_t const true_cont = *p++;
            size_t const false_cont = *p++;

            auto const* elems = reinterpret_cast<tagged_object_t const*>( 
                index_buf + offset );
            vars[idx] = *elems;
            if( vars[idx] != (tagged_object_t)-1 ) {
                eval_block( true_cont, arg_vars, vars, index_buf );
            }
            else {
                eval_block( false_cont, arg_vars, vars, index_buf );
            }
        }
        break;
        case instruction_tc::SET_VALUE:
        {
            size_t const offset = *p++;
            size_t const idx = *p++;
 
            auto* elems = reinterpret_cast<tagged_object_t*>( 
                index_buf + offset );
            *elems = vars[idx];
            index_buf = eenv.index.data(); // awful
        }
        break;
        case instruction_tc::SCAN_VALUE:
        {
            size_t const offset = *p++;
            auto const idxs = read_array( p );
            size_t const cont = *p++;

            auto const* elems = reinterpret_cast<tagged_object_t const*>( 
                index_buf + offset );
            bool have_value = true;
            for( size_t i = 0; i < idxs.size; ++i ) {
                vars[idxs.p[i]] = elems[i];
                if( elems[i] == (tagged_object_t)-1 ) {
                    have_value = false;
                    break;
                }
            }
            if( have_value ) {
                eval_block( cont, arg_vars, vars, eenv.index.data() );
            }
        }
        break;
        case instruction_tc::UNIQUE_INSERT_VALUE:
        {
            size_t const offset = *p++;
            auto const idxs = read_array( p );
            size_t const cont = *p++;

            auto* elems = reinterpret_cast<tagged_object_t*>( 
                index_buf + offset );
            bool new_insert = false;
            for( size_t i = 0; i < idxs.size; ++i ) {
                if( elems[i] == (tagged_object_t)-1 ) {
                    elems[i] = vars[idxs.p[i]];
                    new_insert = true;
                }
                else if( elems[i] != vars[idxs.p[i]] ) {
                    cerr << "Failed to unify value in value index.\n";
                    // TODO: nicer error message
                    exit( 1 );
                }
            }
            if( new_insert ) {
                eval_block( cont, arg_vars, vars, eenv.index.data() );
            }
        }
        break;
        case instruction_tc::SCAN_VALUE_LIST:
        {
            size_t const offset = *p++;
            auto const idxs = read_array( p );
            size_t const cont = *p++;

            auto const& vec = *reinterpret_cast<dynamic_vector const*>( 
                index_buf + offset );
            for( size_t voffset = 0; 
                voffset < vec.offset; 
                voffset += idxs.size*sizeof(tagged_object_t) )
            {
                tagged_object_t const* elems = vec.at<tagged_object_t>( voffset );
                for( size_t i = 0; i < idxs.size; ++i ) {
                    vars[idxs.p[i]] = elems[i];
                }
                eval_block( cont, arg_vars, vars, eenv.index.data() );
            }
        }
        break;
        case instruction_tc::INSERT_VALUE_LIST:
        {
            size_t const offset = *p++;
            auto const idxs = read_array( p );

            auto& vec = *reinterpret_cast<dynamic_vector*>( 
                index_buf + offset );
            auto* newelem = reinterpret_cast<tagged_object_t*>( 
                vec.alloc( idxs.size*sizeof(tagged_object_t) ) );
            TRACE( cout << "ivl " << offset; )
            for( size_t i = 0; i < idxs.size; ++i ) {
                newelem[i] = vars[idxs.p[i]];
                TRACE( cout << ' ' << newelem[i]; )
            }
            TRACE( cout << '\n'; )
        }
        break;
        case instruction_tc::UNIQUE_INSERT_VALUE_LIST:
        {
            size_t const offset = *p++;
            auto const idxs = read_array( p );
            size_t const cont = *p++;

            auto& vec = *reinterpret_cast<dynamic_vector*>( 
                index_buf + offset );
            size_t k = 0;
            for( ;
                k < vec.offset; 
                k += idxs.size*sizeof(tagged_object_t) )
            {
                tagged_object_t const* elem = vec.at<tagged_object_t>( k );
                size_t i = 0;
                for( ; i < idxs.size; ++i ) {
                    if( vars[idxs.p[i]] != elem[i] ) break;
                }

                if( i == idxs.size ) {
                    break; // found duplicate
                }
            }

            if( k >= vec.offset ) {
                // new insert
                auto* newelem = reinterpret_cast<tagged_object_t*>( 
                    vec.alloc( idxs.size*sizeof(tagged_object_t) ) );
                TRACE( cout << "uivl " << offset; )
                for( size_t i = 0; i < idxs.size; ++i ) {
                    newelem[i] = vars[idxs.p[i]];
                    TRACE( cout << ' ' << newelem[i]; )
                }
                TRACE( cout << '\n'; )
                eval_block( cont, arg_vars, vars, eenv.index.data() );
            }
        }
        break;
        case instruction_tc::SCAN_HASH_SET:
        {
            size_t const offset = *p++;
            auto const idxs = read_array( p );
            size_t const cont = *p++;

            typedef indexed_term_traits::node_type node_type;
            auto const& tset = *reinterpret_cast<indexed_term_set const*>( 
                index_buf + offset );
            indexed_term_traits const traits{idxs.size};
            for( size_t k = 0; k < tset.vec.size; ++k ) {
                node_type const* node = (node_type*)tset.vec[k];
                auto const* elems = traits.get_values( *node );
                for( size_t i = 0; i < idxs.size; ++i ) {
                    vars[idxs.p[i]] = elems[i];
                }
                eval_block( cont, arg_vars, vars, eenv.index.data() );
            }
        }
        break;
        case instruction_tc::LOOKUP_HASH_SET:
        {
            size_t const offset = *p++;
            auto const idxs = read_array( p );
            size_t const cont = *p++;

            typedef indexed_term_traits::node_type node_type;
            auto& tset = *reinterpret_cast<indexed_term_set*>( 
                index_buf + offset );

            auto const obj_id = tset.find( 
                make_pair( vars, idxs.p ) );
            if( obj_id != indexed_term_traits::invalid_idx ) {
                eval_block( cont, arg_vars, vars, eenv.index.data() );
            }
        }
        break;
        case instruction_tc::UNIQUE_INSERT_HASH_SET:
        {
            size_t const offset = *p++;
            auto const idxs = read_array( p );
            size_t const cont = *p++;

            typedef indexed_term_traits::node_type node_type;
            auto& tset = *reinterpret_cast<indexed_term_set*>( 
                index_buf + offset );

            auto const next_obj_id = tset.next_idx;
            auto const obj_id = tset.insert(
                make_pair( vars, idxs.p ) );
            if( obj_id == next_obj_id ) {
                eval_block( cont, arg_vars, vars, eenv.index.data() );
            }
        }
        break;
        case instruction_tc::SINK:
        {
            size_t const file_descriptor = *p++;
            auto const idxs = read_array( p );
            bool first = true;
            for( size_t i = 0; i < idxs.size; ++i ) {
                if( !first ) cout << ',';
                cout << vars[idxs.p[i]];
                first = false;
            }
            cout << '\n';
        }
        break;
        case instruction_tc::INIT_ROOT:
        {
            size_t const size = *p++;
            eenv.index.alloc( size );
            index_buf = eenv.index.data();
            eenv.functions.push_back( &print_uint32 );
            eenv.functions.push_back( &print_string );
        }
        break;
        case instruction_tc::INIT_TERM_SET:
        {
            size_t const offset = *p++;
            auto const num_elements = *p++;
            new( eenv.index.data() + offset ) 
                indexed_term_set( indexed_term_traits{ num_elements } );
        }
        break;
        case instruction_tc::INIT_SOURCE:
        {
            auto name = read_string( p );
            eenv.sources.emplace_back( name.to_string() );
        }
        break;
        case instruction_tc::INIT_ARRAY_MAP:
        {
            size_t const offset = *p++;
            auto const element_size = *p++;
            new( index_buf + offset ) array_map{ element_size };
        }
        break;
        case instruction_tc::INIT_HASH_MAP:
        {
            size_t const offset = *p++;
            auto const num_elements = *p++;
            auto const element_size = *p++;
            new( index_buf + offset ) 
                indexed_hash_map( indexed_map_traits{ num_elements, element_size } );
        }
        break;
        case instruction_tc::INIT_VALUE:
        {
            size_t const offset = *p++;
            size_t const num = *p++;
            auto* const p = reinterpret_cast<tagged_object_t*>( index_buf + offset );
            for( size_t i = 0; i < num; ++i ) {
                p[i] = (tagged_object_t)-1;
            }
        }
        break;
        case instruction_tc::INIT_VALUE_LIST:
        {
            size_t const offset = *p++;
            new( index_buf + offset ) dynamic_vector;
        }
        break;
        case instruction_tc::INIT_HASH_SET:
        {
            size_t const offset = *p++;
            auto const num_elements = *p++;
            new( index_buf + offset ) 
                indexed_term_set( indexed_term_traits{ num_elements } );
        }
        break;
        case instruction_tc::PUT_STRING:
        {
            auto str = read_string( p );
            eenv.string_set.insert( str );
        }
        break;
        case instruction_tc::PUT_JOIN_INFO:
        {
            eenv.join_locations.push_back( {
                *p++, *p++, *p++, *p++, // rule location
                *p++, *p++, *p++, *p++, // literal location
                *p++, // join_no
                read_string( p ) // pidx name
            } );
        }
        break;
        case instruction_tc::RAISE_ASSERTION:
        {
            die();
        }
        break;
        case instruction_tc::DEBUG_JOIN_INFO:
        {
            join_info = *p++;
        }
        break;
        default:
            die();
        }
    }
    return invalid_object_idx;
}

auto eval(
    size_t const begin,
    tagged_object_t arg_vars[] )
    -> tagged_object_t
{
    tagged_object_t vars[max_vars];
    return eval_block( begin, arg_vars, vars, eenv.index.data() );
}

auto eval_program( char const* buf ) -> void
{
    tagged_object_t arg_vars[max_args];
    eenv.root = buf;
    eval( 0u, arg_vars );
}
