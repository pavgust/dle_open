project(dle CXX)
cmake_minimum_required(VERSION 2.8)

if(NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE "RelWithDebInfo" 
		CACHE STRING "Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel."
		FORCE)
endif(NOT CMAKE_BUILD_TYPE)

include_directories(${INCLUDE})
link_directories(${LINK})

include_directories(src)

set(
	CMAKE_CXX_FLAGS
	"${CMAKE_CXX_FLAGS} -std=c++14 ${ADDRESS_MODEL} -ftemplate-depth=1024 -fno-rtti" )
set( 
	CMAKE_CXX_FLAGS_RELEASE 
	"-O3 -DNDEBUG -DNASSERT" )
set(
	CMAKE_CXX_FLAGS_RELWITHDEBINFO
	"-O2 -g" )

set( 
	CMAKE_CXX_FLAGS 
	"${CMAKE_CXX_FLAGS} -Wall -Wno-unused-local-typedefs -Wno-unused-variable \
	-Wno-narrowing -Wno-deprecated-declarations \
	-D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS" )

if( "${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" )
	set(
		CMAKE_CXX_FLAGS
		"${CMAKE_CXX_FLAGS} -Wno-unused-but-set-variable" )
endif( "${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" )
if( "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" )
	set(
		CMAKE_CXX_FLAGS
		"${CMAKE_CXX_FLAGS} -Qunused-arguments -Wno-tautological-compare -Wno-deprecated-register -Wno-unused-function" )
endif( "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" )

if( "${ADDRESS_MODEL}" STREQUAL "-m64" )
	set( ADDRESS_MODEL_SUFFIX "64" )
else()
	set( ADDRESS_MODEL_SUFFIX "32" )
endif()

message( STATUS "CMAKE_BUILD_TYPE: " ${CMAKE_BUILD_TYPE} )
message( STATUS "CMAKE_C_COMPILER: " ${CMAKE_C_COMPILER} )
message( STATUS "CMAKE_C_FLAGS: " ${CMAKE_C_FLAGS} )
message( STATUS "CMAKE_CXX_COMPILER: " ${CMAKE_CXX_COMPILER} )
message( STATUS "CMAKE_CXX_FLAGS: " ${CMAKE_CXX_FLAGS} )


set( Boost_USE_STATIC_LIBS ON )
find_package(
	Boost 1.65.1 
	REQUIRED COMPONENTS program_options system filesystem)
include_directories(${Boost_INCLUDE_DIRS})

find_package(BISON)
find_package(FLEX)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/gen)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

include_directories(${BOOST_ROOT}/include)
link_directories(${BOOST_ROOT}/lib)

################################################################################
# introspect
################################################################################
bison_target(
	inspectparser
	src/inspectparser.yy
	${CMAKE_CURRENT_BINARY_DIR}/gen/inspectparser.cc)
flex_target(
	inspectscanner 
	src/inspectscanner.ll 
	${CMAKE_CURRENT_BINARY_DIR}/gen/inspectscanner.cc 
	COMPILE_FLAGS -f)
add_flex_bison_dependency(
	inspectscanner 
	inspectparser
	)
add_custom_target(
	introspectfrontend DEPENDS 
		${CMAKE_CURRENT_BINARY_DIR}/gen/inspectparser.cc 
		${CMAKE_CURRENT_BINARY_DIR}/gen/inspectscanner.cc )

add_executable(
	introspect
	src/stdafx.cpp
	src/introspect.cpp )
target_link_libraries(
	introspect 
	${Boost_LIBRARIES} 
	stdc++ m pthread rt)

add_dependencies(
	introspect 
	introspectfrontend
	)

################################################################################
# factgen
################################################################################
add_custom_command(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/gen/riltypes.hh
	COMMAND introspect 
		src/riltypes.txt 
		${CMAKE_CURRENT_BINARY_DIR}/gen/riltypes.hh
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	DEPENDS introspect
	MAIN_DEPENDENCY src/riltypes.txt )

add_custom_target(
	rilheader DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/gen/riltypes.hh )

bison_target(
	rilparser 
	src/rilparser.yy 
	${CMAKE_CURRENT_BINARY_DIR}/gen/rilparser.cc
	)
flex_target(
	rilscanner 
	src/rilscanner.ll 
	${CMAKE_CURRENT_BINARY_DIR}/gen/rilscanner.cc 
	COMPILE_FLAGS -f
	)
add_flex_bison_dependency(
	rilscanner 
	rilparser
	)
add_custom_target(
	rilfrontend DEPENDS 
		${CMAKE_CURRENT_BINARY_DIR}/gen/rilparser.cc 
		${CMAKE_CURRENT_BINARY_DIR}/gen/rilscanner.cc
	)

add_executable(
	factgen 
	src/stdafx.cpp
	src/factgen.cpp
	src/rilread.cpp
	src/llvmread.cpp
	src/rilparser.cpp
	src/rilscanner.cpp
	src/utility.cpp
	)
target_link_libraries(
	factgen 
	${Boost_LIBRARIES} 
	LLVMLTO LLVMPasses LLVMObjCARCOpts LLVMSymbolize LLVMDebugInfoPDB 
	LLVMDebugInfoDWARF LLVMFuzzMutate LLVMTableGen LLVMDlltoolDriver 
	LLVMLineEditor LLVMOrcJIT LLVMCoverage LLVMMIRParser 
	LLVMObjectYAML LLVMLibDriver LLVMOption LLVMWindowsManifest 
	LLVMX86Disassembler LLVMX86AsmParser LLVMX86CodeGen LLVMGlobalISel LLVMSelectionDAG 
	LLVMAsmPrinter LLVMDebugInfoCodeView LLVMDebugInfoMSF LLVMX86Desc LLVMMCDisassembler 
	LLVMX86Info LLVMX86AsmPrinter LLVMX86Utils LLVMMCJIT LLVMInterpreter LLVMExecutionEngine 
	LLVMRuntimeDyld LLVMCodeGen LLVMTarget LLVMCoroutines LLVMipo LLVMInstrumentation 
	LLVMVectorize LLVMScalarOpts LLVMLinker LLVMIRReader LLVMAsmParser LLVMInstCombine 
	LLVMTransformUtils LLVMBitWriter LLVMAnalysis LLVMProfileData LLVMObject LLVMMCParser 
	LLVMMC LLVMBitReader LLVMCore LLVMBinaryFormat LLVMSupport LLVMDemangle
	tinfo dl
	stdc++ m pthread rt 
	)
add_dependencies(
	factgen
	rilfrontend
	rilheader
	)

################################################################################
# dle
################################################################################
add_custom_command(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/gen/datatypes.hh
	COMMAND introspect 
		src/datatypes.txt 
		${CMAKE_CURRENT_BINARY_DIR}/gen/datatypes.hh
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	DEPENDS introspect
	MAIN_DEPENDENCY src/datatypes.txt )

add_custom_target(
	dleheader DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/gen/datatypes.hh )

add_custom_command(
	OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/header${ADDRESS_MODEL_SUFFIX}.bc
	COMMAND clang++ -I${Boost_INCLUDE_DIRS} ${ADDRESS_MODEL} 
		-fno-strict-aliasing -std=c++14 -c -emit-llvm -O2 -fno-use-cxa-atexit
		-o ${CMAKE_CURRENT_SOURCE_DIR}/header${ADDRESS_MODEL_SUFFIX}.bc
		src/header.cpp
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	MAIN_DEPENDENCY src/header.cpp )

add_custom_target(
	dlebc DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/header${ADDRESS_MODEL_SUFFIX}.bc )

bison_target(
	dleparser 
	src/dleparser.yy 
	${CMAKE_CURRENT_BINARY_DIR}/gen/dleparser.cc)
flex_target(
	dlescanner 
	src/dlescanner.ll 
	${CMAKE_CURRENT_BINARY_DIR}/gen/dlescanner.cc 
	COMPILE_FLAGS -f)
add_flex_bison_dependency(
	dlescanner 
	dleparser)
add_custom_target(
	dlefrontend DEPENDS 
		${CMAKE_CURRENT_BINARY_DIR}/gen/dleparser.cc 
		${CMAKE_CURRENT_BINARY_DIR}/gen/dlescanner.cc )

add_executable(
	${PROJECT_NAME} 
	src/stdafx.cpp
	src/main.cpp
	src/codegen2.cpp
	src/table.cpp
	src/compile.cpp
	src/type.cpp
	src/dleinst.cpp
	src/dleparser.cpp
	src/dlescanner.cpp
	src/utility.cpp)
target_link_libraries(
	${PROJECT_NAME} 
	LLVMLTO LLVMPasses LLVMObjCARCOpts LLVMSymbolize LLVMDebugInfoPDB 
	LLVMDebugInfoDWARF LLVMFuzzMutate LLVMTableGen LLVMDlltoolDriver 
	LLVMLineEditor LLVMOrcJIT LLVMCoverage LLVMMIRParser 
	LLVMObjectYAML LLVMLibDriver LLVMOption LLVMWindowsManifest 
	LLVMX86Disassembler LLVMX86AsmParser LLVMX86CodeGen LLVMGlobalISel LLVMSelectionDAG 
	LLVMAsmPrinter LLVMDebugInfoCodeView LLVMDebugInfoMSF LLVMX86Desc LLVMMCDisassembler 
	LLVMX86Info LLVMX86AsmPrinter LLVMX86Utils LLVMMCJIT LLVMInterpreter LLVMExecutionEngine 
	LLVMRuntimeDyld LLVMCodeGen LLVMTarget LLVMCoroutines LLVMipo LLVMInstrumentation 
	LLVMVectorize LLVMScalarOpts LLVMLinker LLVMIRReader LLVMAsmParser LLVMInstCombine 
	LLVMTransformUtils LLVMBitWriter LLVMAnalysis LLVMProfileData LLVMObject LLVMMCParser 
	LLVMMC LLVMBitReader LLVMCore LLVMBinaryFormat LLVMSupport LLVMDemangle
	${Boost_LIBRARIES} 
	z tinfo dl m pthread rt stdc++ )
add_dependencies(
	${PROJECT_NAME} 
	dlefrontend
	dleheader
	dlebc
)

