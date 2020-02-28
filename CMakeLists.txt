cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

# project name
project(laplace-minimax Fortran)
enable_testing()

# special compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS
      "${CMAKE_Fortran_FLAGS} -O3 -H -ffree-form -Wall"
     )
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
  set(CMAKE_Fortran_FLAGS
      "${CMAKE_Fortran_FLAGS}  -O3 -free -warn all -assume protect_parens -nogen-interfaces"
     )
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES PGI)
  set(CMAKE_Fortran_FLAGS
      "${CMAKE_Fortran_FLAGS}  -O4 -fast -fastsse -Mfree -Mlarge_arrays -Mcache_align -Msmart -Msmartalloc -Minform=inform"
     )
endif()

# Header files
include_directories(    ${PROJECT_SOURCE_DIR}/inc 
    )

# source files
set(SOURCES
    ${PROJECT_SOURCE_DIR}/inc/consts.h
    ${PROJECT_SOURCE_DIR}/inc/laplace_minimax.h
    ${PROJECT_SOURCE_DIR}/src/dd128_arithmetics.F90
    ${PROJECT_SOURCE_DIR}/src/dd128_linalg.F90
    ${PROJECT_SOURCE_DIR}/src/lap_rderror.F90
    ${PROJECT_SOURCE_DIR}/src/lap_rddata.F90
    ${PROJECT_SOURCE_DIR}/src/lap_paraopt.F90 
    ${PROJECT_SOURCE_DIR}/src/lap_maehly.F90
    ${PROJECT_SOURCE_DIR}/src/lap_rmsd.F90
    ${PROJECT_SOURCE_DIR}/src/lap_numlap.F90
    ${PROJECT_SOURCE_DIR}/src/laplace_minimax.F90
)

# test source files
set(TEST_SOURCES
    ${PROJECT_SOURCE_DIR}/src/test_laplace.F90
)

# data files
set(DATA
    ${PROJECT_SOURCE_DIR}/data/init_error.txt
    ${PROJECT_SOURCE_DIR}/data/init_para.txt
)

# creates test suite
add_executable(test_laplace ${TEST_SOURCES} ${SOURCES})

# the static library
add_library(laplace-minimax STATIC ${SOURCES})

# take care of test
find_program (BASH_PROGRAM bash)

if (BASH_PROGRAM)
  add_test (test_laplace-minimax ${BASH_PROGRAM} ${PROJECT_SOURCE_DIR}/bin/run_test.sh)
endif (BASH_PROGRAM)

# Install static library
install(TARGETS laplace-minimax ARCHIVE DESTINATION lib)
        
# Install header (fortran interface)
install(FILES ${PROJECT_SOURCE_DIR}/inc/laplace_minimax.h 
        DESTINATION include/laplace-minimax)

# copy data files
install(FILES ${DATA} DESTINATION data)

# .. if this has any effect?
set(ENV{LAPLACE_ROOT} ${CMAKE_INSTALL_PREFIX})

