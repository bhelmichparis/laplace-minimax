cmake_minimum_required(VERSION 2.8)

project(laplace Fortran)

# HOWTO compile the laplace library from the source code directory:
#
# $ mkdir build && cd build
# $ ccmake ..
# $ make -j4
#
# done! you may want to set the compile type to Debug or Release in the ccmake GUI
#
# in order to set the fortran/c/c++ compiler use in the build directory:
# $ FC=ifort CC=icc CXX=icpc ccmake   -DENABLE_DMRG=ON -DENABLE_64BIT_INTEGERS=ON/OFF ..
# $ FC=gfortran CC=gcc CXX=g++ ccmake -DENABLE_DMRG=ON -DENABLE_64BIT_INTEGERS=ON/OFF ..

option(ENABLE_AOMP2          "enable the atomic-orbital based PT2 implementation" ON)

set(CMAKE_MODULE_PATH
    ${CMAKE_MODULE_PATH}
    ${CMAKE_SOURCE_DIR}/cmake
   )

list(APPEND CMAKE_MODULE_PATH ${CMAKE_ROOT})

######################################################################
# Version information
######################################################################
set(LAPLACE_YEAR 2016)
set(LAPLACE_VERSION_MAJOR 0)
set(LAPLACE_VERSION_MINOR 1)
set(LAPLACE_VERSION_BUILD "")

if(EXISTS ${PROJECT_SOURCE_DIR}/.git)
    find_package(Git)
    if(GIT_FOUND)
        execute_process(
            COMMAND ${GIT_EXECUTABLE} rev-list --max-count=1 HEAD
            WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
            OUTPUT_VARIABLE GIT_REVISION
            ERROR_QUIET
            )
        if(NOT ${GIT_REVISION} STREQUAL "")
            string(STRIP ${GIT_REVISION} GIT_REVISION)
        endif()

        execute_process(
            COMMAND ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
            WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
            OUTPUT_VARIABLE GIT_BRANCH
            ERROR_QUIET
            )
        if(NOT ${GIT_BRANCH} STREQUAL "")
            string(STRIP ${GIT_BRANCH} GIT_BRANCH)
        endif()

        set(LAPLACE_VERSION_BUILD "${GIT_REVISION} (${GIT_BRANCH})")
    endif()
endif(EXISTS ${PROJECT_SOURCE_DIR}/.git)

set(LAPLACE_VERSION "${LAPLACE_VERSION_MAJOR}.${LAPLACE_VERSION_MINOR}")
if(LAPLACE_VERSION_BUILD)
  set(LAPLACE_GIT_VERSION "${LAPLACE_VERSION_BUILD}")
else(LAPLACE_VERSION_BUILD)
  set(LAPLACE_GIT_VERSION "")
endif(LAPLACE_VERSION_BUILD)
set(LAPLACE_VERSION_STRING "LAPLACE version ${LAPLACE_VERSION}")
MESSAGE(STATUS "LAPLACE version: ${LAPLACE_VERSION}")

######################################################################
# Configure and install version file
######################################################################

#configure_file(${PROJECT_SOURCE_DIR}/src/version.h.in ${PROJECT_SOURCE_DIR}/src/version.h)
#install(FILES ${PROJECT_SOURCE_DIR}/src/version.h
#        DESTINATION include COMPONENT headers)

######################################################################
# Search for optional/required packages
######################################################################

############
# optional #
############

##########
# OpenMP #
##########
#if(ENABLE_OPENMP)
#  find_package (OpenMP)
#  if (OPENMP_FOUND)
#    add_definitions(-D_OPENMP_)
#    add_definitions(${OpenMP_Fortran_FLAGS})
#  endif (OPENMP_FOUND)
#endif(ENABLE_OPENMP)

#####################
# compile main libs #
#####################
set(EP_PROJECT  "laplace-minimax")
set(EP_PROJ_DIR "${CMAKE_BINARY_DIR}/external/${EP_PROJECT}")

set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/${EP_PROJECT}")

set(EP_CMAKE_ARGS "${ExternalProjectCMakeArgs}"
                  "-DCMAKE_INSTALL_PREFIX:STRING=${CMAKE_BINARY_DIR}/${EP_PROJECT}"
                  "-DCMAKE_Fortran_COMPILER:STRING=${CMAKE_Fortran_COMPILER}"
                  "-DCMAKE_Fortran_FLAGS:STRING=${CMAKE_Fortran_FLAGS}"
                  "-DLIBDIR:STRING=${CMAKE_BINARY_DIR}/.."
                  "${EP_PROJ_DIR}"
   )
