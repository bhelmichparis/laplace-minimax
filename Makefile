#-------------------------------------------------------------#
#
# tested with the following compilers:
#
#  -  GNU Fortran (MacPorts gcc5 5.4.0_0) 5.4.0
#
#  -  ifort (IFORT) 15.0.2 20150121
#
#  -  pgf95 10.9-0 64-bit target on x86-64 Linux -tp gh-64 
#
#-------------------------------------------------------------#

# printing of variables
$(info fortran compiler: $(FC))
$(info fortran flags given: $(FFLAGS))

# Fortran compiler flags
ifeq ($(FC),)
 $(error Set environment variable FC with Fortran compiler!)
else
 ifeq ($(FC),gfortran)
  FFLAGS+= -O3 -H -ffree-form -Wall
 else
  ifeq ($(FC),ifort)
   FFLAGS+= -O3 -free -warn all -assume protect_parens -nogen-interfaces
  else
   ifeq ($(FC),pgf95)
    FFLAGS+= -O4 -fast -fastsse -Mfree -Mlarge_arrays -Mcache_align -Msmart -Msmartalloc -Minform=inform
   else
    $(error Unknown Fortran compiler. Set environment variable FC to either: ifort, gfortran, or pgf95!)
   endif
  endif
 endif
endif

$(info fortran flags employed: $(FFLAGS))

IDIR=inc
ODIR=obj
SDIR=src
LDIR=lib
BDIR=bin

_DEPS = consts.h  init.h
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))

_OBJ = dd128_arithmetics.o dd128_linalg.o \
       lap_init.o lap_paraopt.o lap_maehly.o lap_rmsd.o laplace_minimax.o
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

$(ODIR)/%.o: $(SDIR)/%.F90  $(DEPS)
	$(FC) -c $(FFLAGS) -I./$(IDIR) -o $@ $<

$(LDIR)/liblaplace_minimax.a: $(OBJ) $(DEPS)
	ar rcs $@ $^

test_laplace: $(OBJ) $(DEPS)
	$(FC) $(FFLAGS) -I./$(IDIR) $(SDIR)/test_laplace.F90 -L./$(LDIR) -llaplace_minimax -o $(BDIR)/test_laplace

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o $(LDIR)/liblaplace_minimax.a $(BDIR)/test_laplace

.PHONY: all 

all:	$(LDIR)/liblaplace_minimax.a $(BDIR)/test_laplace

.PHONY: test

test:
	$(BDIR)/run_test.sh
