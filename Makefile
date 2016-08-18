#-------------------------------------------------------------#
#
# tested with the following compilers:
#
#  -  GNU Fortran (MacPorts gcc5 5.4.0_0) 5.4.0
#
#  -  ifort (IFORT) 15.0.2 20150121
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
   $(error Unknown Fortran compiler. User either ifort or gfortran!)
  endif
 endif
endif

$(info fortran flags employed: $(FFLAGS))

IDIR=inc
ODIR=obj
SDIR=src
LDIR=lib

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
	$(FC) $(FFLAGS) -I./$(IDIR) $(SDIR)/test_laplace.F90 -L./$(LDIR) -llaplace_minimax -o test_laplace

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o $(LDIR)/liblaplace_minimax.a test_laplace

.PHONY: all 

all:	$(LDIR)/liblaplace_minimax.a test_laplace
