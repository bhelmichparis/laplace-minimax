FC=gfortran
FFLAGS= -O3 -H -ffree-form -Wall

IDIR=inc
ODIR=obj
SDIR=src
LDIR=lib

_DEPS = consts.h  init.h
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))

_OBJ = dd128_arithmetics.o dd128_linalg.o \
       lap_init.o lap_paraopt.o lap_maehly.o lap_rmsd.o laplace_minimax.o
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

_OBJT = test_laplace.o
OBJT = $(patsubst %,$(ODIR)/%,$(_OBJT))

$(ODIR)/%.o: $(SDIR)/%.F90  $(DEPS)
	$(FC) -c -o $@ $< $(FFLAGS) -I./$(IDIR)

$(LDIR)/liblaplace_minimax.a: $(OBJ)
	ar rcs $@ $^

test_laplace: $(OBJ) $(OBJT) $(DEPS)
	$(FC) -o test_laplace $(FFLAGS) -I./$(IDIR) -L./$(LDIR) -llaplace_minimax $(OBJT)

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o $(LDIR)/liblaplace_minimax.a test_laplace
