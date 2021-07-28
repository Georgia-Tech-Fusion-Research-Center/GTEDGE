# ======================================================================
# Declarations
# ======================================================================
# Based on: https://stackoverflow.com/questions/35260489

# The compiler
FCOMP = gfortran

# flags for debugging or for maximum performance, comment as necessary
FLFLAGS = 
FCFLAGS = -c -nostartfiles -ffixed-line-length-none -Wall -Wextra -fcheck=all -fmax-errors=5

# libraries needed for linking 
LDFLAGS = -lblas

# List of executables to be built within the package
PROGRAM = gtedge
PRG_OBJ = $(PROGRAM).o

# List of subroutines to be built within the package
SRCS=$(wildcard *.for)
OBJS=$(patsubst %.for,%.o,$(SRCS))

# "make" builds all
default : $(PROGRAM)


# ======================================================================
# General Rules
# ======================================================================

# General rules for building prog.o from prog.f90 or prog.F90; $< is
# used in order to list only the first prerequisite (the source file)
# and not the additional prerequisites such as module or include files


# %o: %.for
# 	$(FCOMP) $(FCFLAGS) $<

%.o: %.for
	$(FCOMP) $(FCFLAGS) $<

# General rule for building prog from prog.o; $^ (GNU extension) is
# used in order to list additional object files on which the
# executable depends
# Linker
$(PROGRAM) : $(OBJS)
	$(FCOMP) $(FLFLAGS) -o $@ $^ $(LDFLAGS) 


# If something doesn't work right, have a 'make debug' to 
# show what each variable contains.
debug:
	@echo "SRCS = $(SRCS)"
	@echo "OBJS = $(OBJS)"
	@echo "MODS = $(MODS)"
	@echo "MOD_OBJS = $(MOD_OBJS)"
	@echo "PROGRAM = $(PROGRAM)"
	@echo "PRG_OBJ = $(PRG_OBJ)"

# Utility targets
.PHONY: debug default clean veryclean

clean:
	rm -f $(OBJS) $(PROGRAM)

veryclean: clean
	rm -f *~ $(PROGRAMS)
