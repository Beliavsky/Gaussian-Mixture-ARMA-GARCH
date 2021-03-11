# Source files list
SRCS = $(wildcard *.f90)
OBJS = $(patsubst %.f90,%.o,$(SRCS))

# Modules list
MODS = $(wildcard mod*.f90)
MOD_OBJS = $(patsubst %.f90,%.o,$(MODS))

# sub-folder modules
SUBF = ./datetime-fortran/src/
SUBMOD = datetime_module.o


# Compiler/Linker settings
FC = gfortran
FLFLAGS = -g
FCFLAGS = -g -c -Wall -Wno-tabs -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5
PROGRAM = EM_core
PRG_OBJ = $(PROGRAM).o

# Make without parameters will make first target found
default : $(PROGRAM).exe

# Compiler step for all objects
$(OBJS) : %.o : %.f90
	$(FC) $(FCFLAGS) -o $@ $< -I$(SUBF)

# Linker
$(PROGRAM).exe : $(OBJS)
	$(FC) $(FLFLAGS) -o $@ $^ $(SUBF)$(SUBMOD)

# Debug to show the value of each variable
debug:
	@echo "SRCS = $(SRCS)"
	@echo "OBJS = $(OBJS)"
	@echo "MODS = $(MODS)"
	@echo "MOD_OBJS = $(MOD_OBJS)"
	@echo "PROGRAM = $(PROGRAM)"
	@echo "PRG_OBJ = $(PRG_OBJ)"

clean:
	rm -rf $(OBJS) $(PROGRAM).exe $(patsubst %.o,%.mod,$(MOD_OBJS))

.PHONY: debug default clean



#--DEPENDENCIES--#

# Main program depends on all modules
$(PRG_OBJ) : $(MOD_OBJS)

# IO depends on DATETIME
#mod_IO.o : ../datetime-fortran/src/datetime_module.o


