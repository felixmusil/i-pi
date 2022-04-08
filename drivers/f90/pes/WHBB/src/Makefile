#Generic makefile for fortran90
LIBDIR := ../libs
MODDIR := ../mods

FC := gfortran
LIBS := -L$(LIBDIR) -lpes3b -lpes2b
FFLAGS := -O -fdefault-real-8 -fdefault-double-8 -J$(MODDIR)

#FC := ifort
#LIBS := -L$(LIBDIR) -lpes3b -lpes2b
#FFLAGS := -O -r8 -I$(MODDIR)

vpath %.f90 src

OBJ := \
constants.o\
math.o\
smear.o\
ttm3f_mod2.o\
mnasa_mod.o\
mnasa.o\
ttm3f_mb.o\
pot_monomer_mod.o\
pot_monomer.o\
pes3b.o\
pes_shell.o\
getpot.o

# Targets:
.PHONY : clean 

%.o : %.f90
	$(FC) -c $(FFLAGS) $< 
%.o : %.f
	$(FC) -c $(FFLAGS) $< 

getpot.x :  $(OBJ)
	$(FC) -o $@  $^ $(FFLAGS) $(LIBS)

clean :
	rm *.o *.mod 
