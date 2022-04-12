#Generic makefile for fortran90

LIBDIR := ../lib
MODDIR := ../mods

FC := gfortran
# LIBS := -static -L$(LIBDIR) -ldms2b
LIBS :=
FFLAGS := -O2 -fdefault-real-8 -fdefault-double-8 -J$(MODDIR)

BLD := /usr/bin/ar -rcv

vpath %.f90 src

OBJ := \
ltp2011_dms.o \
chg.o \
dms_2b.o \
dms_shell.o\
getdms.o\

# Targets:
.PHONY : clean

%.o : %.f90
	$(FC) -c $(FFLAGS) $<
%.o : %.f
	$(FC) -c $(FFLAGS) $<

all : libdms.a

libdms.a : $(OBJ)
	$(BLD) $(@F) $(OBJ)
	mv $(@F) $(LIBDIR)

# to run the command line example
getdms.x :  $(OBJ)
	$(FC) -o $@  $^ $(FFLAGS) $(LIBS)

clean :
	rm *.o *.mod
