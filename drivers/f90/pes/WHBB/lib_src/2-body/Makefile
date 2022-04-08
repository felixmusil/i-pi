FC = gfortran
FFLAGS = -O -fdefault-real-8 -fdefault-double-8 -ffree-line-length-0 -J../../mods
#FC = ifort
#FFLAGS = -O -r8 -u -module ./mods

BLD = /usr/bin/ar -rcv

# Set objlibinv to the list of object files to be included in libinv.a.
objlib = getr0.o \
getrvec.o \
getvec.o \
getx0.o \
h4o2.pes4.o \
inv_mg42_t.o \
inv_share.o

#-----------------------------------------------------------------------
# Pattern rule to make %.o file
%.o : %.f90
	$(FC) -c $(FFLAGS) $<

# Here are the rules to make the libraries.
libpes3b.a : $(objlib)
	$(BLD) libpes2b.a $(objlib)


# Targets 'clean' and 'realclean' clean up the directory.
clean :
	rm -f *.a *.o *.mod *~ core junk