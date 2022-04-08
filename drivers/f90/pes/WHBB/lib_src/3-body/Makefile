FC = gfortran
FFLAGS = -O -fdefault-real-8 -fdefault-double-8 -ffree-line-length-0 -J./mods
#FC = ifort
#FFLAGS = -O -r8 -u -module ./mods

BLD = /usr/bin/ar -rcv

# Set objlibinv to the list of object files to be included in libinv.a.
objlib = inv_wp.o \
bg63_prims.o \
cg3_prims.o \
cg6_prims.o \
inv_mgx.o \
inv_mg63.o\
inv_cxx.o \
inv_cx63.o\
inv.o\
pes0.o \
pes_x6y3.o\
pes1c.o\
pes.o

#-----------------------------------------------------------------------
# Pattern rule to make %.o file
%.o : %.f90
	$(FC) -c $(FFLAGS) $<

# Here are the rules to make the libraries.
libpes3b.a : $(objlib)
	\rm -f $@
	$(BLD) $(@F) $(objlib)


# Targets 'clean' and 'realclean' clean up the directory.
clean :
	rm -f *.o *~ core junk
realclean : clean
	rm -f *.a *.mod *.exe

