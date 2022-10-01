FCOMP = gfortran
FCLAGS = -O -fdefault-real-8 -fdefault-double-8
BLD = /usr/bin/ar -rcv

SRCS = inv_share.f90 inv_mg42_t.f90 getr0.f90  getrvec.f90  getvec.f90  getx0.f90  h4o2.pes4.f90
OBJECTS = $(SRCS:.f90=.o)

LIB = libpes2b.a
LIBDIR = ../../../../lib/watlib
#-----------------------------------------------------------------------
# Rules.

all : $(OBJECTS) $(LIB) $(LIBDIR)/$(LIB)

.PHONY : all clean
.SUFFIXES : $(nil)

# Pattern rule to make a %.o file from a %.f90 file.
%.o: %.f90
	$(FCOMP) $(FCFLAGS) -c $<

# Here are the rules to make the libraries.
$(LIB) : $(OBJECTS)
	$(BLD) $(@F) $(OBJECTS)
	mv $(@F) $(LIBDIR)/$(LIB)

clean :
	rm -f *.o *.mod $(LIBDIR)/$(LIB)

