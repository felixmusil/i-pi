# Makefile for the driver tests
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

.PHONY: all modules clean

LIBDIR := q_aqua/lib/pes_lib
LIBDIR2 := q_aqua/lib/watlib
MODDIR := q_aqua/lib/pes_lib/mod
MODDIR2 := q_aqua/lib/watlib/mod3b

FC := gfortran
LIBS :=   -L$(LIBDIR) -L$(LIBDIR2) -lpes3b -lpes2b -ldms2b -lpes -lpx
FFLAGS := -g -O3 -Wall -ffree-line-length-none -ffixed-line-length-none  -fdefault-real-8 -fdefault-double-8  -I$(MODDIR) -I$(MODDIR2)

MODULES=distance.f90 LJ.f90 SG.f90 pes/pswater.f90 pes/LEPS.f90 LJPolymer.f90 pes/efield.f90 pes/eckart.f90 pes/doublewell.f90 pes/doublewell_1D.f90 pes/MB.f90
PES=pes/morse.f pes/qtip4pf.f pes/utility.f pes/ch52008.f

PWATERDIR = q_aqua/src/potential
PWATER := \
$(PWATERDIR)/constants.o\
$(PWATERDIR)/math.o  $(PWATERDIR)/smear.o  $(PWATERDIR)/ttm3f_mod2.o  $(PWATERDIR)/mnasa_mod.o  $(PWATERDIR)/mnasa.o  $(PWATERDIR)/ttm3f_mb.o \
$(PWATERDIR)/pot_monomer_mod.o  $(PWATERDIR)/pot_monomer.o  $(PWATERDIR)/pes3b.o  $(PWATERDIR)/shell_w.o  \
$(PWATERDIR)/chg.o $(PWATERDIR)/ltp2011_dms.o $(PWATERDIR)/pes_h.o $(PWATERDIR)/dms_h.o $(PWATERDIR)/shell_h.o  \
$(PWATERDIR)/h5o2.pes4B-dms4B.o  $(PWATERDIR)/shell_hw.o  \
$(PWATERDIR)/bemsa.o $(PWATERDIR)/shell_hww.o\
$(PWATERDIR)/shell_hwww.o\
$(PWATERDIR)/pes_shell_wt.o \

OBJECTS=$(MODULES:%.f90=%.o) $(PES:%.f=%.o) $(PWATER:%.f90=%.o)

CC=gcc
all: qaqua2b qaqua3b qaquapes driver.x #driver_pure.x

sockets.o: sockets.c
	$(CC) $(CFLAGS) -c -o sockets.o sockets.c

driver.x: $(OBJECTS) sockets.o fsockets.o driver.o | $(OBJECTS)
	$(FC) -o $@  $^ $(FFLAGS) $(LIBS)
	ln -fs ../drivers/f90/driver.x ../../bin/i-pi-driver

driver_pure.x: $(OBJECTS) fsockets_pure.o driver.o | $(OBJECTS)
	$(FC) $(FFLAGS) -o driver_pure.x $^

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

%.o: %.f
	$(FC) $(FFLAGS) -c $< -o $@

qaqua2b:
	$(MAKE) -C q_aqua/src/water_lib/2-body/pes
	$(MAKE) -C q_aqua/src/water_lib/2-body/dms
qaqua3b:
	$(MAKE) -C q_aqua/src/water_lib/3-body
qaquainv:
	$(MAKE) -C q_aqua/src/pes_lib/inv
qaquapes: qaquainv
	$(MAKE) -C q_aqua/src/pes_lib/pes-xyz


clean:
	$(MAKE) -C q_aqua/src/water_lib/2-body/pes clean
	$(MAKE) -C q_aqua/src/water_lib/2-body/dms clean
	$(MAKE) -C q_aqua/src/water_lib/3-body clean
	$(MAKE) -C q_aqua/src/pes_lib/inv clean
	$(MAKE) -C q_aqua/src/pes_lib/pes-xyz clean
	rm -f *.o *.mod *.x */*.mod */*.o ../../bin/i-pi-driver
