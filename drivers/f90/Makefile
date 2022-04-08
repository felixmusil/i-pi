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

WHBBDIR := pes/WHBB
WHBBLIBDIR := $(WHBBDIR)/libs
WHBBMODDIR := $(WHBBDIR)/mods
WHBBSRCDIR := $(WHBBDIR)/src
# WHBBLIBBS := -L$(WHBBLIBDIR) -lpes3b -lpes2b -lwhbb
WHBBLIBBS := -L$(WHBBLIBDIR) -lWHBB
WHBBFALGS := -I$(WHBBMODDIR)
OBJ := \
$(WHBBSRCDIR)/constants.o\
$(WHBBSRCDIR)/math.o\
$(WHBBSRCDIR)/smear.o\
$(WHBBSRCDIR)/ttm3f_mod2.o\
$(WHBBSRCDIR)/mnasa_mod.o\
$(WHBBSRCDIR)/mnasa.o\
$(WHBBSRCDIR)/ttm3f_mb.o\
$(WHBBSRCDIR)/pot_monomer_mod.o\
$(WHBBSRCDIR)/pot_monomer.o\
$(WHBBSRCDIR)/pes3b.o\
$(WHBBSRCDIR)/pes_shell.o


FLAGS=-g -O3 -Wall
CFLAGS=$(FLAGS)
FFLAGS=$(FLAGS) -ffree-line-length-none -ffixed-line-length-none -Wno-maybe-uninitialized
#FFLAGS=-g -O0 -fcheck=all -Wall -pedantic -ffree-line-length-none

MODULES=distance.f90 LJ.f90 SG.f90 pes/pswater.f90 pes/LEPS.f90 LJPolymer.f90 pes/efield.f90 pes/eckart.f90 pes/doublewell.f90 pes/doublewell_1D.f90 pes/MB.f90
PES=pes/zundel.f pes/morse.f pes/qtip4pf.f pes/utility.f pes/ch52008.f

OBJECTS=$(MODULES:%.f90=%.o) $(PES:%.f=%.o) # $(OBJ)
FC=gfortran
CC=gcc
all: driver.x #driver_pure.x

sockets.o: sockets.c
	$(CC) $(CFLAGS) -c -o sockets.o sockets.c

driver.x: $(OBJECTS) sockets.o fsockets.o driver.o | $(OBJECTS)
	$(FC) $(FFLAGS) $(WHBBFALGS) -o driver.x $^ $(WHBBLIBBS)
	ln -fs ../drivers/f90/driver.x ../../bin/i-pi-driver

driver_pure.x: $(OBJECTS) fsockets_pure.o driver.o | $(OBJECTS)
	$(FC) $(FFLAGS) -o driver_pure.x $^

%.o: %.f90
	$(FC) $(FFLAGS) $(WHBBFALGS) -c $< -o $@

%.o: %.f
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f *.o *.mod *.x */*.mod */*.o ../../bin/i-pi-driver
