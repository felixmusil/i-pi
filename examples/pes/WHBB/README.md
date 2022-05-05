# to compile the WHBB potential

go-to `driver/f90/pes/WHBB/`.
Compile 3-body library `lib_src/3-body/` and `src` using `make`. Then merge the libraries in `libs/` using
```
ar -M < libWHBB.mri
```
# to compile the dipole moment for WHBB

go-to `driver/f90/pes/water_dms/DMS` and run make.


# Then compile `driver.f90`

# Run example in `examples/pes/WHBB`

```
i-pi input.xml > ipi.out  &
sleep 5
i-pi-driver -u -h prism -m WHBB &
```

Using 5th order poly fit for 3-body with `pcf-x6y3.dat`, the 2-body are in `coef.pes2b.dat` and the dipole moment 2-body fits are in `h4o2.dms2b.coeff.dat`.

Cite:

Dipole moments
H. Liu, Y. Wang and J. M. Bowman, J. Phys. Chem. B. 120(8),1735-1742,2016
H. Liu, Y. Wang and J. M. Bowman, J. Chem. Phys. 142,194502,2015


J. Chem. Phys. 134, 094509 (2011); https://doi.org/10.1063/1.3554905
https://doi.org/10.1063/1.4967719