Compile static libraries `lib_src/*-body/` and `src`. Then merge the libraries in `pes/WHBB/libs/` using
```
ar -M < libWHBB.mri
```


Run example in `examples/pes/WHBB`

```
i-pi input.xml > ipi.out  &
sleep 5
i-pi-driver -u -h prism -m WHBB &
```

Using 5th order poly fit with `pcf-x6y3.dat`