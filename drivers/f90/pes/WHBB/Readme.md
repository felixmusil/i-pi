# To compile the WHBB

Compile 3-body library `lib_src/3-body/` and `src` using `make`. Then merge the libraries in `libs/` using
```
ar -M < libWHBB.mri
```