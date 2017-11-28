# cl-libdsfmt

Common Lisp binding for [dSFMT](http://www.math.sci.hiroshima-u.ac.jp/%7Em-mat/MT/SFMT/), a double precision SIMD oriented Fast Mersenne Twister pseudorandom number generator. For the license information, please check out the dSFMT [original license file](https://github.com/macdavid313/cl-libdsfmt/blob/master/dSFMT/LICENSE.txt).

It has been tested on a Linux x86_64 machine, SBCL 1.4.1 and Allegro CL SMP 10.1. We believe it should work fine on MacOS (or even Windows, but pathes needed) and other Lisps, however, I haven't tested yet.

## Usage

It depends on `cffi`, `static-vectors` and `trivial-garbage`. Simply clont it into your `local-projects` path and load it by `ql:quickload`.

```common-lisp
CL-USER> (ql:quickload :cl-libdsfmt)
To load "cl-libdsfmt":
  Load 1 ASDF system:
    cl-libdsfmt
; Loading "cl-libdsfmt"
[package cl-libdsfmt].
The shared library has been successfully compiled and saved to /home/xxx/quicklisp/local-projects/cl-libdsfmt/dSFMT/libdSFMT.so.
The shared libaray libdSFMT has been loaded into Lisp.
.... blah blah blah
```

For a quick glance, one can start generating random _double-float_ numbers by:

```common-lisp
CL-USER> (dsfmt:init-by-seed 0)
; No values
CL-USER> (dsfmt:random-close-open)
0.030581026769374464d0
CL-USER> (dsfmt:fill-array-close-open 10)
#(0.8180370846254899d0 0.9540839246841912d0 0.5031368175742139d0
  0.1661747935703064d0 0.9058518983505672d0 0.8616115954487864d0
  0.9120735512093359d0 0.31153966727757365d0 0.3481385179479686d0
  0.8352162231155917d0)
```

## High Level APIs

From the package `#:dsfmt`, several high-level functions were implemenetd, while various low-level constructs are also provided.

### `*dsfmt*`

The pointer to the global state vector from dSFMT. Considered only useful when you want to use low-level constructs.

### *dsfmt-initialized-p* 

A predicate to decide if `*dsfmt*` has been initialized.

### init-by-seed

This function initializes the internal state array with a 32-bit unsigned integer seed. This function implicitly uses the global variable `*dsfmt*` internally.

### init-by-array

This function initializes the internal state array, with an array of 32-bit integers used as the seeds. This function implicitly uses global variables `*dsfmt*` internally.

### random-uint32

This function generates and returns unsigned 32-bit integer. This is slower than SFMT, only for convenience usage.

### random-close1-open2

This function generates and returns double precision pseudorandom number which distributes uniformly in the range [1, 2). `init-by-seed` or `init-by-array` must be called before this function. This function implicitly uses the global variable `*dsfmt*` internally.

### random-close-open

Just like `random-close1-open2`, but on the range [0, 1).

### random-open-close

Just like `random-close1-open2`, but on the range (0, 1].

### random-open-open

Just like `random-close1-open2`, but on the range (0, 1).

### fill-array-close1-open2

This function generates double precision floating point pseudorandom numbers which are distributed in the range [1, 2) to an (static yet lisp) array. `init-by-seed` or `init-by-array` must be called before this function. This function implicitly uses the global variable `*dsfmt*` internally.

### fill-array-close-open

Just like `fill-array-clos1-open2`, but on the range [0, 1).

### fill-array-open-close

Just like `fill-array-clos1-open2`, but on the range (0, 1].

### fill-array-open-open

Just like `fill-array-clos1-open2`, but on the range (0, 1).

## Author

* David Gu (macdavid313@gmail.com)

## Copyright

Copyright (c) 2017 David Gu (macdavid313@gmail.com)
