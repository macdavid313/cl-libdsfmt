;;;; grovel.lisp
(in-package #:dsfmt)

(cc-flags "-I../dSFMT" "-DNDEBUG" "-DDSFMT_MEXP=19937" "-DDSFMT_DO_NOT_USE_OLD_NAMES" "-O3"
          "-finline-functions" "-fomit-frame-pointer" "-fno-strict-aliasing" "--param"
          "max-inline-insns-single=1800" "-Wmissing-prototypes" "-Wall"
          "-std=c99" "-msse2" "-DHAVE_SSE2")

(include "dSFMT.h")

(constant (+DSFMT-MEXP+ "DSFMT_MEXP")
          :type integer
          :documentation "Mersenne Exponent. The period of the sequence is a multiple of 2^DSFMT_MEXP-1.")
(constant (+DSFMT-N+ "DSFMT_N")
          :type integer
          :documentation "DSFMT generator has an internal state array of 128-bit integers, and N is its size.")
(constant (+DSFMT-N32+ "DSFMT_N32")
          :type integer
          :documentation "N32 is the size of internal state array when regarded as an array of 32-bit integers.")
(constant (+DSFMT-N64+ "DSFMT_N64")
          :type integer
          :documentation "N64 is the size of internal state array when regarded as an array of 64-bit integers.")

