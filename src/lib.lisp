;;;; lib.lisp
(in-package #:dsfmt)

(declaim (inline dsfmt-gen-rand-all get-idstring get-min-array-size
                 dsfmt-fill-array-close1-open2 dsfmt-fill-array-close-open
                 dsfmt-fill-array-open-close dsfmt-fill-array-open-open
                 dsfmt-chk-init-gen-rand dsfmt-chk-init-by-array))

(eval-when (:load-toplevel)
  (defvar *cc-flags*
    '("-DNDEBUG" "-DDSFMT_MEXP=19937" "-DDSFMT_DO_NOT_USE_OLD_NAMES" "-O3"
      "-finline-functions" "-fomit-frame-pointer" "-fno-strict-aliasing"
      "--param" "max-inline-insns-single=1800" "-Wmissing-prototypes" "-Wall"
      "-std=c99" "-msse2" "-DHAVE_SSE2" "-m64"))
  
  (let* ((cc (or (uiop:getenv "CC") "cc"))
         (root-path (asdf:system-source-directory 'cl-libdsfmt))
         (dsfmt-path (merge-pathnames "dSFMT/" root-path))
         (c-file (merge-pathnames "dSFMT.c" dsfmt-path))
         (lib-file (merge-pathnames #+linux "libdSFMT.so"
                                    #+darwin "libdSFMT.dylib"
                                    dsfmt-path))
         (cmd (append (list cc "-o" (namestring lib-file))
                      *cc-flags* '("-shared" "-fPIC")
                      (list (namestring c-file)))))
    (unless (probe-file lib-file)
      ;; compile shared library
      (uiop:run-program cmd :output :interactive :error-output :interactive)
      (format t "~&The shared library has been successfully compiled and saved to ~A.~%" lib-file))
    ;; add '/dSFMT' to system's search path
    (pushnew dsfmt-path *foreign-library-directories* :test 'equal)
    (load-foreign-library '(:default "libdSFMT"))
    (format t "~&The shared libaray libdSFMT has been loaded into Lisp.~%"))
  ) ;; end of eval-when

(defcfun "dsfmt_gen_rand_all" :void
  "This function fills the internal state array with double precision floating point pseudorandom numbers of the IEEE 754 format."
  (dsfmt :pointer))

(defcfun "dsfmt_fill_array_close1_open2" :void
  "This function generates double precision floating point pseudorandom numbers which distribute in the range [1, 2) to the specified array by one call. The number of pseudorandom numbers is specified by the argument size, which must be at least (+SFMT-MEXP+ / 128) * 2 and a multiple of two. The function get-min-array-size returns this minimum size. The generation by this function is much faster than the following fill-array-xxx functions.

For initialization, dsfmt-init-gen-rand or dsfmt-init-by-array must be called before the first call of this function. This function can not be used after calling genrand-xxx functions, without initialization."
  (dsfmt :pointer) (array :pointer) (size :int))

(defcfun "dsfmt_fill_array_open_close" :void
  "This function generates double precision floating point pseudorandom numbers which distribute in the range (0, 1] to the specified array by one call. This function is the same as fill-array-close1-open2 except the distribution range."
  (dsfmt :pointer) (array :pointer) (size :int))

(defcfun "dsfmt_fill_array_close_open" :void
  "This function generates double precision floating point pseudorandom numbers which distribute in the range [0, 1) to the specified array by one call. This function is the same as fill-array-close1-open2 except the distribution range."
  (dsfmt :pointer) (array :pointer) (size :int))

(defcfun "dsfmt_fill_array_open_open" :void
  "This function generates double precision floating point pseudorandom numbers which distribute in the range (0, 1) to the specified array by one call. This function is the same as fill-array-close1-open2 except the distribution range."
  (dsfmt :pointer) (array :pointer) (size :int))

(defcfun "dsfmt_chk_init_gen_rand" :void
  "This function initializes the internal state array with a 32-bit integer seed."
  (dsfmt :pointer) (seed :unsigned-int) (mexp :int))

(defcfun "dsfmt_chk_init_by_array" :void
  "This function initializes the internal state array, with an array of 32-bit integers used as the seeds."
  (dsfmt :pointer) (init-key :pointer) (key-length :int) (mexp :int))

(defcfun ("dsfmt_get_idstring" get-idstring) :string
  "This function returns the identification string. The string shows the Mersenne exponent, and all parameters of this generator.")

(eval-when (:compile-toplevel :load-toplevel)
  (defcfun ("dsfmt_get_min_array_size" get-min-array-size) :int
    "This function returns the minimum size of array used for 'fill-array' functions.")
  ) ;; end of eval-when
