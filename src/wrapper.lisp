;;;; wrapper.lisp
(in-package #:dsfmt)

(flag "-I../dSFMT" "-DNDEBUG" "-DDSFMT_MEXP=19937" "-DDSFMT_DO_NOT_USE_OLD_NAMES" "-O3"
      "-finline-functions" "-fomit-frame-pointer" "-fno-strict-aliasing" "--param"
      "max-inline-insns-single=1800" "-Wmissing-prototypes" "-Wall"
      "-std=c99" "-msse2" "-DHAVE_SSE2")

(include "dSFMT.h")

(declaim (inline dsfmt-get-global-data dsfmt-genrand-uint32
                 dsfmt-genrand-close1-open2 dsfmt-genrand-close-open
                 dsfmt-genrand-open-close dsfmt-genrand-open-open
                 dsfmt-gv-genrand-uint32 dsfmt-gv-genrand-close1-open2
                 dsfmt-gv-genrand-close-open dsfmt-gv-genrand-open-close
                 dsfmt-gv-genrand-open-open dsfmt-gv-fill-array-close1-open2
                 dsfmt-gv-fill-array-close-open dsfmt-gv-fill-array-open-close
                 dsfmt-gv-fill-array-open-open dsfmt-gv-init-gen-rand
                 dsfmt-gv-init-by-array dsfmt-init-by-array dsfmt-init-gen-rand))                 

(c "
static inline dsfmt_t* dsfmt_get_global_data(){
  return &dsfmt_global_data;
}")
                 
(defwrapper "dsfmt_get_global_data" :pointer)

(defwrapper "dsfmt_genrand_uint32" ("uint32_t" :unsigned-int)
  (dsfmt ("dsfmt_t*" :pointer)))

(defwrapper "dsfmt_genrand_close1_open2" :double
  (dsfmt ("dsfmt_t*" :pointer)))

(defwrapper "dsfmt_genrand_close_open" :double
  (dsfmt ("dsfmt_t*" :pointer)))

(defwrapper "dsfmt_genrand_open_close" :double
  (dsfmt ("dsfmt_t*" :pointer)))

(defwrapper "dsfmt_genrand_open_open" :double
  (dsfmt ("dsfmt_t*" :pointer)))

(defwrapper "dsfmt_gv_genrand_uint32" ("uint32_t" :unsigned-int))

(defwrapper "dsfmt_gv_genrand_close1_open2" :double)

(defwrapper "dsfmt_gv_genrand_close_open" :double)

(defwrapper "dsfmt_gv_genrand_open_close" :double)

(defwrapper "dsfmt_gv_genrand_open_open" :double)

(defwrapper "dsfmt_gv_fill_array_open_close" :void
  (array ("double*" :pointer)) (size :int))

(defwrapper "dsfmt_gv_fill_array_close_open" :void
  (array ("double*" :pointer)) (size :int))

(defwrapper "dsfmt_gv_fill_array_open_open" :void
  (array ("double*" :pointer)) (size :int))

(defwrapper "dsfmt_gv_fill_array_close1_open2" :void
  (array ("double*" :pointer)) (size :int))

(defwrapper "dsfmt_gv_init_gen_rand" :void
  (seed ("uint32_t" :unsigned-int)))

(defwrapper "dsfmt_gv_init_by_array" :void
  (init-key ("uint32_t*" :pointer))
  (key-length :int))

(defwrapper "dsfmt_init_gen_rand" :void
  (dsfmt ("dsfmt_t*" :pointer))
  (seed ("uint32_t" :unsigned-int)))

(defwrapper "dsfmt_init_by_array" :void
  (dsfmt ("dsfmt_t*" :pointer))
  (init-key ("uint32_t*" :pointer))
  (key-length :int))
