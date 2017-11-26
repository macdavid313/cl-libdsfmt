;;;; package.lisp
(in-package #:cl-user)
(defpackage #:cl-libdsfmt
  (:use #:cl)
  (:nicknames #:libdsfmt #:dsfmt)
  (:import-from #:cffi
                #:*foreign-library-directories*
                #:load-foreign-library
                #:defcfun)
  (:import-from #:static-vectors
                #:make-static-vector
                #:free-static-vector
                #:static-vector-pointer)
  (:import-from #:trivial-garbage
                #:finalize)
  (:export ;;; variables
           #:+DSFMT-MEXP+ #:+DSFMT-N+
           #:+DSFMT-N32+ #:DSFMT-N64+
           #:*dsfmt* #:+min-array-size+
           ;;; high level APIs
           #:init-by-seed #:init-by-array #:random-uint32
           #:random-close1-open2 #:random-close-open
           #:random-open-close #:random-open-open
           #:fill-array-close1-open2 #:fill-array-close-open
           #:fill-array-open-close #:fill-array-open-open
           ;;; low-level functions
           #:get-idstring #:get-min-array-size
           #:dsfmt-fill-array-close1-open2 #:dsfmt-fill-array-close-open
           #:dsfmt-fill-array-open-close #:dsfmt-fill-array-open-open
           #:dsfmt-chk-init-gen-rand #:dsfmt-chk-init-by-array
           #:dsfmt-get-global-data #:dsfmt-genrand-uint32
           #:dsfmt-genrand-close1-open2 #:dsfmt-genrand-close-open
           #:dsfmt-genrand-open-close #:dsfmt-genrand-open-open
           #:dsfmt-gv-genrand-uint32 #:dsfmt-gv-genrand-close1-open2
           #:dsfmt-gv-genrand-close-open #:dsfmt-gv-genrand-open-close
           #:dsfmt-gv-genrand-open-open #:dsfmt-gv-fill-array-close1-open2
           #:dsfmt-gv-fill-array-close-open #:dsfmt-gv-fill-array-open-close
           #:dsfmt-gv-fill-array-open-open #:dsfmt-gv-init-gen-rand
           #:dsfmt-gv-init-by-array #:dsfmt-init-by-array #:dsfmt-init-gen-rand))
