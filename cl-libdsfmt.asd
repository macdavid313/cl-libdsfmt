#|
  This file is a part of cl-libdsfmt project.
  Copyright (c) 2017 David Gu (macdavid313@gmail.com)
|#

#|
  Common Lisp binding for dSFMT, a double precision SIMD oriented Fast Mersenne Twister pseudorandom number generator.

  Author: David Gu (macdavid313@gmail.com)
|#

(defsystem "cl-libdsfmt"
  :version "0.1.0"
  :author "David Gu"
  :license ""
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("uiop" "cffi" "static-vectors" "trivial-garbage")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "lib")
                 (:cffi-grovel-file "grovel")
                 (:cffi-wrapper-file "wrapper")
                 (:file "dsfmt"))))
  :description "Common Lisp binding for dSFMT, a double precision SIMD oriented Fast Mersenne Twister pseudorandom number generator."
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown")))
