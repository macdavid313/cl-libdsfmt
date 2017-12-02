;;;; dsfmt.lisp
(in-package :dsfmt)

(declaim (optimize speed (safety 1) (space 0) (debug 0))
         (inline init-by-seed init-by-array random-uint32
                 random-close1-open2 random-close-open
                 random-open-close random-open-open
                 fill-array-close1-open2 fill-array-close-open
                 fill-array-open-close fill-array-open-open))

(defvar *dsfmt* (dsfmt-get-global-data)
  "The pointer to the global state vector from dSFMT.")

(defvar *dsfmt-initialized-p* nil
  "A predicate to decide if *dsfmt* has been initialized.")

(defconstant +min-array-size+ (get-min-array-size)
  "The minimal array size for 'fill-array-xxx' functions.")

(deftype unsigned-int ()
  `(integer 0 ,(- (expt 2 32) 1)))

(defun init-by-seed (seed)
  "This function initializes the internal state array with a 32-bit unsigned integer seed. This function implicitly uses the global variable *dsfmt* internally."
  (assert (typep seed 'unsigned-int) nil
          "The seed must be an unsigned-int ~ [0, 4294967295].")
  (setq *dsfmt-initialized-p* t)
  (dsfmt-gv-init-gen-rand seed))

(defun init-by-array (init-key)
  "This function initializes the internal state array, with an array of 32-bit integers used as the seeds. This function implicitly uses global variables *dsfmt* internally."
  (assert (typep init-key '(vector unsigned-int (*))) nil
          "The init-key must be an array of unsigned-int numbers.")
  (setq *dsfmt-initialized-p* t)
  (dsfmt-gv-init-by-array init-key (length init-key)))

(defun random-uint32 ()
  "This function generates and returns unsigned 32-bit integer. This is slower than SFMT, only for convenience usage."
  (assert *dsfmt-initialized-p* nil
          "Please initialize *dsfmt* by either using 'init-by-seed' or 'init-by-array' first.")
  (dsfmt-gv-genrand-uint32))

(defun random-close1-open2 ()
  "This function generates and returns double precision pseudorandom number which distributes uniformly in the range [1, 2). 'init-by-seed' or 'init-by-array' must be called before this function. This function implicitly uses the global variable *dsfmt* internally."
  (assert *dsfmt-initialized-p* nil
          "Please initialize *dsfmt* by either using 'init-by-seed' or 'init-by-array' first.")
  (dsfmt-gv-genrand-close1-open2))

(defun random-close-open ()
  "This function generates and returns double precision pseudorandom number which distributes uniformly in the range [0, 1). 'init-by-seed' or 'init-by-array' must be called before this function. This function implicitly uses the global variable *dsfmt* internally."
  (assert *dsfmt-initialized-p* nil
          "Please initialize *dsfmt* by either using 'init-by-seed' or 'init-by-array' first.")
  (dsfmt-gv-genrand-close-open))

(defun random-open-close ()
  "This function generates and returns double precision pseudorandom number which distributes uniformly in the range (0, 1]. 'init-by-seed' or 'init-by-array' must be called before this function. This function implicitly uses the global variable *dsfmt* internally."
  (assert *dsfmt-initialized-p* nil
          "Please initialize *dsfmt* by either using 'init-by-seed' or 'init-by-array' first.")
  (dsfmt-gv-genrand-open-close))

(defun random-open-open ()
  "This function generates and returns double precision pseudorandom number which distributes uniformly in the range (0, 1). 'init-by-seed' or 'init-by-array' must be called before this function. This function implicitly uses the global variable *dsfmt* internally."  
  (assert *dsfmt-initialized-p* nil
          "Please initialize *dsfmt* by either using 'init-by-seed' or 'init-by-array' first.")
  (dsfmt-gv-genrand-open-open))

(defun fill-array-close1-open2 (size)
  "This function generates double precision floating point pseudorandom numbers which are distributed in the range [1, 2) to an (static yet lisp) array. 'init-by-seed' or 'init-by-array' must be called before this function. This function implicitly uses the global variable *dsfmt* internally."
  (assert (typep size '(integer 0 *)) nil
          "The size must be a postive integer.")
  (assert *dsfmt-initialized-p* nil
          "Please initialize *dsfmt* by either using 'init-by-seed' or 'init-by-array' first.")
  (flet ((%helper (size)
           (let ((arr (make-static-vector size :element-type 'double-float)))
             (finalize arr (lambda () (free-static-vector arr)))
             (dsfmt-gv-fill-array-close1-open2 (static-vector-pointer arr) size)
             arr)))
    (if (< size +min-array-size+)
        (let ((arr (%helper +min-array-size+)))
          (subseq arr 0 size))
        (%helper size))))

(defun fill-array-close-open (size)
  "This function generates double precision floating point pseudorandom numbers which are distributed in the range [0, 1) to an (static yet lisp) array. 'init-by-seed' or 'init-by-array' must be called before this function. This function implicitly uses the global variable *dsfmt* internally."
  (assert (typep size '(integer 0 *)) nil
          "The size must be a postive integer.")
  (assert *dsfmt-initialized-p* nil
          "Please initialize *dsfmt* by either using 'init-by-seed' or 'init-by-array' first.")
  (flet ((%helper (size)
           (let ((arr (make-static-vector size :element-type 'double-float)))
             (finalize arr (lambda () (free-static-vector arr)))
             (dsfmt-gv-fill-array-close-open (static-vector-pointer arr) size)
             arr)))
    (if (< size +min-array-size+)
        (let ((arr (%helper +min-array-size+)))
          (subseq arr 0 size))
        (%helper size))))

(defun fill-array-open-close (size)
  "This function generates double precision floating point pseudorandom numbers which are distributed in the range (0, 1] to an (static yet lisp) array. 'init-by-seed' or 'init-by-array' must be called before this function. This function implicitly uses the global variable *dsfmt* internally."
  (assert (typep size '(integer 0 *)) nil
          "The size must be a postive integer.")
  (assert *dsfmt-initialized-p* nil
          "Please initialize *dsfmt* by either using 'init-by-seed' or 'init-by-array' first.")
  (flet ((%helper (size)
           (let ((arr (make-static-vector size :element-type 'double-float)))
             (finalize arr (lambda () (free-static-vector arr)))
             (dsfmt-gv-fill-array-open-close (static-vector-pointer arr) size)
             arr)))
    (if (< size +min-array-size+)
        (let ((arr (%helper +min-array-size+)))
          (subseq arr 0 size))
        (%helper size))))

(defun fill-array-open-open (size)
  "This function generates double precision floating point pseudorandom numbers which are distributed in the range (0, 1) to an (static yet lisp) array. 'init-by-seed' or 'init-by-array' must be called before this function. This function implicitly uses the global variable *dsfmt* internally."
  (assert (typep size '(integer 0 *)) nil
          "The size must be a postive integer.")
  (assert *dsfmt-initialized-p* nil
          "Please initialize *dsfmt* by either using 'init-by-seed' or 'init-by-array' first.")
  (flet ((%helper (size)
           (let ((arr (make-static-vector size :element-type 'double-float)))
             (finalize arr (lambda () (free-static-vector arr)))
             (dsfmt-gv-fill-array-open-open (static-vector-pointer arr) size)
             arr)))
    (if (< size +min-array-size+)
        (let ((arr (%helper +min-array-size+)))
          (subseq arr 0 size))
        (%helper size))))
