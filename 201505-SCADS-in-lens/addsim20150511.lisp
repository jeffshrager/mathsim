; (load (compile-file "addsim20150511.lisp"))
;(unless (ignore-errors (symbol-function 'correlation) (load "~/Desktop/etc/cllib/stats.dx32fsl"))
;(unless (ignore-errors (symbol-function 'correlation-coefficient)) (load "~/Desktop/etc/cllib/lhstats.dx32fsl"))

;;; ================================================================
;;; Data from Siegler and Shrager 1984 -- Note that this data is under
;;; overt strategy supression instruction!

(defparameter *sns84-data*
  ;; All these are hundreths:
  ;; 0 1 2 3 4 5 6 7 8 9 10 11 other
  '(
    ((1 . 1) (0 5 86 0 2 0 2 0 0 0 0 2 4))
    ((1 . 2) (0 0 9 70 2 0 4 0 0 7 2 2 5))
    ((1 . 3) (0 2 0 11 71 5 2 2 0 0 0 0 7))
    ((1 . 4) (0 0 0 0 11 61 9 7 0 0 0 2 11))
    ((1 . 5) (0 0 0 0 13 16 50 11 0 2 2 0 5))
    ((2 . 1) (0 7 5 79 5 0 0 0 0 0 0 0 4))
    ((2 . 2) (2 0 4 5 80 4 0 5 0 0 0 0 0))
    ((2 . 3) (0 0 4 7 38 34 9 2 2 2 0 0 4))
    ((2 . 4) (0 2 0 7 2 43 29 7 7 0 0 0 4))
    ((2 . 5) (0 2 0 5 2 16 43 13 0 0 2 0 18))
    ((3 . 1) (0 2 0 9 79 4 0 4 0 0 0 0 4))
    ((3 . 2) (0 0 9 11 11 55 7 0 0 0 0 0 7))
    ((3 . 3) (4 0 0 5 21 9 48 0 2 2 2 0 7))
    ((3 . 4) (0 0 0 5 11 23 14 29 2 0 0 0 16))
    ((3 . 5) (0 0 0 7 0 13 23 14 18 0 5 0 20))
    ((4 . 1) (0 0 4 2 9 68 2 2 7 0 0 0 7))
    ((4 . 2) (0 0 7 9 0 20 36 13 7 0 2 0 7))
    ((4 . 3) (0 0 0 5 18 9 9 38 9 0 2 0 11))
    ((4 . 4) (4 0 0 2 2 29 7 7 34 0 4 0 13))
    ((4 . 5) (0 0 0 0 4 9 16 9 11 18 11 4 20))
    ((5 . 1) (0 0 4 0 4 7 71 4 4 0 4 0 4))
    ((5 . 2) (0 0 5 20 2 18 27 25 2 0 2 0 0))
    ((5 . 3) (0 0 2 11 9 18 5 16 23 0 5 0 11))
    ((5 . 4) (0 0 0 0 11 21 16 5 11 16 4 0 16))
    ((5 . 5) (4 0 0 0 0 7 25 11 2 4 34 4 11))
    ))

(defun check-data ()
  (loop for entry in *sns84-data*
	as (problem r*) = entry
	do (print (list problem r* (apply #'+ r*)))
	(unless (= 13 (length r*))
	  (format t " ** ~a data points!~%" (length r*)))))

;;; ================================================================

(defvar *problem->results* (make-hash-table :test #'equal))

(defun run (addfns &key (n-rounds 10000))
  (check-data) ;; Just in case there have been typos.
  (loop for addfn in addfns
	do
	(format t "~%========= Running addfn = ~a ===========~%" addfn)
	(clrhash *problem->results*)
	(loop for round below n-rounds
	      do (loop for a from 1 to 5
		       do (loop for b from 1 to 5
				do (push (let ((r (funcall addfn a b)))
					   (if (<= r 12) r 12))
					 (gethash (cons a b) *problem->results*)))))
	collect (cons addfn (print (compare)))
	))
  
(defun compare ()
  (let ((pairs (loop for a in (loop for (problem obs) in *sns84-data*
				    do (print (list problem obs))
				    append obs)
		     for b in (loop for (problem) in *sns84-data*
				    as sim = (report-sim-results-as-100ths problem)
				    do (print (list problem sim))
				    append sim)
		     collect (list a b))))
    (stats::correlation-coefficient pairs)))

(defun report-sim-results-as-100ths (problem)
  (let* ((results (gethash problem *problem->results*))
	 (n-rounds (length results))
	 )
    (loop for ans below 13
	  collect (* 100.0 (/ (count ans results) n-rounds)))))

;;; ================================================================
;;; Add Functions, all take a+b and return the result, however they
;;; get it.

(defun add-with-sequence-errors (a b)
  "If b = a + 1 sometimes give b + 1, else just add normally"
  (if (= b (+ a 1))
      (if (zerop (random 3))
	  (+ b 1)
	(+ a b))
    (+ a b)))

(defun exact+ (a b)
  "Get the results directly from the observation table."
  (loop with r* = (second (assoc (cons a b) *sns84-data* :test #'equal))
	with l = (length r*)
	with rand = (random 100)
	with sum = 0 
	as i from 0 by 1
	as v in r*
	do 
	;; (print (list r* l rand sum i v))
	(if (>= sum rand)
	       (return  (1- i))
	     (incf sum v))
	finally (return i)))

(defun exact+scrambled (a b)
  (flet ((foo (n) (1+ (mod (+ 3 n) 5))))
	(setf a (foo a) b (foo b)))
  "This gets the states from the observation table, but it rotates the addends so that you're reading the wrong row."
  (loop with r* = (second (assoc (cons a b) *sns84-data* :test #'equal))
	with l = (length r*)
	with rand = (random 100)
	with sum = 0 
	as i from 0 by 1
	as v in r*
	do 
	;; (print (list r* l rand sum i v))
	(if (>= sum rand)
	       (return  (1- i))
	     (incf sum v))
	finally (return i)))

(defun random+-* (a b)
  (case (random 3)
	(0 (+ a b))
	(1 (- a b))
	(2 (* a b))
	))

;;; ================================================================

;(check-data)
(mapcar #'print (run (list #'exact+ #'exact+scrambled #'+ #'(lambda (a b) (random 12)) #'add-with-sequence-errors #'random+-*)))

