;;; (load (compile-file "algorithms.lisp"))

(defvar *fingers* "set by (init)")
(defvar *curhand* "set by (init)")
(defvar *curfing* "set by (init)")
(defvar *mems* "set by (init)")
(defvar *addends* "set by (init)")

(defun init ()
  (setf *fingers* (list (loop for f below 5 collect :down) (loop for f below 5 collect :down))
	*curfing* 0
	*curhand* 0
	*mems* (loop for f below 5 collect nil)
	*addends* (let ((a (1+ (random 4)))
			(b (1+ (random 4))))
		    (format t "Problem is: ~a+~a~%" a b)
		    (list a b))
	
	))

(defun runprog (prog)
  (format t "~%----------------------~%")
  (init)
  (prog (pc k result step plen)
	(setf pc 0 k 30 plen (length prog))
     next-step
	(if (> pc plen) (return :ran-off-end))
	(decf k)
	(if (zerop k) (return :non-terminating))
	(setq step (nth pc prog) result (fingeval step))
	(format t "~a:~a->~a (*mems* = ~a)~%" pc step result *mems*)
	(if (eq result :stop) (return :completed))
	(if (numberp result)
	    (let ((nth (position result prog :test #'equal)))
	      (format t "~%-> Go to ~a (@~a)!~%" result (setf pc nth))
	      (go next-step)))
	(incf pc)
	(go next-step)))

(defun fingeval (step &aux result)
  (if (numberp step) nil ;; Skip numbers which are just goto targets
    (case (car step)
	  (:say (format t "Speak (reg:~a): ~a~%" (second step) (second step)) nil)
	  (:stop :stop)
	  (:go (second step))
	  (:go-if-equal (when (= (nth (second step) *mems*) (nth (third step) *mems*)) (fourth step)))
	  (:go-if-not-equal (when (not (= (nth (second step) *mems*) (nth (third step) *mems*))) (fourth step)))
	  (:addend-to-mem (setf (nth (third step) *mems*) (nth (second step) *addends*)) nil)
	  (:incf (incf (nth (second step) *mems*)) nil)
	  (:decf (decf (nth (second step) *mems*)) nil)
	  )))

(defparameter *test* '((:addend-to-mem 0 0) 100 (:go-if-equal 0 0 200) (:decf 0) (:say 0) (:go 100) 200 (:say 0) (:stop)))

(defun test ()
  (init)
  (print (runprog *test*)))

(test)
