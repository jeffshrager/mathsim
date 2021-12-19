;;; (load (compile-file "refract.lisp"))

;;; REFRACT = REasoner about FRACTions.

;; We need, first off, to represent the problem itself. For that we have
;; the normal forms of (a + b) (a / b) (a - b) and (a * b). We also
;; use (a over b). 

;; The executor has several embedded rules: 1. Cut off search if you
;; run the same sequence of actions to the same point more than
;; once. 2. Any time you have simple numbers in a simple expression,
;; evaluate them.

(defparameter *rules*
  '(
    (:dbmoif ((=1 over =2) / (=3 over =4)) ((=1 over =2) * (=4 over =3))) ;; divide-by-multiplication-of-inverse-fraction
    (:tfup (=1 over =2) (=2 over =1)) ;; turn-fraction-upside-down
    (:xfracts ((=1 over =2) * (=3 over =4)) ((=1 * =3) over (=2 * =4)))
    (:foil++ ((=1 + =2) * (=3 + =4)) ((=1 * =3) + (=2 * =3) + (=1 * =4) + (=2 * =4)))
    (:fad (=1 / =2) (=1 over =2)) ;; Fractionalize a division
    (:daf (=1 over =2) (=1 / =2)) ;; Divisionalize a fraction
    (:from_over1 (=1 over 1) =1) 
    ;; (:to_over1 =1 (=1 over 1)) ;; !!! crashes the prover !!!
   ))

;;; Here there be a theorem prover!

(defparameter *depth-limit* 6)

(defstruct success ccount path)

(defvar *successes* nil)
(defvar *too-long-fails* nil)
(defvar *circular-fails* nil)

;;; You'd think that we could just count the paths for the conclusion
;;; count, but we can't bcs of the recursive embeddedness of the paths
;;; in the search tree. I guess we could just push onto a path
;;; list...that might make some sense.

(defvar *conclusion-count* nil)

(defun run (expr goal &key (depth-limit *depth-limit*) (rule-priorities nil))
  (format t "~%~%======================================~%")
  (format t "Given: ~a, prove: ~a~%  (depth limit ~a, rule priorities: ~a)~%" expr goal depth-limit rule-priorities)
  (init)
  (pprint *rules*)
  (prove expr goal nil 1 depth-limit rule-priorities)
  (format t "~%----------------------------------------~%")
  (let ((nsuccesses (length *successes*))
	(success-locs (reverse (mapcar #'success-ccount *successes*)))
	)
    (format t "Number of successes: ~a (of ~a total conclusions) = ~a%, first @ ~a, mean @ ~a~%"
	    nsuccesses *conclusion-count* (* 100.0 (/ nsuccesses *conclusion-count*))
	    (car success-locs) (float (/ (apply #'+ success-locs) nsuccesses)))
    (format t "~a length fails (~a%), ~a loop fails (~a%)~%"
	    *too-long-fails* (round (* 100.0 (/ *too-long-fails* *conclusion-count*)))
	    *circular-fails* (round (* 100.0 (/ *circular-fails* *conclusion-count*))))
    )
  (format t "~%----------------------------------------")
  (pprint *rule-counts*)
  (format t "~%----------------------------------------")
  (pprint *successes*)
  )

(defvar *rule-counts* nil)

(defun init ()
  (setf *successes* nil *too-long-fails* 0 *circular-fails* 0 *conclusion-count* 0)
  (setf *rule-counts*
	(loop for (rname) in *rules*
	      collect (cons rname 0)))
  )

;;; Rule-priorities can be a list of the names of rules in preferred
;;; rank order.

(defun prove (expr goal path depth depth-limit rule-priorities)
  ;;(print `(:proving ,expr ,path ,depth))
  (cond ((equal expr goal) (push (make-success :ccount *conclusion-count* :path path) *successes*) (incf *conclusion-count*) `(:success! ,path))
	((= depth *depth-limit*) (incf *too-long-fails*) (incf *conclusion-count*) `(:fail!taking-too-long! ,path))
	((repetitious? path) (incf *circular-fails*) (incf *conclusion-count*) `(:fail!going-in-circles! ,path))
	(t (loop for rule@loc in (find-rules@locations expr rule-priorities)
		 as newexpr = (simplify (apply-rule@loc expr (car rule@loc) (cdr rule@loc)))
		 collect (prove newexpr goal (cons (cons newexpr rule@loc) path) (1+ depth) depth-limit rule-priorities)))))

(defun repetitious? (path)
  (< 1 (count (caar path) path :test #'(lambda (target elt) (equal target (car elt))))))

;;; The matcher find all rules that could apply in any location. The
;;; rule applicator does the actual work. We do this in two stages so
;;; that the proof driver can keep track of the path, depth, etc.

(defvar *rule-matches@locs* nil)

(defun find-rules@locations (expr rule-priorities)
  ;;(print `(:findrulesfor ,expr))
  (setq *rule-matches@locs* nil)
  (find-rules@locations2 expr ())
  (setf *rule-matches@locs* (remdups *rule-matches@locs* :test #'equal))
  (when rule-priorities (setf *rule-matches@locs* (sort-rules-by-priority rule-priorities)))
  ;;(print `(:found ,*rule-matches@locs* for ,expr))
  *rule-matches@locs*)

(defun remdups (l &key (test #'eq))
  (loop for l+ on l
	unless (member (car l+) (cdr l+) :test test)
	collect (car l+)))

(defun sort-rules-by-priority (rule-priorities)
  (setq *rule-matches@locs*
	(append
	 ;; Run through the priorities first, bringing them to the front in order:
	 (loop for rule-name in rule-priorities
	       as named-rules = (loop for rule in *rule-matches@locs*
				      when (eq (caar rule) rule-name)
				      collect rule)
	       append named-rules)
	 ;; Now gather everything not named
	 (loop for rule in *rule-matches@locs*
	       when (not (member (caar rule) rule-priorities))
	       collect rule)))
  )


(defun find-rules@locations2 (expr path)
  (when expr
    (loop for rule in *rules*
	  as (name pat rebuild) = rule
	  if (matches? pat expr)
	  do (push (cons rule path) *rule-matches@locs*)
	  (unless (atom expr)
	    (loop for subexpr in expr
		  as eltno from 0 by 1
		  do (find-rules@locations2 subexpr (cons eltno path)))))))
  
(defparameter *vars* '((=1) (=2) (=3) (=4))) ;; Could do this more elegantly

(defun matches? (pat expr)
  (cond ((null pat) (null expr))
	((assoc pat *vars*) expr)
	((atom pat) (eq pat expr))
	((and (listp pat) (listp expr)
	      (matches? (car pat) (car expr))
	      (matches? (cdr pat) (cdr expr))
	      ))))

(defun apply-rule@loc (expr rule loc)
  ;;(print `(:applying ,rule @ ,loc to ,expr))
  (incf (cdr (assoc (car rule) *rule-counts*)))
  (replace@ loc expr (rebuild (bind (second rule) (extract@ loc expr)) (third rule))))

(defun extract@ (loc expr)
  (cond ((null loc) expr)
	(t (extract@ (cdr loc) (nth (car loc) expr)))))

(defun first-n (n l)
  (loop for e in l as i below n collect e))

(defun replace@ (loc expr newsubexpr)
  (cond ((null loc) newsubexpr)
	(t (append (first-n (car loc) expr)
		   (list (replace@ (cdr loc) (nth (car loc) expr) newsubexpr))
		   (nthcdr (1+ (car loc)) expr)))))

(defun bind (pat expr)
  (bind2 pat expr)
  *vars*)

(defun bind2 (pat expr)
  (cond ((null pat) *vars*)
	((atom pat)
	 (let ((var (assoc pat *vars*)))
	   (when var (setf (cdr var) expr))))
	(t (loop for pelt in pat
		 as xelt in expr
		 do (bind2 pelt xelt)))))

(defun rebuild (bindings newexpr)
  (cond ((null newexpr) nil)
	((atom newexpr) (or (cdr (assoc newexpr bindings)) newexpr))
	(t (cons (rebuild bindings (car newexpr)) (rebuild bindings (cdr newexpr))))))

;;; Automatic simplifier does all obvious arithmetic and reduces all
;;; numerical fractions. This is take as having been learned at this
;;; point (it's a little bit arguable as to whether reduction is
;;; learned, but basic arithmetic is). There's also the problem of
;;; whether we really should always simplify because it cuts off some
;;; potential solution paths, although, again, I think at this point,
;;; it's what kids do. (NNN We should be teaching fractions like
;;; algebra. In fact, we should be teaching arithmetic like algebra,
;;; and derivations like proofs! ... There's a revelation in here
;;; someplace that math is really the same thing all the way
;;; down...that's sort of Wu's insight, and mine, although I only
;;; thought it through as far as actual algrbra!)

(defun simplify1 (expr)
  (cond ((null expr) nil)
	((or (numberp expr) (atom expr)) expr)
	((and (numberp (first expr)) (numberp (third expr))
	      (member (second expr) '(+ - * /)))
	 ;; Only do the operation if the result will be an integer. (Only matters to /)
	 (let ((result (eval `(,(second expr) ,(first expr) ,(third expr)))))
	   (if (integerp result) result expr)))
	((and (numberp (first expr)) (numberp (third expr))
	      (eq (second expr) 'over))
	 (reduce-if-possible (first expr) (third expr)))
	(t `(,(simplify1 (first expr)) ,(second expr) ,(simplify1 (third expr))))
	))

(defun reduce-if-possible (a b)
  (if (and (integerp a) (integerp b))
      (let ((gcd (gcd a b)))
	`(,(/ a gcd) over ,(/ b gcd)))
    `(,a over ,b)))

(defun simplify (oldexpr)
  ;;(print `(:simplifying ,oldexpr))
  (let ((newexpr
	 (loop with expr = oldexpr
	       as nexpr = (simplify1 expr)
	       until (equal expr nexpr)
	       finally (return nexpr)
	       do (setf expr nexpr))))
    (if (equal newexpr oldexpr) oldexpr
      (progn
	;;(print `(:simplified ,oldexpr ,newexpr))
	newexpr))))

;;;

(untrace)
;(trace rebuild find-rules@locations bind apply-rule@loc matches? prove replace@ extract@ repetitious? find-rules@locations2)

;;; (pprint (run '((4 over 2) / (2 over 6)) 6))
(pprint (run '((4 over 2) / (2 over 6)) 6 :rule-priorities '(:dbmoif :xfracts)))
;;; (print (prove '(4 over 2) 2))
