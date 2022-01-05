;;; sbcl --eval '(load (compile-file "refract.lisp"))'
;;; REFRACT = REasoner about FRACTions.

;;; Todo: Without DBMOIF it can't find any derivations at all...why not?
;;;       Why does (:to_over1 =1 (=1 over 1)) crashe the prover?
;;;       The binder is making a side-effect mess somehow! .... (:TO_OVER1 (=1 . 6) (=1 OVER 1)))
;;;          (hacked by re-creting the *vars* on every match UUUUUUUUUUUUUUUUUUUUUUUUUU)
;;;       (run :given '(((4 over 2) over (2 over 6)) / ((12 over 2) over (6 over 3))) :goal 2) ???

(setf *print-length* 99999 *print-pretty* nil)

;; We need, first off, to represent the problem itself. For that we have
;; the normal forms of (a + b) (a / b) (a - b) and (a * b). We also
;; use (a over b). 

;; The executor has several embedded rules: 1. Cut off search if you
;; run the same sequence of actions to the same point more than
;; once. 2. Any time you have simple numbers in a simple expression,
;; evaluate them.

(defvar *rules* nil)

(defvar *rule-counts* nil)

(defparameter *all-possible-rules* 
  '(
    (:dbmoif ((=1 over =2) / (=3 over =4)) ((=1 over =2) * (=4 over =3))) ;; divide-by-multiplication-of-inverse-fraction
    (:xfracts ((=1 over =2) * (=3 over =4)) ((=1 * =3) over (=2 * =4)))
    (:fad (=1 / =2) (=1 over =2)) ;; Fractionalize a division
    (:daf (=1 over =2) (=1 / =2)) ;; Divisionalize a fraction
    (:from_over1 (=1 over 1) =1) 
    (:to_over1 (=1) (=1 over 1)) ;; Hack for the crash problem in the next rule
    ;; Invalid operations that kids sometimes do anyways!
    ;; (:tfup (=1 over =2) (=2 over =1)) ;; turn-fraction-upside-down

    ;; (:to_over1 =1 (=1 over 1)) ;; !!! crashes the prover !!!
   ))

(defvar *rules-master* ;; Gets copy-treed into *rules* in init because of a bug in the binder.
  '(
    (:dbmoif ((=1 over =2) / (=3 over =4)) ((=1 over =2) * (=4 over =3))) ;; divide-by-multiplication-of-inverse-fraction
    (:xfracts ((=1 over =2) * (=3 over =4)) ((=1 * =3) over (=2 * =4)))
    (:fad (=1 / =2) (=1 over =2)) ;; Fractionalize a division
    (:daf (=1 over =2) (=1 / =2)) ;; Divisionalize a fraction
    (:from_over1 (=1 over 1) =1) 
    (:to_over1 (=1) (=1 over 1)) ;; Hack for the crash problem in the next rule
    ;; Invalid operations that kids sometimes do anyways!
    ;; (:tfup (=1 over =2) (=2 over =1)) ;; turn-fraction-upside-down

    ;; (:to_over1 =1 (=1 over 1)) ;; !!! crashes the prover !!!
   ))

(defparameter *rule-descriptions*
  '((:dbmoif . "Divide a fraction by multiplying by its inverse.")
    (:tfup . "Turn a fraction upside down.")
    (:xfracts . "Fraction multiplication.")
    (:foil++ . "(positive) FOIL")
    (:fad . "Change a division into a fraction.")
    (:daf . "Change a fraction into a division.")
    (:from_over1 . "Something over 1 is just the thing.")
    ))

;;; Here there be a theorem prover!

(defparameter *default-depth-limit* 6)

(defstruct success ccount path)

(defvar *successes* nil)
(defvar *too-long-fails* nil)
(defvar *circular-fails* nil)
(defvar *wrong-answer-fails* nil)

;;; You'd think that we could just count the paths for the conclusion
;;; count, but we can't bcs of the recursive embeddedness of the paths
;;; in the search tree. I guess we could just push onto a path
;;; list...that might make some sense.

(defvar *conclusion-count* nil)

(defun run (&key given (goal :any-number) (depth-limit *default-depth-limit*) (rule-priorities nil) (verbose? t))
  "returns ccountofearliestsuccess (nil of no successes)"
  (if (null given) (break "You must provide a given."))
  (init)
  (when verbose?
    (format t "~%~%======================================~%")
    (format t "Given: ~a, prove: ~a~%  (depth limit ~a, rule priorities: ~a)~%" given goal depth-limit rule-priorities)
    (pprint *rules*)
  )
  (let ((simplified-given (simplify given)))
    (unless (equal simplified-given given)
      (when verbose? (format t "~% ** Simplified given to: ~a~%" simplified-given))
      (setf given simplified-given)))
  (prove given goal nil 1 depth-limit rule-priorities)
  (let* ((nsuccesses (length *successes*))
	 (success-locs (reverse (mapcar #'success-ccount *successes*)))
	 (earliest-success (car success-locs))
	 )
    (when verbose?
      (format t "~%----------------------------------------~%")
      (if (not (zerop nsuccesses))
	  (format t "Number of successes: ~a (of ~a total conclusions) = ~a%, first @ ~a, mean @ ~a~%"
		  nsuccesses *conclusion-count* (* 100.0 (/ nsuccesses *conclusion-count*))
		  earliest-success (float (/ (apply #'+ success-locs) nsuccesses)))
      	(format t "There were no successes!~%"))
      (format t "~a length fails (~a%), ~a error fails (~a), ~a loop fails (~a%)~%"
	      *too-long-fails* (round (* 100.0 (/ *too-long-fails* *conclusion-count*)))
	      *wrong-answer-fails* (round (* 100.0 (/ *wrong-answer-fails*  *conclusion-count*)))
	      *circular-fails* (round (* 100.0 (/ *circular-fails* *conclusion-count*))))
      (format t "For ~a -> ~a, with rule priorities: ~a and depth limit=~a found first success @ ~a~%"
	      given goal rule-priorities depth-limit earliest-success)
      (format t "~%----------------------------------------")
      (pprint *rule-counts*)
      (format t "~%----------------------------------------")
      (mapcar #'(lambda (s) (ppsuccess s given)) (reverse *successes*))
      )
    earliest-success))

(defun init ()
  (setf *rules* (copy-tree *rules-master*)) ;; Avoids a binder bug....or not???!!!
  (setf *successes* nil *too-long-fails* 0 *circular-fails* 0 *conclusion-count* 0 *wrong-answer-fails* 0)
  (setf *rule-counts*
	(loop for (rname) in *rules*
	      collect (cons rname 0)))
  )

;;; Rule-priorities can be a list of the names of rules in preferred
;;; rank order.

(defun prove (expr goal path depth depth-limit rule-priorities)
  ;;(print `(:proving ,expr ,path ,depth))
  (cond ((and (numberp expr) (eq :any-number goal))
	 (push (make-success :ccount *conclusion-count* :path path) *successes*) (incf *conclusion-count*) `(:success! ,path))
	((and (numberp expr) (not (equal expr goal)))
	 (incf *wrong-answer-fails*) (incf *conclusion-count*) `(:fail!wrong-answer! ,path))
	((equal expr goal) (push (make-success :ccount *conclusion-count* :path path) *successes*) (incf *conclusion-count*) `(:success! ,path))
	((= depth depth-limit) (incf *too-long-fails*) (incf *conclusion-count*) `(:fail!taking-too-long! ,path))
	((repetitious? path) (incf *circular-fails*) (incf *conclusion-count*) `(:fail!going-in-circles! ,path))
	(t (loop for rule@loc in (find-rules@locations expr rule-priorities)
		 as newexpr = (apply-rule@loc expr (car rule@loc) (cdr rule@loc))
		 as simplified-newexpr = (simplify newexpr)
		 collect (prove simplified-newexpr goal (cons (cons (cons simplified-newexpr newexpr) rule@loc) path) (1+ depth) depth-limit rule-priorities)))))

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
	  if (progn (init-vars) (matches? pat expr))
	  do (push (cons rule path) *rule-matches@locs*)
	  (unless (atom expr)
	    (loop for subexpr in expr
		  as eltno from 0 by 1
		  do (find-rules@locations2 subexpr (cons eltno path)))))))
  
(defparameter *vars* nil)
(defun init-vars () ;; UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU
  (setf *vars* (list (list '=1)(list '=2) (list '=3) (list '=4))))

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
	 ;; Only do the operation if the result will be an integer, and isn't /0. (Only matters to /)
	 (let ((result (ignore-errors (eval `(,(second expr) ,(first expr) ,(third expr))))))
	   (if (and result (integerp result)) result expr)))
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

(defun ppsuccess (success given)
  (format t "~%------ At ~a (length ~a):~%  ~a || Given~%" (success-ccount success) (length (success-path success)) given)
  (loop for ((simplified-result . result) (name pat reb)) in (reverse (success-path success))
	do
	(if (equal simplified-result result)
	    (format t "  ~a || ~a   [~a: ~a -> ~a]~%" result (cdr (assoc name *rule-descriptions*)) name pat reb)
	  (format t "  ~a [<-~a] || ~a   [~a: ~a -> ~a]~%" simplified-result result (cdr (assoc name *rule-descriptions*)) name pat reb)))
  (format t "~%------~%~%"))

;;;

(defun test (given &optional (goal :any-number) (depth-limit *default-depth-limit*))
  (format t "~%~%*******************~%")
  (format t "   ~a -> ~a   (d=~a)~%" given goal depth-limit)
  (format t "*******************~%~%")
  (run :given given :goal goal :rule-priorities '(:dbmoif :xfracts) :depth-limit depth-limit)
  (run :given given :goal goal :rule-priorities '(:xfracts :dbmoif) :depth-limit depth-limit)
  (run :given given :goal goal :rule-priorities '(:xfracts) :depth-limit depth-limit)
  (run :given given :goal goal :rule-priorities '(:dbmoif) :depth-limit depth-limit)
  (run :given given :goal goal :rule-priorities '() :depth-limit depth-limit)
  (run :given given :goal goal :rule-priorities '() :depth-limit 10)
  )

(defun run-all-tests ()
  (test '((7 over 2) over (7 over 4)) 2)
  (test '((4 over 2) over (2 over 6)) 6)
  ;; Note that the OVER instead of / interacts with the prioritization
  ;; of DBMOIF to make it much harder to find the solution
  (test '((4 over 2) / (2 over 6)) 6)
  (test '((2) / (2 over 6)) 6)
  (test '((3 over 1) over (6 over 2)) 1) ;; This it can do even w/o DBMOIF
  )

(defun reset-rules ()
  (setf *rules* (copy-tree (setf *rules-master* (copy-tree *all-possible-rules*)))))

(defun drop-rule (name-of-rule-to-drop)
  (format t "~%~%******** Dropping rule: ~a ***********~%~%" name-of-rule-to-drop)
  (setf *rules-master*
	(loop for rule in *rules-master*
	      unless (eq (car rule) name-of-rule-to-drop)
	      collect rule)))

;;; Reports stat on when problems are found under all possible rule rank orders

(defun try-all-rule-orders (given goal &optional (depth-limit *default-depth-limit*))
  (let* ((r (loop for order in (all-complete-orderings (mapcar #'car *rules*))
		  collect (run :given given :goal goal :depth-limit depth-limit :rule-priorities order :verbose? nil))))
    `(:given ,given :prove ,goal :min ,(apply #'min r) :max ,(apply #'max r) :mean ,(float (/ (apply #'+ r) (length r))) :mode ,(mode r))))

(defun mode (l)
  (car (sort (hist l) #'< :key #'car)))

(defun hist (l)
  (let ((r))
    (loop for i in l
	  as p = (assoc i r)
	  do (if p (incf (cdr p))
	       (push (cons i 1) r)))
    r))
    
(defun insert-at-every-position (what into)
  (loop for i below (1+ (length into))
	collect (append (first-n i into)
			(list what)
			(nthcdr i into))))

(defun all-complete-orderings (l)
  (cond ((null (cdr l)) (list l))
	(t (loop for sub in (all-complete-orderings (cdr l))
		 append (insert-at-every-position (car l) sub)))))
  
(untrace)
;(trace rebuild find-rules@locations bind apply-rule@loc matches? prove replace@ extract@ repetitious? find-rules@locations2)

(reset-rules)
(run-all-tests)
(drop-rule :dbmoif)
(run-all-tests)
(reset-rules)
(print (try-all-rule-orders '((4 over 2) / (2 over 6)) 6))
(print (try-all-rule-orders '((4 over 2) over (2 over 6)) 6))

;;; Bugs:
;(print (run :given '((4 over 2) over (2 over 6)) :goal 6 :rule-priorities '(:dbmoif :xfracts) :depth-limit 6 :verbose? t))
;(print (try-all-rule-orders '(((4 over 2) over (2 over 6)) / ((2 over 6) over (2 over 4))) 9))
;(run :given '(((4 over 2) over (2 over 6)) / ((12 over 2) over (6 over 3))) :goal 2)

