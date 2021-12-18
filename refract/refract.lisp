;;; (load (compile-file "refract.lisp"))

;;; REFRACT = REasoner about FRACTions.

;; We need, first off, to represent the problem itself. For that we have
;; the normal forms of (a + b) (a / b) (a - b) and (a * b). We also
;; use (a over b). 

;; The executor has several embedded rules: 1. Cut off search if you
;; run the same sequence of actions to the same point more than
;; once. 2. Any time you have simple numbers in a simple expression,
;; evaluate them.

;;; 

(defparameter *rules*
  '((:tfup (a over b) (b over a)) ;; turn-fraction-upside-down
    (:xfracts ((a over b) * (c over d)) ((a * c) over (b * d)))
    (:foil++ ((a + b) * (c + d)) ((a * c) + (b * c) + (a * d) + (b * d)))
    (:dbmoif ((a over b) / (c over d)) ((a over b) * (d over c))) ;; divide-by-multiplication-of-inverse-fraction
    (:fad (a / b) (a over b)) ;; Fractionalize a division
    (:daf (a over b) (a / b)) ;; Divisionalize a fraction
    ;; Here's where the contentious knowledge enters into the picture
    (:dfbiam ((a over b) / (c over d))) ;; divide fractions by inverting and multiplying *** This is the key piece of knowledge !!!
    (:over1 (:integer a) (a over 1)) ;; any integer n can be represented as n/1 (WWW Matcher must understsand (:integer a)!)
    ))

;;; Here be a theorem prover!

(defparameter *depth-limit* 4)

(defun prove (expr goal &optional path (depth 1))
  (print `(:proving ,expr ,path ,depth))
  (cond ((equal expr goal) `(:success ,path))
	((= depth *depth-limit*) `(:too-deep ,path))
	(t (loop for rule@loc in (find-rules@locations expr)
		 do (prove (apply-rule@loc expr (car rule@loc) (cdr rule@loc))
			   goal (cons rule@loc path) (1+ depth))))))

;;; The matcher find all rules that could apply in any location. The
;;; rule applicator does the actual work. We do this in two stages so
;;; that the proof driver can keep track of the path, depth, etc.

(defvar *rule-matches@locs* nil)

(defun find-rules@locations (expr)
  (print `(:findrulesfor ,expr))
  (setq *rule-matches@locs* nil)
  (find-rules@locations2 expr ())
  (print `(:found ,*rule-matches@locs* for ,expr))
  *rule-matches@locs*)

(defun find-rules@locations2 (expr path)
  (when expr
    (loop for rule in *rules*
	  as (name pat rebuild) = rule
	  if (matches? pat expr)
	  do (push (cons rule path) *rule-matches@locs*)
	  (loop for subexpr in expr
		as eltno from 0 by 1
		do (find-rules@locations2 subexpr (cons eltno path))))))

(defun matches? (pat expr)
  (if (listp pat)
      (if (listp expr)
	  (eq (second pat) (second expr))
	nil)
    (equal pat expr)))

(defun apply-rule@loc (expr rule loc)
  (print `(:applying ,rule @ ,loc to ,expr))
  (replace@ loc expr (rebuild (bind (second rule) (extract@ loc expr))) (third rule))
  )

(defun extract@ (loc expr)
  (cond ((null loc) expr)
	(t (extract@ (cdr loc) (nth (car loc) expr)))))

(defun replace@ (loc expr newsubexpr)
  (cond ((null loc) expr)
	(t ...(extract@ (cdr loc) (nth (car loc) expr)))))

  (:foil++
   ((a + b) * (c + d))
   ((a * c) + (b * c) + (a * d) + (b * d))

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
	 (eval `(,(second expr) ,(first expr) ,(third expr))))
	((and (numberp (first expr)) (numberp (third expr))
	      (eq (second expr) 'over))
	 (reduce-if-possible (first expr) (third expr)))
	(t `(,(simplify1 (first expr)) ,(second expr) ,(simplify1 (third expr))))
	))

(defun reduce-if-possible (a b)
  (let ((gcd (gcd a b)))
    `(,(/ a gcd) over ,(/ b gcd))))

(defun simplify (oldexpr)
  (print `(:simplifying ,oldexpr))
  (let ((newexpr
	 (loop with expr = oldexpr
	       as nexpr = (simplify1 expr)
	       until (equal expr nexpr)
	       finally (return nexpr)
	       do (setf expr nexpr))))
    (if (equal newexpr oldexpr) oldexpr
      (progn (print `(:simplified ,oldexpr ,newexpr)) newexpr))))

;;;

(print (prove '((4 over 2) * (6 over 2)) 6))
