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
  '((:tfup (=1 over =2) (=2 over =1)) ;; turn-fraction-upside-down
    (:xfracts ((=1 over =2) * (=3 over =4)) ((=1 * =3) over (=2 * =4)))
    (:foil++ ((=1 + =2) * (=3 + =4)) ((=1 * =3) + (=2 * =3) + (=1 * =4) + (=2 * =4)))
    (:dbmoif ((=1 over =2) / (=3 over =4)) ((=1 over =2) * (=4 over =3))) ;; divide-by-multiplication-of-inverse-fraction
    (:fad (=1 / =2) (=1 over =2)) ;; Fractionalize a division
    (:daf (=1 over =2) (=1 / =2)) ;; Divisionalize a fraction
    ;; Here's where the contentious knowledge enters into the picture
    (:dfbiam ((=1 over =2) / (=3 over =4)) ((=1 over =2) * (=4 over =3))) ;; divide fractions by inverting and multiplying
    ;;(:over1 (:integer =1) (=1 over 1)) ;; any integer n can be represented as n/1 (WWW Matcher must understsand (:integer a)!)
    ))

;;; Here there be a theorem prover!

(defparameter *depth-limit* 6)

(defun prove (expr goal &optional path (depth 1))
  ;;(print `(:proving ,expr ,path ,depth))
  (cond ((equal expr goal) `(:success ,path))
	((= depth *depth-limit*) `(:too-deep ,path))
	(t (loop for rule@loc in (find-rules@locations expr)
		 as newexpr = (simplify (apply-rule@loc expr (car rule@loc) (cdr rule@loc)))
		 collect (prove newexpr goal (cons (cons newexpr rule@loc) path) (1+ depth))))))

;;; The matcher find all rules that could apply in any location. The
;;; rule applicator does the actual work. We do this in two stages so
;;; that the proof driver can keep track of the path, depth, etc.

(defvar *rule-matches@locs* nil)

(defun find-rules@locations (expr)
  ;;(print `(:findrulesfor ,expr))
  (setq *rule-matches@locs* nil)
  (find-rules@locations2 expr ())
  ;;(print `(:found ,*rule-matches@locs* for ,expr))
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

(defparameter *vars* '((=1) (=2) (=3) (=4))) ;; Could do this more elegantly

(defun matches? (pat expr) ;; !!! Not right -- too simple -- needs to recurse !!!
  (cond ((null pat) (null expr))
	((assoc pat *vars*) expr)
	((atom pat) (eq pat expr))
	((and (listp pat) (listp expr)
	      (matches? (car pat) (car expr))
	      (matches? (cdr pat) (cdr expr))
	      ))))

(defun apply-rule@loc (expr rule loc)
  ;;(print `(:applying ,rule @ ,loc to ,expr))
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
;;;(trace rebuild find-rules@locations bind apply-rule@loc matches? prove replace@ extract@)

(print (prove '((4 over 2) / (2 over 6)) 6))
;;;(print (prove '(4 over 2) 2))
