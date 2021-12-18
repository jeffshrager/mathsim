;;; (load "refract.lisp")

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
    (:foil++ ((a + b) * (c + d)) (((a * c) + (b * c) + (a * d) + (b * d))))
    (:dbmoif ((a over b) / (c over d)) ((a over b) * (d over c))) ;; divide-by-multiplication-of-inverse-fraction
    (:fad (a / b) (a over b)) ;; Fractionalize a division
    (:daf (a over b) (a / b)) ;; Divisionalize a fraction
    ;; Here's where the contentious knowledge enters into the picture
    (:dfbiam ((a over b) / (c over d))) ;; divide fractions by inverting and multiplying *** This is the key piece of knowledge !!!
    (:over1 (:integer a) (a over 1)) ;; any integer n can be represented as n/1 (WWW Matcher must understsand (:integer a)!)
    ))

(defparameter *pathlimit* 10)

(defun run (expr goal &aux path)
  (loop until (or (treequal expr goal)
		  ;; Temporarily just use a numerical limit for testing FFF DDD 
		  (>= (length path) *pathlimit*)) 
	as expr = (try-rules expr)
	do
	(setf expr (compress-simple-math expr))
	(push expr path)
	))

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
  (let ((newexpr
	 (loop with expr = oldexpr
	       as nexpr = (simplify1 expr)
	       until (equal expr nexpr)
	       finally (return nexpr)
	       do (setf expr nexpr))))
    (if (equal newexpr oldexpr) oldexpr
      (progn (print `(:simplified ,oldexpr ,newexpr)) newexpr))))

