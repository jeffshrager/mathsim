;;; sbcl --eval '(load (compile-file "prover.lisp"))'

;;; To Do:
;;;   Do we want the outline (col 1) choices to be selectable in quiz mode? (This will be complicated!)
;;;   Add check functionality (also complex)
;;;   (Might as well go to a server! ... I'll need to go there eventually!)

(defvar *choices-and-ids* nil)

(defmacro out! (text)
  `(format o "~a~%" ,text))
(defmacro outbr! (text)
  `(format o "~a<br>~%" ,text))

(defun render-top (o)
  (out! "
<!DOCTYPE html>
<html>
<head>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<style>
.tooltip {
  display: inline-block;
  width: 450px;
  position: absolute;
  z-index: 1;
}

.tooltip .tooltiptext {
  visibility: hidden;
  width: 75px;
  background-color: #f8ecf2;
  color: #000;
  text-align: left;
  opacity: 0;
  transition: opacity 1.5s;
}

.tooltip:hover .tooltiptext {
  visibility: visible;
  opacity: 1;
}
</style>
</head>
<body>
"))

(defun render-bottom (o)
  (out!  "
<br><hr><br><button onclick=\"checkproof()\">Check Proof</button>
<script>
	 function checkproof() {
  	 let choices = [];
	 ")
  (loop for (target-choice . id) in *choices-and-ids*
	do (out! (format nil "choices.push([~s,~s,document.getElementById(~s).value]);~%" target-choice id id)))
  (out!
   "
     let count = 0
     for (index = 0; index < choices.length; index++) {
      if ((choices[index][0]==choices[index][2]) && (choices[index][0] != \"choose\")) {count++};
      }
     alert(count);
     alert(choices);
     } 	 
</script>
</body>
</html>
"))

(defstruct proof name short-name jpg notes given prove parts)
(defstruct part name steps)

(defvar *proofs* nil)

(defun render-body (proof o mode)
  (outbr!  (format nil "<h2>~a</h2>" (proof-name proof)))
  (outbr! (format nil "<table><tr><td><image src=~s></image></td><td>~a</td></tr></table>" (proof-jpg proof) (proof-notes proof)))
  (outbr! (proof-given proof))
  (outbr! (proof-prove proof))
  (outbr! "<hr>")
  (loop for part in (proof-parts proof)
	as pn from 1 by 1
	do (render-part part o mode pn)))

(defun render-part (part o mode pn)
  (out! "<table border=1>")
  (out! (format nil "<tr><td width=250px; style=\"text-align:left;vertical-align:top;background-color:#f2d9e6\">~%~a~%</td><td><table>~%" (part-name part)))
  (let* ((steps (part-steps part))
	 (statements (mapcar #'second steps))
	 (reasons (mapcar #'third steps)))
    (out! "<tr><td></td><td>Statement</td><td>Rationale</td></tr>")
    (loop for (n target-statement target-reason explanation) in steps
	  do
	  (out! (format nil "<tr><td>~a</td><td>" n))
	  (render-pulldown statements o target-statement mode :s pn n)
	  (out! "</td><td>")
	  (render-pulldown reasons o target-reason mode :r pn n)
	  (out! "</td><td>")
	  (when (and (eq mode :study) explanation)
	    (out! (format nil "<div class=\"tooltip\">&nbsp;&nbsp;&nbsp;?&nbsp;<span class=\"tooltiptext\">~a</span></div>" explanation)))
	  (out! "</td></tr>"))
    (out! "</table></td></table>")
    ))

(defun render-pulldown (all-choices-in-correct-order o target-choice mode s/r pn n &aux (choose? t))
  (let ((key (format nil "~a_~a_~a" s/r pn n)))
    (out! (format nil "<select name=~s id=~a>" target-choice key))
    (push (cons target-choice key) *choices-and-ids*)
    (loop for s in (if (eq mode :quiz) (shuffle all-choices-in-correct-order) all-choices-in-correct-order)
	  do (if (eq mode :quiz)
		 (progn 
		   (when choose?
		     (out! "<option value=choose selected>Choose!</option>")
		     (setf choose? nil))
		   (out! (format nil "<option value=~s>~a</option>" s s))
		   )
	       ;; Default mode (:study)
	       (if (string-equal s target-choice)
		   (out! (format nil "<option value=~s selected>~a</option>" s s))
		 (out! (format nil "<option value=~s>~a</option>" s s)))))
    (out! "</select>")
    ))

(defun shuffle (orginial-sequence)
  (let ((sequence (copy-list orginial-sequence)))
    (loop for i from (length sequence) downto 2
          do (rotatef (elt sequence (random i))
                      (elt sequence (1- i))))
    sequence))

(defun render-proof (short-name) ;; mode is :quiz or :study (study is the default)
  (let* ((proof (eval (with-open-file (i (format nil "proofs/~a/~a.proof" short-name short-name)) (read i)))))
    (loop for mode in '(:study :quiz)
	  do 
	  (setf *choices-and-ids* nil)
	  (with-open-file
	   (o (string-downcase (format nil "proofs/~a/~a_~a.html" short-name short-name mode)) :direction :output :if-exists :supersede)
	   (render-top o)
	   (render-body proof o mode)
	   (render-bottom o)
     ))))

(render-proof "imsp")
