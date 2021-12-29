;;; (load (compile-file "prover.lisp"))

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
.accordion {
background-color: #eee;
color: #444;
cursor: pointer;
padding: 18px;
width: 100%;
border: none;
text-align: left;
outline: none;
font-size: 15px;
transition: 0.4s;
}

.active, .accordion:hover {
background-color: #ccc; 
}

.panel {
padding: 0 18px;
display: none;
background-color: white;
overflow: hidden;
}
.tooltip {
  position: relative;
  display: inline-block;
  border-bottom: 1px dotted black;
  /* Position the tooltip */
  position: absolute;
  z-index: 1;
}

.tooltip .tooltiptext {
  visibility: hidden;
  width: 120px;
  background-color: black;
  color: #fff;
  text-align: center;

}
.tooltip:hover .tooltiptext {
  visibility: visible;
}
</style>
</head>
<body>
"))

(defun render-bottom (o)
  (out!  "
<script>
var acc = document.getElementsByClassName(\"accordion\");
var i;

for (i = 0; i < acc.length; i++) {
acc[i].addEventListener(\"click\", function() {
this.classList.toggle(\"active\");
var panel = this.nextElementSibling;
if (panel.style.display === \"block\") {
panel.style.display = \"none\";
} else {
panel.style.display = \"block\";
}
});
}
</script>
</body>
</html>
"))

(defstruct proof name short-name jpg notes given prove parts)
(defstruct part name steps)

(defvar *proofs*
  (list 
   (make-proof
    :name "Inverse Midsegment Proof"
    :short-name "imsp"
    :jpg "imsp.png"
    :given "Given: CD=DA. and: CE=EB"
    :prove "Prove: DE || AB, and: DE = &frac12;AB"
    :notes "A midsegment is a line parallel to one side of a triangle, connecting the midpoints of the two sides. Interestingly, the midsegment is half the length of the corresponding side (even though it doesn't look it -- that's an illusion...measure it!) Normally you're given the parallel and midpoint facts and have to prove that the large triangle and the smaller triangle created by the midsegment are similar, and then the illusory length ratio. Here we give the midpoint facts, but not the parallelism of the midsegment with the corresponding side of the larger triangle, and have to prove that the parallelism, as well as the ratio." 
    :parts
    (list
     (make-part :name "I: Prove that &Delta;CDE is similar to the larger &Delta;CAB by using the inverse SAS law<br> (i.e., from the observation that sides with a common ratio have and an embedded equal angle are similar)."
		:steps '((1 "&lt;C = &lt;C" "Relexivity")
			 (2 "CA=CD+DA, and CE=CE+EB" "Segment Addition")
			 (3 "CD=DA. and: CE=EB" "Given")
			 (4 "CA=CD+CD, and CE=CE+CE" "Substitution of #3 into #2")
			 (5 "CA=2CD, and CE=2CE" "Combing common terms")
			 (6 "&Delta;CDE&asymp;&Delta;CAB" "Ratio SAS" "This is a use of SAS that concludes similarity instead of congruence. If the ratio of the sides between the two triangles is the same in the case of both sides (and you have an internal angle, as usual), then you can conclude that the triangles are similar with the observed ratio. (Note that, this is actually the same as SAS for congruence, but in that case, the ratio is 1<br><image src=imsp6.png>...</image>")
			 ))
     (make-part :name "II: Use similarity, proved above, by appling the commmon ratio between DE and AB to conclude that DE = &frac12;AB."
		:steps '((7 "DE and AB are corresponding parts similar triangles" "Definition of corresponding parts")
			 (8 "AB=2DE" "Ratios of corresponding parts of similar triangles are equal")
			 (10 "DE = &frac12;AB" "Algebra (division, commutativity of =)")))
     (make-part :name "III: Prove that DE || AB by using similarity, as above, to show corresponding equal angles,<br>and then use the inverse of the corresponding angles across parallel line lemma<br>to conclude that the lines are parallel." 
		:steps '((11 "&lt;CDE = &lt;CABs" "Inverse AAA similarity." "Since we can conclude similarity from AAA (all the same angles), we can use AAA the other way and conclude that all the angles of similar trianlges are equal.")
			 (12 "DE||AB" "Inverse corresponding angles across parallel lines" "Again, we're using a theorem in the opposite direction that we usually use it. We usually use parallel lines and a transecting third line to conclude that corresponding angles are equal. Here we're doing the opposite: Concluding that the lines are parallel because we have found equal corresponding angles."))))
    )))

(defun render-body (proof o mode)
  (outbr!  (format nil "<h2>~a</h2>" (proof-name proof)))
  (outbr! (format nil "<table><tr><td><image src=~s></image></td><td>~a</td></tr></table>" (proof-jpg proof) (proof-notes proof)))
  (outbr! (proof-given proof))
  (outbr! (proof-prove proof))
  (loop for part in (proof-parts proof)
	do (render-part part o mode)))

(defun render-part (part o mode)
  (out! (format nil "<button class=\"accordion\">~a</button>~%<div class=\"panel\">" (part-name part)))
  (out! "<table border=1>")
  (let* ((steps (part-steps part))
	 (statements (mapcar #'second steps))
	 (reasons (mapcar #'third steps)))
    (loop for (n statement reason explanation) in steps
	  do
	  (out! (format nil "<tr><td>~a</td><td>" n))
	  (selections statements o statement mode)
	  (out! "</td><td>")
	  (selections reasons o reason mode)
	  (out! "</td><td>")
	  (when (and (eq mode :reveal) explanation)
	    (out! (format nil "<div class=\"tooltip\">&nbsp;&nbsp;&nbsp;?&nbsp;<span class=\"tooltiptext\">~a</span></div>" explanation)))
	  (out! "</td></tr>"))
    (out! "</table></div>")
    ))

(defun selections (ss o selection mode &aux (choose? t))
  (out! "<select name=\"statements\" id=\"statements\">")
  (loop for s in ss
	do (if (eq mode :quiz)
	       (progn 
		 (when choose?
		   (out! "<option value=choose selected>Choose!</option>")
		   (setf choose? nil))
		 (out! (format nil "<option value=~s>~a</option>" s s))
		 )
	     ;; Default mode (:reveal)
	     (if (string-equal s selection)
		 (out! (format nil "<option value=~s selected>~a</option>" s s))
	       (out! (format nil "<option value=~s>~a</option>" s s)))))
  (out! "</select>")
  )

(defun render-proof (short-name) ;; mode is :quiz or :reveal (reveal is the default)
  (let* ((proof (find short-name *proofs* :key #'proof-short-name :test #'string-equal))
	 )
    (with-open-file
     (o (format nil "~a_r.html" short-name) :direction :output :if-exists :supersede)
     (render-top o)
     (render-body proof o :reveal)
     (render-bottom o)
     )
    (with-open-file
     (o (format nil "~a_q.html" short-name) :direction :output :if-exists :supersede)
     (render-top o)
     (render-body proof o :quiz)
     (render-bottom o)
     )))

(render-proof "imsp")