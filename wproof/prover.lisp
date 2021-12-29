;;; (load (compile-file "prover.lisp"))

(defmacro out! (text)
  `(format o "~a~%" ,text))
(defmacro outbr! (text)
  `(format o "~a<br>~%" ,text))

(defun gen-top (o)
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

(defun gen-bottom (o)
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

(defstruct proof name jpg given prove parts notes)
(defstruct part name steps)

(defvar *proofs*
  (list 
   (make-proof
    :name "Inverse Midsegment Proof"
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
			 (4 "CA=CD+CD, and CE=CE+CE" "Substutition of #3 into #2")
			 (5 "CA=2CD, and CE=2CE" "Combing common terms")
			 (6 "&Delta;CDE&asymp;&Delta;CAB" "Ratio SAS" "This is a use of SAS that concludes similarity instead of congruence. If the ratio of the sides between the two triangles is the same in the case of both sides (and you have an internal angle, as usual), then you can conclude that the triangles are similar with the observed ratio. (Note that, this is actually the same as SAS for congruence, but in that case, the ratio is 1<br><image src=imsp6.png>...</image>")
			 ))
     (make-part :name "II: Use similarity, proved above, by appling the commmon ratio between DE and AB to conclude that DE = &frac12;AB."
		:steps '((7 "s:baz" "r:frob")
			 (8 "s:foo" "r:bar")
			 (9 "s:bozo" "r:nono")
			 (10 "s:waka" "r:kawa")))
     (make-part :name "III: Prove that DE || AB by using similarity, as above, to show corresponding equal angles,<br>and then use the inverse of the corresponding angles across parallel line lemma<br>to conclude that the lines are parallel." 
		:steps '((11 "s:fee" "r:fii")
			 (12 "s:foe" "r:from"))))
    )))

(defun gen-body (proof o)
  (outbr!  (format nil "<h2>~a</h2>" (proof-name proof)))
  (outbr! (format nil "<table><tr><td><image src=~s></image></td><td>~a</td></tr></table>" (proof-jpg proof) (proof-notes proof)))
  (outbr! (proof-given proof))
  (outbr! (proof-prove proof))
  (loop for part in (proof-parts proof)
	do (gen-part part o)))

(defun gen-part (part o)
  (out! (format nil "<button class=\"accordion\">~a</button>~%<div class=\"panel\">" (part-name part)))
  (out! "<table border=1>")
  (let* ((steps (part-steps part))
	 (statements (mapcar #'second steps))
	 (reasons (mapcar #'third steps)))
    (loop for (n statement reason explanation) in steps
	  do
	  (out! (format nil "<tr><td>~a</td><td>" n))
	  (selections statements o)
	  (out! "</td><td>")
	  (selections reasons o)
	  (out! "</td><td>")
	  (when explanation (out! (format nil "<div class=\"tooltip\">&nbsp;&nbsp;&nbsp;?&nbsp;<span class=\"tooltiptext\">~a</span></div>" explanation)))
	  (out! "</td></tr>"))
    (out! "</table></div>")
    ))


(defun selections (ss o)
  (out! "<select name=\"statements\" id=\"statements\">")
  (loop for s in ss
	do (out! (format nil "<option value=~s>~a</option>" s s)))
  (out! "</select>")
  )

(with-open-file
 (o "prover.html" :direction :output :if-exists :supersede)
 (gen-top o)
 (gen-body (first *proofs*) o)
 (gen-bottom o)
 )
