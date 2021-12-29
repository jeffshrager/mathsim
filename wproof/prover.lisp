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

(defstruct proof name jpg given prove parts)
(defstruct part name steps)

(defvar *proofs*
  (list 
   (make-proof
    :name "Inverse Midsegment Proof"
    :jpg "imsp.png"
    :given "Given: CD=DA. and: CE=EB"
    :prove "Prove: DE || AB, and: DE = &frac12;AB"
    :parts
    (list
     (make-part :name "I: Prove that &Delta;CDE is similar to the larger &Delta;CAB by using the inverse SAS law<br> (i.e., from the observation that sides with a common ratio have an embedded equal angle)."
		:steps '(("&lt;C = &lt;C" "Relexivity")
			 ("CA=CD+DA, and CE=CE+EB" "Segment Addition")
			 ("CD=DA. and: CE=EB" "Given")
			 ("CA=CD+CD, and CE=CE+CE" "Substutition of #3 into #2")
			 ("CA=2CD, and CE=2CE" "Combing common terms")
			 ("&Delta;CDE&asymp;&Delta;CAB" "Ratio SAS" "This is a use of SAS that concludes similarity instead of congruence. If the ratio of the sides between the two triangles is the same in the case of both sides (and you have an internal angle, as usual), then you can conclude that the triangles are similar with the observed ratio. (Note that, this is actually the same as SAS for congruence, but in that case, the ratio is 1")
			 ))
     (make-part :name "II: Prove that DE = &frac12;AB by using similarity, proved above, to apply the commmon ratio DE and AB."
		:steps '(("s:baz" "r:frob") ("s:foo" "r:bar") ("s:bozo" "r:nono") ("s:waka" "r:kawa")))
     (make-part :name "III: Prove that DE || AB by using similarity, as above, to show corresponding equal angles,<br>and then use the inverse of the corresponding angles across parallel line lemma<br>to conclude that the lines are parallel." 
		:steps '(("s:fee" "r:fii") ("s:foe" "r:from"))))
    )))

(defun gen-body (proof o)
  (outbr!  (format nil "<h2>~a</h2>" (proof-name proof)))
  (outbr! (format nil "<image src=~s></image>" (proof-jpg proof)))
  (outbr! (proof-given proof))
  (outbr! (proof-prove proof))
  (loop for part in (proof-parts proof)
	with n = 0
	do (setf n (gen-part part o n))))

(defun gen-part (part o n)
  (out! (format nil "<button class=\"accordion\">~a</button>~%<div class=\"panel\">" (part-name part)))
  (out! "<table border=1>")
  (let* ((steps (part-steps part))
	 (statements (mapcar #'first steps))
	 (reasons (mapcar #'second steps)))
    (loop for (statement reason explanation) in steps
	  do
	  (out! (format nil "<tr><td>~a</td><td>" (incf n)))
	  (selections statements o)
	  (out! "</td><td>")
	  (selections reasons o)
	  (out! "</td><td>")
	  (when explanation (out! (format nil "<div class=\"tooltip\">&nbsp;&nbsp;&nbsp;?&nbsp;<span class=\"tooltiptext\">~a</span></div>" explanation)))
	  (out! "</td></tr>"))
    (out! "</table></div>")
    )
  n)


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
