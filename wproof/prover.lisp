;;; (load (compile-file "prover.lisp"))

(defmacro out! (text)
  `(format o "~a~%" ,text))
(defmacro outbr! (text)
  `(format o "~a<br>~%" ,text))

(defun gen-top (o)
  (out!  "<!DOCTYPE html>")
  (out!  "<html>")
  (out!  "<head>")
  (out!  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">")
  (out!  "<style>")
  (out!  ".accordion {")
  (out!  "background-color: #eee;")
  (out!  "color: #444;")
  (out!  "cursor: pointer;")
  (out!  "padding: 18px;")
  (out!  "width: 100%;")
  (out!  "border: none;")
  (out!  "text-align: left;")
  (out!  "outline: none;")
  (out!  "font-size: 15px;")
  (out!  "transition: 0.4s;")
  (out!  "}")
  (out!  "")
  (out!  ".active, .accordion:hover {")
  (out!  "background-color: #ccc; ")
  (out!  "}")
  (out!  "")
  (out!  ".panel {")
  (out!  "padding: 0 18px;")
  (out!  "display: none;")
  (out!  "background-color: white;")
  (out!  "overflow: hidden;")
  (out!  "}")
  (out!  "</style>")
  (out!  "</head>")
  (out!  "<body>")
  (out!  "")
  )

(defun gen-bottom (o)
  (out!  "")
  (out!  "<script>")
  (out!  "var acc = document.getElementsByClassName(\"accordion\");")
  (out!  "var i;")
  (out!  "")
  (out!  "for (i = 0; i < acc.length; i++) {")
  (out!  "acc[i].addEventListener(\"click\", function() {")
  (out!  "this.classList.toggle(\"active\");")
  (out!  "var panel = this.nextElementSibling;")
  (out!  "if (panel.style.display === \"block\") {")
  (out!  "panel.style.display = \"none\";")
  (out!  "} else {")
  (out!  "panel.style.display = \"block\";")
  (out!  "}")
  (out!  "});")
  (out!  "}")
  (out!  "</script>")
  (out!  "")
  (out!  "</body>")
  (out!  "</html>")
  (out!  "")
  (out!  "")
  (out!  "")
  )

(defstruct proof name jpg given prove parts)
(defstruct part name steps)

(defvar *proof*
  (make-proof
   :name "Inverse Midsegment Proof"
   :jpg "imsp.png"
   :given "Given: CD=DA. and: CE=EB"
   :prove "Prove: DE || AB, and: DE = &frac12;AB"
   :parts
   (list
    (make-part :name "I: Prove that &Delta;CDE is similar to the larger &Delta;CAB by using the inverse SAS law<br> (i.e., from the observation that sides with a common ratio have an embedded equal angle)."
	       :steps '(("s:foo" "r:bar") ("s:baz" "r:frob")))
    (make-part :name "II: Prove that DE = &frac12;AB by using similarity, proved above, to apply the commmon ratio DE and AB."
	       :steps '(("s:baz" "r:frob") ("s:foo" "r:bar") ("s:bozo" "r:nono") ("s:waka" "r:kawa")))
    (make-part :name "III: Prove that DE || AB by using similarity, as above, to show corresponding equal angles,<br>and then use the inverse of the corresponding angles across parallel line lemma<br>to conclude that the lines are parallel." 
	       :steps '(("s:fee" "r:fii") ("s:foe" "r:from"))))
   ))

(defun gen-body (proof o)
  (outbr!  (format nil "<h2>~a</h2>" (proof-name proof)))
  (outbr! (format nil "<image src=~s></image>" (proof-jpg proof)))
  (outbr! (proof-given proof))
  (outbr! (proof-prove proof))
  (loop for part in (proof-parts proof)
	do (gen-part part o)))

(defun gen-part (part o)
  (out! (format nil "<button class=\"accordion\">~a</button>~%<div class=\"panel\">" (part-name part)))
  (out! "<table>")
  (let* ((steps (part-steps part))
	 (statements (mapcar #'first steps))
	 (reasons (mapcar #'second steps)))
    (loop for (statement reason) in steps
	do
	(out! "<tr><td>")
	(selections statements o)
	(out! "</td><td>")
	(selections reasons o)
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
 (gen-body *proof* o)
 (gen-bottom o)
 )
