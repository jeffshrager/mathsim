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

(defvar *proofs* nil)

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

(defun shuffle (orginial-sequence)
  (let ((sequence (copy-list orginial-sequence)))
    (loop for i from (length sequence) downto 2
          do (rotatef (elt sequence (random i))
                      (elt sequence (1- i))))
    sequence))

(defun selections (ss o selection mode &aux (choose? t))
  (out! "<select name=\"statements\" id=\"statements\">")
  (loop for s in (if (eq mode :quiz) (shuffle ss) ss)
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
  (let* ((proof (eval (with-open-file (i (format nil "proofs/~a/~a.proof" short-name short-name)) (read i)))))
    (with-open-file
     (o (format nil "proofs/~a/~a_r.html" short-name short-name) :direction :output :if-exists :supersede)
     (render-top o)
     (render-body proof o :reveal)
     (render-bottom o)
     )
    (with-open-file
     (o (format nil "proofs/~a/~a_q.html" short-name short-name) :direction :output :if-exists :supersede)
     (render-top o)
     (render-body proof o :quiz)
     (render-bottom o)
     )))

(render-proof "imsp")
