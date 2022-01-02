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
<script>
   let score = 100;")
  (out! (format nil "
   let max = ~a;" (length *choices-and-ids*)))
  (out!
   "
   function checkproof() {
   let choices = [];")
  (loop for (target-choice . id) in *choices-and-ids*
	do (out! (format nil "choices.push([~s,~s,document.getElementById(~s).value]);" target-choice id id)))
  (out!
   "
     let count = 0
     for (index = 0; index < choices.length; index++) {
       if ((choices[index][0]==choices[index][2]) && (choices[index][0] != \"choose\")) {count++};
       }
     score = score - (max - count)
     document.getElementById(\"thescore\").value = score;
     alert(\"You have \" + count + \" correct choices out of \" + max +\". Your score is now: \" + score + \" out of 100.\");
     } 	 
  ")
  (out! "
   function turnin(){
     if (document.getElementById(\"thescore\").value==\"Not Checked\"){
       alert(\"You need to check the proof before turning it in!\")
       }
       else{alert(\"Automatic turn-in is not yet implemented. Call your teacher or proctor over and show them your score.\")}
    }
")
  (out!
   "
function setSelectedIndex(s, v) {
    s.options[0].selected = true;
    for ( var i = 0; i < s.options.length; i++ ) {
        s.options[i].selected = false;
        if ((s.options[i].value == v) && (Math.random()<0.666)) {
            s.options[i].selected = true;
            return;
        }
    }
}
   function randomize() {
   ")
  (loop for (target-choice . id) in *choices-and-ids*
	do (out! (format nil "setSelectedIndex(document.getElementById(~s),~s);" id target-choice)))
  (out!
   "
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
  (outbr! "<hr>")
  (out! (proof-given proof))
  (outbr! (proof-prove proof))
  (outbr! "<hr>")
  (when (member mode '(:quiz :practice))
    (out! "<button onclick=\"checkproof()\" style=\"font-size: 20px; height:40px; width:200px; background-color: #4dff88;\">Check Proof</button>")
    (outbr! "<font style=\"font-size: 20px;\">Cumulative Score:</font> <input type=\"text\" id=\"thescore\" READONLY value=\"Not Checked\" style=\"font-size: 20px; hight:40px; width:200px;\">"))
  (when (eq mode :quiz) (out! "<button onclick=\"turnin()\" style=\"font-size: 20px; height:40px; width:200px; background-color: red;\">Turn In</button>"))
  (when (eq mode :practice) (out! "<button onclick=\"randomize()\" style=\"font-size: 20px; height:40px; width:200px; background-color: lightblue;\">Randomize</button>"))
  (outbr! "<hr>")
  (loop for part in (proof-parts proof)
	as part-number from 1 by 1
	do (render-part part o mode part-number)))

(defun render-part (part o mode part-number)
  (out! "<table border=1>")
  (out! (format nil "<tr><td width=250px; style=\"text-align:left;vertical-align:top;background-color:#f2d9e6\">~%~a~%</td><td><table>~%" (part-name part)))
  (let* ((steps (part-steps part))
	 (statements (mapcar #'second steps))
	 (reasons (mapcar #'third steps)))
    (out! "<tr><td></td><td>Statement</td><td>Rationale</td></tr>")
    (loop for (step-number target-statement target-reason explanation) in steps
	  do
	  (out! (format nil "<tr><td>~a</td><td>" step-number))
	  (render-pulldown statements o target-statement mode :s part-number step-number)
	  (out! "</td><td>")
	  (render-pulldown reasons o target-reason mode :r part-number step-number)
	  (out! "</td><td>")
	  (when (and (eq mode :study) explanation)
	    (out! (format nil "<div class=\"tooltip\">&nbsp;&nbsp;&nbsp;?&nbsp;<span class=\"tooltiptext\">~a</span></div>" explanation)))
	  (out! "</td></tr>"))
    (out! "</table></td></table>")
    ))

;;; FFF UUU This choice-key thing is an ugly hack and needs to be cleaned up.

(defun render-pulldown (all-choices-in-correct-order o target-choice mode s/r part-number step-number &aux (choose? t))
  (let ((pulldown-key (format nil "~a_~a_~a" s/r part-number step-number))
	(target-choice-key (format nil "c_~a" (loop for c in all-choices-in-correct-order
						    as n from 1 by 1
						    when (string-equal target-choice c)
						    do (return n))))
	)
    (out! (format nil "<select name=~s id=~a>" target-choice pulldown-key))
    (push (cons target-choice-key pulldown-key) *choices-and-ids*)
    (loop for s in (if (eq mode :quiz) (shuffle all-choices-in-correct-order) all-choices-in-correct-order)
	  as choice-key = (format nil "c_~a" (loop for c in all-choices-in-correct-order
						   as n from 1 by 1
						   when (string-equal s c)
						   do (return n)))
	  do (if (member mode '(:quiz :practice))
		 (progn 
		   (when choose?
		     (out! "<option value=choose selected>Choose!</option>")
		     (setf choose? nil))
		   (out! (format nil "<option value=~s>~a</option>" choice-key s))
		   )
	       ;; Default mode (:study)
	       (if (string-equal s target-choice)
		   (out! (format nil "<option value=~s selected>~a</option>" choice-key s))
		 (out! (format nil "<option value=~s>~a</option>" choice-key s)))))
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
    (loop for mode in '(:study :quiz :practice)
	  do 
	  (setf *choices-and-ids* nil)
	  (with-open-file
	   (o (string-downcase (format nil "proofs/~a/~a_~a.html" short-name short-name mode)) :direction :output :if-exists :supersede)
	   (render-top o)
	   (render-body proof o mode)
	   (render-bottom o)
     ))))

(render-proof "imsp")
