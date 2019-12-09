(require "/users/shrager/lib/utils")

;;; Some important global defs (and resets for reloading!)

(setq *perr* 1) ; probability of making counting errors (out of 100)

(setq *tl* 1) ; trace level -- 0 means off

(setq *tc* ()) ; the TOP chain, just an op list

(setq *saved-ms-cycles* 0) ; incf'ed whenever the SS does the work for us.
(setq *strat* ()) ; this is selected by select-strategy
(setq *ncycles* 0) ; counts number of steps taken.

;;; Globals that deal with strategies.

(setq *stb* ()) ; The table of strategies.
(setq *default-confidence* 0.1) ; confidence of an initial or newly discovered stratagy
(setq *novelty-confidence-incr* 0.1) ; kick given to newly discovered strats.

;;; Globals that deal with the subcognitive association chaining.

(setq *chain* ()) ; this is the actual subcognitive associative store
(setq *xthresh* 0) ; set at run time tells us how high transitions are accepted
(setq *xincr* .005) ; How much to increment chain entries on use...
(setq *xinit* .01) ; ...and where they begin.

;;; Globals that deal with the relationships of strategies to 
;;; problem characteristics.

(setq *new-default-confidence* .02) ; for a new problem's characteristic
(setq *conf-incf-on-correct* .02) ; add this when the strategy works.
(setq *conf-decf-on-wrong*   .01) ; and remove this when it doesn't.

(setq *records* ()) ; this gets loaded with the records of the run

;;; Globals that deal which the table of associations.

(setq *memtbl* ()) ; the siegler & shrager table of associations
(setq *memtbl-incr-right* .05) ; add this when you get a problem right
(setq *memtbl-incr-wrong* .03) ; and this when you get one wrong

;;; Globals that deal with meta-cognitive goals.

(setq *mgoal* ()) ; The mgoal that is currently active.
(setq *mgoals* ()) ; The possible mgoals for a given run.
(setq *global-mgoals* ())
(setq *global-mgoals* ()) ; in case of reload

;;; Globals that deal with the short term memory stack.

(setq *memstack* ()) ; The actual memory stack....
(setq *memory-stack-limit* 80) ; ...and how far back it retains traces

;;; Various meta-cognitive control globals.

(setq *solution-completed* ()) ; this tells the driver to stop.

#| 960517 - First attempt to actually do strategy change.  Simply add
   a meta goal to assert the sum strategy and give it a "novelty" boost. |#

#| 960516 - Added meta-level rules to check up on what's going on.  For
   openers, we just check that the system's not in a loop by checking that 
   the hand we're processing hasn't been processed before when we do the
   swaitch.  In order to do this we have to add some memory, which will
   sereve us also in the future.  The current memory scheme records the last
   *memory-stack-limit* images of the world; this is loaded automatically
   whenever anything is done in the world, but can be examined ONLY by
   metacognitive work. |#

;;; This does the work of putting an image of the world into the
;;; memory stack.  It is given an action symbol to tag this stack
;;; entry and a list of state information. Also, old stack stuff is
;;; removed.  Note that the stack, which is just an alist of
;;; (action-symbols . state) is BACKWARDS: most recent first!  The
;;; theory is that whenever you take an action in the world you notice
;;; what you did and what the state of the things that you attended to
;;; were.

(defun mempush (action state)
  (push (cons action state) *memstack*)
  (do ((s *memstack* (cdr s))
       (i (1- *memory-stack-limit*) (1- i)))
      ((zerop i) (if s (rplacd s ())))))

(defun memclear ()
  (setq *memstack* ()))

#| 960515 - Added stuff to record how things go into a side-file. |#

(defun clear-records ()
  (setq *records* ())
  (clear-record))

;;; Each run record it a list of pairs made by record!  Note that the 
;;; *records* are in REVERSE ORDER OF THE RUN!  Some judicious reverses
;;; are called for!

(setq *record* ()) ; holds one run record

(defun clear-record ()
  (setq *record* ()))

(defun record! (id info)
  (push (list id info) *record*))

(defun save-record ()
  (push (reverse *record*) *records*)
  (clear-record))

;;; Be sure to reverse them, and fake a lisp image for convenient loading.

(defun save-records (file)
  (with-open-file (s file :direction :output :if-exists :new-version)
    (format s "(setq data '(~%")
    (mapcar #'(lambda (r) (print r s)) *records*)
    (format s "))~%")
  ))

#| 960514 - Added direct recall memory.  This is stored in an
associative array, just like in Siegler & Shrager, and is updated at
problem conclusion time, just like Siegler and Shrager.  And, just
like... it is consulted ... um, okay, so this is different; it's
consulted first, and then only when the cognitive system has some time
with nothing to do.  Note that the array is actually one wider in each
direction than the possible solutions; the zero index isn't used. |#

;;; Note (960626) the memtbl has to be able to accept results of >10
;;; (the nominal *correct* maximum sum) because the learner can get a
;;; wrong answer and needs to be able to store it.  But really wrong
;;; answers (> *mtlim*) are ignored.

(setq *mtlim* 20)

(defun init-memtbl (&aux i1)
  (setq *memtbl* (make-array (list 6 6 (1+ *mtlim*)) :initial-element 0.01))

  ;; Set in place all the 1+ problems with small positive associations.
  (dotimes (i 5)
    (setq i1 (1+ i))
    (incf (aref *memtbl* i1 1 (1+ i1)) 0.05)
    (incf (aref *memtbl* 1 i1 (1+ i1)) 0.05))

  ;; And all the run-on problems (like 1+2=3)
  (dotimes (i 4)
    (setq i1 (1+ i))
    (incf (aref *memtbl* i1 (1+ i1) (+ 2 i1)) 0.05))

  )

;;; When the problem is completed, update the memory table appropriately
;;; depending upon whether we got it right or wrong.

(defun update-memtbl (a1 a2 result)
  (if (< result *mtlim*)
      (incf (aref *memtbl* a1 a2 result)
	    (if (= result (+ a1 a2))
		*memtbl-incr-right*
	      *memtbl-incr-wrong*))))

(defun show-memtbl ()
  (dotimes (i 5)
    (dotimes (j 5)
      (format t "~a + ~a = " (1+ i) (1+ j))
      (dotimes (k 10)
        (format t "~a (~a), " (1+ k)
		(aref *memtbl* (1+ i) (1+ j) (1+ k)))
	)
      (format t "~%")
      )
    (format t "~%")
    ))

;;; Okay, now we need to be able to get values back; Feed a confidence
;;; critereon we return a guess that comes above the confidence
;;; critereon, or nothing if nothing comes above it.  First a guess is
;;; chosen at random from among the possible solutions in accord with
;;; their strenths.  But the guess is only stated if it is both chosen
;;; AND comes above the cc.

(defun guess (a1 a2 cc)
  (let ((guess (guess2 a1 a2)))
    (if (> (aref *memtbl* a1 a2 guess) cc) guess)))

(defun guess2 (a1 a2 &aux (sum 0.0) result (cume 0.0))
  (dotimes (i 10)
    (incf sum (aref *memtbl* a1 a2 (1+ i))))
  (let ((thresh (/ (random 100) 100.0)))
    (dotimes (i 10 result)
      (if (> (incf cume (/ (aref *memtbl* a1 a2 (1+ i)) sum)) thresh)
	  (progn (setq result (1+ i))
		 (return result)))
      )))

;;; Try-retrieval is the inline call for this.  At the moment is just
;;; gets a random cc and uses the global *ad1* and *ad2*. (This lets
;;; us easily call it inline as if it were a cognitive procedure.

(defun try-retrieval (&aux cc)
  (let ((g (guess *ad1* *ad2* (setq cc (/ (random 100) 100.0)))))
    (trp 1 "********** Trying retrieval w cc=~a and got ~a.~%"
	    cc g)
    (if g 
        (progn 
	  (setq *eb* g)
	  (record! 'Used-retrieval! *eb*)
	  (setq *solution-completed* t) ; tell the driver we succeded!
	  ))
    ))

#| 960513 - Added the STRAT struct that represents the information
about a strategy and its usage.  Also updated the testing &c functions
to run a series of problems, and to analyze problem types and update
strat information according to successes.  NOTE THAT A TON OF
SEEMINGLY DEAD STUFF WAS TAKEN OUT OF THIS AND BACKSTORED IN
ADD.HOLD.960513. |#

;;; An mgoal is bascially a production rule with a pattern and a right
;;; hand side which is just a list of function names in the meta
;;; system.  They're all named as mg.<name> for no good reason.

(defstruct (mgoal)
  name
  pattern
  procedures)

;;; Example mgoals; just for the general happiness of future programmers
;;; who, god forbid, might have to write these!
#|
(defun a-global-mgoal-fn1 () 
  (format t "Firing a-global-mgoal-fn1~%"))

(defun a-global-mgoal-fn2 () 
  (format t "Firing a-global-mgoal-fn2~%"))

(setq mg.a-global-mgoal 
  (make-mgoal :name 'a-global-mgoal
              :pattern #'(lambda () (y-or-n-p "Trying to fire a-global-mgoal? "))
              :procedures (list #'a-global-mgoal-fn1
				#'a-global-mgoal-fn2)))

(setq *global-mgoals* (list mg.a-global-mgoal))

(defun a-local-mgoal-fn1 () 
  (format t "Firing a-local-mgoal-fn1~%"))

(defun a-local-mgoal-fn2 () 
  (format t "Firing a-local-mgoal-fn2~%"))

(setq mg.a-local-mgoal 
  (make-mgoal :name 'a-local-mgoal
              :pattern #'(lambda () (y-or-n-p "Trying to fire a-local-mgoal? "))
              :procedures (list #'a-local-mgoal-fn1
				#'a-local-mgoal-fn2)))
|#

;;; Real mgoals.

;;; Global mgoals.

;;; Mgoal to notice in the trace a sequence as: n, 1, 2, 3, ... n, that is,
;;; where the n is repeated.  Notice that in the memstack this sequence
;;; will be reversed, since things are pushed onto it.

(defun look-for-shortcuts ()
  (trp 1 "********* Looking for a shortcut sequence!~%")
  (let ((lcs (find-longcuts *memstack*)))
    (if lcs
	(progn
	  (print "-----------shortcuts!-------------")
	  (mapcar #'print lcs)
	  (print "-----------shortcuts!-------------")
	  ))))

(defun find-longcuts (memstack)
  (cond ((null memstack) ())
	((and (eq 'say (caar memstack))
	      (is-a-longcut (car memstack) (car memstack) (cdr memstack)))
	 (cons (parse-out-longcut (car memstack) (cdr memstack) 
				  (list (car memstack)))
	       (find-longcuts (cdr memstack))))
	(t (find-longcuts (cdr memstack)))))

(defun is-a-longcut (target carry list &aux n)
  ;; Make (say #) -> #, in all of target, carry and list top (n).
  (let ((temp (car list)))
    (if (eq 'say (car temp)) (setq n (cadr temp))))
  (if (not (numberp carry)) (setq carry (cadr carry)))
  (if (not (numberp target)) (setq target (cadr target)))
  (cond ((null list) ())
	;; If we reach the target number, we're done.
        ((and n (= target n)) t)
	;; If it's the next down, loop.
	((and n (= (1- carry) n))
	 (is-a-longcut target (1- carry) (cdr list)))
	;; If it's a number, but not one of the above, fail!
	(n nil)
	;; Otherwise, anything else is ignored.
	(t (is-a-longcut target (1- carry) (cdr list)))
	))

(defun parse-out-longcut (target list collect &aux n)
  (let ((temp (car list)))
    (if (eq 'say (car temp)) (setq n (cadr temp))))
  (if (not (numberp target)) (setq target (cadr target)))
  (cond ((and n (= target n)) (push (car list) collect) collect)
	(t (parse-out-longcut target (cdr list) (push (car list) collect)))
	))

(setq mg.look-for-shortcuts
  (make-mgoal :name 'look-for-shortcuts
              :pattern #'(lambda () t) ; always possible
              :procedures (list #'look-for-shortcuts)))

(push mg.look-for-shortcuts *global-mgoals*)

;;; The shortcut sum strategy.  This is a bogus discovery, but it's
;;; only the first try!

(defun add-scsum ()
  (trp 1 "********** Added the scsum to the mix!~%")
  (push (make-strat :name 'scsum
                    :chain *scsum-strat*
                    :mgoals ()
		    :default-confidence (+ *default-confidence* 
					   *novelty-confidence-incr*)
		    )
	*stb*))

(setq mg.add-scsum
  (make-mgoal :name 'add-scsum
              ;; Don't re-add it if it's already there.
              :pattern #'(lambda () (not (find-strat-by-name 'scsum)))
              :procedures (list #'add-scsum)))

(defun find-strat-by-name (name)
  (find name *stb* :test #'(lambda (name strat)
			     (eq name (strat-name strat)))))

;;; Add it to the global metagoals.

;(push mg.add-scsum *global-mgoals*)

;;; The min strategy.  Also bogus!!

(defun add-minn ()
  (trp 1 "********** Added the minn to the mix!~%")
  (push (make-strat :name 'minn
                    :chain *minn-strat*
                    :mgoals ()
		    :default-confidence (+ *default-confidence* 
					   *novelty-confidence-incr*)
		    )
	*stb*))

(setq mg.add-minn
  (make-mgoal :name 'add-minn
              ;; Don't re-add it if it's already there, but ensure that
	      ;; scsum *IS* there!
              :pattern #'(lambda () (and (not (find-strat-by-name 'minn))
					 (find-strat-by-name 'scsum)))
              :procedures (list #'add-minn)))

;(push mg.add-minn *global-mgoals*)

;;; Here's the actual metagoal structure for loop stoppage.

;;; Loop detection global mgoal.  Notice that this has to take control
;;; by forcing an 'end! onto the stack and then reseting the xthreshold
;;; relly high to take control away from the peripheral system.  

(defun loop-stop ()
  (trp 1 "********** Checking for a loop.~%")
  (if (in-a-loop?)
      (progn 
	(trp 1 "!!!!!!!!!!!! Ya! We're in a loop!  Bail out now!!!!!!!~%")
	(record! 'Loop-stopped-by-meta-cognitive-system! ())
	(push 'end! *tc*)
       ;(print *tc*)
	(setq *xthresh* 100.0) ; should be high enough! :))
	))
  )

;;; At the moment the definition of a loop is that we swap hands more
;;; than twice to the same hand.

(defun in-a-loop? ()
  (or (remember-doing-more-than 2 'swap-hands)
      (remember-doing-more-than 2 'choose-hand)
      (remember-doing-more-than 2 'raise)
      ))

(defun remember-doing-more-than (n op)
  (> (count op *memstack* 
		:test #'(lambda (x e) (eq x (car e))))
	 n))

(setq mg.loop-stop
  (make-mgoal :name 'loop-stop
              :pattern #'(lambda () t) ; always possible
              :procedures (list #'loop-stop)))

;;; Add it to the global metagoals.

(push mg.loop-stop *global-mgoals*)

;;; Global mgoal to try retrieval.

(setq mg.try-retrieval
  (make-mgoal :name 'try-retrieval
              :pattern #'(lambda () t) ; always possible
              :procedures (list #'try-retrieval)))

;;; Set up the gobal mgoals, which have been defined above, we hope!

(push mg.try-retrieval *global-mgoals*)

;;; 

(defstruct (strat)
  name
  mgoals
  chain
  confidences
  default-confidence)

;;; Initialize the strategy database.

(defun init-strats ()
  (setq *stb* ())
  (push (make-strat :name 'sum
                    :chain *sum-strat*
                    :mgoals ()
		    :default-confidence *default-confidence*)
	*stb*))

;;; Select a strategy for the present problem, given the addends.
;;; (Right now it just grabs a strategy at random!  Eventually this
;;;  has to get done on a problem-based basis.)

(defun select-strategy (a1 a2 &aux maybe-strats)
  (let ((probchars (characterize-problem a1 a2)))
    (nth (random (length *stb*)) *stb*)))

;;; A confidence tells us what confidence criteria to apply to a
;;; strategy depending upon features of the problem.  The problem
;;; features are given by characterize-problem and go into the
;;; features slot as a list of atoms. 

(defstruct (confidence)
  features ; a list of atoms returned by characterize-problem
  value ; an integer
  )

;;; Characterize-problem returns a list of atoms that can be used
;;; to determine what kind of problem this is.

(defun characterize-problem (addend1 addend2 &aux r)
  (if (= addend1 addend2) 
      (push 'equal-addends r)
      (if (> addend1 addend2) (push 'first-larger r)
	(push 'second-larger r))
      )
  (if (or (> addend1 3) (> addend2 3))
      (push 'bignums r)
      (push 'nobignums r))
  r)

#| An abstract model of the mediation learner.  This merely runs two
chains against one another, having one pick up portions of the task
from the other.  The top chain as a sequence of random entries from
the set of ops.  The bottom chain is more complicated, being a set of
entries that include the operator and a transition probability to a
next bottom chain item. |#

#| The *BC* (bottom chain) has a transition probability associated
with each entry.  The *XTHRESH* must be reached in order to strike out
on one's own initiative, and each time a trasntition is done, the
*XINCR* gets added to this item. |#

;;; Structures for a bottom chain entries

(defstruct (bci)
  operator xlist)

(defstruct (xition)
  xprob xbci)

#| Main testing function.  Just runs through problem after problem. |#

#| sets up a top chain with an initial strategy, clears the bottom
chain, and runs thru some number of trials. |#

(defun test (&key (nruns 100) (dc ()) (pause nil))
  (clear-records)
  (init-strats)
  (init-memtbl)
  (format t "~%Strategies initialized: ~a~%." (mapcar #'strat-name *stb*))
  (setq *bc* ()) ; reset for now (maybe later remove this)
  (dotimes (run nruns)

    (setq *ad1* (1+ (random 5)))
    (setq *ad2* (1+ (random 5)))
    (format t "------------------- Problem is ~a+~a.~%" *ad1* *ad2*)
    (record! 'problem (list *ad1* *ad2*))

    (setq *strat* (select-strategy *ad1* *ad2*))
    (format t "Selected to use ~a.~%" (strat-name *strat*))
    (record! 'strat (strat-name *strat*))

    ;; Get the confidence or default for this strategy in these problem
    ;; conditions.  The transition threshold is then (1-confidence), so
    ;; that when you're very confident you use low xthresholds

    (setq *xthresh*
      (let ((c (find-confidence *strat* (characterize-problem *ad1* *ad2*))))
	(if c
	    (- 1 (confidence-value c))
	    (- 1 (strat-default-confidence *strat*)))))
    (trp 1 "Transition threshold is ~a~%" *xthresh*)
    (record! 'xthresh *xthresh*)

    (setq *tc* (strat-chain *strat*))
    (if dc (display-chains))

    ;; Setup the meta goal list for this run.  This is the append of the
    ;; global mgoals and the and local metagoals for this strat.

    (setq *mgoal* ()) ; clear from left over previous runs.
                           ;; pro. doesn't need to be copied
    (setq *mgoals* (append (copy-list (strat-mgoals *strat*))
			   (copy-list *global-mgoals*)))

    (setq *saved-ms-cycles* 0)

    (memclear)

    (setq *ncycles* 0) 

    (drive)

    (if *eb* 
        (progn (update-memtbl *ad1* *ad2* *eb*)
	       (update-strat *strat* *ad1* *ad2* *eb*)))

    (record! 'result *eb*)

    (record! 'ncycles *ncycles*)

    (trp 1 "Number of saved MS cycles = ~a.~%" *saved-ms-cycles*)
    (record! 'saved-ms-cycles *saved-ms-cycles*)

    (save-record) ; be sure to log this run's info!

    (if (and pause (not (y-or-n-p "Continue?"))) (return 'done))
    )
  (summarize-records)
  (if (y-or-n-p "Write records to file?")
      (save-records (prompt-for-string "Filename: ")))
  'done
  )

;;; Update the statistics for the strategy.  If this sort of problem
;;; hasn't been done before, then we have to make a new confidence
;;; struct for its characteristcs, else just change the confidence
;;; values for the strategy on this sort of problem.

(defun update-strat (strat a1 a2 answer)
  (format t "** ~a + ~a = ~a ** ~%" a1 a2 answer)
  (let* ((chars (characterize-problem a1 a2))
	 (confidence (find-confidence strat chars)))
    (if confidence
	(update-confidence confidence (= (+ a1 a2) answer))
          (let ((newconf (make-confidence :features chars
					  :value *new-default-confidence*)))
	    (update-confidence newconf (= (+ a1 a2) answer))
	    (push newconf (strat-confidences strat))))))

(defun update-confidence (c right?)
  (if right?
      (progn (incf (confidence-value c) *conf-incf-on-correct*)
	     (trp 2 "Yaaa....Got it right! (new confidence = ~a)~%"
		     (confidence-value c)))
      (progn (decf (confidence-value c) *conf-decf-on-wrong*)
	     (trp 2 "Oops....Got it wrong! (new confidence = ~a)~%"
		     (confidence-value c)))
      ))

(defun find-confidence (strat features)
  (dolist (c (strat-confidences strat))
    (if (set-equal features (confidence-features c))
	(return c))))

;;; Display utils.

(defun display-chains ()
  (dctop)
  (dcbot))

(defun dctop ()
  (print *tc*)
  (format t "~%")
  )

(defun dcbot ()
  (dcbot2 *bc*)
  (format t "~%")
  )

(defun dcbot2 (el)
  (dolist (entry el)
     (format t "~s:~%" (bci-operator entry)) ; put out the op name
     (dolist (goto (bci-xlist entry))
       (format t "   - ~4,2f -> ~s~%" 
	       (xition-xprob goto) 
	       (bci-operator (xition-xbci goto)))
       )))

#| Driver for the task.  We always start off with the first bc entry
and top entry.  If the transition probability is over xthresh then we
take the bottom path, otherwise we take the top path and update the
bottom path's xprob. |#

(defun drive (&optional (loop-kill-count 1000))
  (setq *solution-completed* ())
  (let* ((top *tc*)
	 )
    (prog (ok-xition current-op next-op)

      ;; Always start at the begnning of the TOP list.

      (setq current-op (pop top))

     loop

      ;; Check for recognition of completion.  (This might better be
      ;; done as an mgoal?)  Anyhow, it has to be better integrated into
      ;; the world. This is set by try-retrieval (if it wins) and by
      ;; end!, which is used at the end of each normal strategy

      (if *solution-completed* (return nil))

      ;; Watch out for internal loops!

      (if (zerop (decf loop-kill-count))
	  (progn (print "Loop Stopped!")
		 (record! 'Loop-stopped-by-force! nil)
		 (break)))

      (incf *ncycles*)

      ;; Normal end of a procedure.  (Usually it will call end! which will
      ;; set *solution-completed* but sometimes we just run off the end
      ;; of the stack.)

      (if (null current-op) 
	  (return nil))

      (exec-op current-op)

      (trp 2 "At ~a, " current-op)

      ;; Is there a bottom transition that's above threshold?

      (setq ok-xition (select-bottom-xsition current-op))

      (if ok-xition

	  ;; If so, use it!

	  (progn
	    (trp 2 "Selected -> ~a (~a)~%"
		    (bci-operator (xition-xbci ok-xition))
		    (xition-xprob ok-xition))

	    ;; Since there's a bottom xition selected, force it onto the
	    ;; operation stack.  This is actually wrong, but it'll do for now.

	    (setq next-op (bci-operator (xition-xbci ok-xition)))

	    ;; Okay, here we go with the metacog. system!
	    (do-a-meta-cycle)

	    )

	  ;; Otherwise don't bother and so the next thing on the TOP stack.

 	  (progn
	    (trp 2 "no transition selected.~%")
	    (if top
		(setq next-op (car top))
	        (setq next-op ()))
	    )
	  )

      (if top (pop top))
	      
      ;; Update xsition probabilities and go around.

      (if next-op
          (update-bottom-xprobs (list current-op next-op)))

      (setq current-op next-op)

;     (print (list current-op (list top)))

      (go loop)
      )
    ))

(defun select-bottom-xsition (current-op)
  (let* ((xition (find current-op *bc* :test
			  #'(lambda (a b) (eq (bci-operator b) a))))
	 (possible-paths (if xition (bci-xlist xition)))
	 )
    (if possible-paths
	(select-path-normalized possible-paths))
    ))

;;; This gets each paths' xprob and normalizes them to 1.0 and then
;;; randomly selects on in accord with their strength.  Only operators
;;; that come over *xthresh* are included in the list.

(defun select-path-normalized (paths &aux (sum 0.0) result)
  (let ((pl (mapcan #'(lambda (p) (if (> (xition-xprob p) *xthresh*)
				      (list (cons (xition-xprob p) p))))
		    paths)))
    (dolist (p pl)
      (rplaca p (incf sum (car p))))
    (let ((thresh (/ (random 100) 100.0)))
      (dolist (p pl result)
        (if (> (/ (car p) sum) thresh)
	    (progn (setq result (cdr p))
		   (return result)))
	))))

;;; --------------

(defun update-bottom-xprobs (tc-portion)
  (let ((from (car tc-portion))
	(to (cadr tc-portion)))
    (if (and from to)
	(ubx2 from to))))

(defun ubx2 (from to)
  (let ((from-bci (find from *bc* :test #'tbp1))
	(to-bci (find to *bc* :test #'tbp1)))
    (cond ((and from-bci to-bci)
	   (ubx3 from-bci to-bci))
	  (from-bci ; from- but no to-bci
	   (ubx3 from-bci (make-new-bci to)))
	  (to-bci ; to- but no from (unlikely but...)
	   (ubx3 (make-new-bci from) to-bci))
	  (t ; neither -- this is common when we start out!
	   (ubx3 (make-new-bci from) (make-new-bci to)))
	  )))

(defun ubx3 (from-bci to-bci)
  (let ((link (find to-bci (bci-xlist from-bci) :test #'findxlbci)))
    (if link ; if there's already a link...
	(incf (xition-xprob link) *XINCR*)
        (progn
	  (push (make-xition :xprob *XINIT* 
			     :xbci to-bci)
		(bci-xlist from-bci)))
	)))

(defun findxlbci (bci xition)
  (equal (bci-operator bci) (bci-operator (xition-xbci xition)))
  )

(defun make-new-bci (operator)
  (let ((new (make-bci :operator operator)))
    (push new *bc*)
    new))

(defun tbp1 (name bci)
  (equal name (bci-operator bci)))

;;; ----- Operators for actual addition strategies and test routines.

#| There are the following systems herein:

1. The experimenter chooses problems, presents them, handles cycling
the world, and gives feedback on results of addition.

2. ASCM (more correctly, the LMT module) knows the specific high level
structure of each strategy, and carries with each a memory strength value.
Each time the memory system is referenced (by the cognitive system), the
referred-to elements get their strengths increased.  This principle gives
us an account of the basic memory effects: (a) whenever a strategy is
used (whether it works or not) its strenght is increased.  (b) when a
strategy gets the right answer, it is increased again (by viture of the 
cognitivie system getting rewarded and so sort of patting the strategy on
the head), and (c) when a new strategy is entered, it gets very high
strength points (because there's a lot of memory interaction involved in
entering it into the memory).

3. The performance system knows how to do the specific things that come
with goals (as stored in the memory to describe a strategy).

4. The cognitive system gets the problem, computes its features, probes
ASCM for a strategy, gets the goals for the selected strategy from the memory,
sets the peripheral system off on the first of these and then monitors
performance as the process takes place.  When the answer appears (in an
echoic buffer at the end of the run) the cognitive system reports this to 
the experimenter, gets the right or wrong feedback, and then, if the answer
was correct, pats ASCM on the head.  The cognitive system also permanently
knows the general structure of a good addition strategy.  This is coded
into the cognitive system and never changes.

     |#

#| General utilities for reporting, etc. |#

(defun trp (tl text &rest args)
  (if (>= *tl* tl)
    (apply #'format (append (list t text) args))
    ))

#| The peripheral system.  This is a very simple representation of 
the stuff needed for the addition domains: ten "fingers", a focus
of attention, and an echoic memory into which numbers are stored.
Operations on these constitute the basic operations of the domain. |#

#| The fingers memory structure; five on each of two hands,
   each of which my be up or down. |#

(setq *left-hand* ())
(setq *right-hand* ())

(defun clear-hands ()
  (setq *left-hand* (copy-list '(d d d d d)))
  (setq *right-hand* (copy-list '(d d d d d)))
  )

#| The focus of attention may point at a particular finger, or may be nil. |#

(setq *foa* nil)

#| The echoic buffer. |#

(setq *eb* nil)

#| Basic operations; most of which operate on what's in foa. |#

;;; Look at a hand, always at the first finger.

(defun focus-on-hand (hand)
   (setq *foa* hand)
   (mempush 'focus-on-hand (list hand))
   (report-hands))

(defun increment-focus ()
         ;; If done the right hand, return nil.
   (cond ((eq *foa* (last *right-hand*))
          ())
         ;; If we're done the left hand, move on to the rigt.
         ((eq *foa* (last *left-hand*))
          (setq *foa* *right-hand*))
         ;; Else shift to the next finger.
         (t (setq *foa* (cdr *foa*)))
         )
   (report-hands)
   )

;;; This is just a reporting function (and helpers).  The fingers are
;;; shown up (u) or down (d) for each hand, and the one begin attended to
;;; is capitalized.

(defun report-hands ()
  (trp 4 "~%")
  (rh *left-hand*)
  (trp 4 "|")
  (rh *right-hand*)
  (trp 4 "~%"))

(defun rh (h)
  (maplist #'rht h))

(defun rht (h)
  (if (eq *foa* h)
    (if (eq 'u (car h))
      (trp 4 "U")
      (trp 4 "D"))
    (if (eq 'u (car h))
      (trp 4 "u")
      (trp 4 "d"))
    ))

;;; Finger raising; always does the focussed finger.

(defun put-up ()
  (setf (car *foa*) 'u))

;;; Maipulation in the echoic buffer where number facts live.  We
;;; assume perfect knowledge of the number sequence.  That way incr
;;; and decr can be used.  This is where all possible errors come into
;;; the picture.  There is a probability (*perr*) of say-next
;;; reporting the WRONG number; this always takes place by simply
;;; failing to count.  Note that there are really a number of ways
;;; that one can get errors in addition, but the basic effect of
;;; correlating errors with the number of step in the procedure is
;;; accomplished by this method.

(defun say (n)
  (trp 2 " <~a> ~%" n)
  (setq *eb* n)
  (mempush 'say (list n)))
	     
(defun say-next ()
  (if (null *eb*)
      (say 1)
      (if (> *perr* (random 100))
        (say *eb*)
        (say (1+ *eb*)))
    ))

;;; Clear EB each time you're about to start a count off.  If you
;;; don't do this, the last number counted will be left in the echoic
;;; buffer and you'll count on from it, which is actually right, of
;;; course, for shortcut-sum.

(defun clear-eb ()
  (setq *eb* ()))

(defun end! ()
  (setq *solution-completed* t) ; this tells the driver to stop.
  )

;;; Raise is an important heart of this process.  The question is how
;;; to do the test-for-done.  That is, when putting up fingers, how
;;; does the child know when he's got the right number up?  In this
;;; version, he uses the echoic buffer trace, but that can't be right
;;; for shortcut sum because the echoic buffer contains the SUM, not
;;; just the single hand's count, so the right hand won't work.
;;; Somehow the child can SAY one thing while covertly counting
;;; another.  This suggests a dissociation of the echoic number
;;; sequence from counting, which can be done covertly.  Instead of
;;; relying upon the echoic trace for the count, We uses a new buffer
;;; (*cb*) to maintain the count of fingers up on a particular hand.
;;; This buffer is cleared by raise2 itself, and is used for the done
;;; test.

(setq *cb* ()) ; counting buffer

(defun exec-op (op)
  (trp 2 "Doing: (~a)~%" op)
  (mempush 'exec-op (list op))
  (funcall op))

;;; This version of raise assumes that hand control is done by the caller.

(defun raise ()
  (setq *cb* 0)
  (prog ()
 loop
    (say-next)
    (put-up)
    (increment-focus)
    (incf *cb*)
    (if (= *cb* *addend*) (return))
    (go loop)
    )
  )

(defun count-fingers ()
    (look-n-count)
    (look-n-count)
    (look-n-count)
    (look-n-count)
    (look-n-count)
    )

(defun look-n-count ()
  (if (eq 'u (car *foa*))
      (say-next))
  (increment-focus))

;;; The hands are external components as well, so 
;;; that all you need to do is select a hand and switch hands.

(setq *hand* ())

(defun choose-hand ()
  (trp 3 "~%Looking to the ")
  (if (zerop (random 2))
    (progn (setq *hand* *right-hand*) (trp 3 "right hand."))
    (progn (setq *hand* *left-hand*) (trp 3 "left hand.")))
  (focus-on-hand2)
  )

(defun focus-on-hand2 ()
   (setq *foa* *hand*)
   (report-hands))

(defun swap-hands ()
  (trp 3 "~%Looking to the ")
  (if (eq *hand* *left-hand*)
    (progn (setq *hand* *right-hand*) 
	   (mempush 'swap-hands '(:from left :to right))
	   (trp 3 "right hand."))
    (progn (setq *hand* *left-hand*) 
	   (mempush 'swap-hands '(:from right :to left))
	   (trp 3 "left hand."))
    )
  (focus-on-hand2)
  )

;;; Finally we need to replace the n1 and n2 with echoic buffers so
;;; that they aren't arguments to a lisp function. This also requires
;;; putting the addends into external stores which, like the hands,
;;; can be attended.  Instead of doing all that I just assume here
;;; that the problem is written on an external board, and that there
;;; is a sort of second set of eyes that can look at one or the other
;;; addend, and swap them, just like with the hands.  We ought to
;;; organize all the different buffers.  I wonder if kids ever get
;;; these mixed up, and if not, why not?

(setq *ad1* ())
(setq *ad2* ())

(setq *addend* ())

(defun choose-addend ()
  (if (zerop (random 2))
    (setq *addend* *ad1*)
    (setq *addend* *ad2*))
  (trp 3 "~%Choose addend ~a.~%" *addend*)
  )

(defun swap-addends ()
  (trp 3 "~%Looking to the other addend: ")
  (if (eq *addend* *ad1*)
    (progn (setq *addend* *ad2*)
   	   (mempush 'swap-addends '(:from 1 :to 2))
	   )
    (progn (setq *addend* *ad1*)
   	   (mempush 'swap-addends '(:from 2 :to 1))
	   )
    )
  (trp 3 "~a.~%" *addend*)
  )

;;; Say-addend is just used to unify the calling form of the main fns
;;; so that they never need an argument

(defun say-addend ()
  (say *addend*)
  )

;;; Here's the operator list for mediation learning.  Note that this requires
;;; initialization, as in the def just above.

(setq *sum-strat* '(

  ;; First addend on first hand.

  clear-hands choose-hand choose-addend
  say-addend clear-eb raise

  ;; Second addend on the other hand.

  swap-hands swap-addends
  say-addend clear-eb raise

  ;; Final count out.

  choose-hand clear-eb
  count-fingers
  swap-hands
  count-fingers

  end!
  ))

;;; Okay, now it's easy to turn sum into shortcut sum by just dropping
;;; a couple of steps.

(setq *scsum-strat* '(

  ;; First addend on first hand.

  clear-hands choose-hand choose-addend
  say-addend clear-eb raise

  ;; Second addend on the other hand.

  swap-hands swap-addends
; say-addend clear-eb 
  raise

  ;; Final count out.

; choose-hand clear-eb 
; count-fingers 
; swap-hands
; count-fingers 

  end!
  ))

;;; Okay, now minn (not MIN which is a lisp fn!) is simply scsum with
;;; preselection!

(setq *minn-strat* '(

  ;; First addend on first hand.

  clear-hands choose-hand choose-larger-addend
  say-addend clear-eb raise

  ;; Second addend on the other hand.

  swap-hands swap-addends
; say-addend clear-eb 
  raise

  ;; Final count out.

; choose-hand clear-eb 
; count-fingers 
; swap-hands
; count-fingers 

  end!
  ))


(defun choose-larger-addend ()
  (if (> *ad1* *ad2*)
    (setq *addend* *ad1*)
    (setq *addend* *ad2*))
  (trp 3 "~%Choose addend ~a." *addend*)
  )

#| Okay, so next we need to replace the lisp function forms above with
P-chain versions.  This requires a lot of additional machinery.  The
image is that there is one and only one occurrance of a procedure, and
that what happens is that you encode trajectories of calls to these
procedures.  Since there are only a few relevant procedures here
(clear-eb, raise, and count-fingers).  Each of these is well
(pre)learned. |#

#| The chain representation is just a list of fn calls.  Each entry
looks like this:

    (my-uid fn list-of-gotos)

My-uid is a unique id assigned to each independent entry.  You'll see
what this is used for below.

Where the list of gotos is each:

    (strat-name xpr new-uid)

Strat-name is the strategy under which this goto was entered.  xpr is
a real valued transition probability, and new-uid is the uid of the
place to go.

The xprs get updated each time you run along this pathway.  (Is it
normalized??)

|#

(setq *lastuid* ())

(defun init-chain ()
  (setq *chain* (copy-tree '((0 start nil)))) ; [copied so that it doesn't get mucked up later]
  (setq *lastuid* 1))

(defun load-strat (name sequence &aux last-step)
  (setq last-step (car *chain*))
  (dolist (step sequence)
    (setq last-step (load-step step (incf *lastuid*) last-step name))
    ))

(defparameter *initial-xpr* 0.1)

(defun load-step (new-step step-uid last-step name &aux new-entry)
  ;; Add the new step to the chain list, unattached as yet.
  ;; This sort of ends up coming all in upside down, but it'll do so long as
  ;; the Car of the chain is always step 0!
  (push (setq new-entry (list step-uid new-step nil)) (cdr *chain*))
  ;; Update the old last-step to include the new step -- this actually attaches
  ;; it to the chain.
  (push (list name *initial-xpr* step-uid)
        (caddr last-step))
  ;; Return the new last-step ... ie., the new chain entry.
  new-entry
  )

;;; Running a strategy is, at this stage, very simple; just walk to
;;; chain from the start.

(defun run-strat (name n1 n2)
  (trp 1 "~%~%>> Testing ~a ~a ~a: ~%" name n1 n2)
  ;; Our friends the a-theoretic buffer loads.
  (setq *ad1* n1)
  (setq *ad2* n2)
  ;; Run the chain!
  (prog (uid step)
    (setq uid 0)
   loop
    (setq step (find-uid uid))
    (execute-step step)
    (setq uid (select-next-step step name))
    (if uid (go loop) (return 'done))
    ))

(defun find-uid (n)
  (find n *chain* :test #'(lambda (a b) (= a (car b)))))

(defun execute-step (step)
  (trp 3 "~%Doing ~a: ~a." (car step) (cadr step))
  (funcall (cadr step)))

(defun select-next-step (step name)
  (caddr (find name (caddr step) :test #'(lambda (a b) (eq a (car b))))))

;;; Metacognitive system; This is any old production system junk you
;;; feel like doing between and betwixt.  There are a number of
;;; general meta goals, in *global-mgoals* and some that are
;;; strategy-specific (in the strat 'mgoals slot).  All the mgoals
;;; that you would like to do during this run get pushed onto the
;;; *mgoals* and selected for when we have time.  The global *mgoal*
;;; contains the current list of active calls for the current
;;; mgoal. If that isn't empty when do-a-meta-cycle gets called, then
;;; we pop and do the top thing, else we scan for applicable meta
;;; goals and push one onto *mgoal*.

;;; Note that choosing what to do counts a a meta-cycle!

(defun do-a-meta-cycle ()
  (trp 2 "### Running a metacycle!~%")
  (incf *saved-ms-cycles*)
  (if *mgoal*
      (funcall (pop *mgoal*))
      (let ((possible-mgoals (find-possible-mgoals)))
	(if possible-mgoals
	    (setq *mgoal* (mgoal-procedures (nth (random (length possible-mgoals))
						 possible-mgoals)))))
	))

(defun find-possible-mgoals ()
  (mapcan #'(lambda (mg) (if (funcall (mgoal-pattern mg))
			     (list mg))
	      )
	  *mgoals*))

;;; Summarizes the results *record*; Remember that it's reversed if you want
;;; to do anything involving when something began to happen.

(defun summarize-records ()
  (format t "--------------------~%Study Summary~%--------------------~%~%")
  (setq *records* (reverse *records*))
  ;; ----------------------------
  (format t "+ correct, - incorrect, x no result (loop or early termination?)~%")
  (dolist (rec *records*)
    (let* ((p (cadr (assoc 'problem rec)))
	   (r (apply #'+ p))
	   (a (cadr (assoc 'result rec))))
      (if a
	  (if (= a r)
	      (format t "+")
	      (format t "-")
	      )
	  (format t "x"))
      ))
  (format t "~%")
  ;; ----------------------------
  (format t "r - retrieved, c - computed, l - looped out.~%")
  (dolist (rec *records*)
    (let* ((r (assoc 'used-retrieval! rec))
	   (l (or (assoc 'LOOP-STOPPED-BY-META-COGNITIVE-SYSTEM! rec)
		  (assoc 'LOOP-STOPPED-BY-FORCE! rec)))
	   )
      (if r
	  (format t "r")
	  (if l
	      (format t "l")
	      (format t "c")
	      )
	  )
      ))
  (format t "~%")
  ;; ----------------------------
  (format t "S - Sum, C - ShortCut Sum, M - Min.~%")
  (dolist (rec *records*)
    (let* ((s (cadr (assoc 'strat rec)))
	   )
      (case s
	(sum (format t "S"))
	(scsum (format t "C"))
	(minn (format t "M"))
	    )
      ))
  (format t "~%")
  ;; ----------------------------
  (format t "Number of saved metacycles (after 9: just '+').~%")
  (dolist (rec *records*)
    (let* ((c (cadr (assoc 'saved-ms-cycles rec)))
	   )
      (if (> c 9)
	  (format t "+")
	  (format t "~a" c))
      ))
  (format t "~%")
  (format t "--------------------~%~%--------------------~%~%")
  )
  
