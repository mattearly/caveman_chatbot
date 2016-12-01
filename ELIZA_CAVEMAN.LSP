;;; ==============================;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File eliza1.lisp: Basic version of the Eliza program

;;;; Modded version is ELIZA_CAVEMAN.LSP: by Matthew Early (matthewjearly@gmail.com)

;;; The basics are in auxfns.lisp; look for "PATTERN MATCHING FACILITY"

;; New version of pat-match with segment variables

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)                ; ***
         (segment-match pattern input bindings))    ; ***
        ((and (consp pattern) (consp input)) 
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) 
                               bindings)))
        (t fail)))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

;;; ==============================

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        ;; We assume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ((pos (position (first pat) input
                             :start start :test #'equal)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                          pat (subseq input pos)
                          (match-variable var (subseq input 0 pos)
                                          bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

;;; ==============================

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

;;; ==============================

(defparameter *eliza-rules*
 '((((?* ?x) hello (?* ?y))      
    (caveman> hullo me hapy to see u gan))
   (((?* ?x) hi (?* ?y))      
    (caveman> hi to you me hapy to see u gan))  
   (((?* ?x) hola (?* ?y))      
    (caveman> por que? misa hapy to see u))   
   (((?* ?x) I want (?* ?y))     
    (caveman> that arr neat - me sense big ?y boom)
    (caveman> like ?y come wind to shiny ocean) 
    (caveman> now yes to ?y and hand new chopper))
   (((?* ?x) if (?* ?y)) 
    (caveman> call to gods of the dirt ?y is future) 
    (caveman> we all see vision when ?y wit sleep)
    (caveman> come hapy ?y see new light of day in distance) 
    (caveman> decision be hard on ?y))
   (((?* ?x) no (?* ?y))
    (caveman> there must be way to bring stone) 
    (caveman> this no think to being like meh)
    (caveman> No ??? must be on ones mind to come up at tame time))
   (((?* ?x) I was (?* ?y))       
    (caveman> could be way to ?) 
    (caveman> ?y small child is baby)
    (caveman> now is oka when ?y is the yesterday))
   (((?* ?x) I feel (?* ?y))     
    (caveman> feel must be reason for ?y trust ?y))
   (((?* ?x) I felt (?* ?y))     
    (caveman> to last end ?y think ?y))
   (((?* ?x) sometimes (?* ?y))     
    (caveman> sometime is hard word for caveman to grasp) 
    (caveman> ?y always come to late for day)
    (caveman> gods sometimes do ?y))
   (((?* ?x) what (?* ?y))     
    (caveman> ?y questions be hard) 
    (caveman> when the sun comes up then we have seen ?y)
    (caveman> my brother came up with the club so ?y) 
    (caveman> you mess with ?y and hard time))
   (((?* ?x) caveman (?* ?y))     
    (caveman> what is caveman?) 
    (caveman> is ?y what you think of my people)
    (caveman> caveman say ?y in other way) (your words are hurtful))
   (((?* ?x) cavemen (?* ?y))     
    (caveman> we outnumber you careful with word ?y) 
    (caveman> is ?y what you think of my people)
    (caveman> caveman say ?y in other way) 
    (caveman> your words are hurtful))
  (((?* ?x) learn (?* ?y))     
    (caveman> ive learned so much from my mistakes) 
    (caveman> Im tired of hunting and gathering)
    (caveman> you cant shave with a rock) 
    (caveman> youve got a caveman on your doorstep))
  (((?* ?x) fire (?* ?y))     
    (caveman> we build fire wit sky shock makes good ?y) 
    (caveman> food stay fire)
    (caveman> wet no start ?y) 
    (caveman> Some cavemen just want to see the world burn))
  (((?* ?x) bye (?* ?y))     
    (caveman> hur har caveman bored if you would like to leave say [abort] in brackets) 
    (caveman> use [abort] to exit so easy a caveman can do it))
  (((?* ?x))     
    (caveman> You must eat ?x) 
    (caveman> Your words are hurtful Ryan) 
    (caveman> Ool no Food) 
    (caveman> Tonda haraka Tonda haraka!) 
    (caveman> Macha)
    (caveman> Fud) 
    (caveman> Thats fire) 
    (caveman> Fend Bobo) 
    (caveman> Zug-zug) 
    (caveman> All right Zug-zug)
    (caveman> You dont sit up in a cave and write the Great American Novel and know it is utterly superb and then throw it page by page into the fire You just dont do that You send it out You have to send it out) 
    (caveman> The poet existed among the cave men he will exist among men of the atomic age - for he is an inherent part of man Even religions have been born from the need for poetry which is a spiritual need and it is through the grace of poetry that the divine spark lives forever in the human flint) 
    (caveman> I would get a lot of writing done if I lived in isolation in a cave under a swamp) 
    (caveman> One just principle from the depths of a cave is more powerful than an army) 
    (caveman> I tried the paleo diet which is the caveman diet - lots of meat And I tried the calorie restriction diet The idea is that if you eat very very little if youre on the verge of starvation you will live a very long time whether or not you want to of course)
    (caveman> Maybe my caveman ancestors invented the wheel or something Im not sure) 
    (caveman> My swing is so bad i look like caveman killing lunch) 
    (caveman> We still carry this old caveman-imprint idea that were small natures big and its everything we can manage to hang on and survive When big geophysical events happen - a huge earthquake tsunami or volcanic eruption - were reminded of that) 
    (caveman> I doona discuss my private life) 
    (caveman> Ill give you caveman)
    (caveman> Ill give you ?x)
    (caveman> cavemen are 4 life)
    (caveman> ?x is ok if you believe in a better sunup)
    (caveman> You believe todays paradigm are not going to change Ask the caveman then if his paradigms changed or not think for yourself you loose think for coming generations you win))
  )
)

;;; ==============================

(defun caveman ()
  "Respond to user input using pattern matching rules."
  (print '"Caveman has a hard time understanding.")
  (print '"Try using words like: feel was, no, if, want")
  (print '"Also be sure to surround your statements in ()") 
  (loop
    (print 'you<)
    (write (flatten (use-eliza-rules (read))) :pretty t)))

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                  (sublis (switch-viewpoint result)
                          (random-elt (rule-responses rule))))))
        *eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis '((I . you) (you . I) (me . you) (am . are) (we . us) (us . we) )
          words))

;;; ==============================

(defun flatten (the-list)
  "Append together elements (or lists) in the list."
  (mappend #'mklist the-list))

(defun mklist (x)
  "Return x if it is a list, otherwise (x)."
  (if (listp x)
      x
      (list x)))

(defun mappend (fn the-list)	
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))


