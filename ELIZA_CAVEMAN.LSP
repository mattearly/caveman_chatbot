;;; ==============================;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig
;;; File eliza1.lisp: Basic version of the Eliza program
;;; Modded version is ELIZA_CAVEMAN.LSP: by Matthew Early (matthewjearly@gmail.com)
;;; The basics are in auxfns.lisp; look for "PATTERN MATCHING FACILITY"
;;; New version of pat-match with segment variables

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
    (cm> hullo me hapy to see u gan))
   (((?* ?x) hi (?* ?y))      
    (cm> hi to you me hapy to see u gan))  
   (((?* ?x) hola (?* ?y))      
    (cm> por que? misa hapy to see u))   
   (((?* ?x) I want (?* ?y))     
    (cm> that arr neat - me sense big ?y boom)
    (cm> like ?y come wind to shiny ocean) 
    (cm> now yes to ?y and hand new chopper)
    (cm> must be careful to ask ?y))
   (((?* ?x) if (?* ?y)) 
    (cm> call to gods of the dirt ?y is future) 
    (cm> we all see vision when ?y wit sleep)
    (cm> come hapy ?y see new light of day in distance) 
    (cm> decision be hard on ?y))
   (((?* ?x) no (?* ?y))
    (cm> there must be way to bring stone) 
    (cm> this no think to being like meh)
    (cm> No ??? must be on ones mind to come up at tame time))
   (((?* ?x) I was (?* ?y))       
    (cm> could be way to ?) 
    (cm> ?y small child is baby)
    (cm> now is oka when ?y is the yesterday))
   (((?* ?x) I feel (?* ?y))     
    (cm> feel must be reason for ?y trust ?y))
   (((?* ?x) I felt (?* ?y))     
    (cm> to last end ?y think ?y))
   (((?* ?x) sometimes (?* ?y))     
    (cm> sometime is hard word for caveman to grasp) 
    (cm> ?y always come to late for day)
    (cm> gods sometimes do ?y))
   (((?* ?x) what (?* ?y))     
    (cm> ?y questions be hard) 
    (cm> when the sun comes up then we have seen ?y)
    (cm> my brother came up with the club so ?y) 
    (cm> you mess with ?y and hard time))
   (((?* ?x) caveman (?* ?y))     
    (cm> what is caveman?) 
    (cm> is ?y what you think of my people)
    (cm> caveman say ?y in other way) 
    (cm> your words are hurtful))
   (((?* ?x) cavemen (?* ?y))     
    (cm> we outnumber you careful with word ?y) 
    (cm> is ?y what you think of my people)
    (cm> caveman say ?y in other way) 
    (cm> your words are hurtful))
  (((?* ?x) learn (?* ?y))     
    (cm> ive learned so much from my mistakes) 
    (cm> Im tired of hunting and gathering)
    (cm> you cant shave with a rock) 
    (cm> youve got a caveman on your doorstep))
  (((?* ?x) fire (?* ?y))     
    (cm> we build fire wit sky shock makes good ?y) 
    (cm> food stay fire)
    (cm> wet no start ?y) 
    (cm> Some cavemen just want to see the world burn))
  (((?* ?x) settle (?* ?y))     
    (cm> telling joke make face red dont tell me to ?y) 
    (cm> ?y learn from you i must))
  (((?* ?x) bye (?* ?y))     
    (cm> hur har caveman bored if you would like to leave say [abort] in brackets) 
    (cm> use [abort] to exit so easy a caveman can do it))
  (((?* ?x))     
    (cm> You must eat ?x) 
    (cm> Your words are hurtful Ryan) 
    (cm> Ool no Food) 
    (cm> Tonda haraka Tonda haraka!) 
    (cm> Macha)
    (cm> Fud) 
    (cm> Thats fire) 
    (cm> Fend Bobo) 
    (cm> Zug-zug) 
    (cm> All right Zug-zug)
    (cm> You dont sit up in a cave and write the Great American Novel and know it is utterly superb and then throw it page by page into the fire You just dont do that You send it out You have to send it out) 
    (cm> Waka Waka) 
    (cm> The poet existed among the cave men he will exist among men of the atomic age - for he is an inherent part of man Even religions have been born from the need for poetry which is a spiritual need and it is through the grace of poetry that the divine spark lives forever in the human flint) 
    (cm> Bring stick) 
    (cm> I would get a lot of writing done if I lived in isolation in a cave under a swamp) 
    (cm> you say match i say fire) 
    (cm> One just principle from the depths of a cave is more powerful than an army) 
    (cm> the sea is blood before dark) 
    (cm> I tried the paleo diet which is the caveman diet - lots of meat And I tried the calorie restriction diet The idea is that if you eat very very little if youre on the verge of starvation you will live a very long time whether or not you want to of course)
    (cm> my 18 sons would think so too) 
    (cm> Maybe my caveman ancestors invented the wheel or something Im not sure) 
    (cm> the wheel was here before me) 
    (cm> My swing is so bad i look like caveman killing lunch) 
    (cm> steep hill is big work) 
    (cm> We still carry this old caveman-imprint idea that were small natures big and its everything we can manage to hang on and survive When big geophysical events happen - a huge earthquake tsunami or volcanic eruption - were reminded of that) 
    (cm> in sky is soft) 
    (cm> I doona discuss my private life) 
    (cm> Ill give you caveman)
    (cm> Momo like you) 
    (cm> Ill give you ?x)
    (cm> ?x can be one) 
    (cm> cavemen are 4 life)
    (cm> ?x is ok if you believe in a better sunup)
    (cm> hot ?x to work) 
    (cm> You believe todays paradigm are not going to change Ask the caveman then if his paradigms changed or not think for yourself you loose think for coming generations you win)
    (cm> ?x is what it takes)
    (cm> point ?x to club)
    (cm> i learn spear trick you learn watch)
    (cm> sharp stick sharp mind)
    (cm> food good when new)
    (cm> i do not have ?x))
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


