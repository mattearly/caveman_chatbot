;;; ==============================;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File eliza1.lisp: Basic version of the Eliza program

;;;; Modded version is ELIZA_CAVEMAN.LSP: updated by Matthew Early (matthewjearly@gmail.com)

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
    (hullo me hapy to see u gan))
    (((?* ?x) hi (?* ?y))      
    (hi to you me hapy to see u gan))   
   (((?* ?x) I want (?* ?y))     
    (that arr neat - me sinc big ?y boom)
    (like ?y come wind to shiny ocean) (now yes to ?y and hand new chopper))
   (((?* ?x) if (?* ?y)) 
    (call to gods of the dirt - ?y is in future) (we all see vision when ?y wit sleep)
    (come hapy ?y see new light of day in distance) (decision be hard on ?y))
   (((?* ?x) no (?* ?y))
    (there must be way to bring stone) (this no think to being like meh)
    ("No" ??? must be on ones mind to come up at tame time))
   (((?* ?x) I was (?* ?y))       
    (could be way to ?) (when the must is the best ?y small child is baby)
    (now is oka when ?y is the yesterday))
   (((?* ?x) I feel (?* ?y))     
    (feel must be reason for ?y trust ?y))
   (((?* ?x) I felt (?* ?y))     
    (to last end ?y is now and the slantest ?y))
   (((?* ?x) sometimes (?* ?y))     
    (sometime is hard word for caveman to grasp) (?y always come to late for day)
    (gods sometimes do ?y))
   (((?* ?x) what (?* ?y))     
    (i r no comprehend) (when the sun comes up then we have seen)
    (my brother came up with the club))
   (((?* ?x) caveman (?* ?y))     
    (what is caveman?) (is ?y what you think of my people)
    (caveman say ?y in other way) (your words hurt))
   (((?* ?x) cavemen (?* ?y))     
    (we outnumber you careful with word ?y) (is ?y what you think of my people)
    (caveman say ?y in other way) (your words hurt))
  )
)

;;; ==============================

(defun caveman ()
  "Respond to user input using pattern matching rules."
  (print '"Caveman has a hard time understanding.")
  (print '"Try using words like: feel, was, no, if, want")
  (print '"Also be sure to surround your statements in ()") 
  (loop
    (print 'caveman>)
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
  (sublis '((I . you) (you . I) (me . you) (am . are))
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


