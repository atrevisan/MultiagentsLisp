;;;-*- Mode: Lisp; Package: "EUZEBIO" -*-
;;;===============================================================================
;;;10/10/06
;;;                               AGENT EUZEBIO
;;;
;;;===============================================================================

#|
EUZEBIO has the following skills:
	- ASK INFO 
	    internal task that sends a subtask, puts the result into
	    FACT/answer and "answer there" into FACTS/input.
	- CN TEST
	    Sends an :hello request to everybody to see who is around using
		contract net (graphics trace)
	- GET ADDRESS 
	    Skill to answer an external request of another PA that wants to 
		obtain an address.
	- GET BIBLIO
	    Skill to answer an external request of another PA that wants to 
		obtain a list of references.
	- GET PHONE NUMBER
	    Skill to answer an external request of another PA that wants to 
		obtain a telephone number.
	- HELLO 
	    Atomic test function, returns a greeting string.
	- SHOW ONTOLOGY
	    Displays a window with an ontology during 20s.
	- TEST
	    Sends an :hello request to everybody to see who is around (graphics 
		trace)

2009
 0730 creating EUZEBIO as a limited clone of ALBERT for Milton
|#
(format t "~%;*** Loading EUZEBIO file ***")

(defpackage :EUZEBIO (:use :moss :omas :cl #+MCL :ccl))
(in-package :EUZEBIO)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :br MOSS::*LANGUAGE-TAGS*))		

(omas::defassistant :EUZEBIO :redefine t :language :br)

;;;===============================================================================
;;; 
;;;                         service macros and functions 
;;;
;;;===============================================================================


;;; ==================================== globals =================================
;;; Globals  can be used within the agent space to simplify programming, although
;;; it is better to use the agent memory.


;;;===============================================================================
;;; 
;;;                                    skills 
;;;
;;;===============================================================================

;;;========================================================================= skill
;;;                                   ASK INFO
;;;===============================================================================

;;; internal skill to ask something to the outside world

(defskill :ask-info :EUZEBIO
  :static-fcn static-ask-info
  :dynamic-fcn dynamic-ask-info
  :time-limit-fcn time-limit-hello
  :timeout-handler timeout-handler-ask-info
  )

(defUn static-ask-info (agent message to my-action args)
  "internal task that sends a subtask
Arguments:
   agent:
   message
   to: whom we should ask (default :all)
   my-action: predicate
   args: arguments of the predicate."
  (declare (ignore message))
  (send-subtask agent :to (or to :all) :action my-action :args args)
  (static-exit agent :done))

(defUn dynamic-ask-info (agent message result)
  "receiving the result of the request. Put it into the results slot of the agent ~
   conversation."
  (declare (ignore message))
  (let* ((conversation (omas::conversation agent)))
    (if conversation
      (progn
        (replace-fact conversation :answer result)
        (replace-fact conversation :raw-input "answer-there")
        )
      ;; otherwise indicate failure
      (error "no conversation active for EUZEBIO"))
    (dynamic-exit agent result :internal t)))

(defUn timeout-handler-ask-info (agent message)
  "documentation"
  (declare (ignore agent message))
  (error "timeout"))

;;;========================================================================= skill
;;;                                   GET ADDRESS
;;;===============================================================================

;;; Skill to answer an external request of another PA that wants to obtain an
;;; address. 

(defskill :get-address :EUZEBIO
  :static-fcn static-get-address
  :dynamic-fcn dynamic-get-address
  )

(defUn static-get-address (agent message data)
  "the skill is triggered by a message from an agent wanting to obtain address ~
   information.
Arguments:
   data: an a-list, e.g. ((:data \"dominique\" \"barthès\")(:pattern ...))
Return:
   \"failure\" or a list of addresses."
  (declare (ignore message))
  (when data
    (send-subtask  agent :to :EUZEBIO-address :action :get-address 
                   :args (list data)))
  (static-exit agent :done))

(defUn dynamic-get-address (agent message result)
  "return result to calling agent"
  (declare (ignore message))
  ;(print result)
  (dynamic-exit agent result))
#|
;;;========================================================================= skill
;;;                                   GET BIBLIO
;;;===============================================================================

;;; Skill to answer an external request of another PA that wants to obtain a
;;; list of references. 

(defskill :get-biblio :EUZEBIO
  :static-fcn static-get-biblio
  :dynamic-fcn dynamic-get-biblio
  )

(defun static-get-biblio (agent message data)
  "the skill is triggered by a message from an agent wanting to obtain biblio ~
   information.
Arguments:
   data: an a-list, e.g. ((:data \"dominique\" \"barthès\")(:pattern ...))
Return:
   \"failure\" or a list of references."
  (declare (ignore message))
  (when data
    (send-subtask  agent :to :biblio :action :get-publications 
                   :args (list data)))
  (static-exit agent :done))

(defun dynamic-get-biblio (agent message result)
  "return result to calling agent"
  ;(print result)
  (declare (ignore message))
  (dynamic-exit agent result))
|#

;;;========================================================================= skill
;;;                              GET PHONE NUMBER
;;;===============================================================================

;;; Skill to answer an external request of another PA that wants to obtain a
;;; telephone number 

(defskill :get-phone-number :EUZEBIO
  :static-fcn static-get-phone-number
  :dynamic-fcn dynamic-get-phone-number
  )

(defUn static-get-phone-number (agent message data)
  "the skill is triggered by a message from an agent wanting to obtain phone-number ~
   information.
Arguments:
   data: an a-list, e.g. ((:data \"dominique\" \"barthès\")(:pattern ...))
Return:
   \"failure\" or a list of phone-numbers."
  (declare (ignore message))
  (when data
    (send-subtask  agent :to :EUZEBIO-address :action :get-phone-number 
                   :args (list data)))
  (static-exit agent :done))

(defUn dynamic-get-phone-number (agent message result)
  "return result to calling agent"
  (declare (ignore message))
  ;(print result)
  (dynamic-exit agent result))

#|
;;; ============================== skill section ==============================

(defParameter *EUZEBIO-hello-functions*
  '((:functions static-hello 
                dynamic-hello)))

(defskill :hello EUZEBIO
  :static-fcn static-hello
  :dynamic-fcn dynamic-hello
  :time-limit-fcn time-limit-hello
  :timeout-handler timeout-handler-hello
  :preconditions preconditions-hello
  :select-best-answer-fcn select-best-answer-hello
  :acknowledge-fcn acknowledge-hello
  )

(defun static-hello (agent message <args>)
  "documentation"
  (static-exit agent <result>))

(defun dynamic-hello (agent message <args>)
  "documentation"
  (dynamic-exit agent <result>))

(defun time-limit-hello (agent message message)
  "documentation"
  )

(defun timeout-handler-hello (agent message)
  "documentation"
  )

(defun preconditions-hello (agent message <args>)
  "documentation"
  )

(defun select-best-answer-hello (agent message answer-message-list)
  "documentation"
  )

(defun acknnowledge-hello (agent message<args>)
  "documentation"
  )
|#

;;;========================================================================= skill
;;;                                  HELLO
;;;===============================================================================
;;; test function

(defskill :hello :EUZEBIO
  :static-fcn static-hello
  )

(defUn static-hello (self message &rest args)
  "greeting function"
  (declare (ignore message args))
  (static-exit self "Olá! Tudo bem?"))

;;;========================================================================= skill
;;;                                   TEST
;;;===============================================================================
;;; sends an :hello request to everybody to see who is around (graphics trace)

(defskill :test :EUZEBIO
  :static-fcn static-test
  :dynamic-fcn dynamic-test
  )

(defUn static-test (self message)
  "greeting function"
  (declare (ignore message))
  (send-subtask  self :to :all :action :hello :strategy :collect-answers)
  (static-exit self "Olá..."))

(defUn dynamic-test (self message result)
  (declare (ignore message))
  (dynamic-exit self result))

;;;========================================================================= skill
;;;                                 CN TEST
;;;===============================================================================

(defskill :cn-test :EUZEBIO
  :static-fcn static-cn-test
  :dynamic-fcn dynamic-cn-test
  )

(defUn static-cn-test (self message)
  "greeting function"
  (declare (ignore message))
  (send-subtask  self :to :all :action :hello :protocol :contract-net 
                 :strategy :collect-answers)
  (static-exit self "Hello there..."))

(defUn dynamic-cn-test (self message result)
  (declare (ignore message))
  (dynamic-exit self result))

;;;===============================================================================
;;; 
;;;                               initial conditions 
;;;
;;;===============================================================================

:ENV

;;;==============================================================================
;;; 
;;;                                    goals
;;;
;;;==============================================================================

;;; ================================= goal section ==============================
"macro that builds a goal structure for an agent.
Arguments:
   goal-name:              name of the goal that will be used to create task-ids
   agent-name:             name of the agent to receive the goal
keys arguments:
   mode:                   activation mode (:rigid or :flexible)
   type:                   type of goal (:permanent :cyclic :1-shot)
   period:                 period for cyclic goals (default is 20s)
   expiration-date:        date at which the goal dies
   expiration-delay:       time to wait until we kill the goal
   importance:             on a 1-100 scale (not yet available)
   urgency:                on a 1-100 scale (not yet available)
   activation-date:        date at which the goal should fire (default is now)
   activation-delay:       time to wait before activating the goal
   activation-level:       on a 1-100 scale (default 50)  (not yet available))
   activation-threshold:   on a 1-100 scale (default 50)  (not yet available)
   activation-change-fcn:  optional function called at each cycle (not yet available)
   status:                 waiting, active, dead,... 
                              may not be useful with activation
   goal-enable-fcn:        function checking the goal enabling conditions
   script:                 script describing the goal, currently a request message
Return:
   goal-name."

#|
(defgoal GGG EUZEBIO
  mode:             
  type: 
  period: 
  expiration-date: 
  expiration-delay: 
  activation-date: 
  activation-delay: 
  status: 
  goal-enable-fcn: 
  script: goal-function  ; must return a list of messages


;;; return T to enable the goal NIL ti inhibit it

(defun enable-ggg (agent script)
  "documentation"
  t)
|#

:EOF
