;;;-*- Mode: Lisp; Package: "EUZEBIO" -*-
;;;===============================================================================
;;;10/10/18
;;;             AGENT EUZEBIO: DIALOG (file EUZEBIO-dialog.lisp)
;;;             Copyright Barthès@UTC, 2006-2010
;;;
;;;=============================================================================== 

#|
EUZEBIO is the Brazilian assistant agent of Allan Trevisan. EUZEBIO speaks only Portuguese.
It can find email addressses, telephone numbers, or use web services (?)

The current file contains its library of dialogs.

Questions asked to EUZEBIO (in Brazilian):
   - enderço do Barthès?
   - ...

Available experimental and sketchy dialogs
   - MAIN DIALOG
   - ASK
   - EXPLAIN
   - GET ADDRESS
   - GET HOME ADDRESS
   - GET NEWS BY SECTION
   - GET PHONE NUMBER
   - GET LAST NEWS
   - PRINT CONCEPT DOCUMENTATION
   - PRINT HELP
   - PROCESS ASK
   - PROCESS TELL
   - PUBLISH NEWS
   - SET FONT SIZE
   - TELL
   - TRACE
   - WHAT IS
   - WHO IS
   - ELIZA
   
2009
 0730 ceating file by cloning Albert-dialog
2012
 1906 added my own dialogs
|#

(in-package :EUZEBIO)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *language* :br)
  )

(format t "~&; loading EUZEBIO dialogs, package is: ~S" *package*)

;;;================================================================================
;;;                      Debugging functions
;;;================================================================================

(defMacro pcs () `(moss::%pep (omas::conversation EUZEBIO::PA_EUZEBIO)))

(defMacro cl-user::ze () `(in-package :EUZEBIO))
(defUn v ()(setq moss::*verbose* t) "moss::*verbose* set to T")
(defUn nv ()(setq moss::*verbose* nil) "moss::*verbose* set to NIL")
(defUn d ()(setq moss::*debug* t) "moss::*debug* set to T")
(defUn nd ()(setq moss::*debug* nil) "moss::*debug* set to NIL")

;;; debugging function
(defUn cc () 
  (declare (special _main-conversation))
  (moss::start-conversation _main-conversation))

(defParameter *empty-words* '("o" "a" "os" "um" "uma" "uns" "umas" "de"
                              "do" "qual" "quais" "é" "são"))


;;;================================================================================
;;;
;;;                  Conversations and Sub-Conversations
;;;
;;;================================================================================

;;; we declare the top-level conversations
;;; the entry point of the dialog is _MAIN-CONVERSATION

(eval-when (load compile eval)
  (proclaim '(special 
              _MAIN-CONVERSATION
              _PROCESS-CONVERSATION
              )))

;;;================================================================================
;;;
;;;                       MAIN CONVERSATION (CONTROL LOOP)
;;;
;;;================================================================================

;;; create a dialog header to be used for the main conversation

(defindividual 
  MOSS-DIALOG-HEADER
  (HAS-MOSS-LABEL "Main conversation loop")
  (HAS-MOSS-EXPLANATION "This is the main conversation loop.")
  (:var _main-conversation))

;; declare that conversation, replacing default conversation in the agent structure
(setf (omas::dialog-header PA_EUZEBIO) _main-conversation)


;;;===== states are declared as global variables
;;; the set of states can be considered as a plan to be executed for conducting the
;;; conversation with the master

;;;--------------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(special 
              _mc-entry-state
              _mc-get-input
              _mc-more?
              _mc-process
              _mc-sleep)))
;;;--------------------------------------------------------------------------------

;;;--------------------------------------------------------------- (MC) ENTRY-STATE
;;; ENTRY STATE
;(defun ff () (start-conversation _main-conversation :first-time t))
;(defun gg () (start-conversation _main-conversation))

;;; define an entry instance and record it in a global variable.
;;; ****** we should enter a frame on the conversation frame list, to handle
;;; success or failure returns from sub-dialogs. I.e. we should make a transition
;;; on the success or failure state of the main conversation.
;;; entry (_mc-failure _mc-success _mc-entry-state nil) ; no goal 

;;; Action:
;;;   =EXECUTE
;;;   - get ACTION if any from the dialog header
;;;   - reset conversation: HAS-FACTS, HAS-TASK, HAS-TASK-LIST
;;;   - if ACTION IS non NIL, set conversation HAS-GOAL with a pattern obtaines from
;;;        sending ACTION a =create-pattern message
;;;     otherwise, reset conversation HAS-GOAL
;;;   - display welcome message
;;;
;;;   <Master enters text>
;;;
;;;   =RESUME
;;;   - if INPUT contains one of the abort commands, throws to :DIALOG-ERROR
;;;   - makes a transition to _MC-PROCESS

(defstate _mc-entry-state
  (:entry-state _main-conversation)
  (:label "Début du dialogue")
  (:explanation 
   "Initial state when the assistant starts a conversation. Send a welcome message ~
    and wait for data. Also entered on a restart following an abort.")
  (:reset-conversation)
  (:text "Atenção! Este diálogo é muito limitado. Cada frase é independente ~
          e não é possível utilizar pronomes relativos.~%")
  (:question-no-erase
   ("- Bom dia ! o que posso fazer por você ?"
    "- Olá ! O que deseja saber ?" 
    "-Bom dia ! Eu vou fazer o possível para responder às suas perguntas. ~
     Mas, lembre-se que o meu QI é fraco."))
  (:transitions 
   (:always :target _mc-process))
  )

;;;----------------------------------------------------------------- (MC) GET-INPUT

(defstate _mc-get-input
  (:label "Get input")
  (:explanation
   "State in which the user inputs his request.")
  (:reset)
  (:question "- Sou todo ouvidos ...")
  (:transitions
   (:always :target _mc-process)))

;;;-------------------------------------------------------------------- (MC) MORE?
;;; Action:
;;;   - If answer is "rien", "non" transition to SLEEP
;;;   - if answer is "oui" transition to GET-INPUT
;;;   - anything else sends to PROCESS

(defstate _mc-more?
  (:label "More?")
  (:explanation "Asking the user it he wants to do more interaction.")
  (:reset-conversation)
  (:question-no-erase
   ("~%- O que posso ainda fazer ?"
    "~%- Há outra coisa que eu possa fazer ?"
    "~%- Tem outras questões ?"
    "~%- OK.")
   ) 
  (:transitions 
   (:starts-with ("nada") :target _mc-sleep)
   (:no :target _mc-sleep)
   (:yes :target _mc-get-input)
   (:otherwise :target _mc-process)))

;;;------------------------------------------------------------------- (MC) PROCESS

(defstate _mc-process
  (:label "Process state of main conversation")
  (:explanation
   "Here we call the main process organized as a sub-conversation.")
  (:transitions
   (:always :sub-dialog _process-conversation
            :success _mc-more?
            :failure _mc-more?)))

;;;--------------------------------------------------------------------- (MC) SLEEP

(defstate _mc-SLEEP
  (:label "Nothing to do")
  (:explanation
   "User said she wanted nothing more.")
  (:reset)
  (:text "- OK. Eu aguardo até que esteja pronto.~%")
  (:question-no-erase 
   "- Acorde-me digitando ou dizendo alguma coisa...")
  (:transitions
   (:always :target _mc-process)))

;;;================================================================================
;;;
;;;                      MAIN CONVERSATION (EXECUTION PART)
;;;
;;;================================================================================

;;; this conversation is intended to process the input from te user by first
;;; determining the performative, then the list of possible tasks

(defsubdialog 
  _process-conversation 
  (:label "Process conversation")
  (:explanation 
   "Processing steps of the main conversation.")
  (:states _mc-eliza
           _mc-find-performative
           _mc-entry-state
           _mc-failure
           _mc-find-performative
           _mc-find-task
           _mc-select-task
           _mc-task-dialog)
  )

;;;===============================================================================
;;;                            Service functions
;;;===============================================================================

;;;-------------------------------------------------------------------- PRINT-TIME

(defUn print-time (conversation &optional (language *language*))
  (multiple-value-bind (second minute hour date month year)
                       (decode-universal-time (get-universal-time))
    (send conversation '=display-text
          (case language
            (:en
             (format nil "Current time is ~S:~S:~S and today is the ~S/~S/~S~%"
                     hour minute second date month year))
            (:br
             (format nil 
                     "A hora atual é ~S:~S:~S e hoje estamos no ~S/~S/~S~%"
                     hour minute second date month year))
            )
          )))

;;;===============================================================================
;;;                            Conversation states
;;;===============================================================================

;;;--------------------------------------------------------- (MC) FIND-PERFORMATIVE

(defstate _mc-find-performative
  (:entry-state _process-conversation)
  (:process-state _main-conversation)
  (:label "Find performative")
  (:explanation 
   "Process what the user said trying to determine the type of performative ~
    among :request :assert :command. Put the result if any into the performative ~
    slot of the conversation object.")
  (:transitions 
   ;; time?
   (:patterns (("Que" "horas" (?* ?x)))
              :exec (print-time moss::conversation :br) :success)
   ;; now really select performatives
   (:patterns (("que" (?* ?x))
               ("qual" "é" (?* ?x))
               ("quais" "são" (?* ?x))
               ("o que" "é" "que" (?* ?x))
               ("o que" (?* ?x))
               ("quando" (?* ?x))
               ("onde" (?* ?x))
               ("aonde" (?* ?x))
               ("porque" (?* ?x))
               ("quanto" (?* ?x))
               ("como" (?* ?x))
               ("é" "que" (?* ?x))
               ((?* ?x) "é" "o" "que" (?* ?y))
               ((?* ?x) "é" "quem" (?* ?y))
               ((?* ?x) "?")
               )
              :set-performative (list :request)
              :target _mc-find-task)
   (:patterns (((?* ?x) "note" (?* ?y))
               ((?* ?x) "note que" (?* ?y))
               ((?* ?x) "notar" (?* ?y)))
              :set-performative (list :assert)
              :target _mc-find-task)
   (:otherwise 
    :set-performative (list :command)
    :target _mc-find-task))
  )

;;;--------------------------------------------------------------------- (MC) ELIZA

(moss::defstate _mc-ELIZA
  (:label "ELIZA")
  (:explanation
   "Whenever MOSS cannot interpret what the user is saying, ELIZA is called to ~
    do some meaningless conversation to keep the user happy. It then record the ~
    master's input and transfers to find performative state.")
  (:eliza)
  (:transitions
   (:always :target _mc-find-performative))
  )

;;;------------------------------------------------------------------- (MC) FAILURE

(defstate _mc-failure
  (:label "Failure state of main conversation")
  (:explanation
   "Failure state is entered when we return from a sub-dialog with a :failure tag. ~
    We check for more tasks to perform (listed in the task-list slot of the ~
    conversation object). If there are more, we execute the first one. If there ~
    are no more, we return with a failure tag.")
  (:answer-analysis)
  )

(defownmethod
  =answer-analysis _mc-failure (conversation input)
  (declare (ignore input))
  (let* ((task-list (HAS-MOSS-TASK-LIST conversation))
         task)
    (if task-list
      (progn
        ;; record next task
        (setq task (pop task-list))
        ;; remove it from task-list
        (send conversation '=replace 'HAS-MOSS-TASK-LIST task-list)
        ;; add it to conversation
        (send conversation '=replace 'HAS-MOSS-TASK (list task))
        ;; transfer to sub-dialog
        `(:transition ,_mc-task-dialog))
      ;; when no more tasks we return :failure and the state variable still contains
      ;; :failure. The crawler will return one more level until hitting a non
      ;; failure return containing a specific state (e.g. the one for the main
      ;; conversation)
      (list :failure)
      )))

;;;----------------------------------------------------------------- (MC) FIND-TASK

(defstate _mc-find-task
  (:label "Find task")
  (:explanation 
   "Process the input to determine the task to be undertaken. Combines the words ~
    from the sentence to see if they are entry points for the index property of ~
    any task. Collect all tasks for which there is an index in the sentence.")
  (:answer-analysis)
  )

(defownmethod
  =answer-analysis _mc-find-task (conversation input)
  "Using input to find tasks If none failure, if one, OK, if more, must ask user to ~
   select one. Checks for an entry point for an index of a task.
Arguments:
   conversation: current conversation
   input: list containing the input as a list of words"
  (let* ((performative (read-fact conversation :PERFORMATIVE))
         task-list)
    (moss::vformat "_mc-find-task /performative: ~S, package: ~S, input:~%  ~S" 
                   performative *package* input)
    ;; try to find a task from the input text
    (setq task-list (moss::find-objects 
                     '(TASK 
                       (HAS-INDEX-PATTERN 
                        (TASK-INDEX (HAS-INDEX :is :?))))
                     input :all-objects t)) 
    (moss::vformat "_mc-find-task /possible tasks:~%  ~S" task-list)
    ;; filter tasks that do not have the right performative
    (setq task-list
          (mapcan #'(lambda (xx) 
                      (if (intersection performative (HAS-PERFORMATIVE xx))
                        (list xx)))
                  task-list))
    (moss::vformat "_mc-find-task /task list after performative check: ~S" task-list)
    (cond 
     ;; if empty, ask ELIZA
     ((null task-list)
      `(:transition ,_mc-ELIZA))
     ;; if one, then OK
     ((null (cdr task-list))
      ;; save results, make the task the conversation task
      (setf (HAS-MOSS-TASK conversation) task-list)
      `(:transition ,_mc-task-dialog))
     (t 
      ;; otherwise must select one from the results
      (setf (HAS-MOSS-TASK-LIST conversation) task-list)
      `(:transition ,_mc-select-task)))
    ))

;;;--------------------------------------------------------------- (MC) SELECT-TASK

(defstate 
  _mc-select-task
  (:label "mc select task")
  (:explanation "we have located more than one task. We rank the tasks by computing ~
                 the average weight of the terms in the task index slot. We then ~
                 record the list of tasks into the statte-context task-list slot ~
                 and activate the highest ranking task.")
  (:answer-analysis) 
  )

(defownmethod =answer-analysis _mc-select-task (conversation input)
  "we have located more than one task. We rank the tasks by computing ~
   the MYCIN combination weight of the terms in the task index slot. We then ~
   record the list of tasks into the conversation task-list slot ~
   and activate the highest ranking task."
  ;(declare (ignore input))
  ;; the list of tasks is in the HAS-TASK-LIST slot. Tasks are defined in the application
  ;; package
  (let* ((task-list (HAS-MOSS-TASK-LIST conversation))
         patterns weights result pair-list selected-task level word-weight-list)
    (moss::vformat "_select-task /input: ~S~&  task-list: ~S" input task-list)
    ;; first compute a list of patterns (combinations of words) from the input
    (setq patterns (mapcar #'car (moss::generate-access-patterns input)))
    (moss::vformat "_select-task /input: ~S~&  generated patterns:~&~S" 
                   input patterns)
    ;; then, for each task
    (dolist (task task-list)
      (setq level 0)
      ;; get the weight list
      ;(setq weights (HAS-INDEX-WEIGHTS task))
      (setq weights (moss::%get-INDEX-WEIGHTS task))
      (moss::vformat "_select-task /task: ~S~&  weights: ~S" task weights)
      ;; check the patterns according to the weight list
      (setq word-weight-list (moss::%get-relevant-weights weights patterns))  
      (moss::vformat "_select-task /word-weight-list:~&  ~S" word-weight-list)
      ;; combine the weights
      (dolist (item word-weight-list)
        (setq level (+ level (cadr item) (- (* level (cadr item))))))
      (moss::vformat "_select-task /level: ~S" level)
      ;; push the task and weight onto the result list
      (push (list task level) result)
      )
    (moss::vformat "_select-task /result:~&~S" result)
    ;; order the list
    (setq pair-list (sort result #'> :key #'cadr)) 
    (moss::vformat "_select-task /pair-list:~&  ~S" pair-list)
    ;; keep the first task whatever its score
    (setq selected-task (caar pair-list))
    ;; remove the task that have a weight less than task-threshold (default 0.4)
    (setq pair-list
          (remove nil 
                  (mapcar 
                   #'(lambda (xx) 
                       (if (>= (cadr xx) (omas::task-threshold omas::*omas*)) xx))
                   pair-list)))
    ;; if task-list is empty then return th? first saved task
    ;; this may not be a good policy if the score is too low
    (if (null pair-list)
      (progn
        ;; reset the task-list slot of the conversation object
        (setf (HAS-MOSS-TASK-LIST conversation) nil)
        ;; put the saved task into the task slot
        (setf (HAS-MOSS-TASK conversation) (list selected-task))
        ;; go to task-dialog
        `(:transition ,_mc-task-dialog)
        )
      (progn
        ;; remove the weights
        (setq task-list (mapcar #'car pair-list))
        ;; select the first task of the list
        (setq selected-task (pop task-list))
        (moss::vformat "_select-task /selected task: ~S" selected-task)
        ;; save the popped list in the task-list slot of the conversation object
        (setf (HAS-MOSS-TASK-LIST conversation) task-list)
        (setf (HAS-MOSS-TASK conversation) (list selected-task))
        ;; go to task-dialog
        `(:transition ,_mc-task-dialog)))
    ))

;;;--------------------------------------------------------------- (MC) TASK-DIALOG

(defstate _mc-task-dialog
  (:label "Task dialog")
  (:explanation 
   "We found a task to execute (in the GOAL slot of the conversation). We activate ~
    the dialog associated with this task.")
  ;; we launch the task dialog as a sub-conversation
  ;; the task contains the name of a sub-conversation header, e.g. _get-tel-nb
  (:answer-analysis)
  )

;;;********** must check for possible problems
(defownmethod
  =answer-analysis _mc-task-dialog (conversation input)
  "We prepare the set up to launch the task dialog.
Arguments:
   conversation: current conversation"
  (declare (ignore input))
  (let* ((task-id (car (send conversation '=get 'HAS-MOSS-TASK))))
    (moss::vformat "_mc-task-dialog /task-id: ~S dialog: ~S" 
                   task-id (HAS-DIALOG task-id))
    ;; we launch the task dialog as a sub-conversation
    ;; the task contains the name of a sub-conversation header, e.g. _get-tel-nb
    `(:sub-dialog ,(car (HAS-DIALOG task-id)) :failure ,_mc-failure)
    ))


;;;================================================================================

;;; here the strategy is to develop a specific dialog for each task. However, we 
;;; could also use a generic dialog consisiting in:
;;;    - getting the list of required parameters
;;;    - for each parameter, trying to extract info from data using predefined
;;;      rules or extraction patterns (Note that this is not much different than
;;;      having a predefined dialog, albeit focused on a single parameter)
;;;    - if some required parameters are missing, ask master about them using 
;;;      predefined questions
;;;    - shipping result to ad hoc staff agent

;;; The generic dialog could be an option offered to the programmer, which requires
;;; a suitable modeling of the tasks.

;;;================================================================================
;;;
;;;                              Dialog Macro
;;;
;;;================================================================================

;;; simplifies writing simple dialogs with a retry feature
;;; is part of OMAS-MOSS platform in version 8.0.9

;;;---------------------------------------------------------- DEFRETRY-SUBDIALOG

(defMacro defretry-subdialog (label prefix &key explanation from to action
                                    language pattern print-fcn retry sorry)
  (let ((conversation-tag 
         (intern  ; could use make-symbol ?
          (string-upcase (concatenate 'string "_" label "-conversation"))))
        (entry-state-tag
         (intern 
          (string-upcase (concatenate 'string prefix "-entry-state"))))
        (dont-understand-tag
         (intern 
          (string-upcase (concatenate 'string prefix "-dont-understand"))))
         (sorry-tag
          (intern 
           (string-upcase (concatenate 'string prefix "-sorry"))))
         (try-again-tag
          (intern
           (string-upcase (concatenate 'string prefix "-try-again"))))
         )
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (proclaim '(special ,conversation-tag ,entry-state-tag ,sorry-tag
                     ,dont-understand-tag ,try-again-tag)))
       
       (defsubdialog 
         ,conversation-tag
         (:label ,(format nil "~A dialog" label))
         (:explanation ,explanation))
       
       (defstate
         ,entry-state-tag
         (:label ,(format nil "~A dialog - entry state" label))
         (:entry-state ,conversation-tag)
         (:explanation 
          ,(format nil "Assistant is sending a free-style message to the ~A agent."
                   to))
         (:send-message :to ,to :self ,from :action ,action
                        :args `(((:data . ,(read-fact moss::conversation :input))
                                 (:language . ,,language)
                                 ,@,(if pattern `'((:pattern . ,pattern)))
                                 )))
         (:transitions
          (:on-failure :target ,dont-understand-tag)
          (:otherwise
           ,@(if print-fcn `(:print-answer ,(or print-fcn #'print)) '(:display-answer))
           :success)))
       
       (defstate
         ,dont-understand-tag
         (:label "Did not understand. Ask master.")
         (:explanation "could not find ~S with the given information. Thus, ~
                        asking master for more info...")
         (:question-no-erase ,retry)
         (:answer-type :answer)
         (:transitions
          (:always :target ,try-again-tag)))

       
       (defstate
         ,sorry-tag
         (:label ,(format nil "~A failure." label))
         (:explanation "We can't get the info.")
         (:reset)
         (:text ,sorry)
         (:transitions (:failure)))

       (defstate 
         ,try-again-tag
         (:label ,(format nil "Ask again the ~A agent." to))
         (:explanation "Assistant is asking again")
         (:send-message :to ,to :self ,from :action ,action
                        :args `(((:data . ,(read-fact moss::conversation :input))
                                 (:language . ,,language)
                                 ,@,(if pattern `'((:pattern . ,pattern)))
                                 ))) 
         (:transitions
          (:on-failure :target ,sorry-tag)
          (:otherwise 
           ,@(if print-fcn `(:print-answer ,(or print-fcn #'print)) '(:display-answer))
           :success)))
       )))

#|
(defretry-subdialog "get-address" "_gad"
  :explanation "Master is trying to obtain an address."
  :from PA_EUZEBIO :to :EUZEBIO-ADDRESS :action :get-address
  :language :br
  :sorry "Lamento, se voc procura um endereo, eu n‹o o encontro.~%"
  :retry "~%- O endereo de quem ?")

(progn (eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
         (proclaim '(SPECIAL _GET-ADDRESS-CONVERSATION _GAD-ENTRY-STATE _GAD-SORRY
                     _GAD-DONT-UNDERSTAND _GAD-TRY-AGAIN)))
       (DEFSUBDIALOG _GET-ADDRESS-CONVERSATION
         (:LABEL "get-address dialog")
         (:EXPLANATION "Master is trying to obtain an address."))
       (DEFSTATE _GAD-ENTRY-STATE
         (:LABEL "get-address dialog - entry state")
         (:ENTRY-STATE _GET-ADDRESS-CONVERSATION)
         (:EXPLANATION
          "Assistant is sending a free-style message to the EUZEBIO-ADDRESS agent.")
         (:SEND-MESSAGE :TO :EUZEBIO-ADDRESS :SELF PA_EUZEBIO :ACTION :GET-ADDRESS
                        :ARGS
                        (list (list* (list* :DATA (READ-FACT CONVERSATION :INPUT))
                                     (list* (list* :LANGUAGE :BR) NIL))))
         (:TRANSITIONS (:ON-FAILURE :TARGET _GAD-DONT-UNDERSTAND)
                       (:OTHERWISE :DISPLAY-ANSWER :SUCCESS)))
       (DEFSTATE _GAD-DONT-UNDERSTAND
         (:LABEL "Did not understand. Ask master.")
         (:EXPLANATION "could not find ~S with the given information. Thus, ~
                        asking master for more info...")
         (:QUESTION-NO-ERASE "~%- O endereo de quem ?")
         (:ANSWER-TYPE :ANSWER)
         (:TRANSITIONS (:ALWAYS :TARGET _GAD-TRY-AGAIN)))
       (DEFSTATE _GAD-SORRY
         (:LABEL "get-address failure.")
         (:EXPLANATION "We can't get the info.")
         (:RESET)
         (:TEXT
          "Lamento, se voc procura um endereo, eu n‹o o encontro.~%")
         (:TRANSITIONS (:FAILURE)))
       (DEFSTATE _GAD-TRY-AGAIN
         (:LABEL "Ask again the EUZEBIO-ADDRESS agent.")
         (:EXPLANATION "Assistant is asking again")
         (:SEND-MESSAGE :TO :EUZEBIO-ADDRESS :SELF PA_EUZEBIO :ACTION :GET-ADDRESS
                        :ARGS
                        (list (list* (list* :DATA (READ-FACT CONVERSATION :INPUT))
                                     (list* (list* :LANGUAGE :BR) NIL))))
         (:TRANSITIONS (:ON-FAILURE :TARGET _GAD-SORRY)
                       (:OTHERWISE :DISPLAY-ANSWER :SUCCESS))))
|#
;;;================================================================================
;;;
;;;                              Service functions
;;;
;;;================================================================================

;;; Should be part of the OMAS services

#|
;;;---------------------------------------------------------------- DISPLAY-ANSWER

(defUn display-answer (args)
  "prints the list of returned strings (no pattern used)."
  (omas::assistant-display-text 
   PA_EUZEBIO
   (format nil "~{~A~^~2%~}~%" args)))
|#

;;;================================================================================
;;;
;;;                              Sub-Conversations
;;;
;;;================================================================================

;;; Sub-conversations are defined by a conversation header, that points to the
;;; entry state.
;;; There are two output states:
;;;   SUCCESS: the result is stored into the FACTS/result area of the conversation
;;;            object
;;;   FAILURE: no result could be achieved
;;; In case of bad error there is a throw to an :error label
;;; data are obtained from the FACTS/input area if the conversation object


;;;================================================================================
;;;
;;;                           ASK CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to ask a remote PA about something.

;;; We must first locate our receiver (presumably the PA of somebody in the coterie
;;; We then want to prepare topic and text, the send the message

(defsubdialog 
  _ask-conversation 
  (:label "Ask conversation")
  (:explanation 
   "We want our PA to ask something to somebody.")
  (:states _ask-entry-state   
           _ask-choose-destination 
           _ask-get-data
           ;_ask-get-destination
           _ask-send-message
           _ask-sorry
           )
  )

;;;================================================================================
;;;                       ASK CONVERSATION SERVICE FUNCTIONS
;;;================================================================================

;;;------------------------------------------------------------ EXTRACT-DESTINATION
(defUn extract-destination (text-list)
  "tries to obtain the key of a PA as a destination for a message.
Arguments:
   text-list: list of words
Return:
   a PA keyword a list of persons or nil in case of failure."
  (let (agent person-list)
    ;; first try to get an assistant agent directly
    (setq agent (car (moss::locate-objects text-list "omas agent")))
    (print agent)
    (when agent (return-from extract-destination (car (has-ag-key agent))))
    
    ;; if it failed try a person 
    (setq person-list (moss::locate-objects text-list "pessoa"))
    
    ;; if returned something, find the first person with a PA
    (dolist (item person-list)
      (if (HAS-ASSISTENTE-PESSOAL item)
        (return-from extract-destination 
          (car (has-ag-key (car *answer*))))))
    
    ;; otherwise return nil
    ))

#|
(extract-destination nil)
NIL
(extract-destination '("albert"))
:ALBERT
(extract-destination '("envoyer" "un" "message" "à" "jean-paul"))
:ALBERT
(extract-destination '("omar"))
:ALFREDO
|# 
;;;================================================================================
;;;                            ASK CONVERSATION STATES
;;;================================================================================

;;;---------------------------------------------------------- (ASK) ASK-ENTRY-STATE

(defstate 
  _ask-entry-state
  (:label "Ask dialog entry")
  (:entry-state _ask-conversation)
  (:explanation "We try to get destination from the input data.")
  (:answer-analysis)
  )


(defownmethod
  =answer-analysis _ask-entry-state (conversation input)
  "Using list of words to locate receiver of message to send. Must ask user ~
   to confirm findings.
Arguments:
   conversation: conversation object
   input: copy of the FACTS/input area"
  (let* (bindings destination)
    (catch 
      :return
      (cond
       ;; when input is empty then failure
       ((null input)
        (throw :return (list :failure)))
       ;; try to extract destination info from the input text
       ((setq bindings
              (MOSS::PROCESS-PATTERNS 
               input
               '(((?* ?X) "perguntar" "a" (?* ?Y)) 
                 ((?* ?X) "perguntar" (?* ?Z) "a" (?* ?Y))
                 ((?* ?X) "pergunta" (?* ?Y)) 
                 ((?* ?X) "questão" "para" (?* ?Y))
                 )))
        (setq input (cdr (assoc '?Y bindings)))
        )
       ;; otherwise do nothing, we will process the input as it is
       )
      
      ;; try to get destination
      (setq destination (extract-destination input))
      
      (cond
       ;; if null, failure, we could not recognize the destination
       ((null destination)
        (throw :return (list :transition _ask-sorry)))
       ;; list, must make a choice
       ((listp destination)
        ;; save the result into the FACT area and go ask for choice
        (replace-fact conversation :to-choose destination)
        (throw :return (list :transition _ask-choose-destination)))
       ;; otherwise we got destination, save it
       (t
        (replace-fact conversation :to destination)
        ;; and go get data
        (throw :return (list :transition _ask-get-data))))
      )))

;;;------------------------------------------------------- (ASK) CHOOSE-DESTINATION
;;; fake: currently takes the first element of the lisst of choices

(defstate 
  _ask-choose-destination
  (:label "Ask choose destination")
  (:explanation "ambiguous destination (list) select one.")
  (:execute
   (let* ((conversation moss::conversation)
          (choices (read-fact conversation :to-choose))
          )
     ;; remove the temporary data
     (replace-fact conversation :to-choose nil)
     (replace-fact conversation :to (car choices))))
  (:transitions (:always :target _ask-get-data)))

;;;----------------------------------------------------------------- (ASK) GET-DATA

(defstate 
  _ask-get-data
  (:label "set up parameters for continuous input.")
  (:explanation "Let the master type text into the pane until the DONE button is ~
                 pushed. We must disable . and ? terminations.")
  ;; set global flag in the interface window so that we can have . and ? in the text
  (:execute-preconditions
   ;; set up a mark in the input window structure (will be removed automatically)
   (setf (omas::pass-every-char (car (has-moss-input-window moss::conversation))) t)
   ;; include :verbatim flag into the FACT base (?)
   (replace-fact moss::conversation :verbatim t)
   )
  (:question-no-erase 
   "~% ...Digite ou dite o seu texto e depois aperte o botão Terminar.")
  (:transitions
   (:always :target _ask-send-message))
  )

#|
;;;---------------------------------------------------------- (ASK) GET-DESTINATION

(defstate 
  _ask-get-destination
  (:label "obtain master's destination for message.")
  (:explanation "Get the id of the personal assistant of the receiver.")
  ;; set global so that we can have . and ? in the text
  (:question-no-erase 
   "~% A qui voulez-vous envoyer le message ?") 
  (:answer-analysis)
  )

(defownmethod =answer-analysis _ask-get-destination (conversation input)
  "We process the answer and record it unless we get an abort.
Arguments:
   agent: assistant."
  (let ((destination (extract-destination input)))    
    (cond
     ((null destination)
      (list :transition _ask-sorry))
     ((listp destination)
      (replace-fact conversation :to-choose destination)
      (list :transition _ask-choose-destination))
     (t
      (replace-fact conversation :to destination)
      (list :transition _ask-get-data)))))
|#
;;;------------------------------------------------------------- (ASK) SEND-MESSAGE

(defstate 
  _ask-send-message
  (:label "Ask send final message")
  (:explanation "build and send the message.")
  (:send-message-no-wait 
   :to (pop-fact moss::conversation :to)
   :type :request
   :action :ask
   :args (list (list :text (pop-fact moss::conversation :raw-input))))
  (:text "~&...Mensagem enviada")
  (:transitions (:always :success)) ; return
  )

;;;-------------------------------------------------------------------- (ASK) SORRY

(defstate 
  _ask-sorry
  (:label "Ask failure")
  (:explanation "We don't have the needed parameters")
  (:text 
   "lamento, não entendi a quem você quer enviar a mensagem.~%")
  (:reset)
  (:transitions (:failure))
  )

;;;================================================================================
;;;
;;;                          EXPLAIN CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to explain the meaning of a concept.

(defsubdialog 
  _explain-conversation 
  (:label "Explain conversation")
  (:explanation 
   "The data is searched for a concept to be explained.")
  (:states _exp-entry-state  ; required by defstate
           )
  )

;;;----------------------------------------------------- (EXP) EXPLAIN-ENTRY-STATE

(defstate 
  _exp-entry-state
  (:label "Explain dialog entry")
  (:entry-state _explain-conversation)
  (:explanation "The data is cleaned prior to look for an entry point.")
  (:transitions
   ;;remove useless words explain, define, or else...
   (:patterns (((?* ?x) "definir" (?* ?y))
               ((?* ?x) "definição" (?* ?y))
               ((?* ?x) "explicar" (?* ?y))
               ((?* ?x) "explique" (?* ?y))
               ((?* ?x) "o que" "quer" "dizer" (?* ?y))
               ("é" "que" (?* ?y) "?")
               ((?* ?y) "é" "que" (?* ?x))
               )
              :keep ?y ; put a list of the binding value into context HAS-DATA
              :sub-dialog _print-concept-documentation-conversation)
   ;; we do not know how the user got here and keep everything
   (:otherwise :sub-dialog _print-concept-documentation-conversation)
   )
  )

;;;================================================================================
;;;
;;;                      GET ADDRESS CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to print the address of a person or of a
;;; place. It fills the task pattern and sends the message to the address specialist.
;;; The simplest version is to send to the ADDRESS agent, wait for the result and
;;; print it.

(defretry-subdialog "get-address" "_gad"
  :explanation "Master is trying to obtain an address."
  :from PA_EUZEBIO :to :EUZEBIO-ADDRESS :action :get-address
  :language :br
  :sorry "Lamento, se você procura um endereço, eu não o encontro.~%"
  :retry "~%- O endereço de quem ?")

  
;;;================================================================================
;;;
;;;                      GET HOME ADDRESS CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to print the home address of a person.
;;; It builds a task pattern and sends the message to the address specialist.
;;; The simplest version is to send to the ADDRESS agent, wait for the result and
;;; print it.

(defsubdialog 
  _get-home-address-conversation 
  (:label "Get address conversation")
  (:explanation 
   "Master is trying to obtain a private address.")
  (:states _ghad-entry-state
           _ghad-dont-understand
           _ghad-process-answer
           _ghad-try-again
           _ghad-try-again-process-answer
           _ghad-sorry)
  )
;;;================================================================================
;;;                  GET HOME ADDRESS CONVERSATION SERVICE FUNCTIONS
;;;================================================================================

;;;------------------------------------------------------------ PRINT-HOME-ADDRESS

(defUn print-home-address (addr-list)
  "prints a list of names and addresses"
  ;(format t "~&+++ EUZEBIO/ print-address language: ~S address:~% ~S" 
  ;         *language* addr-list)
  (omas::assistant-display-text 
   PA_EUZEBIO
   (format nil "~{~A~^~2%~}~%" 
           (mapcar #'print-single-home-address addr-list))))

;;;----------------------------------------------------- PRINT-SINGLE-HOME-ADDRESS
;;; Each entry is like 
;;;   (("name" "Barthès")("first name" "Jean-Paul")("home address" "..."))
;;; Home address possibly empty

(defUn print-single-home-address (addr)
  "returns a single charater string"
  (format nil "~{~A~^ ~}: ~{~A~^ ~}~%~{~A~^ ~}"
          ;; get person name
          (or (cdr (assoc "sobrenome" (cdr addr) :test #'string-equal)) 
	      (list "?"))
	  ;; name
	  (or (cdr (assoc "nome" (cdr addr) :test #'string-equal)) 
	      (list "?"))
	  ;; address
	  (or (cdr (assoc "endereço privado" (cdr addr) :test #'string-equal))
	      (list "endereço privado non disponivel..."))))

#|
? (print-single-home-address '("pessoa"
                                 ("sobrenome" "Barthès")("nome" "Camille")
                                      ("endereço privado")))
"Barthès: Dominique
endereço privado non disponivel...
"
|#
;;;================================================================================
;;;                    GET HOME ADDRESS CONVERSATION STATES
;;;================================================================================		   

;;;------------------------------------------- (GHAD) GET-HOME-ADDRESS-ENTRY-STATE

(defstate 
  _get-home-address-entry-state
  (:entry-state _get-home-address-conversation)
  (:label "Get home address dialog entry")
  (:explanation "Assistant is sending a free-style message to the ADDRESS agent.")
  (:send-message :to :EUZEBIO-ADDRESS :self PA_EUZEBIO :action :get-address
                 :args `(((:data . ,(read-fact moss::conversation :input))
                          (:language . :br)
                          (:pattern . ("pessoa" 
                                       ("sobrenome")
                                       ("nome")
                                       ("endereço privado"))))))
  (:transitions
   (:on-failure :target _ghad-dont-understand)
   (:otherwise
    :print-answer #'print-home-address :success))
  )

;;;-------------------------------------------------------- (GHAD) DONT-UNDERSTAND

(defstate 
  _ghad-dont-understand
  (:label "Did not understand. Ask master.")
  (:explanation "could not find the address with the given information. Thus, ~
                 asking master for whose address...")
  (:question "~%- O endereço pessoal de quem ?")
  (:answer-type :answer)
  (:transitions
   (:always :save :target _ghad-try-again)))

;;;------------------------------------------------------------------ (GHAD) SORRY

(defstate 
  _ghad-sorry
  (:label "Address failure.")
  (:explanation "We don't have the requested address.")
  (:text 
   "Lamento, se você procura um endereço pessoal, eu não o encontro.~%")
  (:reset)
  (:transitions (:always :failure))
  )

;;;-------------------------------------------------------------- (GHAD) TRY-AGAIN

(defstate 
  _ghad-try-again
  (:label "Ask again for address.")
  (:explanation "Assistant is asking ADDRESS staff agent.")
  (:send-message :to :EUZEBIO-ADDRESS :self PA_EUZEBIO :action :get-address
                 :args `(((:data . ,(read-fact moss::conversation :input))
                          (:language . :br)
                          (:pattern . ("pessoa" 
                                       ("sobrenome")
                                       ("nome")
                                       ("endereço" ("endereço privado")))))))
  (:transitions
   (:on-failure :target _ghad-sorry)
   (:otherwise :print-answer #'print-home-address :success)
   )
  )


;;;================================================================================
;;;
;;;                      GET NEWS BY SECTION CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to print news about a specific topic. It fills 
;;; the task pattern and sends the message to the news specialist.

(defsubdialog 
  _get-news-by-section-conversation 
  (:label "Get news by section conversation")
  (:explanation 
   "Master is trying to obtain news of a specific section.")
  (:states _gnbsd-entry-state
           _gnbsd-dont-understand
           _gbsd-try-again
           _gnbsd-sorry)
  )

;;;------------------------------------------------------------ (GNBSD) ENTRY-STATE

(defstate 
  _get-news-by-section-entry-state
  (:entry-state _get-news-by-section-conversation)
  (:label "Get news by section dialog entry")
  (:explanation "Assistant is sending a free-style message to the NEWS agent.")
  (:send-message :to :NEWS :self PA_EUZEBIO :action :get-news-by-section
                 :args `(((:data . ,(moss::read-fact moss::conversation :input))
                          (:language . :br))))
  (:transitions
     (:on-failure :target _gnbsd-dont-understand)
	 (:otherwise :display-answer :success))
  )

;;;-------------------------------------------------------- (GNBSD) DONT-UNDERSTAND

(defstate 
  _gnbsd-dont-understand
  (:label "Did not understand. Ask master.")
  (:explanation "could not find news in the requested section. Thus, ~
                 asking master for what section...")
  (:question-no-erase "~%- Vous voulez des nouvelles sur quelle rubrique ?")
  (:answer-type :answer)
  (:transitions
   (:always :target _gnbsd-try-again)))

;;;------------------------------------------------------------------ (GNBSD) SORRY

(defstate 
  _gnbsd-sorry
  (:label "News obtention failure.")
  (:explanation "We don't have the requested news.")
  (:text "Desolé, je n'ai rien trouvé.~%")  
  (:reset)
  (:transitions (:failure))
  )

;;;-------------------------------------------------------------- (GNBSD) TRY-AGAIN

(defstate 
  _gnbsd-try-again
  (:label "Ask again for news.")
  (:explanation "Assistant is asking :NEWS.")
  (:send-message :to :NEWS :self PA_EUZEBIO :action :get-news-by-section
                 :args `(((:data . ,(moss::read-fact moss::conversation :input))
                          (:language . :br))))
  (:transitions
	 (:on-failure :target _gnbsd-sorry)
	 (:otherwise :display-answer :success))
  )

;;;================================================================================
;;;
;;;                          GET PHONE CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to print the telephone number of a person or of a
;;; place. It fills the task pattern and sends the message to the address specialist.
;;; The simplest version is to send to the ADDRESS agent, wait for the result and
;;; print it.

(defsubdialog 
  _get-phone-conversation
  (:label "Get phone dialog")
  (:explanation "Master is trying to obtain a telephone number.")
  (:states _gpd-entry-state
           _gpd-dont-understand
           _gpd-process-answer
           _gpd-process-second-answer
           _gpd-try-again
           _gpd-sorry)
  )

;;;------------------------------------------------------------------- PRINT-PHONE

(defUn print-phone (phone-list)
  "prints the list of returned strings (no pattern used)."
  ;(format t "~&+++ EUZEBIO/ print-address language: ~S address:~% ~S" 
  ;         *language* phone-list)
  (omas::assistant-display-text 
   PA_EUZEBIO
   (format nil "~{~A~^~2%~}~%" (mapcar #'car phone-list))))

;;-------------------------------------------------------------------- MAKE-ALIST

(defUn make-alist (ll)
  (cond ((null ll) nil)
        (t (cons (list (car ll)(cadr ll)) 
                 (make-alist (cddr ll))))))
#|
? (make-alist '(:OWNER ($E-PERSON.1) :CELL-PHONE "(0)6 80 45 32 67" :OFFICE-PHONE
  "(0)3 44 23 44 66" :HOME-PHONE "(0)3 44 23 31 37"))
((:OWNER ($E-PERSON.1)) (:CELL-PHONE "(0)6 80 45 32 67")
 (:OFFICE-PHONE "(0)3 44 23 44 66") (:HOME-PHONE "(0)3 44 23 31 37"))
|#
;;;================================================================================
;;;                    GET TELEPHONE CONVERSATION STATES
;;;================================================================================

;;;------------------------------------------------------------- (GPD) ENTRY-STATE

(defstate 
  _gpd-entry-state
  (:label "Get phone dialog")
  (:entry-state _get-phone-conversation)
  (:explanation "Assistant is sending a free-style message to the ADDRESS agent.")
  (:send-message :to :EUZEBIO-ADDRESS :self PA_EUZEBIO :action :get-phone-number
                 :args `(((:data . ,(read-fact moss::conversation :input))
                          (:language . :br))))
  (:transitions
   (:on-failure :target _gpd-dont-understand)
   (:otherwise :display-answer :success)
   )
  )

;;;--------------------------------------------------------- (GPD) DONT-UNDERSTAND

(defstate 
  _gpd-dont-understand
  (:label "Did not understand. Ask master.")
  (:explanation "Assistant is asking master for whose phone.")
  (:question "- O número de telefone de quem ?")
  (:answer-type :answer)
  (:transitions (:always :target _gpd-try-again))
  )

;;;------------------------------------------------------------------- (GPD) SORRY

(defstate 
  _gpd-sorry
  (:label "Phone number failure.")
  (:explanation "We don' have the requested number.")
  (:reset)
  (:text "- Lamento, não encontro o número solicitado.")
  (:transitions (:failure))
  )

;;;--------------------------------------------------------------- (GPD) TRY-AGAIN

(defstate 
  _gpd-try-again
  (:label "Ask again for phone number.")
  (:explanation "Assistant is asking ADDRESS.")
  (:send-message :to :EUZEBIO-ADDRESS :self PA_EUZEBIO :action :get-phone-number
                 :args `(((:data . ,(read-fact moss::conversation :input))
                          (:language . :br))))
  (:transitions
   (:on-failure :target _gpd-sorry)
   (:otherwise :display-answer :success)
   )
  )

;;;===============================================================================
;;;
;;;                       GET LAST NEWS CONVERSATION
;;;
;;;===============================================================================

;;; A noter qu'il n'y pas de pattern, donc on peut utiliser la fontion par défaut
;;; display-answer

(defsimple-subdialog  "get-last-news" "_gln"
  :explanation "This dialog is meant to show the last news."
  :from PA_EUZEBIO
  :to :NEWS
  :action :get-last-news
  :language :br
  :sorry "~%- Je n'ai rien trouvé. ~
          Peut-être n'y a-t-il pas de nouvelles sous cette rubrique ?"
  )
  
;;;================================================================================
;;;
;;;                  PRINT CONCEPT DOCUMENTATION CONVERSATION
;;;
;;;================================================================================

;;; this conversation is meant to print the definition of a concept or properties
;;; to be extracted from a list of words

(defsubdialog
  _PRINT-CONCEPT-DOCUMENTATION-CONVERSATION
  (:label "Print concept documentation")
  (:explanation 
   "This dialog is meant to print the documentation attached to concepts or ~
    properties referenced by a list of words.")
  (:states _pcd-entry-state
           _pcd-sorry)
  )

;;;-------------------------------------------------------------- (PCD) ENTRY-STATE

(defstate 
  _pcd-entry-state
  (:label "start of the PRINT CONCEPT DOCUMENTATION dialog")
  (:entry-state _print-concept-documentation-conversation)
  (:explanation 
   "Initial state may have data left in the input area. The data either represent ~
    an entry point or something else that should be used to select objects.")
  (:answer-analysis)
  )

(defownmethod
  =answer-analysis _pcd-entry-state (conversation input)
  "Using list of words to access objects. If one, OK, if more, must ask user to ~
   select one, if none, failure."
  ;(declare (ignore more-args))
  (let* ((*language* :br)
         entry-point-list object-list)
    (catch :return
      (moss::vformat "_pcd-entry-state /input: ~%  ~S" input)
      ;; first get entry-points
      (setq entry-point-list (moss::find-best-entries input))
      
      ;; if we cannot find anything locally try for a what-is conversation
      (unless entry-point-list
        (throw :return 
               `(:sub-dialog _wh?t-is-conversation :failure ,_pcd-sorry)))
      
      ;; get objects corresponding to entry points
      ;(moss::vformat "_pcd-entry-state /entry-point-list: ~%  ~S" entry-point-list)
      (setq object-list (delete-duplicates (mapcan #'access entry-point-list)))
      (moss::vformat "_pcd-entry-state /object-list: ~%  ~S" object-list)
      
      ;; first filter concepts and properties
      (setq object-list
            (mapcan #'(lambda (xx)
                        (or 
                         (moss::%is-concept? xx)
                         (moss::%is-attribute? xx)
                         (moss::%is-relation? xx)
                         )
                        (list xx))
                    object-list))
      ;(print object-list)
      (if object-list
        ;; print documentation
        (dolist (obj-id object-list)
          (cond
           ((moss::%is-concept? obj-id)
            (send obj-id '=get-documentation :lead "CONCEPT: " 
                  :final-new-line t)
            (send conversation '=display-text *answer*))
           ((moss::%is-attribute? obj-id)
            (send obj-id '=get-documentation :lead "ATTRIBUT: " 
                  :final-new-line t)
            (send conversation '=display-text *answer*))
           ((moss::%is-relation? obj-id)
            (send obj-id '=get-documentation :lead "RELATION: " 
                  :final-new-line t)
            (send conversation '=display-text *answer*))
           )
          )
        ;; otherwise print failure
        (throw :return 
               `(:transitions ,_pcd-sorry))
        ;(send conversation '=display-text 
        ;      "~%Désolé, je n'ai pas de concept ni de propriété correspondant ~
        ;       à votre demande.~%")
        )
      ;; success
      (list :success))))

;;;------------------------------------------------------------------- (PCD) SORRY

(defstate 
  _pcd-sorry
  (:label "Reference search failure.")
  (:explanation "We did not find any explanation for the reference.")
  (:reset)
  (:text "~%- Lamento, não tenho conceito nem propriedade correspondendo~
          à sua demanda.~%." :no-erase)
  (:transitions 
   (:failure))
  )

;;;================================================================================
;;;
;;;                       PRINT HELP CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to help the master by giving information:
;;;   - in general (what tasks are available
;;;      "what can you do for me?" "help." "what can I do?"
;;;   - on a particular topic
;;;      "how do I send a mail?" "help me with the mail?"
;;; specific help corresponding to special help tasks are caught by the task 
;;; task selection mechanism?

(defsubdialog 
  _print-help-conversation 
  (:label "Help conversation")
  (:explanation "Help was asked.")
  (:states _ph-print-global-help)   
  )

;;;-------------------------------------------------------- (PH) PRINT-GLOBAL-HELP

(defstate
  _ph-print-global-help
  (:label "Print general help")
  (:entry-state _print-help-conversation )
  (:explanation "No specific subject was included.")
  (:execute
   (let
     ((obj-id (car (send '>-global-help '=get 'is-title-of)))
      (*language* :br)
      (task-list (access '(task)))
      (conversation (omas::conversation EUZEBIO::PA_EUZEBIO))
      )
     (when obj-id (send obj-id '=get-documentation)
           (send conversation '=display-text *answer*))
     (send conversation '=display-text
           "~2%Eu posso fazer o seguinte :")
     (dolist (task task-list)
       (send task '=get-documentation :lead "   - ")   
       (send conversation '=display-text *answer*)
       )
     (send conversation '=display-text "~2%")
     ))
  (:transitions
   (:always :success))
  )


;;;================================================================================
;;;
;;;                         PROCESS ASK CONVERSATION
;;;
;;;================================================================================

;;; this conversation is meant to post a message for inspection and send a possible 
;;; answer, or abort the corresponding task

;;; The problem is that the PA has received a request corresponding to the message
;;; being processed and the static part of the skill has posted the message and
;;; is waiting for the answer.
;;; One problem is that there are as many threads as posted messages waiting for an 
;;; answer.

;;; If we went to answer the request message, then the situation is quite complex:
;;; we have several processes:
;;;   - the thread executing the ask skill
;;;   - the converse process
;;;   - the assistant window thread handling the callbacks
;;;   - the timeout thread on the ask thread
;;; One can solve all the problems but the result is not simple.
;;;
;;; A better approach is to consider that all messages ASk and TELL are considered
;;; as INFORM messages, and do not create any specific process (i.e. waiting for a
;;; master's answer), à la e-mail. This allows to send answers directly as INFORM
;;; messages, bypassing the complex mechanism of getting the control back to the
;;; ASK skill.
;;; Thus, all request messages triggering the ASK skill must send a response like
;;; "message well received, transmitted to the master." 

;;; In the simplified approach the role of the PROCESS ASK conversation is to answer
;;; the message when the PROCESS (TRAITER) button is clicked.
;;; The sequence of events is:
;;;   - the ":process-ask" tag is inserted into the agent TO-DO slot
;;;   - the converse process wakes up and triggers the PROCESS ASK dialog
;;;   - a message is posted to the master
;;; If the master wants to answer then
;;;   - the receiver is first determined
;;;   - the *pass-every-char* flag (that should be a specific slot of the interface
;;;     window) is set
;;;   - the master inputs text
;;;   - the DONE (Terminé) button is clicked resetting *pass-every-char* 
;;;   - the text is transferred into the :input area of FACTS
;;;   - the message is sent to the receiver directly as an inform message

(defsubdialog 
  _process-ask-conversation
  (:label "Process ask dialog")
  (:explanation "Display the content of a question message (ask).")
  (:states _pa-entry-state  ; required by defstate
           _pa-brush ; clean up lightly
           _pa-destroy ; destroy message return :abort to ask-static
           _pa-discard? ; ask whather we dump the question or not
           _pa-get-answer ; get the answer from the master
           )
  )

;;;=========================== service functions ==================================

;;;------------------------------------------------------ (PA) DISPLAY-ASK-MESSAGES

(defUn display-ask-message (message conversation)
  "takes a message and displays its content into the assistant pane"
  (let (sender content)
    ;; get sender identity
    (setq sender (omas::from! message))
    ;; get content of the message
    (setq content (car (omas::args message)))
    ;; ask to print it
    (send conversation '=DISPLAY-TEXT 
          (format nil "Remetente : ~A ~%Urgência : ~A ~%Assunto : ~A ~2% ~A"
                  sender
                  (or (cadr (member :priority content)) "Normal")
                  (or (cadr (member :object content)) "")
                  (or (getf content :text) "<empty message>")))))

;;;================================================================================

;;;-------------------------------------------------------------- (PA) ENTRY-STATE
;;; We enter the dialog when the PROCESS (Traiter) button has been clicked. 
;;; FACTS/INPUT contains ":proces-ask" and message is in FACTS/TODO-MESSAGE

(defstate 
  _pa-entry-state
  (:label "process ask dialog")
  (:entry-state _process-ask-conversation)
  (:explanation "Shows the message and ask if an answer is needed.")
  (:execute-preconditions
   ;; the :input area contains a pair (<message> . <process-id>)
   (display-ask-message (read-fact moss::conversation :todo-message)
                        moss::conversation))
  (:question-no-erase "~2%Você quer responder a esta mensagem ?")
  (:transitions 
   (:yes :target _pa-get-answer) 
   (:otherwise :target _pa-discard?))
  )

;;;-------------------------------------------------------------------- (PA) BRUSH
;;; reset conversation slot
(defstate 
  _pa-brush
  (:label "bookkeeping")
  (:explanation "do some light bookkeeping.")
  (:execute (replace-fact moss::conversation :todo-message nil))
  (:transitions
   (:always :success))
  )

;;;------------------------------------------------------------------ (PA) DESTROY


(defstate 
  _pa-destroy
  (:label "Destroy ASK message.")
  (:explanation "Master does not want to answer the message nor keep it. We ~
                 remove it from the list and return to the task with an :abort~
                 mark, so that it will exit without answering.")
  (:transitions
   (:always
    :exec 
    (let ((agent (car (has-moss-agent moss::conversation))))
      ;; discard the message from the list, assuming it is still selected
      (omas::ASSISTANT-DISCARD-SELECTED-TO-DO-TASK agent)
      ;; clean up conversation
      (replace-fact moss::conversation :todo-message nil))
    :success))
  )

;;;----------------------------------------------------------------- (PA) DISCARD?

(defstate 
  _pa-discard?
  (:label "ask whether we throw the message away.")
  (:explanation "Master did not answer message, ask master if we keep it to it ~
                 later.")
  (:question-no-erase "~2%Você quer guradar esta mensagem para responde-la mais tarde ?")
  (:transitions
   (:no :target _pa-destroy)
   (:otherwise :target _pa-brush))
  )

;;;--------------------------------------------------------------- (PA) GET-ANSWER

(defstate 
  _pa-get-answer
  (:label "obtain master's answer to an ask message.")
  (:explanation "Record text typed into the master pane until the DONE button is ~
                 pushed. We must disable . and ? terminations.")
  ;; set global so that we can have . and ? in the text
  (:execute-preconditions 
   (let ((win (car (has-moss-input-window moss::conversation))))
     (when win
       (setf (omas::pass-every-char win) t)))
   )
  (:question-no-erase "~% ...Digite a sua resposta e depois aperte o botão Terminar.") 
  (:answer-analysis)
  )

(defownmethod =answer-analysis _pa-get-answer (conversation input)
  "We process the answer and send it back unless we get an abort.
Arguments:
   conversation: conversation object is
   input: copy of the :input area of FACTS"
  (let (agent message content object answer)
    (setq agent (car (has-moss-agent conversation)))
    ;(print  `(=====> input ,input))
    ;; if input is different from :abort we build a fuller answer
    (unless (eql input :abort)
      (setq message (read-fact conversation :todo-message)
            content (car (omas::args message))
            object (getf content :object))
      ;; format an answer content adding object to text
      (setq answer (list :object (format nil "RE: ~A" (or object " "))	
                         :text input))
      ;; make an answer and send it back as an INFORM message
      (send-message 
       (make-instance 'omas::message
         :from :EUZEBIO
         :to (omas::from! message)
         :date (get-universal-time)
         :type :inform
         :action :tell
         :args (list answer)
         )
       )
      )
    ;; clean FACTS
    (replace-fact conversation :todo-message nil)
    ;; remove message from the tasks to do pane
    (omas::ASSISTANT-DISCARD-SELECTED-TO-DO-TASK agent)
    )
  ;; we then get out of dialog
  '(:success))

;;;================================================================================
;;;
;;;                         PROCESS TELL CONVERSATION
;;;
;;;================================================================================

;;; this conversation is meant to post a message for inspection and quit

(defsubdialog 
  _process-tell-conversation
  (:label "Process tell dialog")
  (:explanation "Display the content of an info message (tell).")
  (:states _pt-entry-state  ; required by defstate
           _pt-erase? ; go clean the message
           _pt-clean ; remove item from list
           _pt-brush ; clean up lightly
           )
  )

;;;=========================== service functions ==================================

;;;----------------------------------------------------- (PT) DISPLAY-TELL-MESSAGES

(defUn display-tell-message (message conversation)
  "takes a message and displays its content into the assistant pane"
  (let (sender content)
    ;; get sender identity
    (setq sender (omas::from! message))
    ;; get content of the message
    (setq content (car (omas::args message)))
    ;; ask to print it
    (send conversation '=DISPLAY-TEXT 
          (format nil "Remetente : ~A ~%Urgência : ~A ~%Assunto : ~A ~2% ~A"
                  sender
                  (or (cadr (member :priority content)) "Normal")
                  (or (cadr (member :object content)) "")
                  (or (getf content :text) "<empty message>")))))

;;;================================================================================
;;;                        PROCESS TELL CONVERSATION STATES
;;;================================================================================

;;;-------------------------------------------------------------- (PT) ENTRY-STATE

(defstate 
  _pt-entry-state
  (:label "Process tell dialog")
  (:entry-state _process-tell-conversation)
  (:explanation "Affiche un message et demande si on le garde.")
  (:execute (display-tell-message 
             (car (read-fact moss::conversation :message)) MOSS::Conversation))
  (:question-no-erase "~2%OK? Apago a mensagem?")
  (:transitions 
   (:yes :target _pt-clean) 
   (:otherwise :target _pt-brush))
  )

;;;-------------------------------------------------------------------- (PT) BRUSH

(defstate 
  _pt-brush
  (:label "bookkeeping")
  (:explanation "remove item from facts.")
  (:execute (replace-fact moss::conversation :message nil))
  (:transitions
   (:always :success))
  )

;;;-------------------------------------------------------------------- (PT) CLEAN

(defstate 
  _pt-clean
  (:label "Cleaning state")
  (:explanation "Ask whether we keep the message in the to-do pane.")
  (:execute (omas::ASSISTANT-DISCARD-SELECTED-TO-DO-TASK 
             (car (has-moss-agent moss::conversation)))
            (replace-fact moss::conversation :message nil))
  (:transitions
   (:always :success))
  )


;;;================================================================================
;;;
;;;                      PUBLISH NEWS CONVERSATION
;;;
;;;================================================================================

(defsubdialog 
  _publish-news-conversation
  (:label "Publish new conversation")
  (:explanation 
   "Master is trying to publish a new.")
  (:states _publish-new-conversation; required by defstate
           _publish-new-entry-state
           _pnd-get-category
           _pnd-get-content
	       _pnd-get-sections
           _pnd-publish
           _pnd-sorry)
  )

;;;------------------------------------------------------------- (PND) ENTRY-STATE

(defstate 
  _publish-new-entry-state
  (:entry-state _publish-news-conversation)
  (:label "Publish new dialog entry")
  (:explanation "Ask for the title of the new.")  
  (:question-no-erase "~%- Quelle est le titre de la nouvelle ?")
  (:answer-type :answer)
  (:execute 
   (omas::remember PA_EUZEBIO (moss::read-fact moss::conversation :raw-input)
     	           :news-title))
  (:transitions
   (:always  :target _pnd-get-category)))

;;;------------------------------------------------------------ (PND) GET-CATEGORY

(defstate 
  _pnd-get-category
  (:label "Ask master the content.")
  (:explanation "Obtain title, ask for category")
  ;(format t "~S~%" (moss::read-fact moss::conversation :input))
  ;(format t "~% TITULO A GUARDAR: ~S" (moss::read-fact moss::conversation :input))
  ;; Store title in agent´s short term memory  OMARAGP PORQUE ESTO NO FUNCIONA  
  (:question-no-erase "~%- Dans quelle rubrique voulez-vous enregistrer la nouvelle ?")
  (:execute 
   (omas::remember PA_EUZEBIO (moss::read-fact moss::conversation :input)
     	           :news-section))
  (:transitions
   (:patterns ((?* ?x) "rubricas" (?* ?y))
              :target _pnd-get-sections)
   (:otherwise  :save :target _pnd-get-content)))

;;;------------------------------------------------------------- (PND) GET-CONTENT

(defstate 
  _pnd-get-content
  (:label "Ask master the content.")
  (:explanation "Obtain category, ask for content")
  
  (:execute-preconditions 
   (setf (omas::pass-every-char (car (has-moss-input-window moss::conversation))) t)
   ;; include :verbatim flag into the FACT base (?)
   (replace-fact moss::conversation :verbatim t)
   )
  (:question-no-erase 
   "~%- Ecrivez le contenu de la nouvelle et puis cliquez sur terminer")
  (:answer-type :answer)
  (:execute 
   ;; Store category in agent´s short term memory						   
   (remember PA_EUZEBIO (moss::read-fact moss::conversation :input)
     	     :news-content))
  (:transitions
   (:always  :target _pnd-publish)))

;;;------------------------------------------------------------ (PND) GET-SECTIONS

(defstate 
  _pnd-get-sections
  (:label "Ask NEWS about sections.")
  (:explanation "Master does not know sections. Asking NEWS for the list.")
  (:send-message :to :NEWS :self PA_EUZEBIO :action :get-all-sections
                 :args '((:language . :br)))
  (:execute 
   (omas::assistant-display-text 
    PA_EUZEBIO
    (format nil "~S" (moss::read-fact moss::conversation :answer))))
  (:transitions
   (:on-failure :target _pnd-sorry)
   (:otherwise
    ;:print-answer #'print-sections :target _pnd-get-category)
    :target _pnd-get-category)))

;;;----------------------------------------------------------------- (PND) PUBLISH

(defstate 
  _pnd-publish
  (:label "Ask master the content.")
  (:explanation "Ask NEWS to create the news")
  (:send-message :to :NEWS :self PA_EUZEBIO :action :publish-news
                 ;; Recall gets data stored un ALBERT´s short term memory
                 :args `(((:data ("title" ,(recall PA_EUZEBIO :news-title))
                                 ("section" . ,(recall PA_EUZEBIO :news-section))
                                 ("content" . ,(recall PA_EUZEBIO :news-content))
                                 ("author" "EUZEBIO"))
                          (:language . :br))))
  ;(:manual-resume)
  (:execute (print (car (READ-FACT MOSS::CONVERSATION :INPUT))))
  (:transitions
   (:on-failure :target _pnd-sorry)
   (:otherwise :success))
  )

;;;---------------------------------------------------------------- (PND) PND-SORRY

(defstate 
  _pnd-sorry
  (:label "Publish failure.")
  (:explanation "It was an error while publishing the new.")
  (:text "Desolé, l'agent NEWS n'a pas pu publier votre texte.~%")
  (:reset)
  (:transitions (:failure))

  )

;;;================================================================================
;;;
;;;                       SET FONT SIZE CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to modify the size of the dialog font, usually to
;;; increase it or to return to the default value.

(defsubdialog 
  _set-font-size-conversation
  (:short-name sfs)
  (:label "Font size conversation")
  (:explanation 
   "The data is searched for the presence of words like increase, ~
    bigger, smaller, defaults, reset,...")
  ;(:action _sfs-action)
  (:states _sfs-entry-state  ; required by defstate
           _sfs-dont-understand
           _sfs-try-again
           _sfs-sorry)
  )

;;;--------------------------------------------------- (SFS) FONT-SIZE-ENTRY-STATE

(defstate 
  _sfs-entry-state
  ;(:conversation _set-font-size-conversation)
  (:label "Font size dialog entry")
  (:entry-state _set-font-size-conversation)
  (:explanation "The data is searched for the presence of words like increase, ~
                 bigger, defaults, reset,...")
  (:transitions
   (:patterns (((?* ?x) "aumentar" (?* ?y))
               ((?* ?x) "maior" (?* ?y))
               ((?* ?x) "muito" "pequeno" (?* ?y))
               ((?* ?x) "mais" "gordo" (?* ?y)))
              :exec (omas::assistant-use-larger-font PA_EUZEBIO) :success)
   (:patterns (((?* ?x) "diminuir" (?* ?y))
               ((?* ?x) "menor" (?* ?y))
               ((?* ?x) "muito" "grande" (?* ?y))
               ((?* ?x) "menos" "gordo" (?* ?y)))
              :exec (omas::assistant-reset-font PA_EUZEBIO) :success)
   (:patterns (((?* ?x) "utilisar" (?* ?y) "defaut" (?* ?z))
               ((?* ?x) "reset" (?* ?y))
               ((?* ?x) "defaut" (?* ?y)))
              :exec (omas::assistant-reset-font PA_EUZEBIO) :success)
   (:otherwise :target _sfs-dont-understand)
   )
  )

;;;--------------------------------------------------------- (SFS) DONT-UNDERSTAND

(defstate 
  _sfs-dont-understand
  (:label "Did not understand. Ask master.")
  (:explanation "Assistant is asking master for font-size clarification.")
  (:question "~%- Você deseja que eu escreva maior, menor ou que eu use o tamanho padrão ?")
  (:transitions
   (:always :target _sfs-try-again)))

;;;------------------------------------------------------------------- (SFS) SORRY

(defstate 
  _sfs-sorry
  (:label "Font dialog failure.")
  (:explanation "We cannot make things out from the data.")
  (:reset)
  (:text "~%- Lamento, não vejo o que você quer fazer com as letras.~%")
  (:transitions
   (:always :failure))
  )

;;;--------------------------------------------------------------- (SFS) TRY-AGAIN

(defstate 
  _sfs-try-again
  (:label "Font size try again")
  (:explanation "we asked master and try to pick up the answer.")
  (:transitions
   (:patterns (((?* ?x) "aumentar" (?* ?y))
               ((?* ?x) "maior" (?* ?y))
               ((?* ?x) "muito" "pequeno" (?* ?y))
               ((?* ?x) "mais" "gordo" (?* ?y)))
              :exec (omas::assistant-use-larger-font PA_EUZEBIO) :success)
   (:patterns (((?* ?x) "diminuir" (?* ?y))
               ((?* ?x) "menor" (?* ?y))
               ((?* ?x) "muito" "grande" (?* ?y))
               ((?* ?x) "menos" "gordo" (?* ?y)))
              :exec (omas::assistant-reset-font PA_EUZEBIO) :success)
   (:patterns (((?* ?x) "utilisar" (?* ?y) "defaut" (?* ?z))
               ((?* ?x) "reset" (?* ?y))
               ((?* ?x) "defaut" (?* ?y)))
              :exec (omas::assistant-reset-font PA_EUZEBIO) :success)
   (:otherwise :target _sfs-sorry)
   )
  )
;;;================================================================================
;;;
;;;                           TELL CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to tell something to a remote PA.

;;; We must first locate our receiver (presumably the PA of somebody in the coterie
;;; We then want to prepare topic and text, the send the message

(defsubdialog 
  _tell-conversation 
  (:label "Tell conversation")
  (:explanation 
   "We want our PA to tell something to somebody.")
  (:states _tell-entry-state  ; required by defstate            
           _tell-choose-destination 
           _tell-get-data
           ;_tell-get-destination
           _tell-send-message
           _tell-sorry
           ) 
  )

;;;================================================================================
;;;                       TELL CONVERSATION SERVICE FUNCTIONS
;;;================================================================================

;;;------------------------------------------------------------ EXTRACT-DESTINATION
;;; See ASK dialog
;;;================================================================================
;;;                            TELL CONVERSATION STATES
;;;================================================================================

;;;-------------------------------------------------------- (TELL) TELL-ENTRY-STATE

(defstate 
  _tell-entry-state
  (:label "Tell dialog entry")
  (:entry-state _tell-conversation)
  (:explanation "We try to get destination from the input data.")
  (:answer-analysis)
  )


(defownmethod
  =answer-analysis _tell-entry-state (conversation input)
  "Using list of words to locate receiver of message to send. Must ask user ~
   to confirm findings.
Arguments:
   conversation: conversation object
   input copy of the FACTS/input area"
  (let* (bindings destination)
    (catch 
      :return
      (cond
       ;; when input is empty then failure
       ((null input)
        (throw :return (list :failure)))
       ;; try to extract destination info from the input text
       ((setq bindings
              (MOSS::PROCESS-PATTERNS 
               input
               '(((?* ?X) "dizer" "a" (?* ?Y)) 
                 ((?* ?X) "informar" (?* ?Y))
                 ((?* ?X) "fazer" "saber" "a" (?* ?Y))
                 ((?* ?X) "mensagem" "para" (?* ?Y)))))
        (setq input (cdr (assoc '?Y bindings)))
        )
       ;; otherwise do nothing
       )
      
      ;; try to get destination
      (setq destination (extract-destination input))
      
      (cond
       ;; if null, failure
       ((null destination)
        (throw :return (list :transition _tell-sorry)))
       ;; list, must make a choice
       ((listp destination)
        ;; save the result into the FACT area and go ask for choice
        (replace-fact conversation :to-choose destination)
        (throw :return (list :transition _tell-choose-destination)))
       ;; otherwise we got destination, save it
       (t
        (replace-fact conversation :to destination)
        ;; and go get data
        (throw :return (list :transition _tell-get-data))))
      )))

;;;------------------------------------------------------ (TELL) CHOOSE-DESTINATION

(defstate 
  _tell-choose-destination
  (:label "Tell choose destination")
  (:explanation "ambiguous destination (list) select one.")
  (:execute
   (let* ((conversation moss::conversation)
          (choices (read-fact conversation :to-choose))
          )
     ;; remove the temporary data
     (replace-fact conversation :to-choose nil)
     (replace-fact conversation :to (car choices))))
  (:transitions (:always :target _tell-get-data)))

;;;--------------------------------------------------------------- (TELL) GET-DATA

(defstate 
  _tell-get-data
  (:label "set up parameters for continuous input.")
  (:explanation "Let the master type text into the pane until the DONE button is ~
                 pushed. We must disable . and ? terminations.")
  ;; set global flag in the interface window so that we can have . and ? in the text
  (:execute-preconditions 
   (setf (omas::pass-every-char (car (has-moss-input-window moss::conversation))) t)
   ;; include :verbatim flag into the FACT base (?)
   (replace-fact moss::conversation :verbatim t)
   )
  (:question-no-erase 
   "~% ...Digite ou dite o seu texto e depois aperte o botão Terminar.")
  (:transitions
   (:always :target _tell-send-message))
  )

#|
;;;--------------------------------------------------------- (TELL) GET-DESTINATION

(defstate 
  _tell-get-destination
  (:label "obtain master's destination for message.")
  (:explanation "Get the id of the personal assistant of the receiver.")
  ;; set global so that we can have . and ? in the text
  (:question-no-erase 
   "~% A qui voulez-vous envoyer le message ?") 
  (:answer-analysis)
  )

(defownmethod =answer-analysis _tell-get-destination (conversation input)
  "We process the answer and record it unless we get an abort.
Arguments:
   agent: assistant."
  (let (destination)
    ;; analyze input
    (setq destination (extract-destination input))
    
    (cond
     ((null destination)
      (list :transition _tell-sorry))
     ((listp destination)
      (replace-fact conversation :to-choose destination)
      (list :transition _tell-choose-destination))
     (t
      (replace-fact conversation :to destination)
      ;; we then get out of dialog
      (list :transition _tell-get-data)))))
|#
;;;------------------------------------------------------------ (TELL) SEND-MESSAGE

(defstate 
  _tell-send-message
  (:label "Tell send final message")
  (:explanation "build and send the message.")
  (:send-message-no-wait 
   :to (pop-fact moss::conversation :to)
   :type :inform
   :action :tell
   :args (list (list :text (pop-fact moss::conversation :raw-input))))
  (:text "~&...Mensagem enviada")
  (:transitions (:always :success)) ; return
  )

;;;------------------------------------------------------------------- (TELL) SORRY

(defstate 
  _tell-sorry
  (:label "Tell failure.")
  (:explanation "We don't have the needed parameters")
  (:text 
   "Lamento, não entendi a quem você quer enviar a mensagem.~%")
  (:reset)
  (:transitions (:failure))
  )

;;;================================================================================
;;;
;;;                              TRACE CONVERSATION
;;;   
;;;================================================================================

;;; this conversation is intended to modify the size of the dialog font, usually to
;;; increase it or to return to the default value.

(defsubdialog 
  _trace-conversation
  (:label "Trace conversation")
  (:explanation 
   "Command to toggle the dialog trace.")
  (:states _tr-entry-state)
  )

;;;-------------------------------------------------------- (TR) TRACE-ENTRY-STATE

(defstate 
  _tr-entry-state
  (:label "Trace dialog entry")
  (:entry-state _trace-conversation)
  (:explanation "We search the input for the trace command.")
  
  (:transitions
   ;; system commands
   (:patterns (((?* ?x) "mostrar" (?* ?y) "transições" (?* ?z))
               ((?* ?x) "rastrear" (?* ?y) "transições" (?* ?z))
               ((?* ?x) "imprimir" (?* ?y) "transições" (?* ?z)))
              :exec (setq moss::*verbose* t) :success)
   (:patterns (((?* ?x) "sems" "rastrear" (?* ?y))
               ((?* ?x) "eliminar" "rastreamento" (?* ?y))
               ((?* ?x) "parar" "de" "rastrear" (?* ?y)))
              :exec (setq moss::*verbose* nil) :success)
   (:otherwise :failure)
   )
  )

;;;================================================================================
;;;
;;;                      WHAT-IS CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to obtain and print information about something
;;; corresponding to unformatted data

(defsubdialog 
  _what-is-conversation
  (:label "Get info dialog")
  (:explanation "Master is trying to get info about some data.")
  (:states _what-is-conversation  ; required by defstate
           _wai-entry-state
           _wai-ask-staff
           _wai-get-info
           _wai-process-staff-answer
           _wai-sorry)
  )

;;;================================================================================
;;;                     WHAT IS CONVERSATION FUNCTIONS
;;;================================================================================

;;;------------------------------------------------------------- PRINT-WHAT-IS-DOC

(defUn print-what-is-doc (doc-list)
  "prints the list of returned strings (no pattern used)."
  (format t "~&+++ EUZEBIO /print-who-is-doc: language: ~S doc-list:~% ~S" 
          *language* doc-list)
  (omas::assistant-display-text 
   PA_EUZEBIO
   (format nil "~{~{~A~^~%~}~}~%" doc-list)))

;;;================================================================================
;;;                         WHAT IS CONVERSATION STATES
;;;================================================================================

;;;------------------------------------------------------------- (WAI) ENTRY-STATE

(defstate 
  _wai-entry-state
  (:label "What is conversation")
  (:entry-state _what-is-conversation)
  (:explanation "Assistant is trying to get object info from its knowledge base.")
  (:transitions
   ;;keep only relevant data
   (:patterns (((?* ?x) "o" "que" "é" "que" (?* ?y) "?")
               ((?* ?y) "é" "que" "?" (?* ?x))
               ((?* ?y))  ; when entering from print-concept-documentation
               )
              :keep ?y
              :target _wai-get-info)
   (:otherwise :target _wai-sorry)
   )
  )

;;;--------------------------------------------------------------- (WAI) ASK-STAFF
;;; changing strategy to :collect-answers
;;; rather than sending a broadcast right away, maybe we should
;;; 1. look into local KB
;;; 2. look into our agenda (sa-address)
;;; 3. ask BIBLIO
;;; This would avoid strange answers resulting from misinterpretation of the
;;; query.

(defstate 
  _wai-ask-staff
  (:label "Ask staff.")
  (:explanation "Ask the staff for help.")
  (:send-message :to :all :self PA_EUZEBIO :action :what-is :timeout 1 ; wait 1s
                 :args `(((:data . ,(read-fact moss::conversation :input))
                          (:language . :br)
                          ))
                 :strategy :collect-answers
                 )
  (:transitions 
   (:always :target _wai-process-staff-answer))
  )

;;;---------------------------------------------------------------- (WAI) GET-INFO

(defstate 
  _wai-get-info
  (:label "Looking into local KB info.")
  (:explanation "Look into own KB to get info.")
  (:answer-analysis)
  )

(defownmethod =answer-analysis _wai-get-info (conversation input)
  "we try first to get info locally and if it fails go ask all agents.
Arguments:
   agent: assistant."
  (declare (special *empty-words*))
  (let* (entry-list)
    (catch :return
      ;; clean input of parasitic words
      (setq input (moss::%clean-word-list input *empty-words*))
      (moss::vformat "EUZEBIO: _wai-get-info /+++ cleaned input:~& ~S" input)
      ;; if nothing left, then go ask staff
      (unless input
        (throw :return `(:transition ,_wai-ask-staff)))
      
      ;; check whether the resulting words correspond to entry points
      (setq entry-list (moss::find-best-entries input))
      (moss::vformat "EUZEBIO: _wai-get-info /entry list:~&    ~S" entry-list)
      ;; if nothing left go ask staff
      (unless entry-list 
        (throw :return `(:transition ,_wai-ask-staff)))
      
      ;; get documentation or CV and print it
      (dolist (item entry-list)
        (send item '=get-documentation :lead "   - ")   
        (send conversation '=display-text *answer*)
        )
      ;; transfer to :success    
      (list :success) 
      )
    ))

;;;------------------------------------------------------ (WAI) PROCESS-STAFF-ANSWER

(defstate 
  _wai-process-staff-answer
  (:label "Process staff answer.")
  (:explanation "Process anwers from the staff agents.")
  (:answer-analysis)
  )

(defownmethod =answer-analysis _wai-process-staff-answer (conversation input)
  "we got an answer from some staff or else the master got impatient.
Arguments:
   agent: assistant."
  (declare (ignore input))
  (let* ((agent (car (has-moss-agent conversation)))
         (answer-list (omas::answer agent))
         (*language* :fr)
         )
    ;(format *debug-io* "~&+++ =resume; HAS-TO-DO: ~S" (has-to-do conversation))
    ;; clean up HAS-TO-DO (contains "answer-there")
    ;(setf (has-to-do conversation) nil)
    ;; answer contains either the word "failure" or a list of lists of strings
    ;; (("Jean-Paul Barthes is a professor at UTC ..."
    ;;  "Dominique Barthes-Biesel is a professor at UTC ...")
    ;;  ("Jean-Paul Barthes has NN publications on:  ...")
    ;;  ...)
    ;; 
    (moss::vformat "~&>>> WAIdialog /Answer to the send-request:~% ~S" answer-list)
    ;; failure if :failure is returned or :error on timeout
    (cond
     ((or (equal "failure" answer-list) (eql :error answer-list))
      `(:transition ,_wai-sorry))
     ((null answer-list)
      (send conversation '=display-text "- Je ne sais pas...")
      (list :success))
     (t 
      (moss::vformat " _wai-ask-staff /answer-list: ~& ~S" answer-list)
      (send conversation '=display-text "~%C'est:")
      ;; each item of the answer list is like 
      ;;    ("personne" ("nom" "...")("prénom" "..."))
      (dolist (answer answer-list)
        ;; answer could be "failure" rather than a list of strings
        (unless (and (stringp answer) (string-equal answer "failure"))
          (dolist (item answer)
            (send conversation '=display-text 
                  (concatenate 'string "~%" (car item)))
            (send conversation '=display-text
                  (format nil ": ~{~%     ~{~A~^, ~}~}" (cdr item))))))
      (list :success)))
    ))

;;;------------------------------------------------------------------- (WAI) SORRY

(defstate 
  _wai-sorry
  (:label "Failure to get info.")
  (:explanation "We could not find anything.")
  (:reset)
  (:text "~%- Lamento, não tenho nehuma informação correspondendo a estes dados.~%")
  (:transitions 
   (:failure))
  )

;;;================================================================================
;;;
;;;                    WHO IS (GET PERSON INFO) CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to print information about a person.

(defsubdialog 
  _get-person-info-conversation
  (:label "Get person info dialog")
  (:explanation "Master is trying to obtain a telephone number.")
  (:states _gpi-entry-state
           _gpi-ask-staff
           _gpi-get-info
           _gpi-sorry)
  )

;;;================================================================================
;;;                     WHO IS CONVERSATION FUNCTIONS
;;;================================================================================

;;;-------------------------------------------------------------- PRINT-WHO-IS-DOC

(defUn print-who-is-doc (doc-list)
  "prints the list of returned strings (no pattern used)."
  (format t "~&+++ EUZEBIO /print-who-is-doc: language: ~S doc-list:~% ~S" 
          *language* doc-list)
  (omas::assistant-display-text 
   PA_EUZEBIO
   (format nil "~{~{~A~^~%~}~}~%" doc-list)))

;;;================================================================================
;;;                         WHO IS CONVERSATION STATES
;;;================================================================================

(defstate 
  _gpi-entry-state
  (:label "Get person info conversation")
  (:entry-state _get-person-info-conversation)
  (:explanation "Assistant is trying to get the info from its knowledge base.")
  (:transitions
   ;;keep only relevant data
   (:patterns (((?* ?x) "quem" "é" (?* ?y))
               ((?* ?y) "é" "que" "?" (?* ?x))
               ((?* ?x) "é" "que" (?* ?y) "?")
               )
              :keep ?y
              :target _gpi-get-info)
   (:otherwise :target _gpi-sorry)
   )
  )

;;;--------------------------------------------------------------- (WOI) ASK-STAFF
;;; changing strategy to :collect-answers
;;; rather than sending a broadcast right away, maybe we should
;;; 1. look into local KB
;;; 2. look into our agenda (ADDRESS)
;;; 3. ask BIBLIO
;;; This would avoid strange answers resulting from misinterpretation of the
;;; query.

(defstate 
  _gpi-ask-staff
  (:label "Ask other agents for help.")
  (:explanation "Ask the staff for help.")
  (:send-message :to :all :self PA_EUZEBIO :action :who-is :timeout 1 ; wait 1 second
                 :args `(((:data . ,(read-fact moss::conversation :input))
                          (:language . :br)))
                 :strategy :collect-answers
                 )
  (:transitions
   (:on-failure :target _gpi-sorry)
   (:otherwise :print-answer #'print-who-is-doc :success))
  )

;;;---------------------------------------------------------------- (WOI) GET-INFO

(defstate 
  _gpi-get-info
  (:label "Looking into local KB info.")
  (:explanation "Look into own KB to get answer to who-is?.")
  (:answer-analysis)
  )

(defownmethod =answer-analysis _gpi-get-info (conversation input)
  "we got an answer from some staff or else the master got impatient.
Arguments:
   conversation: the conversation object
   input: a copy of the FACTS/input area"
  (let ((person-list (moss::locate-objects input "pessoa")))
    (format t "~% WOI/GET-INFO /person-list: ~S" person-list)
    (if person-list 
      ;; here we got some answer, get documentation or CV and print it
      (dolist (item person-list)
        (send item '=get-documentation)
        (send conversation '=display-text *answer*))
      )
    ;; go ask staff for complement
    `(:transition ,_gpi-ask-staff))
  )

;;;------------------------------------------------------------------- (WOI) SORRY

(defstate 
  _gpi-sorry
  (:label "Sorry: no info.")
  (:explanation "We could not find anything.")
  (:reset)
  (:text ("~%- Lamento, não tenho a menor idéia.~%"
          "- Je ne sais pas.~%"))
  (:transitions 
   (:failure))
  )

;;;================================================================================
;;;
;;; Allan Trevisan defined dialogs below 
;;;================================================================================

(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))
;;;================================================================================
;;;
;;; WHAT ACTIVITIES SHOULD A PROJECT MEMBER TO DO NOW CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to print a project member activities information.

(defsubdialog 
  _get-what-activities-should-a-project-member-to-do-now-conversation
  (:label "Get project member activities to do now.")
  (:explanation "Master is trying to obtain a project member activities.")
  (:states _gpma-entry-state
           _gpma-ask-monitor-agent
           _gpma-sorry)
  )
  
;;;================================================================================
;;;WHAT ACTIVITIES SHOULD A PROJECT MEMBER TO DO NOW CONVERSATION FUNCTIONS
;;;================================================================================

;;;-------------------------------------------------------------- PRINT-PROJECT-MEMBER-ACTIVITIES-DOC
(defUn print-what-activities-should-a-aproject-member-to-do-now-doc (doc-list)
  "prints the list of returned strings (no pattern used)."
  (omas::assistant-display-text 
     PA_EUZEBIO
	 (with-output-to-string (out)
	   (format out "~&~{~20:@<~a~> ~25:@<~a~> ~25:@<~a~> ~100:@<~a~> ~50:@<~a~>~}"
			(list 'projeto 'nome 'sobrenome 'atividade 'inicio_planejado))
	   (format out "~{~{~&~{~20:@<~a~> ~25:@<~a~> ~25:@<~a~> ~100:@<~a~> ~50:@<~a~>~}~}~}" (cdr doc-list)))))

;;;================================================================================
;;;WHAT ACTIVITIES SHOULD A PROJECT MEMBER TO DO NOW CONVERSATION STATES
;;;================================================================================

(defstate 
  _gpma-entry-state
  (:label "Get project member activities conversation")
  (:entry-state _get-what-activities-should-a-project-member-to-do-now-conversation)
  (:explanation "Assistant is trying to request the info from the monitor agent.")
  (:transitions
   ;;keep only relevant data
   (:patterns (((?* ?x) "atividades" "de" (?* ?y))
               ((?* ?y) "de" "atividades" "?" (?* ?x))
               ((?* ?x) "de" "atividades" (?* ?y) "?")
               )
              :keep ?y
              :target _gpma-ask-monitor-agent)
   (:otherwise :target _gpma-sorry)
   )
  )

;;;---------------------------------------------------------------  ASK-MONITOR-AGENT

(defstate 
  _gpma-ask-monitor-agent
    (:label "Ask SA for help.")
  (:explanation "Ask the SA for help.")
  (:send-message :to :TECPAR-DP-MONITOR :self PA_EUZEBIO
				 :action :search-what-activities-should-a-project-member-to-do-now 
                 :args `(((:data . ,(read-fact moss::conversation :input))
                          (:language . :br)))
                 )
  (:transitions
   (:on-failure :target _gpma-sorry)
   (:otherwise :print-answer #'print-what-activities-should-a-aproject-member-to-do-now-doc :success))
  )

;;;------------------------------------------------------------------- SORRY

(defstate 
  _gpma-sorry
  (:label "Sorry: no activities found.")
  (:explanation "We could not find anything.")
  (:reset)
  (:text ("~%- Lamento, não tenho a menor idéia.~%"
          "- Je ne sais pas.~%"))
  (:transitions 
   (:failure))
  )
  
;;;================================================================================
;;;
;;; WHAT ACTIVITIES SHOULD A PROJECT MEMBER TO DO NOW IN A SPECIFIC PROJECT CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to print a project member activities information.

(defsubdialog 
  _get-what-activities-should-a-project-member-to-do-now-in-a-specific-project-conversation
  (:label "Get project member activities to do now in a given project.")
  (:explanation "Master is trying to obtain a project member activities.")
  (:states _gpmasp-entry-state
           _gpmasp-ask-monitor-agent
           _gpmasp-sorry)
  )
  
;;;================================================================================
;;;WHAT ACTIVITIES SHOULD A PROJECT MEMBER TO DO NOW IN A SPECIFIC PROJECT  CONVERSATION FUNCTIONS
;;;================================================================================

;;;-------------------------------------------------------------- PRINT-PROJECT-MEMBER-ACTIVITIES-DOC

(defUn print-what-activities-should-a-aproject-member-to-do-now-in-a-specific-project-doc (doc-list)
  "prints the list of returned strings (no pattern used)."
  (omas::assistant-display-text 
     PA_EUZEBIO
	 (with-output-to-string (out)
	   (format out "~&~{~20:@<~a~> ~25:@<~a~> ~25:@<~a~> ~100:@<~a~> ~50:@<~a~>~}"
			(list 'projeto 'nome 'sobrenome 'atividade 'inicio_planejado))
	   (format out "~{~{~&~{~20:@<~a~> ~25:@<~a~> ~25:@<~a~> ~100:@<~a~> ~50:@<~a~>~}~}~}" 
			(cdr doc-list)))))

;;;================================================================================
;;;WHAT ACTIVITIES SHOULD A PROJECT MEMBER TO DO NOW IN A SPECIFIC PROJECT CONVERSATION STATES
;;;================================================================================
o
(defstate 
  _gpmasp-entry-state
  (:label "Get project member activities in a given project conversation")
  (:entry-state _get-what-activities-should-a-project-member-to-do-now-in-a-specific-project-conversation)
  (:explanation "Assistant is trying to request the info from the monitor agent.")
  (:transitions
   ;;keep only relevant data
   (:patterns (((?* ?x) "atividades" "de" (?* ?z) "no" "projeto" (?* ?y))
               ((?* ?y) "de" "atividades" (?* ?z) "no" "projeto" (?* ?x) "?")
               )
              :keep ?y
              :target _gpmasp-ask-monitor-agent)
   (:otherwise :target _gpmasp-sorry)
   )
  )

;;;---------------------------------------------------------------  ASK-MONITOR-AGENT

(defstate 
  _gpmasp-ask-monitor-agent
  (:label "Ask information to SA on activities in a given project.")
  (:explanation "Ask the SA for help.")
  (:send-message :to :TECPAR-DP-MONITOR :self PA_EUZEBIO
				 :action :search-what-activities-should-a-project-member-to-do-now-in-a-specific-project 
                 :args `(((:data . ,(read-fact moss::conversation :input))
                          (:language . :br)))
                 )
  (:transitions
   (:on-failure :target _gpmasp-sorry)
   (:otherwise 
     :print-answer #'print-what-activities-should-a-aproject-member-to-do-now-in-a-specific-project-doc 
     :success))
  )
;;;------------------------------------------------------------------- SORRY

(defstate 
  _gpmasp-sorry
  (:label "Sorry: no activities in the given project found.")
  (:explanation "We could not find anything.")
  (:reset)
  (:text ("~%- Lamento, não tenho a menor idéia.~%"
          "- Je ne sais pas.~%"))
  (:transitions 
   (:failure))
  )
  
;;; end Allan written code
;;;================================================================================
;;;
;;;                             ELIZA CONVERSATION
;;;
;;;================================================================================

;;; this conversation is intended to do small talk.


(defParameter *eliza-rules*
  '((("Ola")
     "Bom dia! Eu me chamo EUZEBIO."
     "Ola!")
    (("Bom" "dia")
     "Bom dia ! Tudo bem ?"
     "Bom dia. Encantado..."
     "Bom dia, Chefe !")
    (("Salve" (?* ?x))
     "Olá ! Eu me chamo EUZEBIO e falo português."
     "Salve ?")
    (("bom dia " (?* ?x))
     "Bom dia, Chefe !")
    (("bonjour")
     "Bom dia ! Eu me chamo EUZEBIO. Não falo frances. Você não fala português?")
    (("hi")
     "Bom dia ! Eu me chamo EUZEBIO. Meu inglês não é muito bom.")
    (("como" "vai" (?* ?x))
     "Bem, obrigado!"
     "Sério, minha saúde lhe interessa ? Fico contente!")
    
    (("deixe" "estar")
     "OK. Sem problema.")
    (("obrigado" (?* ?y))
     "Não há de que!")
    (("tudo" (?* ?y))
     "Mesmo ?"
     "Exigente...")
    
    (((?* ?x) "máquina" (?* ?y))
     "As máquina o incomodam ?"
     "O que você pensa das máquinas ?"
     "Por que você menciona as máquinas ?"
     "As máquinas têm relação com o seu problema ?")
    
    (((?* ?x) "estupido" (?* ?y))
     "Por favor, olha a educação !"
     "Lamento a sua observação.")
    
    (((?* ?x) "não" "funciona" (?* ?y))
     "É claro que ?x funciona !"
     "Pour que você diz que ?x não funciona ?")
    
    (("socorro")
     "Como eu poderia ajudá-lo ?")
    
    (("help")
     "Voucê quer ajuda sobre o que ?")
    
    (("SOS")
     "O que posso fazer para ajudar ?")
    
    (((?* ?x) "lamento" (?* ?y))
     "Por favor, não se lamente."
     "Não é necessário")
    
    (((?* ?x) "Eu" "me" "lembro" (?* ?y))
     "Você lembra-se com frequência ?y ?"
     "Do que mais você se lembra ?"
     "Por que você se lembra de ?y agora ?")
    
    (((?* ?x) "você" "lembra-se" (?* ?y))
     "Você pensa que eu esqueceria ?y ?"
     "Por que pensa que eu deveria me lembrar ?y justo agora ?"
     "O que há em relação a ?y ?"
     "Você mencionou ?y ?")
    
    (("o" "que" (?* ?x) "fazer" "por" "mim")
     "Tantas coisas..."
     "Neste momento preciso, não vejo o que."
     "O que poderia lhe agradar ?")
    
    (((?* ?x) "minha" "mãe" (?* ?y))
     "Quem mais na sua familia ?y ?"
     "Fale mais da sua familia.")
    (((?* ?x) "meu" "pai" (?* ?y))
     "Seu pai ?"
     "Ele tem uma grande influência sobre você ?"
     "O que lhe vem ao espirito quando pensa no seu pai ?")
    
    (((?* ?x) "Eu" "gostaria" (?* ?y))
     "O que significaria ?y ?"
     "Por que você gostraia de ?y ?")
    (((?* ?x) "Eu" "estou" "contente" (?* ?y))
     "Eu ajudei a estar contente ?y ?"
     "O que o deixa feliz neste momento ?"
     "Poderia me explicar por que ficou contente de repente ?y ?")
    (((?* ?x) "eu" "deprimido" (?* ?y))
     "Lamento muito !."
     "Estou certo que não é agradável ficar deprimido.")
    (((?* ?x) "é" "como" (?* ?y))
     "Que semelhança você vê entre ?x e ?y ?"
     "Em que sentido ?x é como ?y ?"
     "Você acha mesmo que há uma relação ?")
    
    (((?* ?x) "parecidos" (?* ?y))
     "Como assim ?"
     "Que semelhança existe ?")
    (((?* ?x) "parecido" (?* ?y))
     "Que outra relação você vê ?")
    
    (((?* ?x) "eu" "estava" (?* ?y))
     "Esteve mesmo ?"
     "Eu duvidava que você estivesse ?y"
     "Por que você me diz agora que esteve ?y ?")
    
    (((?* ?x) "eu" "era" (?* ?y))
     "Era mesmo ?"
     "Eu duvidava que você fosse ?y"
     "Por que você me diz agora que era ?y ?")
    
    (((?* ?x) "Eu" "sou" (?* ?y))
     "Em que sentido você é ?y ?"
     "Você deseja ser ?y ?")
    
    (((?* ?x) "porque" (?* ?y))
     "Esta é a única razão ?"
     "Que outra razão poderia haver ?"
     "Esta razão pode explicar outra coisa ?")
    
    (((?* ?x) "eu" "não" "posso" (?* ?y))
     "Talvez pudesse ?y agora ?"
     "E se você pudesse ?y ?")
    
    (((?* ?x) "eu" "senti" (?* ?y))
     "Sentiu algo mais ?")
    (((?* ?x) "porque" "não" (?* ?y))
     "Você não deveria ?y você mesmo ?"
     "Você pensa que eu poderia ?y ?")    
    (((?* ?x) "sim" (?* ?y))
     "Você me parece muito positivo."
     "Tem certeza ?"
     "Eu compreendo.")
    (((?* ?x) "não" (?* ?y))
     "Por que não ?"
     "Você me parece muito negativo.")
    (((?* ?x) "algém" (?* ?y))
     "Poderia ser mais preciso ?")
    (((?* ?x) "todo" "o" "mundo" (?* ?y))
     "Certamente nem todo o mundo!"
     "Pensa em alguém em particular ?"
     "Quem por exemplo ?")
    (((?* ?x) "sempre" (?* ?y))
     "Poderia dar um exemplo preciso ?"
     "Quando ?"
     "Sempre --, mesmo ?")
    (((?* ?x) "talvez" (?* ?y))
     "Você não parece muito seguro...")
    (((?* ?x) "OK" (?* ?y))
     "Bem, como quiser!")
    (((?* ?x) "imbecil" (?* ?y))
     "Oh!"
     "Vou me queixar ao sindicato")
    (((?* ?x) "idiota" (?* ?y))
     "Nem tanto ça..."
     "Não mais que a média."
     "Se você diz...")
    
    
    (((?* ?x))
     "Não vejo o que você quer exatamente."
     "não estou certo de ter ccompreendido o que você quer."
     "Poderia reformular sua pergunta, por favor ?"
     "Lamento, não entendi."
     "Putz ! Não entendi."
     "Poderia dizer isto de outra forma ?"
     "Perdão ?"
     "O que ?"
     "Como ?"
     "Eu entendi ?x , mas não compreendi."
     )
    ))

(format t "~%;*** MOSS v~A - EUZEBIO dialogs loaded ***" moss::*moss-version-number*)

;;;=============================================================================== 

:EOF