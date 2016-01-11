;;;-*- Mode: Lisp; Package: "TECPAR-DP-MONITOR" -*-
;;;===============================================================================
;;; 25/04/12
;;;                               AGENT TECPAR-DP-MONITOR
;;;
;;;===============================================================================

;;;   This file contains the description of the agent TECPAR-DP-MONITOR. This agent is
;;; responsable to monitoring the dotproject's database and notify the personal assistant 
;;; related to the user(s) so that the PA provide a kind of alert to the user(s). 
;;;
;;;   The monitoring behaviour is implemented in the form of periodical rigid goals. The
;;; agent is responsable to monitoring the activities associated to a set of projects.
;;; Besides of the autonomous behavour the agent also realises activities by demand answearing
;;; requests to informations related to the dotproject tool.

#|
2010
 0219 useless info has been removed
 0720 improving the style of the file by commenting out most parts
2012 
 2504 agent created
 0905 skill search late activities in a specific project bug
 1105 skill search late activities in a specific project bug fixed
 2105 get-project-ids utilitary list processing dillemma
 2305 code redesign to fit the functional style
|#

;;;===============================================================================
;;; 
;;;                         defining agent package 
;;;
;;;===============================================================================
	 
(defpackage :TECPAR-DP-MONITOR (:use :moss :omas :cl #+MCL :ccl :DATABASE-ACCESS))
(in-package :TECPAR-DP-MONITOR)

;;;===============================================================================
;;; 
;;;                              defining the agent 
;;;
;;;===============================================================================
;;; :TECPAR-DP-MONITOR is a keyword that will designat the agent and be used in the messages
;;; the actual name of the agent is built automatically and is the symbol
;;; TECPAR-DP-MONITOR::SA_TECPAR-DP-MONITOR, i.e. the symbol SA_TECPAR-DP-MONITOR defined in 
;;; the "TECPAR-DP-MONITOR" package. It points to the lisp structure containing the agent data

(omas::defagent :TECPAR-DP-MONITOR :redefine t)

;;;===============================================================================
;;; 
;;;                         service macros and functions 
;;; Place here the macros and finctions used by the skills and goals
;;;===============================================================================

;;;
;;;
;;; ==============================================================================
;;;                        Utilitary Functions
;;; ==============================================================================

(defun add-monitored-project (monitored-projects new-project-name new-project-id)
	(cons (cons new-project-id new-project-name) 
		   monitored-projects))
		

;;; ================================= globals ====================================

(defparameter *monitored-projects* (list (cons 1 "CSCW-TECPAR")))

;;;===============================================================================
;;; 
;;;                                    SKILLS 
;;;
;;;
;;;===============================================================================

;;; ==============================================================================
;;;             Skill  ADD-MONITORED-PROJECT
;;; ==============================================================================

(defskill :ADD-MONITORED-PROJECT :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-ADD-MONITORED-PROJECT
)

(defun static-TECPAR-DP-MONITOR-ADD-MONITORED-PROJECT (agent environment new-project-name)
"Add a new project to be monitored in the list of monitoring projects"
	(declare (ignore environment))
	(setf *monitored-projects* (add-monitored-project *monitored-projects* 
													  new-project-name
													  (select-project-id new-project-name)))
	(static-exit agent nil))



;;;
;;; 
;;;    The folowing skills are responsible to retrive information related to the 
;;;  projects that the agent is concerning to monitor.
;;;
;;;
;;;    Each infrormation retrieving skill generate a list in the form 
;;; (list-of-fields list-of-registers) that should be parsed for whom
;;; is interested in the information

;;; ==============================================================================
;;;             Skill  SEARCH-WHAT-ACTIVITIES-TO-DO-NOW
;;; ==============================================================================

(defskill :SEARCH-WHAT-ACTIVITIES-TO-DO-NOW :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-WHAT-ACTIVITIES-TO-DO-NOW
)

(defun static-TECPAR-DP-MONITOR-SEARCH-WHAT-ACTIVITIES-TO-DO-NOW (agent environment)
"Activities related to all project members in all the projects being monitored."
	(declare (ignore environment))
	
	(let ((todo-list (select-what-activities-to-do-now *monitored-projects*)))
		(terpri)
		(princ todo-list)
		(static-exit agent (format nil "~a" todo-list))))
		
;;; ==============================================================================
;;;             Skill  SEARCH-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW
;;; ==============================================================================

(defskill :SEARCH-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW
)

(defun static-TECPAR-DP-MONITOR-SEARCH-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW 
																   (agent 
																	environment
																	args)
"Activities related to a specific project member in all the projects being monitored.
Arguments:
   agent: :TECPAR-DP-MONITOR
   environment: environment
   args: e.g. ((:data \"Atividades\" \"de\" \"Allan\" \"Trevisan\"))
Return:
   :failure or e.g. ((nome sobrenome atividade) ((Allan Trevisan Criar relatorio da reunião)
												 (Allan Trevisan Marcar reunião com Fulano)))"
	(declare (ignore environment))
	(let ((data (cdr (assoc :data args)))
          (*language* (or (cdr (assoc :language args)) *language*))
           results person-list project-member-name project-member-last-name)
    
	  (setq person-list (delete-duplicates (moss::locate-objects data "pessoa")))
	  (setq project-member-name (car (send (car person-list) '=get-nome)))
	  (setq project-member-last-name (car (send (car person-list) '=get-sobrenome)))
	
	  (format t "~& SKILL SEARCH-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW:")
	  (format t "~% PROJECT MEMBER:")
	  (format t "~% NAME: ~a" project-member-name)
	  (format t "~% LAST-NAME: ~a" project-member-last-name)
    
	  (setq results 
		  (select-what-activities-should-a-project-member-to-do-now  
												project-member-name 
												project-member-last-name
											    (mapcar #'car *monitored-projects*)))
     
		   
		
      (static-exit agent (or results "failure"))))

;;; ==============================================================================
;;; Skill  SEARCH-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW-IN-A-SPECIFIC-PROJECT
;;; ==============================================================================

(defskill :SEARCH-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW-IN-A-SPECIFIC-PROJECT :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW-IN-A-SPECIFIC-PROJECT
)

(defun static-TECPAR-DP-MONITOR-SEARCH-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW-IN-A-SPECIFIC-PROJECT 
																   (agent 
																	environment
																	args)
"Activities related to a specific project member in a specific project.
Arguments:
   agent: :TECPAR-DP-MONITOR
   environment: environment
   args: e.g. ((:data \"Atividades\" \"de\" \"Allan\" \"Trevisan\"))
Return:
   :failure or e.g. ((nome sobrenome atividade) ((Allan Trevisan Criar relatorio da reunião)
												 (Allan Trevisan Marcar reunião com Fulano)))"
	(declare (ignore environment))
	(let ((data (cdr (assoc :data args)))
          (*language* (or (cdr (assoc :language args)) *language*))
           results 
		   person-list 
		   project-member-name 
		   project-member-last-name 
		   projects-list 
		   project-name)
    (format t "~& <<<<args>>>> ~s" args)
      (setq person-list  (moss::locate-objects data "pessoa"))
	  (setq project-member-name (car (send (car person-list) '=get-nome)))
	  (setq project-member-last-name (car (send (car person-list) '=get-sobrenome)))
	  (format t "~& <<<<person-list>>>> ~a" person-list)
	  (setq projects-list (delete-duplicates (moss::locate-objects data "projeto")))
	  (setq project-name (car (send (car projects-list) '=get-nome)))

	  (format t "~& SKILL SEARCH-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW-IN-A-SPECIFIC-PROJECT:")
	  (format t "~% PROJECT MEMBER:")
	  (format t "~% NAME: ~a" project-member-name)
	  (format t "~% LAST-NAME: ~a" project-member-last-name)
	  (format t "~% PROJECT:")
	  (format t "~% PROJECT NAME: ~a" project-name)
    
	  (setq results 
		  (select-what-activities-should-a-project-member-to-do-now-in-a-specific-project  
																		project-member-name
																		project-member-last-name
																		project-name))
     
		   
		
	  (static-exit agent (or results "failure"))))

;;; ==============================================================================
;;;             Skill  SEARCH-LATE-END-DATE-ACTIVITIES
;;; ==============================================================================

(defskill :SEARCH-LATE-END-DATE-ACTIVITIES :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-LATE-END-DATE-ACTIVITIES
)

(defun static-TECPAR-DP-MONITOR-SEARCH-LATE-END-DATE-ACTIVITIES (agent environment)
"Activities related to all project members in all the projects being monitored."
	(declare (ignore environment))
	(let ((late-end-date-activities (select-late-end-date-activities (mapcar #'car *monitored-projects*))))
		(terpri)
		(princ late-end-date-activities)
		(static-exit agent nil)))
		
;;; ==============================================================================
;;;             Skill  SEARCH-PROJECT-MEMBER-LATE-END-DATE-ACTIVITIES
;;; ==============================================================================

(defskill :SEARCH-PROJECT-MEMBER-LATE-END-DATE-ACTIVITIES :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-PROJECT-MEMBER-LATE-END-DATE-ACTIVITIES
)

(defun static-TECPAR-DP-MONITOR-SEARCH-PROJECT-MEMBER-LATE-END-DATE-ACTIVITIES (agent 
																				environment
																				project-member-first-name
																				project-member-last-name)
"Activities related to a specific project member in all the projects being monitored."
	(declare (ignore environment))
	(let ((late-end-date-activities (select-project-member-late-end-date-activities 
																	 *monitored-projects*
																	 project-member-first-name 
																	 project-member-last-name)))
		(terpri)
		(princ late-end-date-activities)
		(static-exit agent nil)))
		
;;; ==============================================================================
;;; Skill  SEARCH-PROJECT-MEMBER-LATE-END-DATE-ACTIVITIES-IN-A-SPECIFIC-PROJECT
;;; ==============================================================================

(defskill :SEARCH-PROJECT-MEMBER-LATE-END-DATE-ACTIVITIES-IN-A-SPECIFIC-PROJECT :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-PROJECT-MEMBER-LATE-END-DATE-ACTIVITIES-IN-A-SPECIFIC-PROJECT
)

(defun static-TECPAR-DP-MONITOR-SEARCH-PROJECT-MEMBER-LATE-END-DATE-ACTIVITIES-IN-A-SPECIFIC-PROJECT 
																			   (agent 
																				environment
																				project-name
																				project-member-first-name
																				project-member-last-name)
"Activities related to a specific project member in a specific project"
	(declare (ignore environment))
	(let ((late-end-date-activities (select-project-member-late-end-date-activities-in-a-specific-project
																	 project-name
																	 project-member-first-name 
																	 project-member-last-name)))
		(terpri)
		(princ late-end-date-activities)
		(static-exit agent nil)))
		
;;; ==============================================================================
;;;             Skill  SEARCH-LATE-START-DATE-ACTIVITIES
;;; ==============================================================================

(defskill :SEARCH-LATE-START-DATE-ACTIVITIES :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-LATE-START-DATE-ACTIVITIES
)

(defun static-TECPAR-DP-MONITOR-SEARCH-LATE-START-DATE-ACTIVITIES (agent environment)
"Activities related to all project members in all the projects being monitored."
	(declare (ignore environment))
	(let ((late-start-date-activities (select-late-start-date-activities (mapcar #'car *monitored-projects*))))
		(terpri)
		(princ late-start-date-activities)
		(static-exit agent nil)))
		
;;; ==============================================================================
;;;             Skill  SEARCH-PROJECT-MEMBER-LATE-START-DATE-ACTIVITIES
;;; ==============================================================================

(defskill :SEARCH-PROJECT-MEMBER-LATE-START-DATE-ACTIVITIES :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-PROJECT-MEMBER-LATE-START-DATE-ACTIVITIES
)

(defun static-TECPAR-DP-MONITOR-SEARCH-PROJECT-MEMBER-LATE-START-DATE-ACTIVITIES 
																			   (agent 
																				environment
																				project-member-first-name
																				project-member-last-name)
"Activities related to a specific project member in all the projects being monitored."
	(declare (ignore environment))
	(let ((late-start-date-activities (select-project-member-late-start-date-activities 
																	 *monitored-projects*
																	 project-member-first-name 
																	 project-member-last-name)))
		(terpri)
		(princ late-start-date-activities)
		(static-exit agent nil)))
		
;;; ==============================================================================
;;; Skill  SEARCH-PROJECT-MEMBER-LATE-START-DATE-ACTIVITIES-IN-A-SPECIFIC-PROJECT
;;; ==============================================================================

(defskill :SEARCH-PROJECT-MEMBER-LATE-START-DATE-ACTIVITIES-IN-A-SPECIFIC-PROJECT :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-PROJECT-MEMBER-LATE-START-DATE-ACTIVITIES-IN-A-SPECIFIC-PROJECT
)

(defun static-TECPAR-DP-MONITOR-SEARCH-PROJECT-MEMBER-LATE-START-DATE-ACTIVITIES-IN-A-SPECIFIC-PROJECT 
																			   (agent 
																				environment
																				project-name
																				project-member-first-name
																				project-member-last-name)
"Activities related to a specific project member in a specific project"
	(declare (ignore environment))
	(let ((late-start-date-activities (select-project-member-late-start-date-activities-in-a-specific-project
																	 project-name
																	 project-member-first-name 
																	 project-member-last-name)))
		(terpri)
		(princ late-start-date-activities)
		(static-exit agent nil)))

;;; ==============================================================================
;;;             Skill  SEARCH-ACTIVITIES-WITH-A-TERM-ENDING-SOON
;;; ==============================================================================

(defskill :SEARCH-ACTIVITIES-WITH-A-TERM-ENDING-SOON :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-ACTIVITIES-WITH-A-TERM-ENDING-SOON
)

(defun static-TECPAR-DP-MONITOR-SEARCH-ACTIVITIES-WITH-A-TERM-ENDING-SOON (agent environment remaining-days)
"Activities related to all project members in all the projects being monitored."
	(declare (ignore environment))
	(let ((activities-with-a-term-ending-soon (select-activities-with-a-term-ending-soon
															remaining-days	
															*monitored-projects*)))
		(terpri)
		(princ activities-with-a-term-ending-soon)
		(static-exit agent nil)))
		
;;; ==============================================================================
;;;             Skill  SEARCH-PROJECT-MEMBER-ACTIVITIES-WITH-A-TERM-ENDING-SOON
;;; ==============================================================================

(defskill :SEARCH-PROJECT-MEMBER-ACTIVITIES-WITH-A-TERM-ENDING-SOON :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-PROJECT-MEMBER-ACTIVITIES-WITH-A-TERM-ENDING-SOON
)

(defun static-TECPAR-DP-MONITOR-SEARCH-PROJECT-MEMBER-ACTIVITIES-WITH-A-TERM-ENDING-SOON 
															(agent 
															 environment 
															 remaining-days
															 project-member-first-name
															 project-member-last-name)
"Activities related to a specific project member in all the projects being monitored."
	(declare (ignore environment))
	(let ((activities-with-a-term-ending-soon (select-project-member-activities-with-a-term-ending-soon
															remaining-days	
															*monitored-projects*
															project-member-first-name
															project-member-last-name)))
		(terpri)
		(princ activities-with-a-term-ending-soon)
		(static-exit agent nil)))
		
;;; ==============================================================================
;;; Skill  SEARCH-PROJECT-MEMBER-ACTIVITIES-WITH-A-TERM-ENDING-SOON-IN-A-SPECIFIC-PROJECT
;;; ==============================================================================

(defskill :SEARCH-PROJECT-MEMBER-ACTIVITIES-WITH-A-TERM-ENDING-SOON-IN-A-SPECIFIC-PROJECT :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-PROJECT-MEMBER-ACTIVITIES-WITH-A-TERM-ENDING-SOON-IN-A-SPECIFIC-PROJECT
)

(defun static-TECPAR-DP-MONITOR-SEARCH-PROJECT-MEMBER-ACTIVITIES-WITH-A-TERM-ENDING-SOON-IN-A-SPECIFIC-PROJECT 
															(agent 
															 environment 
															 remaining-days
															 project-name
															 project-member-first-name
															 project-member-last-name)
"Activities related to a specific project member in a specific project being monitored."
	(declare (ignore environment))
	(let ((activities-with-a-term-ending-soon (select-project-member-activities-with-a-term-ending-soon-in-a-specific-project
															remaining-days	
															project-name
															project-member-first-name
															project-member-last-name)))
		(terpri)
		(princ activities-with-a-term-ending-soon)
		(static-exit agent nil)))
		
;;; ==============================================================================
;;; 						Skill  SEARCH-INACTIVE-PROJECTS
;;; ==============================================================================

(defskill :SEARCH-INACTIVE-PROJECTS :TECPAR-DP-MONITOR
	:static-fcn static-TECPAR-DP-MONITOR-SEARCH-INACTIVE-PROJECTS
)

(defun static-TECPAR-DP-MONITOR-SEARCH-INACTIVE-PROJECTS (agent environment)
	(declare (ignore environment))
	(let ((inactive-projects-list (select-inactive-projects *monitored-projects*)))
		(terpri)
		(princ inactive-projects-list)
		(static-exit agent nil)))
;;;==============================================================================
;;; 
;;;                                    goals
;;;
;;;==============================================================================
;;;
;;;       			Dotproject database monitoring goals
;;;==============================================================================
;;;
;;; 
;;; ==============================================================================
;;;                   Goal  SEARCH-WHAT-ACTIVITIES-TO-DO-NOW
;;; ==============================================================================
#|

(defun goal-search-what-activities-to-do-now (agent)
	(list (make-instance 'omas::message :type :request
		                 :from (omas::key agent) :to (omas::key agent)
						 :action :search-what-activities-to-do-now
						 :args nil
						 :task-id :T0)))
		
	
(make-goal 'MONITOR-PROJECT-TASKS :TECPAR-DP-MONITOR
	:type :cyclic
	:period 30 ; seconds
	:activation-date (+ (get-universal-time) 3) ; call at t=1 then every 30 seconds
	:script 'goal-search-what-activities-to-do-now)


|#
;;;===============================================================================
;;; 
;;;                               initial conditions 
;;;
;;;===============================================================================

;;; insert here whatever initial conditions need to be provided in the agent memory
;;; Ex. (deffact SA_book-seller-1 0 :money)

:EOF