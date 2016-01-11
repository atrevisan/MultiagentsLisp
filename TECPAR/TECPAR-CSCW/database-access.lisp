;;; using franz's direct connect library
(defpackage :DATABASE-ACCESS (:use :cl #+MCL :ccl :dbi.mysql)
							 (:export 
								:select-what-activities-to-do-now
								:select-what-activities-should-a-project-member-to-do-now
								:select-what-activities-should-a-project-member-to-do-now-in-a-specific-project
								:select-late-end-date-activities
								:select-project-member-late-end-date-activities
								:select-project-member-late-end-date-activities-in-a-specific-project
								:select-late-start-date-activities
								:select-project-member-late-start-date-activities
								:select-project-member-late-start-date-activities-in-a-specific-project
								:select-activities-with-a-term-ending-soon
								:select-project-member-activities-with-a-term-ending-soon
								:select-project-member-activities-with-a-term-ending-soon-in-a-specific-project
								:select-inactive-projects))
(in-package :DATABASE-ACCESS)


;;; ================================= globals ====================================

(defparameter *database-name* "dotproject")
(defparameter *username* "root")

;;; 
;;;
;;; ==============================================================================
;;;                        SQL SELECT STRINGS SECTION
;;; ==============================================================================

;;; ==============================================================================
;;; 					SELECT PROJECT ID
;;; ==============================================================================
(defparameter *select-project-id* 
	"select
						dotp_projects.project_id as id_projeto
	from
						dotp_projects
	where
						 dotp_projects.project_name = '~a'")
						 
;;; ==============================================================================
;;; 					SELECT WHAT ACTIVITIES TO DO NOW
;;; ==============================================================================
(defparameter *select-what-activities-to-do-now* 
	"select
						dotp_projects.project_name as projeto,
						dotp_contacts.contact_first_name as nome,
						dotp_contacts.contact_last_name as sobrenome,
						dotp_tasks.task_name as o_que_fazer_agora,
						dotp_user_tasks.perc_assignment as perc_dedicacao,
						dotp_tasks.task_percent_complete as percentagem_completa,
						dotp_tasks.task_start_date as inicio_planejado
	from
						dotp_contacts, dotp_projects, dotp_tasks,
						dotp_users, dotp_user_tasks
	where
						dotp_users.user_contact = dotp_contacts.contact_id
						and dotp_tasks.task_project = dotp_projects.project_id
						and dotp_tasks.task_percent_complete < '100'
						and dotp_tasks.task_dynamic <> '1'
						and dotp_user_tasks.task_id = dotp_tasks.task_id
						and dotp_user_tasks.user_id = dotp_users.user_id
						order by dotp_tasks.task_start_date asc")
						
;;; ==============================================================================
;;; SELECT WHAT ACTIVITIES SHOULD A PROJECT MEMBER TO DO NOW 
;;; ==============================================================================
(defparameter *select-what-activities-should-a-project-member-to-do-now* 

	"select
						dotp_projects.project_name as projeto,
						dotp_contacts.contact_first_name as nome,
						dotp_contacts.contact_last_name as sobrenome,
						dotp_tasks.task_name as o_que_fazer_agora,
						dotp_tasks.task_start_date as inicio_planejado
	from
						dotp_contacts, dotp_projects, dotp_tasks,
						dotp_users, dotp_user_tasks
	where
					    dotp_contacts.contact_first_name = '~a'
						and dotp_contacts.contact_last_name = '~a'
						and dotp_users.user_contact = dotp_contacts.contact_id
						and dotp_projects.project_id = '~a'
						and dotp_tasks.task_project = dotp_projects.project_id
						and dotp_tasks.task_percent_complete < '100'
						and dotp_tasks.task_dynamic <> '1'
						and dotp_user_tasks.task_id = dotp_tasks.task_id
						and dotp_user_tasks.user_id = dotp_users.user_id
						order by dotp_tasks.task_start_date asc")
						
;;; ==============================================================================
;;; SELECT WHAT ACTIVITIES SHOULD A PROJECT MEMBER TO DO NOW IN A SPECIFIC PROJECT
;;; ==============================================================================
(defparameter *select-what-activities-should-a-project-member-to-do-now-in-a-specific-project*
	"select
						dotp_projects.project_name as projeto,
						dotp_contacts.contact_first_name as nome,
						dotp_contacts.contact_last_name as sobrenome,
						dotp_tasks.task_name as o_que_fazer_agora,
						dotp_tasks.task_start_date as inicio_planejado
	from
						dotp_contacts, dotp_projects, dotp_tasks,
						dotp_users, dotp_user_tasks
	where
					    dotp_contacts.contact_first_name = '~a'
						and dotp_contacts.contact_last_name = '~a'
						and dotp_users.user_contact = dotp_contacts.contact_id
						and dotp_projects.project_name = '~a' 
						and dotp_tasks.task_project = dotp_projects.project_id
						and dotp_tasks.task_percent_complete < '100'
						and dotp_tasks.task_dynamic <> '1'
						and dotp_user_tasks.task_id = dotp_tasks.task_id
						and dotp_user_tasks.user_id = dotp_users.user_id
						order by dotp_tasks.task_start_date asc")

;;; ==============================================================================
;;; 					SELECT LATE END DATE ACTIVITIES
;;; ==============================================================================
(defparameter *select-late-end-date-activities*
	"select
						dotp_projects.project_name as projeto,
						dotp_contacts.contact_first_name as responsavel_nome,
						dotp_contacts.contact_last_name as responsavel_sobrenome,
						dotp_tasks.task_name as atividade_que_ja_devia_ter_terminado,
						dotp_tasks.task_end_date as final_planejado,
						dotp_tasks.task_percent_complete as andamento_registrado
	from
						dotp_tasks, dotp_projects, dotp_contacts,
						dotp_user_tasks, dotp_users
	where
						dotp_projects.project_id = '~a' 
						and dotp_projects.project_id = dotp_tasks.task_project
						and dotp_tasks.task_end_date < now()
						and dotp_tasks.task_percent_complete < '100'
						and dotp_user_tasks.task_id = dotp_tasks.task_id
						and dotp_user_tasks.user_id = dotp_users.user_id
						and dotp_users.user_contact = dotp_contacts.contact_id
						order by dotp_tasks.task_end_date asc")

;;; ==============================================================================
;;; 					SELECT PROJECT MEMBER LATE END DATE ACTIVITIES
;;; ==============================================================================
(defparameter *select-project-member-late-end-date-activities*
	"select
						dotp_projects.project_name as projeto,
						dotp_contacts.contact_first_name as responsavel_nome,
						dotp_contacts.contact_last_name as responsavel_sobrenome,
						dotp_tasks.task_name as atividade_que_ja_devia_ter_terminado,
						dotp_tasks.task_end_date as final_planejado,
						dotp_tasks.task_percent_complete as andamento_registrado
	from
						dotp_tasks, dotp_projects, dotp_contacts,
						dotp_user_tasks, dotp_users
	where
						dotp_projects.project_id = dotp_tasks.task_project
						and dotp_tasks.task_end_date < now()
						and dotp_tasks.task_percent_complete < '100'
						and dotp_contacts.contact_first_name = '~a'
						and dotp_contacts.contact_last_name = '~a'
						and dotp_user_tasks.task_id = dotp_tasks.task_id
						and dotp_user_tasks.user_id = dotp_users.user_id
						and dotp_users.user_contact = dotp_contacts.contact_id
						order by dotp_tasks.task_end_date asc")

;;; ==============================================================================
;;; SELECT PROJECT MEMBER LATE END DATE ACTIVITIES IN A SPECIFIC PROJECT
;;; ==============================================================================
(defparameter *select-project-member-late-end-date-activities-in-a-specific-project*
	"select
						dotp_projects.project_name as projeto,
						dotp_contacts.contact_first_name as responsavel_nome,
						dotp_contacts.contact_last_name as responsavel_sobrenome,
						dotp_tasks.task_name as atividade_que_ja_devia_ter_terminado,
						dotp_tasks.task_end_date as final_planejado,
						dotp_tasks.task_percent_complete as andamento_registrado
	from
						dotp_tasks, dotp_projects, dotp_contacts,
						dotp_user_tasks, dotp_users
	where
					    dotp_projects.project_name = '~a' 
						and dotp_projects.project_id = dotp_tasks.task_project
						and dotp_tasks.task_end_date < now()
						and dotp_tasks.task_percent_complete < '100'
						and dotp_contacts.contact_first_name = '~a'
						and dotp_contacts.contact_last_name = '~a'
						and dotp_user_tasks.task_id = dotp_tasks.task_id
						and dotp_user_tasks.user_id = dotp_users.user_id
						and dotp_users.user_contact = dotp_contacts.contact_id
						order by dotp_tasks.task_end_date asc")
						
;;; ==============================================================================
;;; 						SELECT LATE START DATE ACTIVITIES
;;; ==============================================================================
(defparameter *select-late-start-date-activities*
	"select
						dotp_projects.project_name as projeto,
						dotp_contacts.contact_first_name as responsavel_nome,
						dotp_contacts.contact_last_name as responsavel_sobrenome,
						dotp_tasks.task_name as atividade_com_inicio_atrasado,
						dotp_tasks.task_start_date as inicio_planejado
	from
						dotp_tasks, dotp_projects, dotp_contacts,
						dotp_user_tasks, dotp_users
	where
						dotp_projects.project_id = '~a' 
						and dotp_projects.project_id = dotp_tasks.task_project
						and dotp_tasks.task_start_date < now()
						and dotp_tasks.task_percent_complete = '0'
						and dotp_user_tasks.task_id = dotp_tasks.task_id
						and dotp_user_tasks.user_id = dotp_users.user_id
						and dotp_users.user_contact = dotp_contacts.contact_id
						order by dotp_tasks.task_start_date asc")
						
;;; ==============================================================================
;;; 						SELECT PROJECT MEMBER LATE START DATE ACTIVITIES
;;; ==============================================================================
(defparameter *select-project-member-late-start-date-activities*
	"select
						dotp_projects.project_name as projeto,
						dotp_contacts.contact_first_name as responsavel_nome,
						dotp_contacts.contact_last_name as responsavel_sobrenome,
						dotp_tasks.task_name as atividade_com_inicio_atrasado,
						dotp_tasks.task_start_date as inicio_planejado
	from
						dotp_tasks, dotp_projects, dotp_contacts,
						dotp_user_tasks, dotp_users
	where
						dotp_projects.project_id = dotp_tasks.task_project
						and dotp_tasks.task_start_date < now()
						and dotp_tasks.task_percent_complete = '0'
						and dotp_contacts.contact_first_name = '~a'
						and dotp_contacts.contact_last_name = '~a'
						and dotp_user_tasks.task_id = dotp_tasks.task_id
						and dotp_user_tasks.user_id = dotp_users.user_id
						and dotp_users.user_contact = dotp_contacts.contact_id
						order by dotp_tasks.task_start_date asc")
						
;;; ==============================================================================
;;;  SELECT PROJECT MEMBER LATE START DATE ACTIVITIES IN A SPECIFIC PROJECT
;;; ==============================================================================
(defparameter *select-project-member-late-start-date-activities-in-a-specific-project*
	"select
						dotp_projects.project_name as projeto,
						dotp_contacts.contact_first_name as responsavel_nome,
						dotp_contacts.contact_last_name as responsavel_sobrenome,
						dotp_tasks.task_name as atividade_com_inicio_atrasado,
						dotp_tasks.task_start_date as inicio_planejado
	from
						dotp_tasks, dotp_projects, dotp_contacts,
						dotp_user_tasks, dotp_users
	where
						dotp_projects.project_name = '~a' 
						and dotp_projects.project_id = dotp_tasks.task_project
						and dotp_tasks.task_start_date < now()
						and dotp_tasks.task_percent_complete = '0'
						and dotp_contacts.contact_first_name = '~a'
						and dotp_contacts.contact_last_name = '~a'
						and dotp_user_tasks.task_id = dotp_tasks.task_id
						and dotp_user_tasks.user_id = dotp_users.user_id
						and dotp_users.user_contact = dotp_contacts.contact_id
						order by dotp_tasks.task_start_date asc")
						
;;; ==============================================================================
;;; 				 SELECT ACTIVITIES WITH A TERM ENDING SOON
;;; ==============================================================================
(defparameter *select-activities-with-a-term-ending-soon*
	"select
						dotp_projects.project_name as projeto,
						dotp_tasks.task_name as atividade_por_encerrar,
						dotp_contacts.contact_first_name as responsavel_nome,
						dotp_contacts.contact_last_name as responsavel_sobrenome,
						dotp_tasks.task_end_date as final_planejado,
						dotp_tasks.task_percent_complete as andamento_registrado
	from
						dotp_tasks, dotp_projects, dotp_contacts,
						dotp_user_tasks, dotp_users
	where
						dotp_projects.project_id = dotp_tasks.task_project
						and dotp_tasks.task_end_date > now()
						and dotp_tasks.task_end_date < now() + ~a * 86400
						and dotp_tasks.task_percent_complete < '100'
						and dotp_user_tasks.task_id = dotp_tasks.task_id
						and dotp_user_tasks.user_id = dotp_users.user_id
						and dotp_users.user_contact = dotp_contacts.contact_id
						order by dotp_tasks.task_end_date asc")

;;; ==============================================================================
;;; 				 SELECT PROJECT MEMBER ACTIVITIES WITH A TERM ENDING SOON
;;; ==============================================================================
(defparameter *select-project-member-activities-with-a-term-ending-soon*
	"select
						dotp_projects.project_name as projeto,
						dotp_tasks.task_name as atividade_por_encerrar,
						dotp_contacts.contact_first_name as responsavel_nome,
						dotp_contacts.contact_last_name as responsavel_sobrenome,
						dotp_tasks.task_end_date as final_planejado,
						dotp_tasks.task_percent_complete as andamento_registrado
	from
						dotp_tasks, dotp_projects, dotp_contacts,
						dotp_user_tasks, dotp_users
	where
						dotp_projects.project_id = dotp_tasks.task_project
						and dotp_tasks.task_end_date > now()
						and dotp_tasks.task_end_date < now() + ~a * 86400
						and dotp_tasks.task_percent_complete < '100'
						and dotp_contacts.contact_first_name = '~a'
						and dotp_contacts.contact_last_name = '~a'
						and dotp_user_tasks.task_id = dotp_tasks.task_id
						and dotp_user_tasks.user_id = dotp_users.user_id
						and dotp_users.user_contact = dotp_contacts.contact_id
						order by dotp_tasks.task_end_date asc")
						
;;; ==============================================================================
;;; SELECT PROJECT MEMBER ACTIVITIES WITH A TERM ENDING SOON IN A SPECIFIC PROJECT
;;; ==============================================================================
(defparameter *select-project-member-activities-with-a-term-ending-soon-in-a-specific-project*
	"select
						dotp_projects.project_name as projeto,
						dotp_tasks.task_name as atividade_por_encerrar,
						dotp_contacts.contact_first_name as responsavel_nome,
						dotp_contacts.contact_last_name as responsavel_sobrenome,
						dotp_tasks.task_end_date as final_planejado,
						dotp_tasks.task_percent_complete as andamento_registrado
	from
						dotp_tasks, dotp_projects, dotp_contacts,
						dotp_user_tasks, dotp_users
	where
						dotp_projects.project_name = '~a' 
						and dotp_projects.project_id = dotp_tasks.task_project
						and dotp_tasks.task_end_date > now()
						and dotp_tasks.task_end_date < now() + ~a * 86400
						and dotp_tasks.task_percent_complete < '100'
						and dotp_contacts.contact_first_name = '~a'
						and dotp_contacts.contact_last_name = '~a'
						and dotp_user_tasks.task_id = dotp_tasks.task_id
						and dotp_user_tasks.user_id = dotp_users.user_id
						and dotp_users.user_contact = dotp_contacts.contact_id
						order by dotp_tasks.task_end_date asc")
						
;;; ==============================================================================
;;; 						SELECT INACTIVE PROJECTS
;;; ==============================================================================
(defparameter *select-inactive-projects*
	"select distinct
				dotp_projects.project_name as projeto_inativo
	from
				dotp_projects
	where
				dotp_projects.project_id NOT IN
					(
						select distinct
							dotp_projects.project_id
						from
							dotp_tasks, dotp_projects
						where
							dotp_tasks.task_percent_complete > '0'
							and dotp_tasks.task_percent_complete < '100'
					)
					order by dotp_projects.project_name asc")
;;;===============================================================================

;;; 
;;;
;;; ==============================================================================
;;;                        Macros section
;;; ==============================================================================

(defmacro with-database-connection ((registers fields)
									(&key selector args)
									&body body)
"Creates an abstraction releasing connection resources and avoiding repeated code"
	`(let* ((db (connect :database *database-name* :user *username*))
			   (query-string-list (append (list nil ,selector) ,args))
			   (query-string (apply #'format query-string-list)))
			(multiple-value-bind (,registers ,fields) (sql query-string :db db)
				(disconnect :db db)
				,@body)))
		
				
;;;
;;;
;;; ==============================================================================
;;;                        Utilitary Functions
;;; ==============================================================================

(defun parse-query-results (registers)
"discard null and multiple nested registers"
				(car (remove-if #'null (if (> (list-length registers) 1)
										(apply #'append registers)
									registers))))
									
(defun filter-registers-by-project-name (registers monitored-projects-list)
"filter the query result to return just projects that the agent is concerned to monitor"
	(remove-if-not (lambda (register)
                        (member-if (lambda (monitored-project)
                                       (string= (car register) (cdr monitored-project))) 
									monitored-projects-list)) 
					registers))

;;;
;;; Functions that realize querries in the project mangement tool database
;;;
;;; ==============================================================================
;;;                        Database query functions
;;; ==============================================================================

;;; ----------------------------- SELECT-PROJECT-ID															
(defun select-project-id (project-name)
"Returns the project id of the given project" 
	(with-database-connection (registers fields) 
								(:selector *select-project-id* 
								 :args (list project-name)) 
						   
		(car registers)))
							
;;; ----------------------------- SELECT-WHAT-ACTIVITIES-TO-DO-NOW
(defun select-what-activities-to-do-now (monitored-projects-list)
	"The query should return activities so that:
		- have the lowest inicial dates
		- have not been finished yet
		- related to all project members in all projects being monitored"
		(with-database-connection (registers fields) (:selector *select-what-activities-to-do-now*) 
						   
		   (list fields (filter-registers-by-project-name registers monitored-projects-list))))
		
;;; ----------------------------- SELECT-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW																
(defun select-what-activities-should-a-project-member-to-do-now (project-member-first-name 
													             project-member-last-name
																 project-ids-list)
"The query should return activities so that:
		- have the lowest inicial dates
		- have not been finished yet
		- belong to all projects that project member participate" 
	(labels ((select-activities-in-a-specific-project (project-id)
				(with-database-connection (registers fields) 
				                   (:selector *select-what-activities-should-a-project-member-to-do-now*
								    :args (list project-member-first-name 
										        project-member-last-name
										        project-id))
						   
					(values registers fields))))				
								
		(multiple-value-bind (registers fields) (select-activities-in-a-specific-project (car project-ids-list))
				(setf registers (mapcar #'select-activities-in-a-specific-project
										project-ids-list))		   
				(list fields (parse-query-results registers)))))
							
;;; ----------------------------- SELECT-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW-IN-A-SPECIFIC-PROJECT																
(defun select-what-activities-should-a-project-member-to-do-now-in-a-specific-project 
																(project-member-first-name 
													             project-member-last-name
																 project-name)
"The query should return activities so that:
		- Be in the project X and belong to project member Y
		- have the lowest inicial dates
		- have not been finished yet"  
	(with-database-connection (registers fields) 
	            (:selector *select-what-activities-should-a-project-member-to-do-now-in-a-specific-project*
				 :args (list project-member-first-name 
				             project-member-last-name
			                 project-name)) 
						   
		(list fields registers)))
							
;;; ------------------------------------------------- SELECT-LATE-END-DATE-ACTIVITIES
(defun select-late-end-date-activities (project-ids-list)
"The query should return activities so that:
		- have not been finished yet
		- belong to all projects
		- the end date is gone
		- are fewer then 100% complete" 
	(labels ((select-activities-in-a-specific-project (project-id)
				(with-database-connection (registers fields) 
				                   (:selector *select-late-end-date-activities*
								    :args (list project-id))
						   
					(values registers fields))))				
								
		(multiple-value-bind (registers fields) (select-activities-in-a-specific-project (car project-ids-list))
				(setf registers (mapcar #'select-activities-in-a-specific-project
										project-ids-list))		   
				(list fields (parse-query-results registers)))))
				
;;; ------------------------------------------------- SELECT-PROJECT-MEMBER-LATE-END-DATE-ACTIVITIES
(defun select-project-member-late-end-date-activities (monitored-projects-list
													   project-member-first-name 
													   project-member-last-name)
"The query should return activities so that:
		- have not been finished yet
		- belong to all projects being monitored
		- belong to a specific project member
		- the end date is gone
		- are fewer then 100% complete" 
	(with-database-connection (registers fields) 
	            (:selector *select-project-member-late-end-date-activities*
				 :args (list project-member-first-name 
				             project-member-last-name)) 
						   
		(list fields (filter-registers-by-project-name registers monitored-projects-list))))
					
;;; ------------------------------------------------- SELECT-PROJECT-MEMBER-LATE-END-DATE-ACTIVITIES-IN-A-SPECIFIC-PROJECT
(defun select-project-member-late-end-date-activities-in-a-specific-project (project-name
																			 project-member-first-name 
																			 project-member-last-name)
"The query should return activities so that:
		- have not been finished yet
		- belong to a specific project
		- belong to a specific project member
		- the end date is gone
		- are fewer then 100% complete" 
	(with-database-connection (registers fields) 
	            (:selector *select-project-member-late-end-date-activities-in-a-specific-project*
				 :args (list project-name
				             project-member-first-name 
				             project-member-last-name)) 
						   
		(list fields registers)))
		
;;; ------------------------------------------------- SELECT-LATE-START-DATE-ACTIVITIES
(defun select-late-start-date-activities (project-ids-list)
"The query should return activities so that:
		- have not been finished yet
		- belong to all projects
		- are 0% complete" 
	(labels ((select-activities-in-a-specific-project (project-id)
				(with-database-connection (registers fields) 
				                   (:selector *select-late-start-date-activities*
								    :args (list project-id))
						   
					(values registers fields))))				
								
		(multiple-value-bind (registers fields) (select-activities-in-a-specific-project (car project-ids-list))
				(setf registers (mapcar #'select-activities-in-a-specific-project
										project-ids-list))		   
				(list fields (parse-query-results registers)))))
				
;;; ------------------------------------------------- SELECT-PROJECT-MEMBER-LATE-START-DATE-ACTIVITIES
(defun select-project-member-late-start-date-activities
													  (monitored-projects-list
													   project-member-first-name 
													   project-member-last-name)
"The query should return activities so that:
		- have not been finished yet
		- belong to all projects being monitored
		- belong to a specific project member
		- are 0% complete" 
	(with-database-connection (registers fields) 
	            (:selector *select-project-member-late-start-date-activities*
				 :args (list project-member-first-name 
				             project-member-last-name)) 
						   
		(list fields (filter-registers-by-project-name registers monitored-projects-list))))
		
;;; ------------------------------------------------- SELECT-PROJECT-MEMBER-LATE-START-DATE-ACTIVITIES-IN-A-SPECIFIC-PROJECT
(defun select-project-member-late-start-date-activities-in-a-specific-project 
																			(project-name
																			 project-member-first-name 
																			 project-member-last-name)
"The query should return activities so that:
		- have not been finished yet
		- belong to a specific project
		- belong to a specific project member
		- are 0% complete" 
	(with-database-connection (registers fields) 
	            (:selector *select-project-member-late-start-date-activities-in-a-specific-project*
				 :args (list project-name
					         project-member-first-name 
						     project-member-last-name)) 
						   
		(list fields registers)))
		
;;; ------------------------------------------------- SELECT-ACTIVITIES-WITH-A-TERM-ENDING-SOON
(defun select-activities-with-a-term-ending-soon (remaining-days monitored-projects-list)
"The query should return activities so that:
		- have not been finished yet
		- are 0% complete
		- the term is finished in remaining-days" 
	(with-database-connection (registers fields) 
	            (:selector *select-activities-with-a-term-ending-soon*
				 :args (list remaining-days)) 
						   
		(list fields (filter-registers-by-project-name registers monitored-projects-list))))
		
;;; ------------------------------------------------- SELECT-PROJECT-MEMBER-ACTIVITIES-WITH-A-TERM-ENDING-SOON
(defun select-project-member-activities-with-a-term-ending-soon (remaining-days 
																 monitored-projects-list
																 project-member-first-name
																 project-member-last-name)
"The query should return activities so that:
		- have not been finished yet
		- are 0% complete
		- the term is finished in remaining-days
		- belongs to the specified project member" 
	(with-database-connection (registers fields) 
	            (:selector *select-project-member-activities-with-a-term-ending-soon*
				 :args (list remaining-days project-member-first-name project-member-last-name)) 
						   
		(list fields (filter-registers-by-project-name registers monitored-projects-list))))
		
;;; ------------------------------------------------- SELECT-PROJECT-MEMBER-ACTIVITIES-WITH-A-TERM-ENDING-SOON-IN-A-SPECIFIC-PROJECT
(defun select-project-member-activities-with-a-term-ending-soon-in-a-specific-project 
																(remaining-days 
																 project-name
																 project-member-first-name
																 project-member-last-name)
"The query should return activities so that:
		- have not been finished yet
		- are 0% complete
		- the term is finished in remaining-days
		- belongs to the specified project member
		- is related to the specified project" 
	(with-database-connection (registers fields) 
	            (:selector *select-project-member-activities-with-a-term-ending-soon-in-a-specific-project*
				 :args (list project-name
							 remaining-days				 
							 project-member-first-name 
							 project-member-last-name)) 
						   
		(list fields registers)))
		
;;; ------------------------------------------------- SELECT-INACTIVE-PROJECTS
(defun select-inactive-projects (monitored-projects-list)
	(with-database-connection (registers fields) 
	            (:selector *select-inactive-projects*) 
		(list fields (filter-registers-by-project-name registers monitored-projects-list))))
:EOF