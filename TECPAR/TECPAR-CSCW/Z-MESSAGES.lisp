;;;===============================================================================
;;;<date>
;;;                 PREDEFINED MESSAGES (file: Z-messages.lisp)
;;;
;;;===============================================================================

;;; loaded in the package of the loading environment (usually OMAS)

(in-package :omas)

;;; insert messages you want to load with the application, e.g.
;;;    (defmessage :DF-0 :to :UTC-FAC-1 :type :request :action :dumb-fac :args (5))

;;; default message
(defmessage :hello :to :all :type :request :action :hello)
(defmessage :add-new-project :to :TECPAR-DP-MONITOR :type :request :action :ADD-MONITORED-PROJECT :args ("Nome do Projeto"))
(defmessage :what-activities-to-do-now? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-WHAT-ACTIVITIES-TO-DO-NOW)
(defmessage :what-activities-should-a-project-member-to-do-now? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW :args ("Allan" "Trevisan"))
(defmessage :what-activities-should-a-project-member-to-do-now-in-a-specific-project? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW-IN-A-SPECIFIC-PROJECT :args ("CSCW-TECPAR" "Allan" "Trevisan"))
(defmessage :what-activities-are-late-end-date? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-LATE-END-DATE-ACTIVITIES)
(defmessage :what-project-member-activities-are-late-end-date? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-PROJECT-MEMBER-LATE-END-DATE-ACTIVITIES :args ("Allan" "Trevisan"))
(defmessage :what-project-member-activities-are-late-end-date-in-a-specific-project? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-PROJECT-MEMBER-LATE-END-DATE-ACTIVITIES-IN-A-SPECIFIC-PROJECT :args ("CSCW-TECPAR" "Allan" "Trevisan"))
(defmessage :what-activities-are-late-start-date? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-LATE-START-DATE-ACTIVITIES)
(defmessage :what-project-member-activities-are-late-start-date? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-PROJECT-MEMBER-LATE-START-DATE-ACTIVITIES :args ("Allan" "Trevisan"))
(defmessage :what-project-member-activities-are-late-start-date-in-a-specific-project? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-PROJECT-MEMBER-LATE-START-DATE-ACTIVITIES-IN-A-SPECIFIC-PROJECT :args ("CSCW-TECPAR" "Allan" "Trevisan"))
(defmessage :what-activities-are-ending-soon? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-ACTIVITIES-WITH-A-TERM-ENDING-SOON :args (3))
(defmessage :what-project-member-activities-are-ending-soon? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-PROJECT-MEMBER-ACTIVITIES-WITH-A-TERM-ENDING-SOON :args (3 "Allan" "Trevisan"))
(defmessage :what-project-member-activities-are-ending-soon-in-a-specific-project? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-PROJECT-MEMBER-ACTIVITIES-WITH-A-TERM-ENDING-SOON-IN-A-SPECIFIC-PROJECT :args (3 "CSCW-TECPAR" "Allan" "Trevisan"))
(defmessage :what-projects-are-inactive? :to :TECPAR-DP-MONITOR :type :request :action :SEARCH-INACTIVE-PROJECTS)
:EOF