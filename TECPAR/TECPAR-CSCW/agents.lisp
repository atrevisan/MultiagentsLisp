;;;===============================================================================
;;;08/03/12
;;;                      A G E N T S (file agents.lisp)
;;;
;;;                      Copyright JP BArthès @UTC, 2004-2010
;;;
;;;===============================================================================

;;; This file contains the names of the agents that must be loaded into the local 
;;; lisp environment. It is called when OMAS is initialized (v5 and above)

(in-package :omas)

;;; the global variable *local-coterie-agents* is already defined
;;; agents should be referred to by a keyword (e.g. :book-buyer)
;;; usually there is a single PA although one could have several ones e.g. speaking
;;; different languages
;;; There are as many transfer agents as there are connections with foreignn systems
;;; there could be none

;;; replace the keywords :SAAA-1, :SAAA-2, :PBBB, :XCCC with the keywords specifying
;;; your agents. They will name the agent files. Remove the three dots ...

(setq *local-coterie-agents* 
      '(:database-access :TECPAR-DP-MONITOR :EUZEBIO :EUZEBIO-address))

;;; you can also define variables to control the initialization/debugging process
;;; and an initializing function
;;;   (defun init () ...)

#| For example here we define a message to start a simulation and send it at loading
time. We could have declared a goal...

(defmessage :SIMUL-TATIN :from :UTC-TATIN :to :XTATIN :type :request
  :action :simulate-jade)

(defun init ()
  (declare (special simul-tatin))
  (send-message simul-tatin)
  (setq *application-parameters* '((:no-echo t)))
  ;; trace conversation (verbose)
  (v+)
  )
|#

:EOF