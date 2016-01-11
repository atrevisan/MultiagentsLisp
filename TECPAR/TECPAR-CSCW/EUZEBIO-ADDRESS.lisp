;;;-*- Mode: Lisp; Package: "EUZEBIO-ADDRESS" -*-
;;;===============================================================================
;;;10/10/18
;;;                               AGENT EUZEBIO-ADDRESS 
;;;
;;;===============================================================================

;;; we call our agent EUZEBIO-ADDRESS to avoid conflicts with other staff agents of
;;; other PAs

#|
2007
 0831 adding a few functions to select using all words in the input
 0901 shift most functions into MOSS services
 1017 adding :what-is skill
2008
 0605 checking failures on unknown persons modifying get-phone-fill-pattern
2009
 0209 adding master option to agent definition
 0801 renaming agent EUZEBIO-ADDRESS. ARS stans for ARISTIDE
2010
 1018 cleaning the file
|#

(defpackage :EUZEBIO-ADDRESS (:use :moss :omas :cl #+MCL :ccl))
(in-package :EUZEBIO-ADDRESS)

;(omas::defagent :EUZEBIO-ADDRESS :master (:EUZEBIO) :redefine t)
(omas::defagent :EUZEBIO-ADDRESS :redefine t)

;;;===============================================================================
;;; 
;;;                         service macros and functions 
;;;
;;;===============================================================================


;;; ==================================== globals =================================
;;; Globals  can be used within the agent space to simplify programming, although
;;; it is better to use the agent memory.

(defParameter *address-words* 
  '("address" "home address" "live" "lives"
    "adresse" "domicile" "habite" "vit"
    "endereço" "residência" "endereço resdidencial" "mora" "vive" "vivem"))

(defParameter *empty-words* 
  '("the" "a" "an" "of" "from" "to" "those" "these" "that" "s" "?"
    "le" "la" "les" "de" "du" "des" "à" "aux" "ce" "ces" "d" "l"
    "o" "a" "os" "as" "um" "uma" "uns" "umas" ""))

(defParameter *phone-words*
  '("phone" "telephone" "phone-number" "telephone-number" "number" "cell"
    "téléphone" "numéro" "numéro-de-téléphone" "portable"
    "telefone" "celular" "profissional" "privado"))

#|
(defParameter *default-person-phone-pattern*
  '("personne" ("nom")("prénom")
    ("téléphone" ("téléphone"))))

(defParameter *default-organization-phone-pattern*
  '("organisation" ("nom") ("sigle")
    ("téléphone" ("téléphone"))))
|#
;;;===============================================================================
;;; 
;;;                                    skills 
;;;
;;;===============================================================================

;;; ========================== skill section GET ADDRESS =========================

;;; the :get-address skill corresponds to a request for business address of a person 
;;; or an organization. It returns addresses as a filled pattern or a string if no
;;; pattern is present.
;;; The strategy is the following:
;;;  1. if the input contains the entry point of a person, then get the address(es) 
;;;     of this person
;;;  2. if the input contains the entry point of an organization, then return the
;;;     address(es) of this organization
;;;  3. otherwise, try to obtain a path to a person from the input, if success,
;;;     then return the addresses of the person(s)
;;;  4. finally try to obtain a path to an organization, if successful, then return
;;;     the address(es) of th organization
;;;  5. if all failed, return :failure

(defskill :GET-address :EUZEBIO-ADDRESS
  :static-fcn static-GET-ADDRESS
  )

;;;------------------------------------------------------------ STATIC-GET-ADDRESS

;;; When called from dialog there is no pattern. Hence the return is a list containing
;;; sublists of strings, each being an address.
;;; However, when wanting only provate addresses the pattern could be:
;;;   ("pessoa" ("sobrenome")("nome")("endereço privado"))
;;; See examples.

(defUn static-get-address (agent message args)
  "We receive a request for an address. We try to obtain the info from the data
Arguments:
   agent: :ADDRESS
   message: incoming message
   args: e.g. ((:data \"de\" \"Barthès\")(:pattern ...))
Return:
   :failure ou une liste d'adresses."
  (declare (ignore message))
  ;; first we should get some entry-point from data
  ;; remove empty words from the data
  (let ((data (cdr (assoc :data args)))
        (*language* (or (cdr (assoc :language args)) *language*))
        (pattern (cdr (assoc :pattern args)))
        input results)
    ;(format *debug-io* 
    ;    "~&+++ :ADDRESS static-get-address /data: ~S in package: ~S~
    ;     ~&pattern: ~S" data *package* pattern)
    ;; clean input of parasitic words
    (setq input (moss::%clean-word-list data nil *address-words*))
    
    (format *debug-io* "~&+++ static-get-address /cleaned input: ~S" input)
    ;; if nothing left, locate-objects will return nil
    
    (setq results
	  (cond
	   ;; 1. try to see if there is a reference to a person. the last argument 0 
	   ;;    means that we do not even consider neighbors
           ((moss::locate-objects input "pessoa" 0))
	   ;; 2. try organizations
	   ((moss::locate-objects input "organização" 0))
	   ;; 3. if if fails, increase length of path using default
	   ((moss::locate-objects input "pessoa"))
	   ;; 4. again with organizations
	   ((moss::locate-objects input "organização"))
	   ;; otherwise give up
	   ))
    
    (format *debug-io* "~&+++ static-get-address /located results: ~S" results)
    
    (when results 
      (setq results
	    (if pattern 
              ;; fill-pattern is a MOSS funtion
              (fill-pattern results pattern)
              ;; make a list of string addresses
              (reduce #'append (broadcast results '=get-address)))))
    
    (static-exit agent (or results :failure))))

#| Examples
   ========
? (static-get-address 'sa_ze-address  nil '((:data "endereço" "do" "UTC")))
(("Université de Technologie de Compiègne (UTC)
rue Roger Couttolenc
60200 Compiègne
França"))
  
? (static-get-address 'sa_ze-address  nil 
         '((:data "endereço" "do" "UTC")
           (:pattern . ("organização" ("nome")
		                ("endereço profissional"
		                 ("endereço profissional"
					       ("rua e número")
						   ("CEP")
						   ("cidade")
						   ("país")))))))
(("organização" ("nome" "Université de Technologie de Compiègne")
  ("endereço profissional"
   ("endereço profissional" ("rua e número" "rue Roger Couttolenc") ("CEP" "60200")
    ("cidade" "Compiègne") ("país" "França")))))
	
? (static-get-address 'sa_ze-address  nil 
         '((:data "endereço" "do" "UTC")
           (:pattern . ("organização" ("nome")
		                  ("endereço profissional")))))
(("organização" ("nome" "Université de Technologie de Compiègne")
  ("endereço profissional" "rue Roger Couttolenc
60200 Compiègne
França")))

Inherited address from employer
? (static-get-address 'sa_ze-address  nil '((:data "endereço" "do" "Paraiso")))
(("Paraiso: Emerson
= endereço profissional
Universidade Pontifical do Parana (PUC)
rua Immaculata Conceiçaõ
 Curitiba, Parana
Brasil")) 

;; severl addresses
? (static-get-address 'sa_ze-address  nil '((:data "endereço" "do" "barthès")))
(("Barthès: Jean-Paul
= endereço profissional
Université de Technologie de Compiègne
Centre de Recherche de Royallieu
Departement de Génie Informatique
UMR CNRS 6599 HEUDIASYC
60206 Compiègne cedex
França
= endereço privado
14, Allée de la Tilloye
60200 Compiègne
França")
 ("Barthès, Barthès-Biesel: Dominique
= endereço profissional
Université de Technologie de Compiègne
Centre de Recherche de Royallieu
Département de Génie Biomédical
60206 Compiègne cedex

= endereço privado
14, Allée de la Tilloye
60200 Compiègne
França")
 ("Barthès: Camille - *endereço non disponível*"))
 
;; extracting only private addresses 
? (static-get-address 'sa_ze-address  nil 
                  '((:data "endereço" "do" "barthès")
                    (:pattern . ("pessoa" ("sobrenome")("nome")
					                      ("endereço privado")))))
(("pessoa" ("sobrenome" "Barthès") ("nome" "Jean-Paul")
  ("endereço privado" "14, Allée de la Tilloye
60200 Compiègne
França"))
 ("pessoa" ("sobrenome" "Barthès" "Barthès-Biesel") ("nome" "Dominique")
  ("endereço privado" "14, Allée de la Tilloye
60200 Compiègne
França"))
 ("pessoa" ("sobrenome" "Barthès") ("nome" "Camille") ("endereço privado")))
|#
;;; ======================= skill section GET PHONE NUMBER =======================

;;; Strategy to get a phone number.(similar to addresses)
;;; We get a list of words or sentence that can be:
;;; 1. - numéro de téléphone de barthès?
;;; 2. - phone Dominique?
;;;or, in French
;;; 3. - téléphone du président de l'UTC
;;; 4. - téléphone du CNRS?
;;;

(defskill :GET-PHONE-NUMBER :EUZEBIO-ADDRESS
  :static-fcn static-GET-PHONE-NUMBER
  )

(defUn static-get-phone-number (agent message args)
  "We receive a request for a phone number. We try to obtain the info from the ~
   data.
Arguments:
   agent: :EUZEBIO-ADDRESS
   message: incoming message
   args: alist e.g. ((:data \"of\" \"Barthes\")(:language :BR))
Return:
   :failure or filled pattern."
  (declare (ignore message))
  
  (let ((data (cdr (assoc :data args)))
        (*language* (or (cdr (assoc :language args)) *language*))
        (pattern (cdr (assoc :pattern args)))
        input results)
    ;(format *debug-io* 
    ;    "~&+++ :ADDRESS static-get-address /data: ~S in package: ~S~
    ;     ~&pattern: ~S" data *package* pattern)
    ;; clean input of parasitic words
    (setq input (moss::%clean-word-list data nil *phone-words*))
    
    (format *debug-io* "~&+++ static-get-address /cleaned input: ~S" input)
    ;; if nothing left, locate-objects will return nil
    
    (setq results
	  (cond
	   ;; 1. try to see if there is a reference to a person. the last argument 0 
	   ;;    means that we do not even consider neighbors
           ((moss::locate-objects input "pessoa" 0))
	   ;; 2. try organizations
	   ((moss::locate-objects input "organização" 0))
	   ;; 3. if if fails, increase length of path using default
	   ((moss::locate-objects input "pessoa"))
	   ;; 4. again with organizations
	   ((moss::locate-objects input "organização"))
	   ;; otherwise give up
	   ))
    
    (format *debug-io* "~&+++ static-get-address /located results: ~S" results)
    
    (when results 
      (setq results
	    (if pattern 
              ;; fill-pattern is a funtion of the MOSS API
              (fill-pattern results pattern)
              ;; make a list of string addresses, not a list of lists of strings
              (reduce #'append (broadcast results '=get-phone)))))
    
    (static-exit agent (or results :failure))))

#|
? (static-get-phone-number SA_EUZEBIO-ADDRESS nil '((:data "milton")))
(("Ramos: Milton - *telefone non disponível*"))


? (static-get-phone-number SA_EUZEBIO-ADDRESS nil '((:data "telefone" "de" "barthès")))
(("Barthès: Jean-Paul
= telefone profissional
+33 (0)3 44 23 44 66
= telefone privado
+33 (0)3 44 23 31 37
= telefone celular
+33 (0)6 80 45 32 67")
 ("Barthès, Barthès-Biesel: Dominique
= telefone profissional
+33 (0)3 44 23 43 93
= telefone privado
+33 (0)3 44 23 31 37
= telefone celular
+33 (0)6 81 24 46 66")
 ("Barthès: Camille
= telefone privado
+86 571 8701 0782
= telefone celular
+86 134 5678 3173"))
|#	
;;;===============================================================================
;;;                          skill section :WHO-IS 
;;;===============================================================================

(defskill :WHO-IS :EUZEBIO-ADDRESS
  :static-fcn static-WHO-IS
  )

(defUn static-who-is (agent environment args)
  "We receive a request for info on a person.
Arguments:
   agent: :EUZEBIO-ADDRESS
   environment: environment
   args: e.g. ((:data \"of\" \"Barthes\"))
Return:
   :failure or e.g. (:address \"03 44 20 31 37\")."
  (declare (ignore environment))
  ;; first we should get some entry-point from data
  ;; remove empty words from the data
  (let ((data (cdr (assoc :data args)))
        (*language* (or (cdr (assoc :language args)) *language*))
        results person-list)
    
    (setq person-list (moss::locate-objects data "pessoa"))
    ;; if we got something here, we should access the publications, counts them,
    (setq results 
          (broadcast 
           (delete-duplicates person-list)
           '=get-documentation :no-summary t))
		   
		
    ;(format *debug-io* "~&+++ EUZEBIO-ADDRESS WHO-IS /results: ~S" results)
    (static-exit agent (or results "failure"))))

#|
? (static-who-is SA_EUZEBIO-ADDRESS nil '((:data "quem" "é" "barthès")))
("
Jean-Paul Barthès é professor da UTC." "
Dominique Barthès-Biesel é professora da UTC." "
Camille Barthès é um coolie chines que mora em Qingdao.")
|#
;;;============================ skill section WHAT-IS ============================

(defskill :WHAT-IS :EUZEBIO-ADDRESS
  :static-fcn static-WHAT-IS
  )

(defUn static-what-is (agent message args)
  "We receive a request for info on a person.
Arguments:
   agent: :EUZEBIO-ADDRESS
   message: incoming message
   args: e.g. ((:data \"of\" \"Barthes\"))
Return:
   :failure or a list of documentation strings."
  (declare (ignore message))
  ;; first we should get some entry-point from data
  ;; remove empty words from the data
  (let ((*language* (or (cdr (assoc :language args)) *language*))
        entries results)
    ;; try to get entry points from data
    (setq entries 
          (moss::find-best-entries (cdr (assoc :data args)) :all t))
    ;(format *debug-io* "~&+++ EUZEBIO-ADDRESS WHAT-IS /entries: ~S in package: ~S"
    ;  entries *package*)
    
    (when entries
      (setq results (broadcast entries '=get-documentation)))
    ;(format *debug-io* "~&+++ EUZEBIO-ADDRESS WHAT-IS /results: ~S" results)
    (static-exit agent (or results :failure))))

#|
? (static-what-is SA_EUZEBIO-ADDRESS nil '((:data "DGA" "o" "é" "que")))
("
Délégation Générale pour l'Armement (DGA) : Maître d'ouvrage des programmes d'armement, la DGA est responsable 
de la conception, de l'acquisition et de l'évaluation des systèmes qui équipent 
les forces armées. Son action couvre toute la durée de vie de ces programmes.")

? (static-what-is SA_EUZEBIO-ADDRESS nil '((:data "PUC" "o" "é" "que")))
("
Universidade Pontifical do Parana (PUC) : *sorry no documentation available*")
|#
;;;===============================================================================
;;; 
;;;                               initial conditions 
;;;
;;;===============================================================================

:ENV


:EOF