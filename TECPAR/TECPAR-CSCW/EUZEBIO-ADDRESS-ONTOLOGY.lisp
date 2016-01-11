;;;-*- Mode: Lisp; Package: "EUZEBIO-address" -*-
;;;===============================================================================
;;;10/10/18
;;;             AGENT EUZEBIO-address: ONTOLOGY (file stevens-ontology.lisp)
;;;             Copyright Ramos@UTC, 2009-2010
;;;
;;;=============================================================================== 

#|
IMPORTANT:
Please note that the EUZEBIO-ADDRESS ontology must extend the EUZEBIO ontology,
meaning that all concepts and properties of EUZEBIO must appear in EUZEBIO-ADDRESS,
although some attributes of EUZEBIO can be more detailed, leading to a relation
in EUZEBIO-ADDRES, e.g. "country". If this is not the case EUZEBIO will not print 
the results correctly.

EUZEBIO-ADDRESS is one of the staff agent providing address information.
It can find email addressses, telephone numbers, web pages (?)
EUZEBIO-ADDRESS receives a message containing a pattern and transfers back the
filled pattern. When no pattern, transfers the object in which neighbors
have been replaced by their summary.
Example:
     (:send-message :to :EUZEBIO-ADDRESS :self PA_EUZEBIO :action :get-address
                 :args `(((:data . ,(read-fact moss::conversation :input))
                          (:language . :br)
                          (:pattern . ("personne" 
                                       ("nom")
                                       ("prénom")
                                       ("adresse" 
                                        ("adresse domicile" 
                                         ("rue") ("code postal") ("ville"))))))))
EUZEBIO-ADDRESS has the responsibility of filling this pattern with the right data. To
do so it uses the fill-pattern function.
With the pattern example a typical example can be:
("personne" ("nom" "Barthès" "Barthès-Biesel") ("prénom" "Dominique")
 ("adresse" ("adresse domicile" ("rue" "14, Allée de la Tilloye") ("code postal" "60200")
            ("ville" "Compiègne"))))
			
With the same data and a different pattern:
      ("personne" 
           ("nom")
           ("prénom")
           ("adresse"))
->
  ("personne" ("nom" "Barthès" "Barthès-Biesel") ("prénom" "Dominique")
    ("adresse" 
	   "14, Allée de la Tilloye
       60200 Compiègne
       França" 
	   "Université de Technologie de Compiègne
       Centre de Recherche de Royallieu
       Département de Génie Biomédical
       60206 Compiègne cedex
       "))		   
In that case, the =summary method is used on the address objects

2009
 0801 setting package to EUZEBIO-address
|#

(in-package :EUZEBIO-address)


;(break "EUZEBIO-address")

(defontology 
  (:title "EUZEBIO-address ontology")
  (:version "1.0")
  (:package :EUZEBIO-address)
  (:language :all)
  )

;;;-------------------------------------------------------------------------------
;;;                                 ADDRESS
;;;-------------------------------------------------------------------------------

(defconcept (:en "address" :fr "Adresse" :br "Endereço")
  (:doc :en "An ADDRESS can be different things:
   - a postal home address
   - a professional address
   - an email
   - a web address, i.e. the address of a personal web page
Each of those can be multiple."
        :fr "Une ADDRESSE peut être:
   - l'adresse du domicile
   - l'adresse professionnelle
   - l'adresse email ou courriel
   - une adress web, c'est-à-dire l'adresse du site personnel
Chacune de ces adresses peut être multiple."
        :br "Um ENDEREÇO pode ser:
   - o endereço residencial
   - o endereço profissional
   - o endereço eletrônico ou email
   - o endereço Web, ou seja, endereço da página pessoal
Cada destes pode ser múltiplo"
	))

;;;-------------------------------------------------------------------------------
;;;                                 CELL PHONE
;;;-------------------------------------------------------------------------------

(defconcept (:en "cell phone" :fr "portable; mobile" 
                 :br "telefone celular; celular")
  (:doc :en "A CELL PHONE is a portable telephone."
        :fr "Un PORTABLE c’est un téléphone mobile."
        :br "Um TELEFONE CELULAR é um telefone móvel.")
  (:is-a "phone")
  )

;;;-------------------------------------------------------------------------------
;;;                                 CONFERENCE
;;;-------------------------------------------------------------------------------

(defconcept (:en "conference; congress" :fr "congrès; colloque"
                 :br "congresso; conferência")
  (:is-a "meeting")
  )

;;;-------------------------------------------------------------------------------
;;;                                   CITY
;;;-------------------------------------------------------------------------------

(defconcept (:en "city; town" :fr "ville; cité" :br "cidade")
  (:is-a "territory")
  (:att (:en "zip" :fr "code postal" :br "CEP; código postal"))
  (:rel (:en "district" :fr "département" :br "estado") (:to "district"))
  )

;;;-------------------------------------------------------------------------------
;;;                                 COUNTRY
;;;-------------------------------------------------------------------------------

(defconcept (:en "country" :fr "pays" :br "país")
  (:is-a "territory")
  )

;;;-------------------------------------------------------------------------------
;;;                                  DATE
;;;-------------------------------------------------------------------------------

(defconcept (:en "date" :fr "date" :br "data")
  (:att (:en "year" :fr "année" :br "ano"))
  (:att (:en "month" :fr "mois":br "mês"))
  (:att (:en "jour" :fr "day" :br "dia"))
  (:att (:en "week day" :fr "jour de la semaine" :br "dia da semana"))
  )

;;;-------------------------------------------------------------------------------
;;;                                 DISTRICT
;;;-------------------------------------------------------------------------------

(defconcept (:en "district" :fr "département" :br "estado")
  (:is-a "territory")
  (:att (:en "number" :fr "numéro"))
  (:rel (:en "region" :fr "région" :br "região") (:to "region"))
  )

;;;-------------------------------------------------------------------------------
;;;                                 EMAIL ADDRESS
;;;-------------------------------------------------------------------------------

(defconcept (:en "email address; email" :fr "adresse email; courriel" 
                 :br "email; endereço eletrônico")
  (:is-a "address")
  (:doc :en "An EMAIL ADDRESS is the code allowing to receive an electronic ~
             message."
        :fr "Une ADRESSE EMAIL ou COURRIEL est celle où l'on peut recevoir un ~
             courrier électronique."
        :br "Um ENDEREÇO ELETRÔNICO ou EMAIL é aquele onde podemos receber um ~
             correio eletrônico")
  (:att (:en "e-address" :fr "e-adresse" :br "e-endereço")))

;;;-------------------------------------------------------------------------------
;;;                                  ENTERPRISE
;;;-------------------------------------------------------------------------------

(defconcept (:en "company; firm; enterprise" 
                 :fr "société; entreprise"
                 :br "empresa; firma; companhia")
  (:is-a "organization")
  (:doc :en "A COMPANY, FIRM, or ENTERPRISE usually undertakes activities to make ~
             some profit for its owner(s)."
        :fr "Une SOCIETE, ou ENTREPRISE a en général des activités de façon à ~
             réaliser des profits pour ses propriétaires."
        :br "Uma EMPRESA, FIRMA ou COMPANHIA normalmente desenvolve atividades ~
             para realizar lucros para os seus proprietários.")
  )

;;;-------------------------------------------------------------------------------
;;;                                 HOME ADDRESS
;;;-------------------------------------------------------------------------------

(defconcept (:en "home address; private address" 
                 :fr "domicile; adresse privée; adresse personnelle"
                 :br "endereço residêncial, endereço privado, endereço pessoal")
  (:is-a "postal address")
  (:doc :en "A HOME ADDRESS is the information needed to locate ~
             the place where a person is living."
        :fr "Le DOMICILE est l'adresse de l'endroit où la personne habite."
        :br "O DOMICÍLIO é o endereço do lugar onde a pessoa mora.")
  )

;;;-------------------------------------------------------------------------------
;;;                                 HOME PHONE
;;;-------------------------------------------------------------------------------

(defconcept (:en "home phone; home telephone" 
                 :fr "téléphone du domicile; téléphone privé"
                 :br "telefone residencial; telefone particular")
  (:doc :en "A HOME PHONE is a telephone installed at the private address ~
             of the owner."
        :fr "Le TELEPHONE DU DOMICILE est installé au domicile de son ~
             propriétaire."
        :br "O TELEFONE RESIDENCIAL está instalado na residência do seu ~
             proprietário.")
  (:is-a "phone")
  )

;;;-------------------------------------------------------------------------------
;;;                                 MEETING
;;;-------------------------------------------------------------------------------

(defconcept (:en "meeting"
                 :fr "manifestation; rencontre; réunion; meeting"
                 :br "encontro; reunião")
  (:att (:en "name" :fr "nom"))
  (:att (:en "acronym" :fr "sigle"))
  (:rel (:en "place" :fr "lieu" :br "local") (:to "place"))
  (:att (:en "time" :fr "moment" :br "período"))
  (:rel (:en "office address" 
             :fr "adresse professionnelle" 
             :br "endereço profissional") 
        (:to "address"))
  (:rel (:en "President of the PC"
             :fr "président du comité de programme"
             :br "Presidente do comite de programa") (:to "person"))
  (:rel (:en "organizer" 
             :fr "organisateur" 
             :br "organizador") 
        (:to "person"))
  )

;;;-------------------------------------------------------------------------------
;;;                           NON-PROFIT ORGANIZATION
;;;-------------------------------------------------------------------------------

(defconcept (:en "non profit organization"
                 :fr "organisation sans but lucratif"
                 :br "organização sem fins lucrativos")
  (:is-a "organization")
  (:rel (:en "president" 
             :fr "président; directeur"
             :br "presidente; diretor") (:to "person"))
  (:rel (:en "executive committee" :fr "bureau" :br "conselho") (:to "person") (:min 2))
  (:doc :en "A NON PROFIT ORGANIZATION is not allowed to derive profits from its ~
             activities."
        :fr "Une ORGANISATION SANS BUT LUCRATIF est une organisation aui ne peut ~
             retirer des profits de ses activités."
        :br "Uma ORGANIZAÇÃO SEM FINS LUCRATIVOS é uma organização que não pode ~
             gerar lucros das suas atividades.")
  )

;;;-------------------------------------------------------------------------------
;;;                                OFFICE ADDRESS
;;;-------------------------------------------------------------------------------

(defconcept (:en "office address; professional address; business address"
                 :fr "adresse professionnelle"
                 :br "endereço profissional")
  (:is-a "postal address")
  (:doc :en "An OFFICE or PROFESSIONAL ADDRESS is the address of a place where a ~
             person works."
        :fr "Une ADRESSE PROFESSIONNELLE est l'adresse du lieu où une personne ~
             travaille."
        :br "Um ENDEREÇO PROFISSIONAL é o endereço do lugar onde a pessoa trabalha")
  )

#|
(send '$e-office-address.1 '=summary)
("     Centre de Recherche de Royallieu
     BP 20529
     60205 Compiègne cedex
     França")
|#
;;;-------------------------------------------------------------------------------
;;;                                 OFFICE PHONE
;;;-------------------------------------------------------------------------------

(defconcept (:en "office phone; office telephone" 
                 :fr "téléphone professionnel"
                 :br "telefone profissional")
  (:doc :en "An OFFICE PHONE is a telephone installed at the work-place."
        :fr "Le TELEPHONE PROFESSIONNEL est celui de l'endroit où la personne ~
             travaille."
        :br "O TELEFONE PROFISSIONAL é aquele do lugar onde a pessoal trabalha.")
  (:is-a "phone")
  )

;;;-------------------------------------------------------------------------------
;;;                                 ORGANIZATION
;;;-------------------------------------------------------------------------------

(defconcept (:en "organization" :fr "organisation" :br "organização")
  (:att (:en "name" :fr "nom" :br "nome") (:entry))
  (:att (:en "acronym" :fr "sigle" :br "sigla") (:entry))
  (:rel (:en "subsidiary" :fr "filiale" :br "filial") (:to "organization"))
  (:rel (:en "division" :fr "division" :br "divisão")(:to "organization"))
  (:rel (:en "department" :fr "département" :br "departamento") (:to "organization"))
  (:rel (:en "office address" 
             :fr "adresse professionnelle" 
             :br "endereço profissional")
        (:to "postal address"))
  (:rel (:en "home address"
             :fr "domicile"
             :br "endereço privado")
        (:to "postal address"))
  (:rel (:en "office phone"
             :fr "téléphone professionnel"
             :br "telefone profissional")
        (:to "phone"))
  (:rel (:en "web page" :fr "site web" :br "página web") (:to "web page"))
  (:doc :en "An ORGANIZATION is a social structure private or public that has some ~
             social goals."
        :fr "Une ORGANISATION est une structure sociale privée o publique qui ~
             poursuit un but social."
        :br "Uma ORGANIZAÇÃO é uma estrutura social pública ou privada que ~
             tem uma meta social.")
  )

;;;-------------------------------------------------------------------------------
;;;                                 PERSON
;;;-------------------------------------------------------------------------------

(defconcept (:en "person" :fr "personne" :br "pessoa")
  (:att (:en "name" :fr "nom" :br "sobrenome") (:entry))
  (:att (:en "first-name" :fr "prénom" :br "nome")(:entry))
  (:att (:en "trade" :fr "métier" :br "ocupação"))
  (:rel (:en "office address" 
             :fr "adresse professionnelle" 
             :br "endereço profissional")
        (:to "postal address"))
  (:rel (:en "home address"
             :fr "domicile"
             :br "endereço privado")
        (:to "postal address"))
  (:rel (:en "employer" :fr "employeur" :br "firma") (:to "organization"))
  (:rel (:en "office phone" 
             :fr "téléphone professionnel" 
             :br "telefone profisional")
        (:to "phone"))
  (:rel (:en "home phone"
             :fr "téléphone privé"
             :br "telefone privado")
        (:to "phone"))
  (:rel (:en "cell phone"
             :fr "portable"
             :br "celular")
        (:to "phone"))
  (:rel (:en "email" :fr "email" :br "email") (:to "email"))
  (:rel (:en "web page" :fr "page web" :br "página web") (:to "web address"))
  (:rel (:en "husband" :fr  "époux" :br "esposo")(:to "person")(:unique))
  (:rel (:en "wife" :fr "épouse" :br "esposa") (:to "person")(:unique))
  (:rel (:en "mother" :fr "mère" :br "mãe") (:to "person")(:unique))
  )

;;;-------------------------------------------------------------------------------
;;;                                   PLACE
;;;-------------------------------------------------------------------------------

(defconcept (:en "place" :fr "lieu; endroit" :br "local")
  (:att (:en "name" :fr "nom" :br "nome"))
  (:rel (:en "location" :fr "localisation" :br "localização") (:to "territory"))
  )

;;;-------------------------------------------------------------------------------
;;;                                 POSTAL ADDRESS
;;;-------------------------------------------------------------------------------

(defconcept (:en "postal address" :fr "adresse postale" :br "endereço postal")
  (:is-a "address")
  (:doc :en "A POSTAL ADDRESS is the information needed to locate ~
             the place where a person is living or working."
        :fr "L'ADRESSE POSTALE est l'adresse ou le courrier est distribué."
        :br "O ENDEREÇO POSTAL é o endereço onde o correio é entregue")
  (:att (:en "street and number; street" :fr "rue" :br "rua e número"))
  (:att (:en "town; city" :fr "ville" :br "cidade") (:unique))
  (:rel (:en "country" :fr "pays" :br "país") (:unique)(:to "country"))
  (:att (:en "zip; postal code" :fr "code postal" :br "CEP; código postal") 
        (:unique))
  )

;;;-------------------------------------------------------------------------------
;;;                                  REGION
;;;-------------------------------------------------------------------------------

(defconcept (:en "region" :fr "région" :br "região")
  (:is-a "territory")
  (:rel (:en "country" :fr "pays") (:to "country"))  
  )

;;;-------------------------------------------------------------------------------
;;;                                  STUDENT
;;;-------------------------------------------------------------------------------

(defconcept (:en "student" :fr "étudiant" :br "estudante")
  (:is-a "person")
  (:rel (:en "school" :fr "école" :br "escola") (:to "university"))
  )

;;;-------------------------------------------------------------------------------
;;;                                  SYMPOSIUM
;;;-------------------------------------------------------------------------------

(defconcept (:en "symposium" :fr "symposium" :br "simpósio")
  (:is-a "meeting")
  )

;;;-------------------------------------------------------------------------------
;;;                             TEACHING ORGANIZATION
;;;-------------------------------------------------------------------------------

(defconcept (:en "teaching organization" :fr "organisme d'enseignement" :br "organização de ensino")
  (:is-a "organization")
  (:att (:en "level" :fr "niveau" :br "nível"))
  (:rel (:en "president" 
             :fr "président; directeur"
             :br "reitor; diretor") (:to "person"))
  (:doc :en "A TEACHING ORGANIZATION is an organization that provides teaching to ~
             persons called students. It may be an elementary school, a standard ~
             school, a college, or a university."
        :fr "Un organisme d'enseignement est une organisation qui offre des cours ~
             à des personnes appelées étudiants. Ce peut être une école maternelle, ~
             un collège, un lycée, ou une une université."
        :br "Uma ORGANIZAÇÃO DE ENSINO é uma organização que fornece cursos a pessoas ~
             chamadas estudantes. Ela pode ser uma escola maternla, um colégio, ~
             um liceu, uma faculdade, ou uma universidade.")
  )

;;;-------------------------------------------------------------------------------
;;;                                  TELEPHONE
;;;-------------------------------------------------------------------------------

(defconcept (:en "phone; telephone" :fr "téléphone" :br "telefone")
  (:doc :en "A TELEPHONE is a means to talk when away. One can have different types ~
             of telephone number: home phone, office phone, cell phone, etc."
        :fr "Un TELEPHONE est un moyen qui permet de communiquer à distance par la ~
             voix. il y a plusieurs types de téléphones : le téléphone du domicile, ~
             le téléphone professionnel, le portable, etc."
        :br " Um TELEFONE é um meio vocal de comunicação à distância. Há diversos ~
             tipos de telefones: telefone residencial, telefone profissional, celular, etc.")
  (:att (:en "number" :fr "numéro" :br "número"))
  )

;;;-------------------------------------------------------------------------------
;;;                                  TERRITORY
;;;-------------------------------------------------------------------------------

(defconcept (:en "territory" :fr "territoire" :br "território")
  (:att (:en "name" :fr "nom" :br "nome")(:entry))
  )

;;;-------------------------------------------------------------------------------
;;;                                  UNIVERSITY
;;;-------------------------------------------------------------------------------

(defconcept (:en "university" :fr "université" :br "universidade")
  (:is-a "teaching organization")
  (:doc :en "A UNIVERSITY is an organization involved in higher education."
        :fr "Une UNIVERSITE est un établissement d'enseignement supérieur."
        :br "Uma UNIVERSIDADE é uma organização de ensino superior.")
  )

;;;-------------------------------------------------------------------------------
;;;                                 WEB ADDRESS
;;;-------------------------------------------------------------------------------

(defconcept (:en "web address; web page; personal web page"
                 :fr "adresse web"
                 :br "endereço web; página web; página web pessoal; site pessoal")
  (:doc :en "A WEB ADDRESS is the URL of a web page containing personal data."
        :fr "Une ADRESSE WEB est l'URL d'une page contenant des informations ~
             personnelles."
        :br "Um ENDEREÇO WEB é a URL de uma página contendo informações pessoais.")
  (:att (:en "URL" :fr "URL" :br "URL")))

;;;-------------------------------------------------------------------------------
;;;                                 WORKSHOP
;;;-------------------------------------------------------------------------------

(defconcept (:en "workshop" :fr "séminaire" :br "workshop; seminário")
  (:is-a "meeting")
  )


;;;===============================================================================
;;;
;;;                            METHODS
;;;
;;;===============================================================================

;;;--------------------------------------------------------- =GET-ADDRESS (PERSON)
;;; NB Language dependent messages could be centralized as global variables, using
;;; the defvariable macro. They could also be defined as multilingual names, e.g.
;;; (:fr "= adresse privée" :en "= home address" :br "= endereço privado") et la
;;; bonne formulation extraite par 
;;;      (moss::%mln-get-canonical-name *private-header*)
;;; However, the defvariable mechanism is not currently implemented, and varaibles
;;; are not saved in persistent storage.

(definstmethod =get-address PERSON ()
  "Tries to obtain the address of a person returning a list of one string. ~
   will return the summary for the person, then the summary for the ~
   addresses, and if not there, the summary for the employer and the ~
   employer's address."
  (let ((professional-header (case *language*
                               (:fr "= adresse professionnelle")
                               (:en "= office address")
                               (:br "= endereço profissional")))
        (private-header      (case *language*
                               (:fr "= adresse privée")
                               (:en "= home address")
                               (:br "= endereço privado")))
        (sorry-message       (case *language*
                               (:fr "- *adresse inconnue*")
                               (:en "- *unknown address")
                               (:br "- *endereço non disponível*")))
        initial-result result)
    ;; first get person id
    (setq result (send *self* '=summary)
          initial-result result) ; save initial result for later comparison
    ;; then, if office address is there, send summary
    (if (send *self* '=has-value "office address")
      (setq result (append result 
	                   (cons professional-header
	                         (mapcar #'car (broadcast *answer* '=summary)))))
      ;; otherwise we try with employer's address. First get list of employers
      (if (send *self* '=get "employer")
        (when *answer*
          ;; if there are employer(s) try to get the address of the employer(s)
          (setq *answer*
                ;; make it a list of strings
                (mapcar #'car (broadcast *answer* '=get-address)))
          ;; if anything left, OK
          (if *answer*
            (setq result (append result 
                                 (cons professional-header *answer*)))))))
    ;; then try home address
    (if (send *self* '=get "home address")
      (setq result (append result 
                           (cons private-header
                                 (mapcar #'car (broadcast *answer* '=summary))))))
    ;; return list of single string
    (if (equal result initial-result)
      ;; we could not find any address
      (list (format nil "~{~A~^~%~} ~A" result sorry-message))
      (list (format nil "~{~A~^~%~}" result)))))

#|
;; office address
? (send '$e-person.1 '=get-address)
("de Azevedo: Hilton
= endereço profissional
CEFET, Avenida sete de Setembro
 Curitiba, Parana
Brasil")

;; office and home address
? (send '$e-person.2 '=get-address)
("Barthès: Jean-Paul
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

;; inheriting address from employer
? (send '$e-person.6 '=get-address)
("Bouabdallah: Madjid
= endereço profissional
Université de Technologie de Compiègne (UTC)
rue Roger Couttolenc
60200 Compiègne
França")

;; address unknown
? (send '$e-person.15 '=get-address)
("Manabe: Yusuke")
|#
;;;--------------------------------------------------- =GET-ADDRESS (ORGANIZATION)

(definstmethod =get-address ORGANIZATION ()
  "Tries to obtain the address of an organization returning a list of one ~
   string. Will return the summary for the organizatio, then the summary ~
   for the address. If not there return nil."
  (let (result)
    ;; first get name and acronym
    (setq result (send *self* '=summary))
    ;; get address
    (send *self* '=get "office address")
    (if *answer*
      (list
       (format nil  "~{~A~^~%~}" 
               (append result (mapcar #'car (broadcast *answer* '=summary))))))))

#|
? (send '$e-university.1 '=get-address)
("Universidade Pontifical do Parana (PUC)
rua Immaculata Conceiçaõ
 Curitiba, Parana
Brasil")
|#
;;;----------------------------------------------------------- =GET-PHONE (PERSON)

;;; we adopt the same strategy as for addresses

(definstmethod =get-phone PERSON ()
  "returns a list containing the identification of the person and phone ~
   numbers."
  (let ((professional-header (case *language*
                               (:fr "= téléphone professionnel")
                               (:en "= office phone")
                               (:br "= telefone profissional")))
        (private-header      (case *language*
                               (:fr "= téléphone privé")
                               (:en "= home phone")
                               (:br "= telefone privado")))
        (cell-header         (case *language*
                               (:fr "= portable")
                               (:en "= cell phone")
                               (:br "= telefone celular")))
        (sorry-message       (case *language*
                               (:fr "- *pas de téléphone connu*")
                               (:en "- *no phone available")
                               (:br "- *telefone non disponível*")))
        initial-result result)
    ;; first get person id
    (setq result (send *self* '=summary)
          initial-result result) ; save initial result for later comparison
    ;; then, if office phone is there, send summary
    (if (send *self* '=has-value "office phone")
      (setq result (append result 
	                   (cons professional-header
	                         (mapcar #'car (broadcast *answer* '=summary)))))
      ;; otherwise we try with employer's phone. First get list of employers
      (if (send *self* '=get "employer")
        (when *answer*
          ;; if there are employer(s) try to get the address of the employer(s)
          (setq *answer*
                ;; make it a list of strings
                (mapcar #'car (broadcast *answer* '=get-phone)))
          ;; if anything left, OK
          (if *answer*
            (setq result (append result 
                                 (cons professional-header *answer*)))))))
    ;; then try home phone
    (if (send *self* '=get "home phone")
      (setq result (append result 
	                   (cons private-header
                                 (mapcar #'car (broadcast *answer* '=summary))))))
    ;; then try cell phone
    (if (send *self* '=get "cell phone")
      (setq result (append result 
	                   (cons cell-header
                                 (mapcar #'car (broadcast *answer* '=summary))))))
    
    ;; return list of a single string
    (if (equal result initial-result)
      ;; we could not find any address
      (list (format nil "~{~A~^~%~} ~A" result sorry-message))
      (list (format nil "~{~A~^~%~}" result)))))

#|
;; multiple phones
? (send '$e-person.2 '=get-phone)
("Barthès: Jean-Paul
= telefone profissional
+33 (0)3 44 23 44 66
= telefone privado
+33 (0)3 44 23 31 37
= telefone celular
+33 (0)6 80 45 32 67")

;; inherited phone
? (send '$e-person.6 '=get-phone)
("Bouabdallah: Madjid
= telefone profissional
Université de Technologie de Compiègne (UTC)
+33 3 44 32 44 23")

;; no phone
(send '$e-person.10 '=get-phone)
("Dubuisson: Bernard - *telefone non disponível*")
|#
;;;----------------------------------------------------- =GET-PHONE (ORGANIZATION)

(definstmethod =get-phone ORGANIZATION ()
  "Tries to obtain the phone of an organization returning a list of one ~
   string. Will return the summary for the organizatio, then the summary ~
   for the phone. If not there return nil."
  (let (result)
    ;; first get name and acronym
    (setq result (send *self* '=summary))
    ;; get address
    (send *self* '=get "office phone")
    (if *answer*
      (list
       (format nil  "~{~A~^~%~}" 
               (append result (mapcar #'car (broadcast *answer* '=summary))))))))

;;;--------------------------------------------------- =IF-NEEDED (PERSON/ADDRESS)

;;; if no address is mentioned for a person, then we try the employer's address
(defownmethod =if-needed 
  ("office address" "MOSS property name" "MOSS relation" :class-ref "person")  
  (obj-id)
  (let ((employer-list (HAS-EMPLOYER obj-id)))
    (if employer-list 
      (reduce #'append
              (broadcast employer-list '=get "office address")))))

;;;------------------------------------------------------------ =SUMMARY (COUNTRY)

(definstmethod =summary COUNTRY ()
  (list (moss::%mln-get-canonical-name (car (has-name)))))

;;;------------------------------------------------------ =SUMMARY (EMAIL-ADDRESS)

(definstmethod =summary EMAIL-ADDRESS ()
  (list (format nil "~{~A~^, ~}" (HAS-E-ADDRESS))))

;;;------------------------------------------------------- =SUMMARY (HOME-ADDRESS)

(definstmethod =summary POSTAL-ADDRESS ()
  "used for displaying home addresses in the overview window."
  (list (moss::%pformat "~{~A~^~&~}~%~{~A~^ ~} ~{~A~^ ~}~%~{~A~^ ~}"
                        (cdr (send *self* '=make-print-alist))
                        "street" "zip" "town" "country"
                        )))

#|
EUZEBIO-ADDRESS(6): (send '$e-home-address.1 '=summary)
("14, Allée de la Tilloye
 60200 Compiègne
 França")
|#
;;;------------------------------------------------------- =SUMMARY (ORGANIZATION)

(definstmethod =summary ORGANIZATION ()
  "if organization has a name and acronym returns both, otherwise return ~
   return or acronym"
  (let ((name (HAS-NAME))
        (acronym (HAS-ACRONYM)))
    (if (and name acronym)
      (list (format nil "~{~A ~}(~{~A~^ ~})" name acronym))
      (list (format nil "~{~A~^ ~}" (or name acronym))))))

#|
? (send '$e-organization.1 '=summary)
("Délégation Générale pour l'Armement (DGA)")

? (send '$e-university.2 '=summary)
("Université de Technologie de Compiègne (UTC)")
|#
;;;------------------------------------------------------------- =SUMMARY (PERSON)

(definstmethod =summary PERSON ()
  (list (format nil "~{~A~^, ~}: ~{~A~^, ~}" (HAS-NAME) (HAS-FIRST-NAME))))

#|
? (send '$e-PERsON.10 '=summary)
("Dubuisson: Bernard")
|#
;;;-------------------------------------------------------------- =SUMMARY (PHONE)

(definstmethod =summary PHONE ()
  (list (format nil "~{~A~^, ~}" (HAS-NUMBER))))

;;;----------------------------------------------------- =SUMMARY (POSTAL-ADDRESS)

(definstmethod =summary POSTAL-ADDRESS ()
  (list (moss::%pformat "~{~A~^~&~}~%~{~A~} ~{~A~^ ~}~%~{~A~^ ~}"
                        (send *self* '=make-print-alist)
                        "street" "zip" "town" "country" 
                        )))

#|
? (send '$E-OFFICE-ADDRESS.3 '=summary)
(send '$E-OFFICE-ADDRESS.3 '=summary)
("rua Immaculata Conceiçaõ
 Curitiba, Parana
Brasil")
|#
;;;-------------------------------------------------------- =SUMMARY (WEB-ADDRESS)

(definstmethod =summary WEB-ADDRESS ()
  (list (format nil "~{~A~^, ~}" (send *self* '=get 'HAS-URL))))


;;;===============================================================================
;;;===============================================================================
;;;
;;;                                INDIVIDUALS
;;;
;;;===============================================================================
;;;===============================================================================


;;;=================================================================== TERRITORIES 

(defindividual "country"
  ("name" (:en "France" :fr "France" :br "França"))
  )

(defindividual "country"
  ("name" (:en "Brazil" :fr "Brésil" :br "Brasil"))
  )

(defindividual "country"
  ("name" (:en "Mexico" :fr "Mexique" :br "México"))
  )

(defindividual "country"
  ("name" (:en "Japan" :fr "Japon" :br "Japão"))
  )

;;;===================================================================== ADDRESSES 

(defindividual "home address"
  (:doc :en "The address of Jean-Paul an Dominique Barthès."
        :fr "L'adresse de Jean-Paul et Dominique Barthès."
        :br "O endereço de Jean-Paul e Dominique Barthès"
        )
  ("street" "14, Allée de la Tilloye")
  ("town" "Compiègne")
  ("country" "france")
  ("zip" "60200")
  (:var _jpb-addr))

(defindividual "postal address"
  (:doc :en "The address of PUC"
        :fr "L'adresse de la PUC"
        :br "O endereçco da PUC")
  ("street" "PUC, rua Immaculada Conceição")
  ("town" "Curitiba, Parana")
  ("country" "Brasil")
  (:var _puc-addr)
  )

(defindividual "postal address"
  (:doc :en "The address of CEFET"
        :fr "L'adresse du CEFET"
        :br "O endereço do CEFET")
  ("street" "CEFET, Avenida sete de Setembro")
  ("town" "Curitiba, Parana")
  ("country" "brasil")
  (:var _cefet-addr)
  )

(defindividual "postal address"
  (:doc :en "The address of Cinvestav"
        :fr "L'adresse de Cinvestav"
        :br "O endereço do CINVESTAV")
  ("street" "CINVESTAV")
  ("town" "Guadelajara, Jalisco")
  ("country" "mexico")
  (:var _cinvestav-addr)
  )

(defindividual "postal address"
  (:doc :en "The address of CIT"
        :fr "L'adresse de CIT"
        :br "O endereço do CIT")
  ("street" "CIT")
  ("town" "Chiba")
  ("country" "japan")
  (:var _cit-addr)
  )

(defindividual "postal address"
  (:doc :en "The address of TECPAR"
        :fr "L'adresse du TECPAR"
        :br "O endereço do TECPAR")
  ("street" "rua Prof. Algacyr Munhoz Mader, 3775")
  ("town" "Curitiba, Parana")
  ("country" "brasil")
  (:var _tecpar-addr)
  )

(defindividual "postal address"
  (:doc :en "The address of UTC"
        :fr "L'adresse de l'UTC"
        :br "O endereço da UTC")
  ("street" "rue Roger Couttolenc")
  ("town" "Compiègne")
  ("zip" "60200")
  ("country" "france")
  (:var _utc-addr)
  )

(defindividual "postal address"
  (:doc :en "The address of UTC Research Center"
        :fr "L'adresse du Centre de recherches de Royllieu"
        :br "O endereço do Centro de Pesquisa da UTC")
  ("street" "Centre de Recherche de Royallieu" "BP 20529")
  ("town" "Compiègne cedex")
  ("zip" "60205")
  ("country" "france")
  (:var _utc-crr-addr)
  )

;;;======================================================================== PEOPLE 

(defindividual "person"
  ("name" "de Azevedo")
  ("first name" "Hilton")
  ("office address" _cefet-addr)
  (:doc :en "Dr. Hilton deA zevedo is working at CEFET."
        :fr "Le Dr. Hilton de Azevedo est professeur au CEFET."
        :br "O Dr. Hilton de Azevedo é professor no CEFET.")
  (:var _eda)
  )

(defindividual "person"
  ("name" "Barthès")
  ("first name" "Jean-Paul")
  ("home address" _jpb-addr)
  ("office address"
   (:new "office address"
         ("street " "Université de Technologie de Compiègne"
          "Centre de Recherche de Royallieu"
          "Departement de Génie Informatique"
          "UMR CNRS 6599 HEUDIASYC")
         ("zip" "60206")
         ("city" "Compiègne cedex")
         ("country" "france")))
  ("email" (:new "email"
                 ("e-address" "barthes@utc.fr")))
  ("web page" (:new "web page"
                    ("URL" "http://www.utc.fr/~barthes")))
  ("home phone" 
   (:new  "home phone"
          ("number" "+33 (0)3 44 23 31 37")
          (:var _jpb-home-phone)))
  ("office phone"
   (:new "office phone"
         ("number" "+33 (0)3 44 23 44 66")))
  ("cell phone"
   (:new "cell phone"
         ("number" "+33 (0)6 80 45 32 67")))
  (:var _jpb)
  (:doc :en "Jean-Paul Barthès is a professor at University of ~
             Technology of Compiègne."
        :fr "Jean-Paul Barthès est professeur à l'UTC."
        :br "Jean-Paul Barthès é professor da UTC.")
  )

(defindividual "person"
  ("name" "Barthès" "Barthès-Biesel" )
  ("first name" "Dominique")
  ("home address" _jpb-addr)
  ("office address"
   (:new "office address"
         ("street" "Université de Technologie de Compiègne"
          "Centre de Recherche de Royallieu"
          "Département de Génie Biomédical")
         ("zip" "60206")
         ("city" "Compiègne cedex")))
  ("email" (:new "email"
                 ("e-address" "dbb@utc.fr")))
  ("home phone" _jpb-home-phone)
  ("office phone"
   (:new "office phone"
         ("number" "+33 (0)3 44 23 43 93")))
  ("cell phone"
   (:new "cell phone"
         ("number" "+33 (0)6 81 24 46 66")))
  (:var _dbb)
  (:doc :en "Dominique Barthès Biesel is a professor at University of ~
             Technology of Compiègne."
        :fr "Dominique Barthès-Biesel est professeur à l'UTC"
        :br "Dominique Barthès-Biesel é professora da UTC.")
  )

(defindividual "person"
  ("name" "Barthès")
  ("first name" "Camille")
  ("email" (:new "email"
                 ("e-address" "kangming421@126.com")))
  ("home phone" 
   (:new "home phone"
         ("number" "+86 571 8701 0782")))
  ("cell phone"
   (:new "cell phone"
         ("number" "+86 134 5678 3173")))
  (:var _cxb)
  (:doc :en "Camille Barthès is a Chinese coolie living currently at Qingdao."
        :fr "Camille Barthès est un coolie chinois qui habite Qingdao."
        :br "Camille Barthès é um coolie chines que mora em Qingdao.")
  )

(defindividual "person"
  ("name" "Bonnifait")
  ("first name" "Philippe")
  ("employer" "UTC")
  (:doc :fr "Philippe Bonnifait est professeur à l'UTC"
        :br "Philippe Bonnifait é professor da UTC.")
  )

(defindividual "person"
  ("name" "Bouabdallah")
  ("first name" "Madjid")
  ("employer" "UTC")
  (:doc :fr "Madjid Bouabdallah est professeur à l'UTC et chercheur au ~
             laboratoire HEUDIASYC."
        :br "Madjid Bouabdallah é professor da UTC e pesquisador do ~
             laboratório HEUDIASYC.")
  )

(defindividual "person"
  ("name" "Charara")
  ("first name" "Ali")
  ("employer" "UTC")
  (:doc :fr "Ali Charara est le directeur du laboratoire HEUDIASYC à l'UTC"
        :br "Ali Charara é o diretor do laboratório HEUDIASYC da UTC.")
  )

(defindividual "person"
  ("name" "Charreyron")
  ("first name" "Pierre Olivier")
  ("cell phone" (:new "cell phone" ("number" "+33 (0)3 44 23 44 23")))
  ("employer" "UTC")
  (:doc :en "Pierre-Olivier Charreyron is the president of UTC."
        :fr "Pierre-Olivier Charreyron est le président de l'UTC."
        :br "Pierre-Olivier Charreyron é o presidente da UTC.")
  )

(defindividual "student"
  ("name" "Chen")
  ("first name" "Kejia")
  (:var _kc)
  (:doc :en "Kejia Chen was a PhD student at UTC and is now teaching at Nanjing."
        :fr "Kejia Chen était doctorante à l'UTC et est MC à Nanjing."
        :br "Kejia Chen foi estudante de doutorado da UTC e agora é ~
             professora em Nanjing")
  ("office address" _utc-crr-addr)
  )

(defindividual "person"
  ("name" "Denoeux")
  ("first name" "Thierry")
  ("office address" _utc-crr-addr)
  (:doc :fr "Thierry Denoeux est professeur à l'UTC et directeur adjoint du ~
             laboratoire HEUDIASYC."
        :br "Thierry Denoeux é professor da UTC e diretor adjunto do ~
             laboratório HEUDIASYC.")
  )

(defindividual "person"
  ("name" "Dubuisson")
  ("first name" "Bernard")
  ("office address" _utc-crr-addr)
  (:doc :fr "Bernard Dubuisson est professeur à l'UTC et conseiller à la DGA."
        :br "Bernard Dubuisson é professor da UTC e conceslheiro na DGA.")
  )

(defindividual "person"
  ("name" "Enembreck")
  ("first name" "Fabricio")
  ("employer" "PUC")
  (:doc :en "Dr. Fabricio Enembreck is working at PUC."
        :fr "Le Dr. Fabricio Enembreck est professeur à la PUC."
        :br "Dr. Fabrício Enembreck é professor da PUCPR")
  )

(defindividual "person"
  ("name" "Fontaine")
  ("first name" "Dominique")
  (:doc :fr "Dominique Fontaine est maître de conférences at UTC.")
  ("office address" _utc-crr-addr)
  )

(defindividual "person"
  ("name" "Lourdeaux")
  ("first name" "Domitile")
  ("employer" "UTC")
  (:doc :fr "Domitile Lourdeaux est enseignant-chercheur à l'UTC")
  )

(defindividual "person"
  ("name" "Lozano")
  ("first name" "Rogelio")
  ("employer" "utc")
  (:doc :fr "Rogelio Lozano est chercheur dans le laboratoire HEUDIASYC et directeur ~ 
		 de l'UMI franco-mexicaine."
        :br "Rogelio Lozano é pesquisador no laboratório HEUDIASYC e diretor da ~
             UMI franco-mexicana." )
  )

(defindividual "person"
  ("nom" "Manabe")
  ("first name" "Yusuke")
  (:doc :en "Yusuke Manabe is an assistant professor at CIT."
        :br "Yusuke Manabe é professor assistente no CIT")
  ("cell phone" (:new "cell phone" ("number" "+81 12 34 56 78")))
  )

(defindividual "person"
  ("nom" "Mathieu")
  ("first name" "Philippe")
  (:doc :fr "Philippe Mathieu est professeur d'informatique à Lille 1."
        :br "Philippe Mathieu é professor de informática em Lille 1.")
  )

(defindividual "person"
  ("name" "Paraiso")
  ("first name" "Emerson")
  ("employer" "PUC")
  (:doc :en "Dr. Emerson Paraiso is working at PUC."
        :fr "Le docteur Emerson Paraiso est professeur à la PUC"
        :br "Dr. Emerson Paraiso é professor da PUCPR")
  )

(defindividual "person"
  ("name" "Real")
  ("trade" "taxi")
  ("cell phone" (:new "cell phone" ("number" "+33 (0)6 07 54 02 27")))
  (:doc :en "Real had a deal with UTC to drive persons at a decent cost ~
             but unfortunately died in 2008.")
  )

(defindividual "person"
  ("name" "Stephan")
  ("first name" "Ronan")
  ("cell phone" (:new "cell phone" ("number" "+33 (0)3 44 23 44 23")))
  (:doc :en "Ronan Stephan was the president of UTC."
        :fr "Ronan Stephan a été le président de l'UTC."
        :br "Ronan Stephan éra o presidente da UTC.")
  )

(defindividual "person"
  ("name" "Ramos")
  ("first name" "Milton")
  ("office address" _tecpar-addr)
  (:doc :en "Dr. Milton Ramos is working at TECPAR."
        :fr "Le Dr. Milton Ramos est responsable des activités IA au TECPAR."
        :br "Dr. Milton Ramos é o responsável pelas atividades em IA do TECPAR.")
  )

(defindividual "person"
  ("name" "Ramos")
  ("first name" "Feliz")
  ("office address" _cinvestav-addr)
  (:doc :en "Dr. Felix Ramos is working at CINVESTAV."
        :br "Dr. Felix Ramos é professor do CINVESTAV.")
  )

(defindividual "person"
  ("name" "Scalabrin")
  ("first name" "Edson")
  ("office address" _puc-addr)
  (:doc :en "Dr. Edson Scalabrin is working at PUC."
        :fr "Le Dr. Edson Scalabrin est professeur à la PUC."
        :br "Dr. Edson Scalabrin é professor da PUCPR.")
  )

(defindividual "person"
  ("name" "Sugawara")
  ("first name" "Kenji")
  ("office address" _cit-addr)
  (:doc :en "Prof. Sugawara is working at CIT in Japan."
        :fr "Le Prof. Sugawara est professeur au CIT."
        :br "Dr. Sugawara é professor do CIT.")
  )

(defindividual "person"
  ("name" "Tacla")
  ("first name" "Cesar")
  ("office address" _cefet-addr)
  (:doc :en "Dr. Cesar Tacla is working at CEFET."
        :br "Dr. Cesar Tacla é professor do CEFET.")
  )

;;;---------- students

(defindividual "student"
  ("name" "You")
  ("first name" "Wei")
  ("office phone" (:new "office phone"
                        ("number" "+33 (0)3 44 23 44 23 ext: 4202")))
  ("office address" _utc-crr-addr)
  ("school" "UTC")
  (:doc :fr "You Wei est doctorante à l'UTC."
        :br "You Wei esta estudante de doutorado na UTC.")
  )


;;;============================================================ PUBLISHED MATERIAL


;;;================================================================= ORGANIZATIONS 

(defindividual "organization"
  ("name" "Délégation Générale pour l'Armement")
  ("acronym" "DGA")
  ("division"
   (:new "organization"
         ("name" "direction des systèmes de forces et des stratégies industrielle, 
technologique et de coopération.")
         ("acronym" "D4S"))
   (:new "organization"
         ("acronym" "MRIS")
         ("name" "Mission pour la Recherche et l'Innovation Scientifique"))
   )
  (:doc :fr "Maître d'ouvrage des programmes d'armement, la DGA est responsable 
de la conception, de l'acquisition et de l'évaluation des systèmes qui équipent 
les forces armées. Son action couvre toute la durée de vie de ces programmes.")
  )

(defindividual "university"
  ("name" "Universidade Pontifical do Parana")
  ("acronym" "PUC")
  ("office address" 
   (:new "office address"
         (:doc "The address of PUC")
         ("street" "rua Immaculata Conceiçaõ")
         ("town" "Curitiba, Parana")
         ("country" "brasil")))
  )

(defindividual "organization"
  ("acronym" "TECPAR")
  ("office address" _tecpar-addr)
  )

(defindividual "university"
  ("name" "Université de Technologie de Compiègne")
  ("acronym" "UTC")
  ("office address" 
   (:new "office address"
         (:doc "The address of UTC")
         ("street" "rue Roger Couttolenc")
         ("town" "Compiègne")
         ("zip" "60200")
         ("country" "france")))
  ("president" "charreyron")
  ("office phone" 
   (:new "office phone"
         ("number" "+33 3 44 32 44 23")))
  )

(defindividual "organization"
  ("name" "Centre National pour la Recherche Scientifique")
  ("acronym" "CNRS")
  )

(defindividual "non profit organization"
  ("name" "Institut International pour l'Intelligence Artificielle")
  ("acronym" "IIIA")
  ("office address" 
   (:new "office address" 
         ("street" "Centre de Transfert" "66 avenue de Landshut")
         ("town" "Compiègne")("zip" "60200")))
  ("office phone" 
   (:new "cell phone"
         ("number" "+33 6 80 45 32 67")))
  ("president" ("person" ("name" :is "barthès")("first name" :is "jean-paul")))
  )

;;;====================================================================== MEETINGS 

(defindividual "manifestation"
  ("nom" "Journées Francophones pour les Systèmes Multi-Agents")
  ("sigle" "JFSMA")
  ("président du comité de programme" "mathieu")
  ("office address"
   (:new "adresse postale"
         ("rue" "Hotel Terminus")
         ("ville" "Carcassonne")))
  ("time" "octobre")
  (:doc :fr "Les Journées Francophones pour les Systèmes Multi-Agents sont une ~
             manifestation annuelle regroupant des chercheurs de langue française.")
  )

;;;=============================================================================== 

:EOF