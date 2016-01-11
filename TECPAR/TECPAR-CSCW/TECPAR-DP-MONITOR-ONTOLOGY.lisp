;;;===============================================================================
;;;25/06/2012
;;;             AGENT TECPAR-DP-MONITOR: ONTOLOGY (file TECPAR-DP-MONITOR-ONTOLOGY.lisp)
;;;             Copyright Ramos@TECPARC, 2010
;;;
;;;=============================================================================== 

#|
EUZEBIO is the Brazilian assistant agent of Allan Trevisan.
It can find email addressses, telephone numbers, or use web services (?)

Its ontology is used to process dialogs, answer requests or trigger actions, i.e.
send messages to other agents.
The current ontology contains information about addresses, publications and 
research specialties.
However, TECPAR-DP-MONITOR is not a specialist of those subjects and its ontology is rather
shallow. It contains enough vocabulary to ask a specialist (one of the staff agent).

Questions asked to TECPAR-DP-MONITOR:
   - what is the telephone number of "Trigano"?
   - phone "dbb"?
   - home phone of "Moulin"?
   - extension of DICIT?
   - send message to "gapenne".
   - ...
   
2009
 0720 adding the description of people/pa combinations in the TECPAR-DP-MONITOR package
2012
 2506 created by cloning EUZEBIO-ONTOLOGY 
|#

(in-package :TECPAR-DP-MONITOR)

(defontology 
  (:title "TECPAR-DP-MONITOR ontology")
  (:version "1.0")
  (:language :br)
  )

;;;-------------------------------------------------------------------------------
;;;                                 CELULAR
;;;-------------------------------------------------------------------------------

(defconcept "celular; telefone celular"
  (:doc "Um CELULAR é um telefone que se pode carregar consigo.")
  (:is-a "telefone")
  )

;;;-------------------------------------------------------------------------------
;;;                                 ENDEREÇO
;;;-------------------------------------------------------------------------------

(defconcept "Endereço"
  (:doc "Um endereço pode ser:
	- um endereço postal residencial
	- um endereço postal profissional
	- um endereço de email
	- um endereço Web, como o endereço para a página pessoal
Cada um deles pode ser ser miltiplo"))

;;;-------------------------------------------------------------------------------
;;;                                 EMPRESA
;;;-------------------------------------------------------------------------------

(defconcept "empresa; companhia; sociedade; firma"
  (:is-a "organização")
  (:doc "Uma EMPRESA, COMPANHIA, SOCIEDADE, OU FIRMA realiza geralmente atividades~
             lucrativas para seus proprietários")
  )

;;;-------------------------------------------------------------------------------
;;;                              ENDEREÇO DE CASA
;;;-------------------------------------------------------------------------------

(defconcept "residência; endereço privado; endereço de casa"
  (:is-a "endereço postal")
  (:doc "ENDEREÇO RESIDENCIAL é o edndereço postal de onde a pessoa mora")
  )

;;;-------------------------------------------------------------------------------
;;;                            ENDEREÇO ELETRÔNICO
;;;-------------------------------------------------------------------------------

(defconcept "endereço de email; correio eletrônico; endereço eletrônico"
  (:is-a "Endereço")
  (:doc "ENDEREÇO EMAIL é aquele onde se pode receber mensagens de correio eletrônico"
        )
  (:att "endereço eletrônico"))

;;;-------------------------------------------------------------------------------
;;;                               ENDEREÇO POSTAL
;;;-------------------------------------------------------------------------------

(defconcept "endereço postal"
  (:is-a "endereço")
  (:doc "ENDEREÇO POSTAL é o enderço onde o correio é distribuído")
  (:att "rua e número")
  (:att "cidade" (:unique))
  (:att "país" (:unique))
  (:att "código postal; CEP" (:unique))
  )

;;;-------------------------------------------------------------------------------
;;;                             ENDEREÇO PROFISSIONAL
;;;-------------------------------------------------------------------------------

(defconcept "endereço profissional; endereço de trabalho"
  (:is-a "endereço postal")
  (:doc "ENDEREÇO PROFISSIONAL é o edndereço postal de onde a pessoa trabalha")
  )

;;;-------------------------------------------------------------------------------
;;;                                ENDEREÇO WEB
;;;-------------------------------------------------------------------------------

(defconcept "endereço web"
  (:doc "ENDEREÇO WEB é o URL da página web que contém informações pessoais")
  (:att "URL")
  )


;;;-------------------------------------------------------------------------------
;;;                                 ESTUDANTE
;;;-------------------------------------------------------------------------------

(defconcept "estudante"
  (:is-a "pessoa")
  (:rel "escola" (:to "universidade"))
  )

;;;-------------------------------------------------------------------------------
;;;                           NON-PROFIT ORGANIZATION
;;;-------------------------------------------------------------------------------

(defconcept "organização sem fins lucrativos"
  (:is-a "organização")
  (:rel "direção" (:to "pessoa"))
  (:rel "comitê executivo" (:to "pessoa") (:min 2))
  (:doc "Uma ORGANIZAÇÃO SEM FINS LUCRATIVOS é uma organização que não pode ~
         obter lucros das suas atividades")
  )

;;;-------------------------------------------------------------------------------
;;;                                 ORGANIZAÇÃO
;;;-------------------------------------------------------------------------------

(defconcept "organização"
  (:att "nome" (:entry))
  (:att "sigla" (:entry))
  (:rel "endereço" (:to "endereço"))
  (:rel "telefone" (:to "telefone"))
  (:rel "página web" (:to "endereço web"))
  (:doc "Uma ORGANIZAÇÃO é uma estrutura social pública ou privada que ~
         tem um objetivo social")
  )

;;;-------------------------------------------------------------------------------
;;;                                 PESSOA
;;;-------------------------------------------------------------------------------

(defconcept "pessoa"
  (:att "sobrenome" (:entry))
  (:att "nome"(:entry))
  (:rel "endereço" (:to "endereço"))
  (:rel "email" (:to "endereço de email"))
  (:rel "página web" (:to "endereço web"))
  (:rel "marido" (:to "pessoa")(:unique))
  (:rel "esposa" (:to "pessoa")(:unique))
  (:rel "mãe" (:to "pessoa"))
  (:rel "assistente pessoal" (:to "omas agent"))
  )
  
;;;-------------------------------------------------------------------------------
;;;                            PROJETO
;;;-------------------------------------------------------------------------------

(defconcept "projeto"
  (:doc "Define um projeto em que os funcionarios do TECPAR irão estar engajados.")
  (:att "nome do projeto" (:entry)))

;;;-------------------------------------------------------------------------------
;;;                                  TELEFONE
;;;-------------------------------------------------------------------------------

(defconcept "telefone; fone"
  (:doc "Um TELEFONE é um meio de comunicação à distância. Pode haver diversos ~
         tipos de telefones: residencial, profissional, comercial, celular, etc.")
  (:att "número")
  )

;;;-------------------------------------------------------------------------------
;;;                             TELEFONE PROFISSIONAL
;;;-------------------------------------------------------------------------------

(defconcept "telefone profissional; telefone comercial"
  (:doc "O TELEFONE PROFISSIONAL OU COMERCIAL é aquela do lugar onde a pessoa trabalha.")
  (:is-a "telefone")
  )

;;;-------------------------------------------------------------------------------
;;;                            TELEFONE RESIDENCIAL
;;;-------------------------------------------------------------------------------

(defconcept "telefone residencial"
  (:doc "O TELEFONE RESIDENCIAL está instalado na residêncial do proprietário")
  (:is-a "telefone")
  )

;;;-------------------------------------------------------------------------------
;;;                                UNIVERSIDADE
;;;-------------------------------------------------------------------------------

(defconcept "universidade"
  (:is-a "organização")
  (:doc "Uma UNIVERSIDADE é uma organização envolvida em educação superior")
  )


;;;===============================================================================
;;;
;;;                            METHODS
;;;
;;;===============================================================================

;;;------------------------------------------------------------- =GET-SOBRENOME (PERSON)
(definstmethod =get-sobrenome PESSOA ()
	(send *self* '=get "sobrenome"))

;;;------------------------------------------------------------- =GET-NOME (PERSON)
(definstmethod =get-nome PESSOA ()
	(send *self* '=get "nome"))

;;;------------------------------------------------------------- =SUMMARY (PERSON)

(definstmethod =summary PESSOA ()
  (list (format nil "~{~A~^, ~}: ~{~A~^, ~}" (HAS-SOBRENOME) (HAS-NOME))))

;;;------------------------------------------------- =SUMMARY (PERSONAM ASSISTANT)

(definstmethod =summary OMAS-AGENT ()
  (list (format nil "~A ~{~A~^, ~}" "Agent" (HAS-AG-NAME))))

;;;-------------------------------------------------------------- =SUMMARY (PHONE)

(definstmethod =summary TELEFONE ()
  (list (format nil "~{~A~^, ~}" (HAS-NÚMERO))))

;;;------------------------------------------------------------- =GET-NOME (PROJETO)

(definstmethod =get-nome PROJETO ()
	(send *self* '=get "nome do projeto"))

;;;=============================================================================== 
;;;=============================================================================== 
;;;
;;;                                INDIVIDUALS
;;;
;;;=============================================================================== 
;;;=============================================================================== 

(defindividual "MOSS documentation"  ; class is defined in MOSS
  ("MOSS title" "global help")
  ("MOSS documentation" 
   (:en 
    "This is the global help... As your assistant I can do a certain number of ~
     things for you. They correspond to tasks I know of. The easiest way to ~
     use me is to ask me using simple sentences what you want to do.
     But remember, my vocabulary and understanding is quite limited."
    :fr 
    "Aide globale... En tant qu'assistant, je peux faire un certain nombre ~
     de choses pour vous. La meilleure façon de m'utiliser est de faire des ~
     phrases courtes. Toutefois, je vous rappelle que mon vocabulaire est  ~
     limité."
    :br
    "Ajuda global... Como seu assitente, eu posso lhe fazer um certo número ~
     de coisas. A melhor maneira de utilizar meus serviços é faser frases ~
     curtas. Entretanto, eu alerto que o meu vocabulário é muito curto."))
  )

;;; TECPAR-DP-MONITOR has been defined as an omas-agent when created
;;; ALBERT and ALFREDO have also been defined as OMAS agents but are not in TECPAR-DP-MONITOR 
;;; environment

(defindividual "omas agent"
  ("ag name" "stevens")
  ("ag key" :stevens)
  (:doc "STEVENS is a personal assistant speaking English.")
  )

(defindividual "omas agent"
  ("ag name" "albert")
  (:doc "ALBERT is a personal assistant speaking French.")
  ("ag key" :albert)
  ;(:language :fr)
  )

(defindividual "omas agent"
  ("ag name" "alfredo")
  ("ag key" :alfredo)
  (:doc "ALFREDO is a personal assistant speaking Spanih.")
  ;(:language :en)
  )

(defindividual "omas agent"
  ("ag name" "Gordon")
  (:doc "Gordon is a personal assistant speaking French.")
  ("ag key" :gordon)
  ;(:language :fr)
  )
  
(defindividual "omas agent"
  ("ag name" "Euzebio")
  ("ag key" :euzebio)
  ;(:language :br)
  )

(defindividual "pessoa"
  ("sobrenome" "Trevisan")
  ("nome" "Allan")
  ("assistente pessoal" ("omas agent" ("ag name" :is "EUZEBIO")))
  )

(defindividual "pessoa"
  ("sobrenome" "Ramos")
  ("nome" "Milton")
  ("assistente pessoal" ("omas agent" ("ag name" :is "GORDON")))
  )
  
(defindividual "projeto"
  ("nome do projeto" "TECPAR-CSCW")
  )
  

  
(defindividual "pessoa"
  ("sobrenome" "Barthès")
  ("nome" "Jean-Paul")
  ("assistente pessoal" ("omas agent" ("ag name" :is "ALBERT")))
  )

(defindividual "pessoa"
  ("sobrenome" "Gonzales")
  ("nome" "Omar")
  ("assistente pessoal" ("omas agent" ("ag name" :is "alfredo")))
  )


;;;=============================================================================== 

:EOF