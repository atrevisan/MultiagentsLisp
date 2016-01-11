;;;-*- Mode: Lisp; Package: "EUZEBIO" -*-
;;;===============================================================================
;;;10/10/18
;;;             AGENT EUZEBIO: TASKS (file EUZEBIO-tasks.lisp)
;;;             Copyright Barthès@UTC, 2009-2010, Ramos@TECPAR, 2010
;;;
;;;=============================================================================== 

#|
EUZEBIO is the Brazilian assistant agent of Allan Trevisan. EUZEBIO speaks only Portuguese.
It can find email addressses, telephone numbers, or use web services (?)

Its ontology is used to process dialogs, answer requests or trigger actions, i.e.
send messages to other agents.
The current file is its task library. It contains a model of all tasks the agent
can execute, related to questions asked to EUZEBIO:
   - what is the telephone number of "Trigano"?
   - phone "dbb"?
   - home phone of "Moulin"?
   - extension of DICIT?
   - send message to "gapenne".
   - ...

A task can be coupled with a dialog used to fill the task pattern (T-CONTEXT). The 
mechanism is similar to the ACTION mechanism of the MOSS dialogs.

2009
 0730 creation by cloning Albert
2010
 1007 upgrade file with deftask
2012
 1906 added my own tasks	
|#

(in-package :EUZEBIO)

;;;=============================================================================== 
;;;   
;;;                               TASK MODEL
;;;
;;;===============================================================================

(defconcept (:en "task index")
  (:doc :en "A TASK INDEX is a property of a task. The index attribute specifies ~
             the term being used, the weight attribute gives its importance for the ~
             task.")
  (:att (:en "index"))
  (:att (:en "weight"))
  )

(definstmethod =summary TASK-INDEX ()
  (list (has-index)(has-weight)))

(defconcept (:en "task")
  (:is-a moss::$QTSKE)
  (:doc :en "A TASK is a model of task to be performed by the assistant.")
  (:att (:en "task name") (:unique)(:entry))
  (:att (:en "performative") (:one-of :request :assert :command))
  (:att (:en "dialog"))
  (:rel (:en "index pattern")(:to "task index"))
  )

(definstmethod =summary TASK ()
  (HAS-TASK-NAME))
#|
;;; we define a macro for simplifying the writing of tasks


(defMacro deftask (name &key target (performative :request) dialog indexes doc
                        where-to-ask action)
  (let (res)
    `(defindividual "TASK"
       (:doc ,@(if doc (list doc) '("*no documentation*")))
       ,@ (if target `(("target" ,target)))
          ("task name" ,name)
          ("performative" ,performative)
          ("dialog" ,@(if dialog (list dialog) (error "missing dialog for ~S" name)))
          ("index pattern"
           ,@(loop
               (unless indexes (return (reverse res)))
               (unless (cdr indexes)
                 (error "arg to :indexes should be an even list in ~S" name))
               (push `(:new "task index" ("index" ,(pop indexes))
                            ("weight" ,(pop indexes)))
                     res)))
          ,@ (if where-to-ask `(("where to ask" ,where-to-ask)))
             ,@ (if action `(("message action" ,action)))
                )))
|#

;;;=============================================================================== 
;;;   
;;;                               TASK LIBRARY
;;;
;;;=============================================================================== 

;;; we could use weights for indexes...

;;;=========================================================================== ASK

(deftask "ask agent"
  :doc "Tarefa (requisição) para pedir uma informação a um outro agente."
  :performative :command
  :dialog _ask-conversation
  :indexes ("sollicitar" .8 "pedir" .8 "enviar mensagem a" .8 "mensagem" .4)
  )

;;;======================================================================= EXPLAIN 

(deftask "define concept"
  :doc "Tarefa (comando) dando a definição de um conceito da ontologia."
  :performative :command
  :dialog _explain-conversation
  :indexes ("definir" .6)
  )

;;;=============================================================== GETTING ADDRESS 

(deftask "get address"
  :doc "Tarefa para obter o endereço de alguém"
  :dialog _get-address-conversation
  :indexes ("endereço" .5 "endereço de" .7 "se encontra" .6 "acontece" .6
            "tem lugar" .6 "têm lugar" .6 "acontecem" .6 "aonde" .6
            "onde" .2)
  )

#|
;;;===================================================== GETTING BIBLIO REFERENCES

(deftask
  :doc "Tarefa (comando) para obter referências bibliográficas."
  :dialog _get-biblio-conversation
  :indexes ("artigo" .5 "artigos" .5 "paper" .6 "papers" .6
            "proceedings" .7 "anais" .7 "publicação" .8 "publicações" .8
			"publicado" .6 "publicados" .6 "memorando" .6 "memorandos" .6
			"manual" .6 "manuais" .6 "relatório interno" 
			"relatórios internos" .6 "bibliografia" .6 "referências" .4
			"conferências" .3)
  )
|#
;;;========================================================== GETTING HOME ADDRESS 
;;; must be updated for MCL

(deftask "get home address"
  :doc "Tarefa para obter o endereço pessoal de alguém."
  :dialog _get-home-address-conversation
  :indexes ("residência" .5 "endereço privado" .7 "domicílio" .4
            "habita" .7 "vive" .7 "endereço pessoal" .7 "aonde" .2
            "onde" .2)
  )


;;;================================================ GETTING DEFINITION INFORMATION

;;;======================================================= GETTING NEWS BY SECTION

(deftask "get news by section"
  :doc "Tarefa permitindo obter notícias de uma página específica."
  :dialog _get-news-by-section-conversation
  :indexes ("notícias" .5 "notícia" .5 "últimas" .4 "última" .25
            "fato" .2 "fatos" .2 "evento" .2 "eventos" .2 "novo" .3
			"nova" .25 "novos" .25 "de novo" .5 "pagina" .5
			"seção" .5 "em relação" .5 "relacionado" .5 "relacionados" .5
			"sobre" .3)
  )

;;;================================================= GETTING TELEPHONE INFORMATION 

(deftask "get phone number"
  :doc "Tarefa para obter um número de telefone."
  :dialog _get-phone-conversation
  :indexes ("número de telefone" .7 "número" .4 "telefone" .5 "de" .2
            "fone" .5 "celular" .7 "telefone celular" .7 
            "número de celular" .7)
  )

;;;========================================================================== HELP 

(deftask "help"
  :doc "Tarefa (commando) permitindo imprimir uma informação geral."
  :performative :command
  :dialog _print-help-conversation
  :indexes ("socorro" .5 "SOS" .6 "ajuda" .7 "perdido" .4 "perdida" .4)
  )

(deftask "help"
  :doc "Tarefa (pergunta) permitindo imprimir uma informação geral."
  :performative :request
  :dialog _print-help-conversation
  :indexes ("o que você pode fazer" .4 "que serviços" .4)
  )

#|
;;;=============================================================== LOAD REFERENCES

(deftask "import references"
  :doc "Tarefa permitindo importar um arquivo de referência no formato Bibtex."
  :performative :command
  :dialog _load-references-conversation
  :indexes ("caregar" .3 "carga" .3 "importar" .4 "importa" .4 "abrir" .3
            "arquivo" .3 "biblio" .4 "bibliografia" .4 "referências" .4
			"formato" .1 "bibtex" .4)
  )

;;;====================================================== MAKING THEMATIC WEB PAGE

(deftask "make thematic web page"
  :doc "Tarefa permitindo criar uma página Web para um conjunto de assuntos."
  :performative :command
  :dialog _make-thematic-web-page-conversation
  :indexes ("construir" .1 "criar" .1 "fazer" .1 "web" .6 "página" .2 "temática" .7
            "assuntos" .6 "assunto" .6 "domínios" .6 "tema" .6 "temas" .6)
  )

;;;======================================================== MAKING YEARLY WEB PAGE

(deftask "make yearly web page"
  :doc "Tarefa permitindo criar uma página Web para um determinado ano."
  :performative :command
  :dialog _make-yearly-web-page-conversation
  :indexes ("construi" .1 "criar" .1 "fazer" .1 "web" .6 "página" .2 "ano" .3)
  )
|#  
;;;======================================================== OBTAINING RECENT NEWS

(deftask "show last news"
  :doc "Tarefa permitindo mostrar as notícias mais recentes."
  :dialog _get-last-news-conversation
  :indexes ("notícias" .51 "notícia" .51 "últimas" .41 "última" .251 "fato" .21
            "fatos" .21 "evento" .21 "eventos" .21 "novo" .31 "novos" .251
			"nova" .251 "novas" .251 "de novo" .51)
  )

;;;=================================================================== PROCESS ASK 
;;; Generic task to process ASK messages

(deftask "process ask"
  :doc "Tarefa genérica para tratar de uma mensagem do tipo ASK."
  :performative :command
  :dialog _process-ask-conversation
  :indexes ("process-ask" 1)
  )

;;;================================================================== PROCESS TELL 
;;; Generic task to process TELL messages
;;;   This task displays the content of the message and either keeps it or deletes
;;; it.

(deftask "process tell"
  :doc "Tarefa genérica para tratar de uma mensagem do tipo TELL."
  :performative :command
  :dialog _process-tell-conversation
  :indexes ("process-tell" 1)
  )

;;;================================================================== PUBLISH NEWS

(deftask "publish news"
  :doc "Tarefa permitindo publicar uma notícia"
  :performative :command
  :dialog _publish-news-conversation
  :indexes ("publicar" .6 "compartilhar" .3 "externar" .3 "noticia" .5
            "aconteceu" .3 "fato" .3 "evento" .3 "coisa" .2)
  )

;;;===================================================================== SET FONTS 

(deftask "font handler"
  :doc "Tarefa permitinod motdificar o tamanho dos caracteres na tela."
  :performative :command
  :dialog _set-font-size-conversation
  :indexes ("fontes" .5 "fonte" .5 "texto" .3 "saída" .3 "ler" .3
            "letras" .3 "escrever" .3)
  )

;;;========================================================================== TELL

(deftask "tell agent"
  :doc "Tarefa (informe) para enviar uma informação para um outro agente."
  :performative :command
  :dialog _tell-conversation
  :indexes ("informar" .8 "dizer a" .7 "informação para" .7 "prevenir" .7)
  )

;;;========================================================================= TRACE 

(deftask "set trace"
  :doc "Tarefa permitindo ativar ou desligar o rastreamento de diálogos."
  :performative :command
  :dialog _trace-conversation
  :indexes ("transições" .5 "mostrar" .3 "rastrear" .7 "tagarelar" .5
            "eliminar" .3)
  )

;;;======================================================================= WHAT-IS 

(deftask "what is"
  :doc "Tarefa permitindo obter informações sobre uma expressão."
  :dialog _what-is-conversation
  :indexes ("que" .25 "é" .2 "o que é" .7 "é o que" .7)
  )

	

(deftask "who is"
  :doc "Tarefa permitindo obter informações de alguém."
  :dialog _get-person-info-conversation
  :indexes ("quem" .5 "é" .2 "quem é" .7 "que" .3)
  )
  
;;; Allan Trevisan defined tasks below
;;;===================================== WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW 

(deftask "what activities should a project member to do now"
  :doc "Tarefa permitindo obter informações relativas às atividades que alguém deve realizar no momento."
  :dialog _get-what-activities-should-a-project-member-to-do-now-conversation
  :indexes ("atividades" .5 "de" .2 "atividades de" .7)
  )
  
;;;============================ WHAT-ACTIVITIES-SHOULD-A-PROJECT-MEMBER-TO-DO-NOW-IN-A-SPECIFIC-PROJECT 

(deftask "what activities should a project member to do now in a specific project"
  :doc "Tarefa permitindo obter informações relativas às atividades que alguém deve realizar no momento em um determinado projeto."
  :dialog _get-what-activities-should-a-project-member-to-do-now-in-a-specific-project-conversation
  :indexes ("atividades" .5 "de" .2 "atividades de" .7  "no projeto" .8 "projeto" .5 "no" .3)
  )
;;;=============================================================================== 

:EOF