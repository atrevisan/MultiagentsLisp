TEMPLATES FOR DEVELOPING AN OMAS APPLICATION (JP BARTHES, July 2010)
====================================================================

The files enclosed in this folder can be used to develop a new OMAS application. There are four types of agents:
	- standard service agents
	- personal assistants
	- Transfer agents also called postmen
	- logical agents
All agents have a small predefined internal ontology. However, it is possible to extend this ontology, except for logical agents (that are experimental).

Files are organized as follows:

Service agent
-------------
	- SAAA.lisp, main agent file SAAA is the name of the agent
	- SAAA-ONTOLOGY.lisp, optional ontology file
Personal assistant
------------------
	- PBBB.lisp, main agent file. PBBB is the name of the agent (the file is similar to SAAA.lisp)
	- PBBB-ONTOLOGY.lisp, file containing the ontology of the agent in the MOSS format
	- PBBB-TASKS.lisp, file containing the description of tasks the agent can do (required)
	- PBBB-DIALOG.lisp, file containing the high-level dialog loop (required)
Transfer agent or postman
-------------------------
 	- XCCC.lisp, main agent file. XCCC is the name of the agent
	- XCCC-ONTOLOGY.lisp, optional ontology file (rarely used)
Logical agent (experimental feature, not only really available)
-------------
	- IA-DDD.lisp, main agent file. IA-DDD is the name of the agent. The file contains only rules determining the agent behavior

Service files
-------------
	- agents.lisp, file used to load the agent files (required)
	- Z-messages.lisp, file used to predefine test messages (useful when building an application, avoids to have to redefine messages every time one reloads the application)


The list of agents are those that are going to run on your machine in the same Lisp environment, and does not include agents that will run on different machines, belonging to the same local coterie.

IMPORTANT
=========
All agent files are encoded using UTF-8 to allow foreign languages like Japanese or Chinese. The ACL editor does  not like UTF-8. It can read the file but introduces spurious characters, which leads to errors difficult to detect. I thus advise to use a different editor like emacs or notepad++, rather than the ACL editor.

29/06/2012 Allan Trevisan

APPLICATION LINK ->> https://www.dropbox.com/sh/2d3ltz2mrkdm4rd/NtTOyPg8kk

HOW TO USE THIS APLICATION
===========================
1. Download and install wampserver at ->> http://www.wampserver.com/en/
2. Clik wampserver icon and phpMyAdmin
3. in phpMyAdmin menu go to more -> import
4. load the dotproject.sql file in TECPAR-CSCW directory
5. Run OMAS with TECPAR-CSCW aplication
6. Use the folowing dialogs to interacr with the PA
		Quias são as atividades de Allan Trevisan?
		Quias são as atividades de Allan Trevisan no projeto TECPAR-CSCW?