<map version="0.9.0">
<!-- To view this file, download free mind mapping software FreeMind from http://freemind.sourceforge.net -->
<node BACKGROUND_COLOR="#33ccff" CREATED="1328007426687" ID="ID_1703045018" MODIFIED="1330437742534" TEXT="OMAS DOCUMENTATION">
<node COLOR="#ff3366" CREATED="1328008351491" FOLDED="true" HGAP="34" ID="ID_397240273" MODIFIED="1330437739742" POSITION="right" TEXT="Asistentes Pesoais" VSHIFT="-32">
<node COLOR="#6633ff" CREATED="1328008773523" FOLDED="true" ID="ID_1835588725" MODIFIED="1330434993461" TEXT="Criando um PA">
<node CREATED="1328008865727" ID="ID_1971484185" MODIFIED="1328009120135" TEXT="(defassistant :albert)"/>
</node>
<node COLOR="#6633ff" CREATED="1328009126969" FOLDED="true" ID="ID_631045581" MODIFIED="1330437735967" TEXT="Janela de Intera&#xe7;&#xe3;o com o PA">
<node CREATED="1328009328888" FOLDED="true" ID="ID_1074339541" MODIFIED="1330437723533" TEXT="Lado Direito">
<node CREATED="1328009346509" ID="ID_663131792" MODIFIED="1328009388053" TEXT="Comunica&#xe7;&#xe3;o entre Mestre  e PA"/>
</node>
<node CREATED="1328009394268" FOLDED="true" ID="ID_164475474" MODIFIED="1330437734313" TEXT="Lado Esquerdo">
<node CREATED="1328009428346" FOLDED="true" ID="ID_943420189" MODIFIED="1330437733517" TEXT="Comunica&#xe7;&#xe3;o com agentes exeternos">
<node CREATED="1328009780612" FOLDED="true" ID="ID_1645080888" MODIFIED="1328011894797" STYLE="bubble" TEXT="Answers/Info">
<node CREATED="1328009797345" ID="ID_1330604610" MODIFIED="1328009965460" TEXT="Contem respostas de requisi&#xe7;&#xf5;es feitas pelo PA em prol do Mestre"/>
</node>
<node CREATED="1328010082955" FOLDED="true" ID="ID_1844761930" MODIFIED="1328011896383" STYLE="bubble" TEXT="Tasks to do">
<node CREATED="1328010100773" ID="ID_735888783" MODIFIED="1328010190301" TEXT="Lista de requisi&#xe7;&#xf5;es que foram feitas ao mestre e que o PA n&#xe3;o pode processar sozinho "/>
</node>
<node CREATED="1328010202287" FOLDED="true" ID="ID_1385540063" MODIFIED="1328010621239" STYLE="bubble" TEXT="Pending Requests">
<node CREATED="1328010227477" ID="ID_844114637" MODIFIED="1328010580917" TEXT="Requisi&#xe7;&#xf5;es que foram ffeitas pelo mestre e que n&#xe3;o recedberam uma resposta ainda (tem um processo esperando pela resposta)."/>
</node>
<node CREATED="1328010370848" FOLDED="true" ID="ID_850114750" MODIFIED="1328010714158" STYLE="bubble" TEXT="Discarded Messasges">
<node CREATED="1328010381460" ID="ID_1172307772" MODIFIED="1328010704123" TEXT="Aonde o assistente coloca as menssagens irrelevantes (o mestre pode resgata-las do cesto de lixo)"/>
</node>
</node>
</node>
</node>
<node COLOR="#6633ff" CREATED="1328010870721" FOLDED="true" ID="ID_1679201540" MODIFIED="1330437719259" TEXT="Principio do dialogo com um PA">
<node COLOR="#00cccc" CREATED="1328010922578" ID="ID_781655975" MODIFIED="1328263916243" STYLE="bubble" TEXT="*The user tells something to her PA, like &quot;what are the current projects? &quot;;">
<icon BUILTIN="full-1"/>
</node>
<node COLOR="#00cccc" CREATED="1328010931423" ID="ID_1650360444" MODIFIED="1328263927228" STYLE="bubble" TEXT="* For each task in the library the PA checks the sentence for phrases speci&#xc;ed in the index pattern describing the task, and computes a score by using a MYCIN-like formula1;">
<icon BUILTIN="full-2"/>
</node>
<node COLOR="#00cccc" CREATED="1328010951559" ID="ID_51806298" MODIFIED="1328263939225" STYLE="bubble" TEXT="* Tasks are then ordered by decreasing scores;">
<icon BUILTIN="full-3"/>
</node>
<node COLOR="#00cccc" CREATED="1328010961670" ID="ID_692725210" MODIFIED="1328263952921" STYLE="bubble" TEXT="* The task with the higher score is selected as well as all tasks with a score above a speci&#xc;c threshold.">
<icon BUILTIN="full-4"/>
</node>
<node COLOR="#00cccc" CREATED="1328010976157" ID="ID_448424293" MODIFIED="1328263959863" STYLE="bubble" TEXT="* The task with the highest score is launched, i.e. its associated dialog is triggered.">
<icon BUILTIN="full-5"/>
</node>
</node>
<node COLOR="#00cccc" CREATED="1328011065625" FOLDED="true" ID="ID_341896357" MODIFIED="1330353480411" STYLE="bubble" TEXT="Tasks">
<node COLOR="#ff9933" CREATED="1328011094302" FOLDED="true" ID="ID_1082808479" MODIFIED="1330353479787" STYLE="bubble" TEXT="The library of tasks">
<node COLOR="#9966ff" CREATED="1328011354350" ID="ID_388515599" MODIFIED="1328263867540" TEXT="Define uma biblioteca de a&#xe7;e&#xf5;es possiveis definidas como individuos de task concept"/>
</node>
</node>
<node COLOR="#00cccc" CREATED="1328012832978" FOLDED="true" ID="ID_1079620636" MODIFIED="1330434985302" TEXT="Dialogos">
<node COLOR="#ff9966" CREATED="1328012850187" FOLDED="true" ID="ID_1731808180" MODIFIED="1330353507727" TEXT="The DIALOG &#xc;file is the heart of the interaction mechanism and is by far the most complex. Um determinado estado de um dialogo (n&#xf3; do grafo) pode ser expandido em sub-dialogos. The dialog has three parts:">
<node COLOR="#9966ff" CREATED="1328014138428" FOLDED="true" ID="ID_1973375518" MODIFIED="1328264268557" STYLE="bubble" TEXT="a top level conversation;">
<node CREATED="1328014503781" ID="ID_1553072414" MODIFIED="1328015512323" TEXT="&#xc9; usada para selecionar uma tarefa na lista de tarefas."/>
</node>
<node COLOR="#9966ff" CREATED="1328014143507" FOLDED="true" ID="ID_55399694" MODIFIED="1328264263861" STYLE="bubble" TEXT="a set of task-related sub-conversations (sub-dialogs);">
<node CREATED="1328180630666" ID="ID_94891821" MODIFIED="1328180669917" TEXT="Task dialogs may be simple or very complex, depending on the task to be executed. For each task to be executed by the agent, an associated dialog must be provided."/>
</node>
<node COLOR="#9966ff" CREATED="1328014162308" ID="ID_1067251615" MODIFIED="1328263867540" STYLE="bubble" TEXT="an escape set of patterns used in case of failure of the analysis of the input text."/>
</node>
</node>
<node COLOR="#00cccc" CREATED="1328182620858" FOLDED="true" ID="ID_1309869061" MODIFIED="1330437666890" TEXT="System Internals">
<node COLOR="#ff9933" CREATED="1328182643939" FOLDED="true" ID="ID_748737289" MODIFIED="1330437665735" TEXT="Debugging is di&#xe;cult and can be somewhat improved as explained in the next sections.">
<node COLOR="#9966ff" CREATED="1328182692041" FOLDED="true" ID="ID_45745868" MODIFIED="1330437664082" TEXT="Viewing the defstate Code">
<node CREATED="1328182720036" ID="ID_62256931" MODIFIED="1328182722908" TEXT="set the moss::*debug* global variable to T, which can be done by calling (d+);"/>
<node CREATED="1328182724583" ID="ID_54011210" MODIFIED="1328182740903" TEXT="execute the (defstate ...) expression."/>
</node>
<node COLOR="#9966ff" CREATED="1328182776929" FOLDED="true" ID="ID_699809459" MODIFIED="1330353316471" TEXT="The MOSS vformat Macro">
<node CREATED="1328182815656" ID="ID_1948624942" MODIFIED="1328182831570" TEXT="vformat is a macro de&#xc;ned in the :moss package. Thus, vformat prints to the *debug-io* channel whenever moss::*verbose* is not nil."/>
</node>
<node COLOR="#9966ff" CREATED="1328182929107" FOLDED="true" ID="ID_1138550869" MODIFIED="1330353267331" TEXT="Tracing the Dialog">
<node CREATED="1328182941969" ID="ID_1716397145" MODIFIED="1328182944779" TEXT="Tracing the dialog is obtained by setting the following global variables: (setq moss::*transition-verbose* t) or (v+) (setq moss::*traced-agent* albert::ALBERT)"/>
</node>
<node COLOR="#9966ff" CREATED="1328183490788" FOLDED="true" ID="ID_1338531887" MODIFIED="1330353322399" TEXT="More on the defstate Macro">
<node CREATED="1328183495601" ID="ID_1841706031" MODIFIED="1328183501000" TEXT="Ler o capitulo"/>
</node>
<node CREATED="1328265328754" ID="ID_1293537256" MODIFIED="1328265331937" TEXT="Syntax of the defstate Macro"/>
</node>
</node>
</node>
<node CREATED="1330358264896" ID="ID_376918214" MODIFIED="1330358333640" POSITION="left" TEXT=""/>
<node CREATED="1330358324138" FOLDED="true" ID="ID_154433787" MODIFIED="1330437637842" POSITION="right" TEXT="compara&#xe7;&#xe3;o jade OMAS ">
<node CREATED="1330358359058" FOLDED="true" ID="ID_437188589" MODIFIED="1330432659515" TEXT="Introduction">
<node CREATED="1330358373372" FOLDED="true" ID="ID_1066335416" MODIFIED="1330432549020" TEXT="Main Purpose of the Comparison">
<node CREATED="1330358455296" ID="ID_327927008" MODIFIED="1330358484454" TEXT="JADE relacionado a WEB, OMAS a pesquisa"/>
</node>
<node CREATED="1330358501386" FOLDED="true" ID="ID_232552436" MODIFIED="1330432552015" TEXT="Problem 0">
<node CREATED="1330358589082" ID="ID_389812049" MODIFIED="1330358596135" TEXT="FACTORIAL"/>
</node>
<node CREATED="1330358607794" FOLDED="true" ID="ID_1682072398" MODIFIED="1330432613245" TEXT="Global Remarks">
<node CREATED="1330358736439" FOLDED="true" ID="ID_1337957959" MODIFIED="1330432568473" TEXT="jade">
<node CREATED="1330358760684" ID="ID_1026199307" MODIFIED="1330358791822" TEXT="agentes s&#xe3;o instancias de classes"/>
<node CREATED="1330358828971" ID="ID_323645142" MODIFIED="1330358843731" TEXT="possuem objetivos (comportamento)"/>
<node CREATED="1330358853065" ID="ID_175701324" MODIFIED="1330358864783" TEXT="agemte &#xe9; encerrado ao atingir o objetivo"/>
</node>
<node CREATED="1330358753607" FOLDED="true" ID="ID_1153298727" MODIFIED="1330432611233" TEXT="OMAS">
<node CREATED="1330358892309" ID="ID_1932001958" MODIFIED="1330358906100" TEXT="agentes s&#xe3;o entidades separadas"/>
<node CREATED="1330358906355" ID="ID_1871987395" MODIFIED="1330358929281" TEXT="n&#xe3;o s&#xe3;o instancias de uma classe"/>
<node CREATED="1330358917252" FOLDED="true" ID="ID_600847884" MODIFIED="1330432582607" TEXT="agente permanece ativo ate que:">
<node CREATED="1330358954965" ID="ID_620380900" MODIFIED="1330358968055" TEXT="alguem mate ele"/>
<node CREATED="1330358968482" ID="ID_1386532162" MODIFIED="1330358973741" TEXT="comete suicidio"/>
</node>
<node CREATED="1330358993029" FOLDED="true" ID="ID_1256794745" MODIFIED="1330432602107" TEXT="possui skills">
<node CREATED="1330359011273" ID="ID_929296762" MODIFIED="1330359045111" TEXT="permite que servi&#xe7;os sejam realisados quando lhe pedem"/>
</node>
<node CREATED="1330359062932" FOLDED="true" ID="ID_1125533530" MODIFIED="1330432609392" TEXT="possui objetivos">
<node CREATED="1330359073501" ID="ID_1410581079" MODIFIED="1330359098744" TEXT="quando o objetivo &#xe9; atingido o agente o agente ainda permanece vivo"/>
</node>
</node>
</node>
<node CREATED="1330359236280" FOLDED="true" ID="ID_1121967412" MODIFIED="1330432653618" TEXT="Content of the Chapter">
<node CREATED="1330359254774" FOLDED="true" ID="ID_631077404" MODIFIED="1330432651918" TEXT="a compara&#xe7;&#xe3;o &#xe9; baseada nos seguintes estudos de problemas:">
<node CREATED="1330359293938" ID="ID_1380123380" MODIFIED="1330359329227" TEXT="fatorial simples"/>
<node CREATED="1330359295583" ID="ID_467104134" MODIFIED="1330359343142" TEXT="manipula&#xe7;&#xe3;o de menssagems"/>
<node CREATED="1330359296387" ID="ID_830453538" MODIFIED="1330359455478" TEXT=" registrar e descobrir servi&#xe7;os"/>
<node CREATED="1330359297284" ID="ID_1173671455" MODIFIED="1330359595473" TEXT="linguagemde conteudo"/>
<node CREATED="1330359298025" ID="ID_1975962897" MODIFIED="1330359610870" TEXT="objetivos de comportamentos"/>
<node CREATED="1330359298766" ID="ID_872522049" MODIFIED="1330359621899" TEXT="Contract-Net protocol"/>
<node CREATED="1330359299413" ID="ID_1817837922" MODIFIED="1330359635752" TEXT="manipula&#xe7;&#xe3;o de tempo"/>
<node CREATED="1330359635960" ID="ID_755989989" MODIFIED="1330359639987" TEXT="comcorrencia"/>
<node CREATED="1330359640212" ID="ID_1786221058" MODIFIED="1330359653614" TEXT="ontologies and how they are used in each platform"/>
<node CREATED="1330359672761" ID="ID_1513627" MODIFIED="1330359676226" TEXT="compares the platform execution environment and the debugging tools"/>
</node>
</node>
</node>
<node CREATED="1330359709413" FOLDED="true" ID="ID_533826668" MODIFIED="1330432725956" TEXT="Simple Approach to Problem 0">
<node CREATED="1330359725817" FOLDED="true" ID="ID_615556900" MODIFIED="1330432689623" TEXT="Overall Approach">
<node CREATED="1330361149093" FOLDED="true" ID="ID_1096894238" MODIFIED="1330432679405" TEXT="agente multiplicador">
<node CREATED="1330361174763" ID="ID_682770478" MODIFIED="1330361176902" TEXT="Nature of the Agent File"/>
<node CREATED="1330361177423" ID="ID_1675648213" MODIFIED="1330361184772" TEXT="Behaviours and skills"/>
<node CREATED="1330361191814" ID="ID_1597577315" MODIFIED="1330361193797" TEXT="Messages"/>
<node CREATED="1330361200355" ID="ID_1947270243" MODIFIED="1330361202447" TEXT="Message Content"/>
<node CREATED="1330361210362" ID="ID_1406702923" MODIFIED="1330361212018" TEXT="Processing the Message"/>
<node CREATED="1330361218404" ID="ID_262928249" MODIFIED="1330361220496" TEXT="Name Space"/>
</node>
<node CREATED="1330361876546" FOLDED="true" ID="ID_1756787262" MODIFIED="1330432687611" TEXT="agente fatorial">
<node CREATED="1330428435547" ID="ID_65196774" MODIFIED="1330428436828" TEXT="Initial Argument"/>
<node CREATED="1330428442715" ID="ID_923296556" MODIFIED="1330428444464" TEXT="Agent Life Cycle"/>
<node CREATED="1330428449711" ID="ID_677253091" MODIFIED="1330428451273" TEXT="Program values"/>
<node CREATED="1330428458003" ID="ID_1680862962" MODIFIED="1330428459814" TEXT="Processes"/>
</node>
</node>
<node CREATED="1330428576056" ID="ID_253911469" MODIFIED="1330428577883" TEXT="Launching the Platform"/>
<node CREATED="1330428718757" ID="ID_1359503525" MODIFIED="1330428720226" TEXT="Loading New Agents"/>
<node CREATED="1330428818933" FOLDED="true" ID="ID_1946682471" MODIFIED="1330432712508" TEXT="Executing Agents">
<node CREATED="1330428859189" FOLDED="true" ID="ID_865344661" MODIFIED="1330432710605" TEXT="JADE">
<node CREATED="1330428887097" ID="ID_1694198740" MODIFIED="1330428889283" TEXT="as soon as an agent is loaded, it executed its behaviours"/>
</node>
<node CREATED="1330428863050" FOLDED="true" ID="ID_1049089095" MODIFIED="1330432709763" TEXT="OMAS">
<node CREATED="1330428970440" ID="ID_716499418" MODIFIED="1330428985496" TEXT=" AGENTE PERMANECE INERTE ESPERANDO POR UMA MENSAGEM"/>
</node>
</node>
<node CREATED="1330429004612" FOLDED="true" ID="ID_1650302608" MODIFIED="1330432723444" TEXT="Debugging Agents">
<node CREATED="1330429179917" ID="ID_1574834597" MODIFIED="1330429210199" TEXT="mensagens JADE nao tem campos para especificar time-limits e time-outs"/>
</node>
</node>
<node CREATED="1330429254525" FOLDED="true" ID="ID_270394242" MODIFIED="1330432792037" TEXT="Handling Messages">
<node CREATED="1330429282066" FOLDED="true" ID="ID_755668269" MODIFIED="1330432790399" TEXT="Receiving Messages">
<node CREATED="1330429610143" ID="ID_1169153661" MODIFIED="1330429613202" TEXT="JADE"/>
<node CREATED="1330429613551" FOLDED="true" ID="ID_1753782214" MODIFIED="1330432769136" TEXT="OMAS">
<node CREATED="1330429654837" FOLDED="true" ID="ID_426829898" MODIFIED="1330432753739" TEXT="uma thread especifica processa a msg, podendo">
<node CREATED="1330429840672" FOLDED="true" ID="ID_1509953337" MODIFIED="1330432752288" TEXT="processa-la imediatamente">
<node CREATED="1330429875335" ID="ID_1565110718" MODIFIED="1330429878255" TEXT="abort"/>
<node CREATED="1330429878728" ID="ID_1010723082" MODIFIED="1330429881039" TEXT="cancel"/>
<node CREATED="1330429881279" ID="ID_1324510163" MODIFIED="1330429884978" TEXT="inform"/>
</node>
<node CREATED="1330429847903" FOLDED="true" ID="ID_878155814" MODIFIED="1330432751337" TEXT="escalona-la para ser processada mais tarde">
<node CREATED="1330429901770" ID="ID_677458115" MODIFIED="1330429904439" TEXT="request"/>
<node CREATED="1330429904929" ID="ID_306566056" MODIFIED="1330429908020" TEXT="answwer"/>
<node CREATED="1330429908431" ID="ID_707359574" MODIFIED="1330429914127" TEXT="bid messages"/>
</node>
</node>
<node CREATED="1330430015798" ID="ID_989803423" MODIFIED="1330430031353" TEXT="pode executar varias tarefas em paraelelo"/>
<node CREATED="1330430108985" FOLDED="true" ID="ID_1901128652" MODIFIED="1330432763723" TEXT="all agents receive all messages">
<node CREATED="1330430122736" ID="ID_412046699" MODIFIED="1330430124501" TEXT="useful for knowledge management applications"/>
</node>
</node>
</node>
<node CREATED="1330430149124" FOLDED="true" ID="ID_71422168" MODIFIED="1330432786936" TEXT="Recovering the Message Content (simple case)">
<node CREATED="1330430154014" ID="ID_1946817899" MODIFIED="1330430156871" TEXT="JADE"/>
<node CREATED="1330430157454" FOLDED="true" ID="ID_1917276137" MODIFIED="1330432786094" TEXT="OMAS">
<node CREATED="1330430296957" ID="ID_350723463" MODIFIED="1330430311389" TEXT="additional arguments of the skill functions"/>
</node>
</node>
</node>
<node CREATED="1330430350941" FOLDED="true" ID="ID_1018494944" MODIFIED="1330432867510" TEXT="Discovering Services">
<node CREATED="1330430423224" FOLDED="true" ID="ID_1288690633" MODIFIED="1330432807356" TEXT="When requiring an unknown service">
<node CREATED="1330430427896" FOLDED="true" ID="ID_1828055842" MODIFIED="1330432799666" TEXT="JADE">
<node CREATED="1330430436619" ID="ID_1458724972" MODIFIED="1330430446197" TEXT="Yelow pages"/>
</node>
<node CREATED="1330430431024" FOLDED="true" ID="ID_840848186" MODIFIED="1330432806202" TEXT="OMAS">
<node CREATED="1330430447373" ID="ID_406408331" MODIFIED="1330430459013" TEXT="broascast messages"/>
</node>
</node>
<node CREATED="1330430484134" ID="ID_968942189" MODIFIED="1330430486289" TEXT="Service Registration"/>
<node CREATED="1330430712417" FOLDED="true" ID="ID_763983940" MODIFIED="1330432830382" TEXT="broadcast">
<node CREATED="1330430948594" ID="ID_415922123" MODIFIED="1330430952106" TEXT="JADE"/>
<node CREATED="1330430952346" FOLDED="true" ID="ID_140463437" MODIFIED="1330432829181" TEXT="OMAS">
<node CREATED="1330430957892" ID="ID_1207283670" MODIFIED="1330430959750" TEXT="(send-subtask agent :to :ALL :action :multiply :args (list nn (decf nn)))"/>
<node CREATED="1330430986432" FOLDED="true" ID="ID_1474698436" MODIFIED="1330432827917" TEXT="processa respostas de tres maneiras possiveis">
<node CREATED="1330431018716" ID="ID_470848466" MODIFIED="1330431023944" TEXT=":take-&#xc;rst-answer"/>
<node CREATED="1330431024371" ID="ID_779845420" MODIFIED="1330431037306" TEXT=":take-&#xc;rst-n-answers"/>
<node CREATED="1330431048044" ID="ID_657938765" MODIFIED="1330431049918" TEXT=":collect-answers"/>
</node>
</node>
</node>
</node>
<node CREATED="1330431254191" FOLDED="true" ID="ID_1145152405" MODIFIED="1330432866340" TEXT="Content Language">
<node CREATED="1330431988837" FOLDED="true" ID="ID_549128711" MODIFIED="1330432838697" TEXT="JADE">
<node CREATED="1330432009678" ID="ID_693189688" MODIFIED="1330432019290" TEXT="ambiente java"/>
<node CREATED="1330432019592" ID="ID_91527314" MODIFIED="1330432027363" TEXT="varias linguagens"/>
<node CREATED="1330432036370" ID="ID_859342101" MODIFIED="1330432039929" TEXT="ontologias"/>
<node CREATED="1330432040215" ID="ID_624815979" MODIFIED="1330432040215" TEXT=""/>
</node>
<node CREATED="1330431992479" FOLDED="true" ID="ID_305847445" MODIFIED="1330432850491" TEXT="OMAS">
<node CREATED="1330432108645" FOLDED="true" ID="ID_1572075546" MODIFIED="1330432843424" TEXT="aceita todo oconteudo">
<node CREATED="1330432123241" ID="ID_344007546" MODIFIED="1330432128935" TEXT="como string"/>
<node CREATED="1330432129174" ID="ID_1442471746" MODIFIED="1330432132999" TEXT="como lista"/>
</node>
<node CREATED="1330432232314" ID="ID_1187321589" MODIFIED="1330432234126" TEXT="OMAS agents do not require a class/instance format"/>
</node>
</node>
<node CREATED="1330432313957" FOLDED="true" ID="ID_735354826" MODIFIED="1330434253194" TEXT="Goals and Skills vs. Behaviours">
<node CREATED="1330433426702" FOLDED="true" ID="ID_1268158096" MODIFIED="1330433633389" TEXT="JADE">
<node CREATED="1330433439806" FOLDED="true" ID="ID_1780699819" MODIFIED="1330433574156" TEXT="podde executar behaviours concorrentemente, por&#xe9;m">
<node CREATED="1330433486645" ID="ID_211897053" MODIFIED="1330433496257" TEXT="h&#xe1; apenas uma thread"/>
<node CREATED="1330433514031" FOLDED="true" ID="ID_216485900" MODIFIED="1330433572502" TEXT="the programmer has to handle all concurrency problems">
<node CREATED="1330433532119" ID="ID_210855728" MODIFIED="1330433534056" TEXT="blocking and unblocking processes"/>
<node CREATED="1330433541971" ID="ID_1909917552" MODIFIED="1330433544141" TEXT="answering system events"/>
</node>
</node>
</node>
<node CREATED="1330433428910" FOLDED="true" ID="ID_985975541" MODIFIED="1330434251759" TEXT="OMAS">
<node CREATED="1330433755902" ID="ID_1285976867" MODIFIED="1330433758540" TEXT="skills"/>
<node CREATED="1330433759264" FOLDED="true" ID="ID_1517962249" MODIFIED="1330434170841" TEXT="goals">
<node CREATED="1330433822171" FOLDED="true" ID="ID_1644781991" MODIFIED="1330434169749" TEXT="correspond to the proactive behavior of the agent">
<node CREATED="1330434099001" ID="ID_655812364" MODIFIED="1330434102154" TEXT="rigidos"/>
<node CREATED="1330434102394" FOLDED="true" ID="ID_113449146" MODIFIED="1330434163291" TEXT="flexiveis">
<node CREATED="1330434151199" ID="ID_967802556" MODIFIED="1330434161232" TEXT="baseado em um nivel de energia"/>
</node>
</node>
</node>
</node>
</node>
<node CREATED="1330434256023" FOLDED="true" ID="ID_782282482" MODIFIED="1330437036070" TEXT="Contract-Net">
<node CREATED="1330434479891" FOLDED="true" ID="ID_1757475375" MODIFIED="1330437035212" TEXT="duas abordagens">
<node CREATED="1330434488497" ID="ID_521140798" MODIFIED="1330434540674" TEXT="to take the first bid (proposal) that is received and grant the task to the corresponding agent">
<icon BUILTIN="full-1"/>
</node>
<node CREATED="1330434557809" ID="ID_1193757064" MODIFIED="1330434563224" TEXT="better, to wait for several bids (proposals) and select one or several o&#xb;ers that are considered interesting">
<icon BUILTIN="full-2"/>
</node>
</node>
</node>
<node CREATED="1330436676934" ID="ID_1733153262" MODIFIED="1330436679572" TEXT="Handling Time"/>
</node>
</node>
</map>
