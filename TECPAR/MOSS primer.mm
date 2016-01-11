<map version="0.9.0">
<!-- To view this file, download free mind mapping software FreeMind from http://freemind.sourceforge.net -->
<node CREATED="1330299969234" ID="ID_570038336" MODIFIED="1330299984093" TEXT="MOSS primer">
<node CREATED="1330300234859" FOLDED="true" ID="ID_1506582964" MODIFIED="1330346425734" POSITION="right" TEXT="Introduction">
<node CREATED="1330300246046" FOLDED="true" ID="ID_574653783" MODIFIED="1330344684843" TEXT="History">
<node CREATED="1330300308031" ID="ID_1978706125" MODIFIED="1330300311062" TEXT="Objects use the PDM4 format"/>
<node CREATED="1330300348171" ID="ID_716910286" MODIFIED="1330300350171" TEXT="a main goal of supporting multi-user interactive AI applications."/>
<node CREATED="1330300450078" ID="ID_104843191" MODIFIED="1330300451828" TEXT="Figure 1: Overall architecture of the MOSS system"/>
</node>
<node CREATED="1330300464187" FOLDED="true" ID="ID_1459641229" MODIFIED="1330344716948" TEXT="MOSS Today">
<node CREATED="1330300500703" FOLDED="true" ID="ID_670173542" MODIFIED="1330344697435" TEXT="pode ser usado sozinho">
<node CREATED="1330300543453" ID="ID_1168565918" MODIFIED="1330300545890" TEXT="for representing knowledge"/>
<node CREATED="1330300557750" ID="ID_1353276548" MODIFIED="1330300559984" TEXT="development of ontologies and associated knowledge bases"/>
</node>
<node CREATED="1330300570296" FOLDED="true" ID="ID_346072927" MODIFIED="1330344703628" TEXT="pode ser usado em conjunto">
<node CREATED="1330300579343" ID="ID_618846443" MODIFIED="1330300631468" TEXT="integrado na plataforma OMAS para descrever o conhecimento de cada agente"/>
</node>
<node CREATED="1330300660937" ID="ID_853456986" MODIFIED="1330300673984" TEXT="SOL usado para traduzir em OWL"/>
<node CREATED="1330300778281" ID="ID_1091830069" MODIFIED="1330300780093" TEXT="contains only the object manager OMS and the message passing system MPS"/>
</node>
<node CREATED="1330300800031" FOLDED="true" ID="ID_1619934417" MODIFIED="1330344722963" TEXT="Who should Read the Manual">
<node CREATED="1330300852531" ID="ID_1137200956" MODIFIED="1330300889968" TEXT="who would like to get familiar with MOSS by exercising"/>
</node>
</node>
<node CREATED="1330301203218" ID="ID_1077066584" MODIFIED="1330301205109" POSITION="right" TEXT="How to Get Started"/>
<node CREATED="1330301315625" FOLDED="true" ID="ID_159058927" MODIFIED="1330349535596" POSITION="right" TEXT="Working with MOSS">
<node CREATED="1330301402156" FOLDED="true" ID="ID_107325794" MODIFIED="1330345504988" TEXT="Creating a Concept">
<node CREATED="1330301418750" ID="ID_1590484181" MODIFIED="1330301420406" TEXT="(defconcept &quot;Person&quot;)"/>
<node CREATED="1330301544843" FOLDED="true" ID="ID_69220591" MODIFIED="1330345189900" TEXT="Internal and external names: object-ids and entry points">
<node CREATED="1330344849654" ID="ID_728675288" MODIFIED="1330344870047" TEXT="objetos n&#xe3;o precisam necessariamente ser conhecidos"/>
<node CREATED="1330345016734" FOLDED="true" ID="ID_1759995118" MODIFIED="1330345188011" TEXT="external names or entry points">
<node CREATED="1330345043034" ID="ID_463894723" MODIFIED="1330345055394" TEXT="s&#xe3;o indices para acessar um dado objeto diretamente"/>
</node>
</node>
<node CREATED="1330345181149" FOLDED="true" ID="ID_1758617738" MODIFIED="1330345390291" TEXT="Cases">
<node CREATED="1330345321362" ID="ID_1742064097" MODIFIED="1330345346365" TEXT="o nome do comceito ser&#xe1; imprimido como PERSON em todos os casos">
<node CREATED="1330345346370" ID="ID_1979721780" MODIFIED="1330345346370" TEXT="">
<node CREATED="1330345352885" ID="ID_875019046" MODIFIED="1330345352885" TEXT="(defconcept PERSON)"/>
<node CREATED="1330345360837" ID="ID_1165604486" MODIFIED="1330345362829" TEXT="(defconcept Person)"/>
<node CREATED="1330345370006" ID="ID_1395126810" MODIFIED="1330345371869" TEXT="(defconcept persSON)"/>
<node CREATED="1330345379147" ID="ID_1093228320" MODIFIED="1330345381012" TEXT="(defconcept PeRsOn)"/>
</node>
</node>
</node>
<node CREATED="1330345399936" FOLDED="true" ID="ID_377642436" MODIFIED="1330345426252" TEXT="Documentation">
<node CREATED="1330345422795" ID="ID_111396385" MODIFIED="1330345424621" TEXT="(defconcept &quot;Person&quot; (:doc &quot;This describes a PERSON, bla, bla,...&quot;))"/>
</node>
<node CREATED="1330345431903" FOLDED="true" ID="ID_1220724657" MODIFIED="1330345491107" TEXT="Redefining Concepts">
<node CREATED="1330345480763" ID="ID_355883948" MODIFIED="1330345489961" TEXT="redefinir um conceito causa um erro"/>
</node>
</node>
<node CREATED="1330301536125" FOLDED="true" ID="ID_1957671677" MODIFIED="1330346353572" TEXT="Creating Concept Properties">
<node CREATED="1330345555965" FOLDED="true" ID="ID_1798278824" MODIFIED="1330345964171" TEXT="Attributes">
<node CREATED="1330345789573" ID="ID_1622182566" MODIFIED="1330345791936" TEXT="(defconcept &quot;Person&quot; (:att &quot;name&quot; (:min 1)(:max 3)(:entry)) (:att &quot;first name&quot;))"/>
<node CREATED="1330345815135" ID="ID_781803467" MODIFIED="1330345817873" TEXT="(defattribute &quot;sex&quot;)"/>
<node CREATED="1330345827648" FOLDED="true" ID="ID_1186069302" MODIFIED="1330345962124" TEXT="(defattribute &quot;sex&quot; (:min 1) (:max 1) (:concept PERSON))">
<node CREATED="1330345957876" ID="ID_1011299299" MODIFIED="1330345960490" TEXT="The external name of the attribute in both cases is constructed by adding the prefix HAS: HAS-NAME."/>
</node>
</node>
<node CREATED="1330345986215" FOLDED="true" ID="ID_1751282769" MODIFIED="1330346231604" TEXT="Relations">
<node CREATED="1330346191899" ID="ID_1146387688" MODIFIED="1330346193959" TEXT="(defrelation &quot;brother&quot; PERSON PERSON)"/>
<node CREATED="1330346202392" ID="ID_756433727" MODIFIED="1330346204153" TEXT="(defrelation &quot;neighbor&quot; :any :any)"/>
<node CREATED="1330346221752" ID="ID_1484182204" MODIFIED="1330346223040" TEXT="(defconcept &quot;Course&quot; (:att &quot;code&quot; (:min 1)(:max 3)(:entry)) (:rel &quot;department&quot; DEPARTMENT))"/>
</node>
</node>
<node CREATED="1330346355121" FOLDED="true" ID="ID_130925715" MODIFIED="1330346518051" TEXT="Defining Sub-Concepts">
<node CREATED="1330346514418" ID="ID_979326564" MODIFIED="1330346516369" TEXT="(defconcept &quot;Student&quot; (:is-a &quot;Person&quot;) (:rel &quot;course&quot; &quot;course&quot;))"/>
</node>
<node CREATED="1330346530770" FOLDED="true" ID="ID_374622143" MODIFIED="1330347040972" TEXT="Creating Individual Concepts">
<node CREATED="1330346611412" ID="ID_1479877890" MODIFIED="1330346612968" TEXT="(defindividual &quot;person&quot;)"/>
<node CREATED="1330346800174" FOLDED="true" ID="ID_807540249" MODIFIED="1330346939699" TEXT="atribuindo um individuo a uma variavel">
<node CREATED="1330346831577" ID="ID_1609772322" MODIFIED="1330346833826" TEXT="(setq _p1 (defindividual PERSON))"/>
<node CREATED="1330346841146" ID="ID_736658145" MODIFIED="1330346843242" TEXT="(defindividual PERSON (:var _p1))"/>
</node>
<node CREATED="1330346899210" FOLDED="true" ID="ID_811290146" MODIFIED="1330346940812" TEXT="assign values to properties of individuals.">
<node CREATED="1330346935486" ID="ID_332109795" MODIFIED="1330346937971" TEXT="Note that the HAS-XXX symbols can be replaced with the string &quot;XXX&quot;"/>
</node>
</node>
<node CREATED="1330347042189" FOLDED="true" ID="ID_422500509" MODIFIED="1330347919955" TEXT="Printing Information">
<node CREATED="1330347567685" ID="ID_1262606663" MODIFIED="1330347569287" TEXT="(send &#x2019;$E-person &#x2019;=print-self)"/>
<node CREATED="1330347569796" ID="ID_287033531" MODIFIED="1330347579361" TEXT="(send _person &#x2019;=print-self)"/>
<node CREATED="1330347589144" ID="ID_593247172" MODIFIED="1330347612087" TEXT="o sinal &quot;=&quot; &#xe9; usado para distinguir nomes de metodos"/>
<node CREATED="1330347801651" ID="ID_1706382817" MODIFIED="1330347823467" TEXT="_person &#xe9; definido automaticamente pelo MOSS"/>
<node CREATED="1330347839690" ID="ID_363607022" MODIFIED="1330347847626" TEXT="(send PERSON &#x2019;=print-self) causa um erro"/>
<node CREATED="1330347875487" ID="ID_243020868" MODIFIED="1330347896517" TEXT="da pra imprimir o conteudo de inividuos, relacoes, etc..."/>
</node>
<node CREATED="1330347922046" FOLDED="true" ID="ID_1300620455" MODIFIED="1330349324300" TEXT="Object Behavior">
<node CREATED="1330348176687" FOLDED="true" ID="ID_769381530" MODIFIED="1330349322821" TEXT="MOSS uses three types of methods">
<node CREATED="1330348187639" FOLDED="true" ID="ID_46723852" MODIFIED="1330348547276" TEXT="General Behavior">
<node CREATED="1330348380210" ID="ID_93240212" MODIFIED="1330348451970" TEXT="para redefinir metodos usamos a macro definstmethod "/>
</node>
<node CREATED="1330348551474" FOLDED="true" ID="ID_459491658" MODIFIED="1330348761356" TEXT="Exceptions, Own Methods">
<node CREATED="1330348601991" ID="ID_1421721633" MODIFIED="1330348618978" TEXT="associamos a individuos especificos"/>
<node CREATED="1330348632256" ID="ID_231878046" MODIFIED="1330348633610" TEXT="defownmethod"/>
</node>
<node CREATED="1330348774260" FOLDED="true" ID="ID_778268574" MODIFIED="1330348839220" TEXT="Universal Methods">
<node CREATED="1330348805253" ID="ID_1093073460" MODIFIED="1330348837956" TEXT="s&#xe3;o metodos predefinidos que se aplicam a todos os objetos ex: =get e =print-self"/>
</node>
</node>
<node CREATED="1330348892368" FOLDED="true" ID="ID_1156482871" MODIFIED="1330349204452" TEXT="Inheritance Mechanism">
<node CREATED="1330348994033" FOLDED="true" ID="ID_1361321180" MODIFIED="1330349184124" TEXT="quando um objeto recebe uma mensagem ele procura o metodo na seguinte ordem:">
<node CREATED="1330349041015" ID="ID_1725840054" MODIFIED="1330349092746" TEXT="own-method">
<icon BUILTIN="full-1"/>
</node>
<node CREATED="1330349046667" ID="ID_162330093" MODIFIED="1330349109122" TEXT="instance method at the concept level">
<icon BUILTIN="full-2"/>
</node>
<node CREATED="1330349049489" ID="ID_429151319" MODIFIED="1330349123844" TEXT="tries to inherit some from super-concepts">
<icon BUILTIN="full-3"/>
</node>
<node CREATED="1330349053067" ID="ID_538737173" MODIFIED="1330349142411" TEXT="looks for a universal method">
<icon BUILTIN="full-4"/>
</node>
<node CREATED="1330349167441" ID="ID_1390303850" MODIFIED="1330349181355" TEXT="quits if none is found">
<icon BUILTIN="full-5"/>
</node>
</node>
</node>
</node>
<node CREATED="1330349223907" FOLDED="true" ID="ID_1697033309" MODIFIED="1330349516780" TEXT="Creating Orphans">
<node CREATED="1330349274434" ID="ID_1288802256" MODIFIED="1330349276570" TEXT="An orphan is an object that does not have a concept but that may have any predefined property"/>
<node CREATED="1330349444419" FOLDED="true" ID="ID_390972118" MODIFIED="1330349514404" TEXT="(defobject (:var _p4)(&quot;NAME&quot; &quot;George&quot;)(:is-a _p2))">
<node CREATED="1330349503966" ID="ID_336186884" MODIFIED="1330349505633" TEXT="p2 is thus a prototype for the object we just defined."/>
</node>
</node>
</node>
<node CREATED="1330349537166" FOLDED="true" ID="ID_948106296" MODIFIED="1330347917815" POSITION="right" TEXT="Programming">
<node CREATED="1330349548712" FOLDED="true" ID="ID_941855673" MODIFIED="1330346231406" TEXT="Message Passing">
<node CREATED="1330346196858" ID="ID_1891164232" MODIFIED="1330346209379" TEXT="a fun&#xe7;&#xe3;o send merece destaque"/>
<node CREATED="1330346215635" ID="ID_1430003227" MODIFIED="1330346229915" TEXT="pode-se fazer broadcast para um conjunto de objetos"/>
</node>
<node CREATED="1330346249946" FOLDED="true" ID="ID_1082831268" MODIFIED="1330346396614" TEXT="Service Functions">
<node CREATED="1330346331903" ID="ID_864428763" MODIFIED="1330346358163" TEXT="O moss disponibilisa fun&#xe7;&#xf5;es utilitaria"/>
<node CREATED="1330346360804" ID="ID_772842753" MODIFIED="1330346380003" TEXT="Some of them are very general and should actually be LISP primitives"/>
<node CREATED="1330346381219" ID="ID_70908068" MODIFIED="1330346395052" TEXT="others are related to the structure of the objects (PDM)"/>
</node>
<node CREATED="1330346419988" FOLDED="true" ID="ID_674773" MODIFIED="1330346853943" TEXT="Predefined Methods">
<node CREATED="1330346460676" FOLDED="true" ID="ID_1784745606" MODIFIED="1330346852534" TEXT="Two kinds are of interest here:">
<node CREATED="1330346495958" FOLDED="true" ID="ID_1620171457" MODIFIED="1330346533079" TEXT="basic methods">
<node CREATED="1330346502807" ID="ID_816360476" MODIFIED="1330346510938" TEXT="nao devem ser usados diretamente"/>
<node CREATED="1330346529367" ID="ID_838671456" MODIFIED="1330346531563" TEXT="they can destroy the overall PDM structure (logical consistency)"/>
</node>
<node CREATED="1330346549231" FOLDED="true" ID="ID_1315890273" MODIFIED="1330346848495" TEXT="PDM or kernel methods">
<node CREATED="1330346681057" ID="ID_1853870352" MODIFIED="1330346682987" TEXT="should be used whenever possi- ble. They respect the PDM structure (logical integrity constraints)"/>
<node CREATED="1330346714269" FOLDED="true" ID="ID_37340032" MODIFIED="1330346845887" TEXT="Among the universal methods, some are of interest:">
<node CREATED="1330346725132" ID="ID_391579309" MODIFIED="1330346726809" TEXT="=get-properties"/>
<node CREATED="1330346734683" ID="ID_898887387" MODIFIED="1330346736953" TEXT="=print-methods"/>
<node CREATED="1330346790707" ID="ID_1255211293" MODIFIED="1330346792250" TEXT="=what?"/>
</node>
</node>
</node>
</node>
<node CREATED="1330346855084" FOLDED="true" ID="ID_1452866283" MODIFIED="1330347226447" TEXT="Writing a Method">
<node CREATED="1330346863169" FOLDED="true" ID="ID_916823895" MODIFIED="1330347173087" TEXT="Global Variables">
<node CREATED="1330347131812" ID="ID_137373288" MODIFIED="1330347133571" TEXT="*self*"/>
<node CREATED="1330347140382" ID="ID_79790852" MODIFIED="1330347142691" TEXT="*args*"/>
<node CREATED="1330347149882" ID="ID_1481266917" MODIFIED="1330347151723" TEXT="*sender*"/>
</node>
<node CREATED="1330347191585" FOLDED="true" ID="ID_423076939" MODIFIED="1330347203150" TEXT="Redefining a method">
<node CREATED="1330347200532" ID="ID_813942386" MODIFIED="1330347201667" TEXT="A method is redefined by re-executing the definstmethod or defownmethodmacro. This automatically cleans the environment."/>
</node>
</node>
<node CREATED="1330347222394" FOLDED="true" ID="ID_1252276366" MODIFIED="1330347916599" TEXT="Debugging Helps">
<node CREATED="1330347442695" ID="ID_35319239" MODIFIED="1330347444635" TEXT="(trace-message)"/>
<node CREATED="1330347453110" ID="ID_392770170" MODIFIED="1330347454667" TEXT="(trace-object _ob1)"/>
<node CREATED="1330347480377" ID="ID_1005315729" MODIFIED="1330347481923" TEXT="(trace-method &#x2019;=get-id)"/>
</node>
<node CREATED="1330347905843" ID="ID_1214155265" MODIFIED="1330347908948" TEXT="Defining Test Files"/>
</node>
<node CREATED="1330347918908" ID="ID_1711665172" MODIFIED="1330347922016" POSITION="right" TEXT="Advanced Features"/>
</node>
</map>
