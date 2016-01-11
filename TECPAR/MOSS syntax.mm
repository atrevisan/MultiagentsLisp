<map version="0.9.0">
<!-- To view this file, download free mind mapping software FreeMind from http://freemind.sourceforge.net -->
<node CREATED="1330349511156" ID="ID_327747968" MODIFIED="1330349553432" TEXT="MOSS syntax">
<node CREATED="1330349640794" FOLDED="true" ID="ID_1036325552" MODIFIED="1330444745547" POSITION="right" TEXT="Introduction">
<node CREATED="1330349649724" ID="ID_196523673" MODIFIED="1330349658057" TEXT="For Beginners"/>
<node CREATED="1330349746608" FOLDED="true" ID="ID_335132380" MODIFIED="1330443398801" TEXT="Global Information">
<node CREATED="1330349773776" FOLDED="true" ID="ID_124469430" MODIFIED="1330443398052" TEXT="capturando erros">
<node CREATED="1330349788479" ID="ID_684646138" MODIFIED="1330349791772" TEXT="(catch :error &lt;macro execution&gt;)"/>
</node>
</node>
<node CREATED="1330349831090" FOLDED="true" ID="ID_297281155" MODIFIED="1330443433667" TEXT="Names, Multilingual Names, References, Internal Names, Identifiers">
<node CREATED="1330349901532" FOLDED="true" ID="ID_1440983555" MODIFIED="1330443429237" TEXT="Name">
<node CREATED="1330349926110" ID="ID_1884003217" MODIFIED="1330349940199" TEXT="usar referencias no formato de string em vez de simbolos"/>
<node CREATED="1330349963215" ID="ID_474433575" MODIFIED="1330349965011" TEXT="A name corresponds to a Lisp symbol, e.g. PERSON, AGE."/>
</node>
<node CREATED="1330350028040" FOLDED="true" ID="ID_1672204393" MODIFIED="1330350257183" TEXT="Strings">
<node CREATED="1330350061074" ID="ID_1317215564" MODIFIED="1330350095006" TEXT="deve ser usado quando o nome (conceito ou propriedade) tiver muitas palavras"/>
<node CREATED="1330350180765" ID="ID_1413128570" MODIFIED="1330350196414" TEXT="mantem o case na hora de imprimir "/>
<node CREATED="1330350242439" ID="ID_1624181654" MODIFIED="1330350244033" TEXT="any name expressed as a symbol can also be expressed as a string"/>
</node>
<node CREATED="1330350258266" FOLDED="true" ID="ID_358386270" MODIFIED="1330350420609" TEXT="Multilingual name">
<node CREATED="1330350347365" ID="ID_1212101237" MODIFIED="1330350349177" TEXT="used for defining concepts using different languages,"/>
</node>
<node CREATED="1330350421941" FOLDED="true" ID="ID_1100829592" MODIFIED="1330350492635" TEXT="References">
<node CREATED="1330350479716" ID="ID_694229476" MODIFIED="1330350480888" TEXT="A reference stands for a name, string or multilingual name."/>
</node>
<node CREATED="1330350493577" FOLDED="true" ID="ID_448337049" MODIFIED="1330350558514" TEXT="Synonyms">
<node CREATED="1330350550992" ID="ID_134166977" MODIFIED="1330350552492" TEXT="(defattribute &quot;name ; given name&quot; (:class &quot;student&quot;)))"/>
</node>
<node CREATED="1330350576600" FOLDED="true" ID="ID_870088510" MODIFIED="1330351288618" TEXT="Internal Names">
<node CREATED="1330351108740" ID="ID_1100516756" MODIFIED="1330351110770" TEXT="internal names are not supposed to be seen by the user"/>
<node CREATED="1330351123474" ID="ID_1532294816" MODIFIED="1330351125161" TEXT="provide unique identifiers"/>
<node CREATED="1330351158286" FOLDED="true" ID="ID_681730073" MODIFIED="1330351241498" TEXT="conceitos">
<node CREATED="1330351198199" ID="ID_1546402844" MODIFIED="1330351215719" TEXT="ficam uper case e hifenizados"/>
</node>
<node CREATED="1330351230303" FOLDED="true" ID="ID_1369529538" MODIFIED="1330351282675" TEXT="propriedades">
<node CREATED="1330351273281" ID="ID_599135524" MODIFIED="1330351281146" TEXT="s&#xe3;o preficados com HAS-"/>
</node>
</node>
<node CREATED="1330351330323" FOLDED="true" ID="ID_1420173693" MODIFIED="1330351383802" TEXT="Variables">
<node CREATED="1330351341142" ID="ID_307292304" MODIFIED="1330351342735" TEXT="(defindividual Person (&quot;name&quot; &quot;Dupond&quot; &quot;Smith&quot;)(:var _ds))"/>
<node CREATED="1330351379338" ID="ID_132758058" MODIFIED="1330351381134" TEXT="They should start with an underscore,"/>
</node>
<node CREATED="1330351410265" FOLDED="true" ID="ID_374628433" MODIFIED="1330351427045" TEXT="Identifiers">
<node CREATED="1330351424204" ID="ID_1779284952" MODIFIED="1330351426484" TEXT="are given by the MOSS system automatically"/>
</node>
<node CREATED="1330351454710" FOLDED="true" ID="ID_1551038943" MODIFIED="1330443419627" TEXT="Macros Options">
<node CREATED="1330351478118" FOLDED="true" ID="ID_107941805" MODIFIED="1330443418785" TEXT="are lists starting with a keyword,">
<node CREATED="1330351510698" ID="ID_569024548" MODIFIED="1330351512619" TEXT="(:max 3)"/>
<node CREATED="1330351517321" ID="ID_1373973313" MODIFIED="1330351519007" TEXT="(:entry)"/>
<node CREATED="1330351528670" ID="ID_885709575" MODIFIED="1330351530419" TEXT="(:one-of &quot;young&quot; &quot;old&quot; &quot;ancient&quot;)"/>
<node CREATED="1330351531579" ID="ID_644624195" MODIFIED="1330351537509" TEXT="etc..."/>
</node>
</node>
</node>
<node CREATED="1330351564784" FOLDED="true" ID="ID_1158573347" MODIFIED="1330443440906" TEXT="Namespaces">
<node CREATED="1330351612325" ID="ID_1292660890" MODIFIED="1330351613980" TEXT="use the concept of Lisp package."/>
<node CREATED="1330351662409" ID="ID_435738709" MODIFIED="1330351664579" TEXT="applications can use their own package(name space)"/>
<node CREATED="1330351720043" ID="ID_1380312587" MODIFIED="1330351735551" TEXT="OMAS usa um pacote especifico para cada agente"/>
</node>
<node CREATED="1330351921166" ID="ID_1777786585" MODIFIED="1330351923056" TEXT="Contexts or Versions"/>
</node>
<node CREATED="1330352456789" FOLDED="true" ID="ID_1655055428" MODIFIED="1330445304035" POSITION="right" TEXT="Creating an Attribute">
<node CREATED="1330443556236" FOLDED="true" ID="ID_1105359200" MODIFIED="1330445302675" TEXT="Syntax">
<node CREATED="1330443568036" ID="ID_1924426523" MODIFIED="1330443569567" TEXT="defattribute reference &amp;rest option-list"/>
</node>
</node>
<node CREATED="1330445305573" ID="ID_816612331" MODIFIED="1330445308497" POSITION="right" TEXT="Creating a Concept">
<node CREATED="1330445419865" FOLDED="true" ID="ID_1857084235" MODIFIED="1330445584531" TEXT="individuos n&#xe3;o precisam ser um espelho exato de um comceito, eles podem:">
<node CREATED="1330445465436" ID="ID_1330734328" MODIFIED="1330445489880" TEXT="ter propriedades a mais do que o conceito"/>
<node CREATED="1330445475123" ID="ID_791935814" MODIFIED="1330445507117" TEXT="ter propriedades a menos do que o conceito"/>
</node>
<node CREATED="1330445619187" ID="ID_1829086163" MODIFIED="1330445632838" TEXT="&#xe9; a descri&#xe7;&#xe3;o de um individuo tipico"/>
<node CREATED="1330445643856" ID="ID_264494816" MODIFIED="1330445649055" TEXT="syntaxe">
<node CREATED="1330445660005" ID="ID_436184691" MODIFIED="1330445662001" TEXT="defconcept reference &amp;rest option-list"/>
</node>
</node>
</node>
</map>
