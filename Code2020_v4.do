* Elasticidad Intergenaracionl de Ingresos (EII) para Bolivia *
* Muestra secundaria: Encuesta de Hogares de 2020. (Muestra principal)

*** Entorno de trabajo
	clear all
	cls
	set more off
*** Directorio de trabajo
	cd "C:\Users\Admin\Downloads"
	use 2020.dta
	
*** Selección de variables ***
	keep folio s01a_05 area ylab ynolab z p0 p1 p2 factor yhog yhogpc s01a_02 s01a_03 depto yper aestudio nro niv_ed_g niv_ed s01a_09 aestudio s04b_12 s04b_13 ocupado s04b_14 condact s01a_08
	
	*(area 1: urbano 0: rural)-(sexo 1: hombre 2:mujer)
	rename s01a_05 parentesco
	rename s01a_09 estciv
	rename s01a_02 sexo
	rename s01a_03 edad
	rename nro nro1
	rename aestudio aoesc
	
*********************************
*-------------------------------*
*-------------------------------*
*********************************
	
*** Generación de variables ***
	gen exp = edad - aoesc - 6
	gen exp2 = exp*exp
	gen edad2 = edad*edad
	gen lyper = log(yper)
	gen lylab = log(ylab)
	gen casado = 1 if estciv==1
	replace casado = 0 if estciv!=1
	
	* Mercado de trabajo
	gen mt=.
	replace mt=1 if s04b_12==8 & ocupado==1 /*domestico*/
	replace mt=2 if (s04b_13==1 | s04b_13==2) & (mt!=1)/*estatal*/
	replace mt=3 if (s04b_12==3 | s04b_12==6) & (mt!=1 | mt!=2)/*familiar*/
	replace mt=4 if (s04b_12==1 | s04b_12==2 | s04b_12==4 | s04b_12==5 | s04b_12==6) & (s04b_14<=4) & (mt!=1 & mt!=2 & mt!=3)/*semiempresarial*/
	replace mt=5 if (s04b_12==1 | s04b_12==2 | s04b_12==4 | s04b_12==5 | s04b_12==6) & (s04b_14>4) & (mt!=1 & mt!=2 & mt!=3 & mt!=4)/*empresarial*/
	replace mt=6 if ocupado==1 & (mt!=1 & mt!=2 & mt!=3 & mt!=4 & mt!=5) /*otros*/

	label var mt "Mercado de Trabajo"
	label define mt 1 "Doméstico" 2 "Estatal" 3 "Familiar" 4 "Semiempresarial" 5 "Empresarial" 6 "otros"
	label values mt mt
	
	gen merctrab = .
	replace merctrab = mt if mt!=6
	
	label var merctrab "Mercado Trabajo"
	label define merctrab 1 "Doméstico" 2 "Estatal" 3 "Familiar" 4 "Semiempresarial" 5 "Empresarial"
	label values merctrab merctrab
	
	gen byte merctrab1 = (merctrab==1)
	gen byte merctrab2 = (merctrab==2)
	gen byte merctrab3 = (merctrab==3)
	gen byte merctrab4 = (merctrab==4)
	gen byte merctrab5 = (merctrab==5)
	
	* Nivel general educativo
	rename niv_ed_g niv_ed_g1
	gen niv_ed_g=.
	replace niv_ed_g = niv_ed_g1 if niv_ed_g1!=4

	label var niv_ed_g "Nivel educativo general"
	label define niv_ed_g 0 "Ninguno" 1 "Primaria" 2 "Secundaria" 3 "Superior" 
	label values niv_ed_g niv_ed_g
	drop niv_ed_g1
	
	* Generación de dummys categórica
	gen byte niv_ed_g0 = (niv_ed_g==0)
	gen byte niv_ed_g1 = (niv_ed_g==1)
	gen byte niv_ed_g2 = (niv_ed_g==2)
	gen byte niv_ed_g3 = (niv_ed_g==3)
	
	* Estimación del empleo informal
	gen formal=.
	replace formal=1 if (mt==2 | mt==5)
	* persona en el sector Informal
	replace formal=2 if (mt==4 | mt==3)
	* persona en trabajo doméstico
	replace formal=3 if mt==1
	replace formal=. if condact~=1 & formal~=.  
	label var  formal "Mercado Formal e Informal"
	label define formal 1 "Formal" 2 "Informal" 3 "Doméstico"
	label values formal formal  
	* Esta es la categoria que se tomará para la investigación:
	* Solo dos categorias Informal(Informal y doméstico):1 /Formal:0
	gen informal = 1 if formal == 2
	replace informal = 0 if formal == 1
	label var informal "Informalidad"
	label define informal 1 "Informal" 0 "Formal"
	label values informal informal
	
	* Departamento eje central 
	gen eje=1 if (depto==2 | depto==3 | depto==7)
	replace eje=0 if (depto==1 | depto==4 | depto==5 | depto==6 | depto==8 | depto==9)
	
	label var eje "Eje Central"
	label define eje 1 "Central" 0 "Secundario"
	label values eje eje
	
	* Pareja 
	gen pareja=1 if (estciv==2 | estciv==3)
	replace pareja=0 if (estciv==1 | estciv==4 | estciv==5)
	replace pareja=. if (estciv==6)
	
	label var pareja "Acompañado/a"
	label define pareja 1 "Con pareja" 0 "Sin pareja"
	label values pareja pareja
	
	* Indigena
	gen indigena=0
	replace indigena=1 if s01a_08==1
********************************************
* 	
	
	
	/*************/
	/*************/
	  * preserve
	/*************/
	/*************/

*********************************
*-------------------------------*
*-------------------------------*
*********************************

*** Hombres: Segmentación de grupos de edades 
	sort folio
	*____browse folio nro1 sexo edad parentesco ylab
	
*	Padres (hombres): Grupos de edad 30 y 50 años (jefe de hogar)
	gen papa = 1 if edad>=52 & edad<=72 & parentesco==1 
	*gen papa = 1 if edad>=30 & edad<=50 & (parentesco==1 | parentesco==2) & sexo==1
* 	Miembros del hogar por cada folio
	egen mhogar = count(nro1), by(folio)
*	Hijos (varones O mujeres) que comprenden la edad entre 8 y 28 años
	gen hijos = 1 if edad>=30 & edad<=50 & parentesco==3 
*   Emparejando padres (hombres) e hijos varones
	egen mhogarhijos = count(hijos), by(folio)
*	Generando 1 papá (hombre) con 1 hijo (hombre)
	gen hijos2 = 1 if papa==1 & mhogarhijos>=1


*** Eliminación de filas
	drop if papa!=1
	drop if hijos2!=1
*** Guardamos la data generada
	save "eh2020men.dta", replace

*** Estimación del ingreso permanente del padre 
	reg lylab aoesc exp exp2 casado 
* 	Estimación del ingreso del padre con theta 1999
	gen lylab_p1 = .1406829*aoesc + .0141378*exp -.0002504*exp2-.3035502*casado + 5.42	
*   Con mercado de trabajo y nivel de educación
	gen lylab_p = .1406829*aoesc + .0141378*exp -.0002504*exp2-.3035502*casado + 5.42
	
* 	Estimación ingreso padre con theta de 1999 
	*gen lylab_pp = 5.201219+.114559*aoesc+.1240358*edad-.0015588*edad2-.7511936*area-.2904366*casado-.5391432*merctrab3-.0307272*merctrab4+.3406464*merctrab5-.3460323*niv_ed_g1-.6102962*niv_ed_g2-.8717443*niv_ed_g3
	gen lylab_pp = 5.266147+.1120462*aoesc+.0889019*edad-.0010712*edad2-.6677365*area-.1131745*casado-.5155368*merctrab3+.0258457*merctrab4+.4030622*merctrab5-.1637085*niv_ed_g1-.3491774*niv_ed_g2-.6006542*niv_ed_g3
	
	*gen lylab_pp = 4.963733+.1251258*aoesc+.0715827*edad-.0007775*edad2-.4848439*area-.0793885*casado-.4783452*merctrab3+.0703417*merctrab4+.3823433*merctrab5-.1076255*niv_ed_g1-.3271463*niv_ed_g2-.6619503*niv_ed_g3
	
*	Logaritmo del ingreso del hijo 
	gen ylab_h = ylab if hijos2==1
	gen lylab_h = log(ylab_h)
* Estimación ingreso permanente del hijo
	reg lylab_h exp aoesc 
	predict lylab_hp, xb
	
	
*** Estimación del ingreso permanente del hijo	
	reg lylab_hp lylab_pp, vce(bootstrap)
	bootstrap, reps(1000): reg lylab_h lylab_pp
	
*** Variables de control a la estimación
	reg lylab_hp lylab_pp exp casado, vce(bootstrap) 
	reg lylab_hp lylab_pp exp casado mhogar, vce(bootstrap) 
	
	* Urbana
	reg lylab_hp lylab_pp casado mhogar if area==1
	* Rural
	reg lylab_hp lylab_pp casado mhogar if area==2
	
	* Regresión general
	reg lylab_hp lylab_pp casado mhogar
	
	* Pobre
	reg lylab_hp lylab_pp if p0==1
	* No pobre
	reg lylab_hp lylab_pp casado mhogar if p0==0
	
	* Informal
	reg lylab_hp lylab_pp casado mhogar if informal==1
	* Formal
	reg lylab_hp lylab_pp casado mhogar if informal==0
	
	* Eje central
	reg lylab_hp lylab_pp mhogar if eje==1
	* Eje secundario
	reg lylab_hp lylab_pp casado if eje==0
	
	* Indigena
	reg lylab_hp lylab_pp mhogar casado if indigena==1
	* No indigena
	reg lylab_hp lylab_pp pareja mhogar if indigena==0
	
	* Regresión son controles
	reg lylab_hp lylab_pp
	reg lylab_hp lylab_pp [fweight=int(factor)]
	
	* Regresión general
	reg lylab_hp lylab_pp  mhogar [fweight=int(factor)]
	
	/*************/
	/*************/
	 *   restore
	/*************/
	/*************/

****************************

*********************************
*-------------------------------*
*-------------------------------*
*********************************
