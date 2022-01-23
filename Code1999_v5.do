* Elasticidad Intergenaracional de Ingresos (EII) para Bolivia *
* Muestra secundaria: Encuesta de Hogares de 1999.

*** Entorno de trabajo
	clear all
	cls
	set more off
*** Directorio de trabajo
	cd "C:\Users\Admin\Downloads"
	use 1999.dta
	
*** Selección de variables
	keep folio s105 urbrur ylab ynolab z fgt_0 fgt_1 fgt_2 factor yhog yhogpcf sexo edad ciudad yper aoesc nro1 merctrab s108 s402a
	*(urbrur 1: urbano 2: rural)-(sexo 1: hombre 2:mujer)
	rename s105 parentesco
	rename urbrur area
	rename  fgt_0 p0
	rename  fgt_1 p1
	rename  fgt_2 p2
	rename yhogpcf yhogpc
	rename s108 estciv
	rename s402a niv_edu
*** Generación de variables
	gen exp = edad - aoesc - 6
	gen exp2 = exp*exp
	gen edad2 = edad*edad
	gen lyper = log(yper)
	gen lylab = log(ylab)
	gen casado = 1 if estciv==1
	replace casado = 0 if estciv!=1
*********************************************
* 	*Nivel educativo (clasificación)
	gen niv_ed_g = . 
	replace niv_ed_g = 0 if aoesc==0
	replace niv_ed_g = 1 if (aoesc>=1 & aoesc<=6)
	replace niv_ed_g = 2 if (aoesc>=7 & aoesc<=12)
	replace niv_ed_g = 3 if (aoesc>=13)

	label var niv_ed_g "Nivel educativo general"
	label define niv_ed_g 0 "Ninguno" 1 "Primaria" 2 "Secundaria" 3 "Superior" 
	label values niv_ed_g niv_ed_g
	
	*gen byte d1=(var1 >25) if var1 != . 
	gen byte niv_ed_g0 = (niv_ed_g==0)
	gen byte niv_ed_g1 = (niv_ed_g==1)
	gen byte niv_ed_g2 = (niv_ed_g==2)
	gen byte niv_ed_g3 = (niv_ed_g==3)
	
* 	*Quintiles de ingreso
	capture drop quintil
	*xtile quintil=ylab if ylab!=. & ylab!=0, nq(5) 
	xtile quintil=ylab, nq(5) 
		
*	* Mercado de trabajo 
	gen byte merctrab1 = (merctrab==1)
	gen byte merctrab2 = (merctrab==2)
	gen byte merctrab3 = (merctrab==3)
	gen byte merctrab4 = (merctrab==4)
	gen byte merctrab5 = (merctrab==5)
*	* Pareja 
	gen pareja=1 if (estciv==1 | estciv==2)
	replace pareja=0 if (estciv==4 | estciv==5 | estciv==6)
	replace pareja=. if (estciv==3)
	
	label var pareja "Acompañado/a"
	label define pareja 1 "Con pareja" 0 "Sin pareja"
	label values pareja pareja

	/*************/
	/*************/
	   *preserve
	/*************/
	/*************/
	
*** Hombres: Segmentación de grupos de edades 
	sort folio
	browse folio nro1 sexo edad parentesco ylab
	
*	Padres (hombre o mujer): Grupos de edad 30 y 50 años (jefe de hogar)
	gen papa = 1 if edad>=30 & edad<=50 & parentesco==1 
	*gen papa = 1 if edad>=30 & edad<=50 & (parentesco==1 | parentesco==2) & sexo==1
* 	Miembros del hogar por cada folio
	egen mhogar = count(nro1), by(folio)
*	Hijos (varones O mujeres) que comprenden la edad entre 8 y 28 años
	gen hijos = 1 if edad>=8 & edad<=28 & parentesco==3 
*   Emparejando padres (hombres) e hijos varones
	egen mhogarhijos = count(hijos), by(folio)
*	Generando 1 papá (hombre) con 1 hijo (hombre)
	gen hijos2 = 1 if papa==1 | hijos==1
	*gen hijos2 = 1 if papa==1 & mhogarhijos>=1
* 	Generamos el ingreso del padre para cada hijo o hija
	* Elimanos aquellos miembro diferentes a jefe de hogar e hijos
	drop if hijos2==.
	gen ylab_a = ylab if parentesco==1
	egen ylab_p = total(ylab_a), by(folio)

*** Eliminación de filas
	*drop if papa!=1
	drop if hijos2!=1
	drop if papa==.
	drop if hijos2==.
	drop if ylab_p==0
*** Guardamos la data generada
	save "eh1999men.dta", replace

*** Estimación del ingreso del padre 
	
	reg lylab aoesc edad edad2 area 
	
	reg lylab aoesc edad edad2 area casado i.merctrab i.niv_ed_g
	
*** Parámetros significativos todoas (regresión candidata)	
	reg lylab aoesc edad edad2 area merctrab2 merctrab4 merctrab5 niv_ed_g0 niv_ed_g1 niv_ed_g2 niv_ed_g3, nocon
*** Al introducir el nivel educativo son no significativos
	reg lylab aoesc edad edad2 area merctrab2 merctrab4 merctrab5 
*** Regresión con interacciones la mejor
	reg lylab aoesc edad edad2 merctrab2 merctrab4 merctrab5 niv_ed_g1##area 
	gen prim_rural = area*niv_ed_g1
	* esta si va
	reg lylab aoesc edad edad2 merctrab2 merctrab4 merctrab5 niv_ed_g1 area prim_rural
*** Parámetros estimaddos para la muestra principal

/*

lylab	Coef.	Std. Err.	t	P>t	[95% Conf.	Interval]
						
aoesc		.0853103	.0082832	10.30	0.000	.0690614	.1015593
edad		.140068		.0672147	2.08	0.037	.0082148	.2719212
edad2		-.0017085	.0008365	-2.04	0.041	-.0033494	-.0000677
merctrab2	.5579987	.0961829	5.80	0.000	.3693192	.7466782
merctrab4	.5486841	.0874838	6.27	0.000	.3770695	.7202987
merctrab5	.8760635	.0755468	11.60	0.000	.7278653	1.024262
niv_ed_g1	.5042809	.2153789	2.34	0.019	.0817778	.9267841
area		-.5904861	.0915788	-6.45	0.000	-.7701339	-.4108383
prim_rural	-.266016	.1267667	-2.10	0.036	-.5146909	-.0173412
_cons		3.424538	1.343229	2.55	0.011	.7895621	6.059514



lylab	Coef.	Std. Err.	t	P>t	[95% Conf.	Interval]
						
aoesc			.0853103	.0082832	10.30	0.000	.0690614	.1015593
edad			.140068		.0672147	2.08	0.037	.0082148	.2719212
edad2			-.0017085	.0008365	-2.04	0.041	-.0033494	-.0000677
merctrab2		.5579987	.0961829	5.80	0.000	.3693192	.7466782
merctrab4		.5486841	.0874838	6.27	0.000	.3770695	.7202987
merctrab5		.8760635	.0755468	11.60	0.000	.7278653	1.024262
niv_ed_g1		.2382649	.1037149	2.30	0.022	.0348101	.4417197
area 			-.5904861	.0915788	-6.45	0.000	-.7701339	-.4108383
1#area rur		-.266016	.1267667	-2.10	0.036	-.5146909	-.0173412
_cons			2.834052	1.334375	2.12	0.034	.2164437	5.45166

						
*/
	
	/*************/
	/*************/
	    *restore
	/*************/
	/*************/
	
***  Borrar y restaurar  ***
preserve
	display "Hola mundo"
restore
****************************

*********************************
*-------------------------------*
*-------------------------------*
*********************************
