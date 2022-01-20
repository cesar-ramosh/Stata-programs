* Elasticidad Intergenaracionl de Ingresos (EII) para Bolivia *
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

	/*************/
	/*************/
	   *preserve
	/*************/
	/*************/
	
*** Hombres: Segmentación de grupos de edades 
	sort folio
	*_____browse folio nro1 sexo edad parentesco ylab
	
*	Padres (hombres): Grupos de edad 30 y 50 años (jefe de hogar)
	gen papa = 1 if edad>=30 & edad<=50 & parentesco==1 
	*gen papa = 1 if edad>=30 & edad<=50 & (parentesco==1 | parentesco==2) & sexo==1
* 	Miembros del hogar por cada folio
	egen mhogar = count(nro1), by(folio)
*	Hijos (varones O mujeres) que comprenden la edad entre 8 y 28 años
	gen hijos = 1 if edad>=8 & edad<=28 & parentesco==3 
*   Emparejando padres (hombres) e hijos varones
	egen mhogarhijos = count(hijos), by(folio)
*	Generando 1 papá (hombre) con 1 hijo (hombre)
	gen hijos2 = 1 if papa==1 & mhogarhijos>=1


*** Eliminación de filas
	drop if papa!=1
	drop if hijos2!=1
	drop if papa==.
	drop if hijos2==.
	drop if ylab==0
*** Guardamos la data generada
	save "eh1999men.dta", replace

*** Estimación del ingreso del padre 
	reg lylab aoesc exp exp2 casado 
	
	reg lylab aoesc edad edad2 area casado i.merctrab i.niv_ed_g
	
	reg lylab aoesc edad edad2 area casado merctrab3 merctrab4 merctrab5 niv_ed_g1 niv_ed_g2 niv_ed_g3 
	
	reg lylab aoesc exp exp2 casado 
/*
lylab	Coef.	Std. Err.	t    P>t     [95% Conf.	Interval]		
aoesc		 .114559	.0219461	 5.22   0.000     .0714871	.1576308
edad		.1240358	.0871814	 1.42   0.155    -.0470682	.2951398
edad2		-.0015588	.0010756	-1.45   0.148    -.0036698	.0005522
area		-.7511936	.0826578	-9.09   0.000    -.9134196	-.5889676
casado		-.2904366	.0904204	-3.21   0.001    -.4678975	-.1129757
merctrab3	-.5391432	.1225712	-4.40   0.000     -.779704	-.2985823
merctrab4	-.0307272	.1437165	-0.21   0.831    -.3127882	 .2513339
merctrab5	.3406464	.1276341	 2.67   0.008      .090149	 .5911438
niv_ed_g1	-.3460323	.1730566	-2.00   0.046    -.6856769	-.0063878
niv_ed_g2	-.6102962	.2585991	-2.36   0.018    -1.117828	-.1027641
niv_ed_g3	-.8717443	.3710564	-2.35   0.019    -1.599987	-.1435012
_cons		 5.201219	1.756285	 2.96   0.003     1.754298	  8.64814
				
						
lylab	Coef.	Std. Err.	t	P>t	[95% Conf.	Interval]
						
aoesc		 .1120462	.0210744     5.32	0.000	 .0706942	.1533982
edad		 .0889019	.0832425	 1.07	0.286	-.0744358	.2522396
edad2		-.0010712	.0010255	-1.04	0.296	-.0030834	.000941
area		-.6677365	.0776811	-8.60	0.000	-.8201618	-.5153113
casado		-.1131745	.0739636	-1.53	0.126	-.2583052	.0319562
merctrab3	-.5155368	.1133319	-4.55	0.000	-.7379158	-.2931578
merctrab4	 .0258457	.1360723	 0.19	0.849	-.2411542	.2928456
merctrab5	 .4030622	.1210861	 3.33	0.001	 .1654679	.6406564
niv_ed_g1	-.1637085	.1436788	-1.14	0.255	-.4456338	.1182168
niv_ed_g2	-.3491774	.2352721	-1.48	0.138	-.8108264	.1124715
niv_ed_g3	-.6006542	.3482509	-1.72	0.085	-1.283989	.0826808
_cons		 5.266147	1.678285	 3.14	0.002	  1.97303	8.559265
						
lylab	Coef.	Std. Err.	t	P>t	[95% Conf.	Interval]
						
aoesc		 .1251258	.0175872	 7.11	0.000	.0906296	.159622
edad		 .0715827	.0332561	 2.15	0.032	.006353	    .1368124
edad2		-.0007775	.0003946	-1.97	0.049	-.0015515	-3.47e-06
area		-.4848439	.0624859	-7.76	0.000	-.607406	-.3622818
casado		-.0793885	.0646754	-1.23	0.220	-.2062453	.0474683
merctrab3	-.4783452	.0914122	-5.23	0.000	-.6576445	-.2990459
merctrab4	 .0703417	.1146434	 0.61	0.540	-.154524	.2952075
merctrab5	 .3823433	.0998615	 3.83	0.000	.1864713	.5782152
niv_ed_g1	-.1076255	.1148155	-0.94	0.349	-.3328289   	.1175779
niv_ed_g2	-.3271463	.196106		-1.67	0.095	-.7117958	 .0575032
niv_ed_g3	-.6619503	.2875823	-2.30	0.021	-1.226025	-.0978758
_cons		 4.963733	.7022582	 7.07	0.000	 3.586298	 6.341168

						
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
