*** Note that variables you see in this dofile might have been cleaned beforehand. This dofile does not contain all cleaning procedures and mainly describes how tier variables were created. 
			
			
			*----------------------------------------------------*
			*        	 Electricity Tier Calculation            *  
			*----------------------------------------------------*

				
*********************************** SET UP *************************************
	
* Add household size var
	
	use "$wip_hh\ROSTER_A1.dta", clear
	gen mbrcount=1
	bys hhid: egen hhsize = total(mbrcount) 
	keep hhid hhsize
	duplicates drop
	lab var hhsize "Household Size"
	save "$wip_hh\hhsize.dta", replace 
		
		
	use "$wip_hh\SECTION_C1.dta", clear
	ta _m
	drop _m
	sort hhid
	merge hhid using "$wip_hh\hhsize.dta"
	ta _m
	save "$wip_hh\SECTION_C1.dta", replace 

	
* Defining tier label

	use "$wip_hh\SECTION_C1.dta", clear
	label define tier 0 "Tier 0" 1 "Tier 1" 2 "Tier 2" 3 "Tier 3" 4 "Tier 4" 5 "Tier 5", modify		
	save "$wip_hh\SECTION_C1.dta", replace 
		
		
****************************** A. CAPACITY TIER ********************************
					
// MAIN SOURCE OF ELECTRICITY
* C171: Of all the sources that you mentioned above, which is the source that you use the most in your household?

	use "$wip_hh\SECTION_C1.dta", clear
	
* 1. Defining main source of electricity and creating variables for the analysis
	
	// main source of electricity variable
	gen main_src_elec=1 if C171==1					// national grid
	replace main_src_elec=2 if C171==2				// mini-grid
	replace main_src_elec=3 if C171==3				// electric generator  
	replace main_src_elec=4 if C171==6 				// SHS
	replace main_src_elec=5 if C171==5 				// SLS
	replace main_src_elec=6 if C171==4   			// Solar Lantern 
	replace main_src_elec=7 if C171==7   				// rechargeable battery 
	replace main_src_elec=8 if C171==8 | C171==111		// dry-cell battery or no electricity 

	la var main_src_elec "Household main source of electricity"
	la define main_src_elec 1 "National Grid" 2 "Mini-grid" 3 "Electric Generator" 4 "SHS" 5 "SLS" 6 "Solar Lantern" 7 "Rechargeable battery" 8 "No Electricity/dry-cell", modify
	la values main_src_elec main_src_elec

	// binary for mobile charging
	**C132: Can you charge your phone using this [DEVICE]?
	gen mob_chrge= C132==1 

	// binary for radio
	**C134: Can you power your radio using [DEVICE]? 
	gen radio= C134==1 
	
	save "$wip_hh\SECTION_C1.dta", replace 
					
* 2. Appliances Power level Set up

	** Prep appliance module and merge
	use "$wip_hh\SECTION_EF1.dta" , clear
	keep hhid E00A- F10
	sort hhid
	save "$wip_hh\SECTION_EF1_formerge.dta", replace
	
	use "$wip_hh\SECTION_C1.dta" , clear
	sort hhid
	ta _m
	drop _m
	merge hhid using "$wip_hh\SECTION_EF1_formerge.dta"
	save "$wip_hh\SECTION_C1.dta", replace 
	

	
// RECHARGEABLE BATTERY (Reviewed households whose electricity source is rech battery "only," and it is their main source of electricity.)
	
	use "$wip_hh\SECTION_C1.dta" , clear
	
	gen rech_app_tier1=0
	replace rech_app_tier1=1 if C002==0&C041==0&C082==0&C106==1&C127==0&C171==7& (E03A>0|E04A>0|E05A>0|E06A>0|E07A>0|E28A>0|E29A>0)
	gen rech_app_tier2=0
	replace rech_app_tier2=1 if C002==0&C041==0&C082==0&C106==1&C127==0&C171==7& (E32A>0)
	//gen rech_app_tier3=0
	//replace rech_app_tier3=1 if C002==0&C041==0&C082==0&C106==1&C127==0&C171==7& 
	gen rech_app_tier4=0
	replace rech_app_tier4=1 if C002==0&C041==0&C082==0&C106==1&C127==0&C171==7& E13A>0
	
	gen rech_app_tier=.
	replace rech_app_tier=1 if rech_app_tier1==1
	replace rech_app_tier=2 if rech_app_tier2==1
	//replace rech_app_tier=3 if rech_app_tier3==1
	replace rech_app_tier=4 if rech_app_tier4==1
	lab define rech_app_tier 1 "Tier 1" 2 "Tier 2"  4 "Tier 4", modify
	lab val rech_app_tier rech_app_tier
	lab var rech_app_tier "Power level of appliances used by rech. batteries - categorical var"

	
//SOLAR
	*[solar roster] C135: Do you use any of these appliances with the [DEVICE]?
	*[main solar device] C170: What appliances do you use today that you did not use with your first solar lighting device? 

	
	/* App list used by either rech batt or solar - by tier
	
	Tier 1
	ta C170_1 		//  Mobile phone charger 
	ta C170_2		//  Radio 
	ta C170_6		//  Light 
	ta E03A if E03A>0 		// CFL bulb 	
	ta E05A 	// Flashlight/torch 
	ta E06A 	// Radio
	ta E07A 	// radio cd player
	ta E04A		// Light/LED Light
	ta E28A , E29A		// smart phone, typical phone charger 
	ta C135_7 // Electric lamp
	ta C135_8 // Radio
	ta C135_9 // Telephone (mobile phone)
	
	Tier 2
	ta C135_1 C170_3 E32A 		// Television 
	ta C135_2 C170_4 		// FAN 
	ta C135_4 		// Tablet/Laptop/Computer 

	Tier 3
	ta C135_3 C170_5 		// Refrigerator 
	
	Tier 4
	ta E13A 		// Iron
	
	Tier 5
	C135_11  // water heater (One household said they use water heater, but not clear if with SHS or the national grid. It falls into Capacity Tier 5 anyways from its use of the national grid as the primary electricity source.)
	
	No appliances
	ta C135_5==111 
	
	*/

	gen applpowr=.
	replace applpowr=4 if applpowr==. & (rech_app_tier==4) 
	replace applpowr=3 if applpowr==. & (C135_3==1|C170_5==1)
	replace applpowr=2 if applpowr==. & (C170_3==1|C170_4==1|C135_1==1|C135_2==1|C135_4==1|rech_app_tier==2)
	replace applpowr=1 if applpowr==. & (C170_1==1|C170_2==1|C170_6==1| rech_app_tier==1|C135_7==1|C135_8==1|C135_9==1)
	replace applpowr=0 if applpowr==. & C135_5==111

	ta applpowr
	
	
* 3. Capacity tier calculation
	/*	main_src_elec: 
						1 "National Grid" 
						2 "Mini-grid" 
						3 "Electric Generator" 
						4 "SHS" 
						5 "SLS" 
						6 "Solar Lantern" 
						7 "Rechargeable battery" 
						8 "No Electricity/dry-cell"				*/


	** Tier 5 (main=national grid | mini grid) 
	gen elc_cap_tier5 = (main_src_elec==1 | main_src_elec==2) 
	
	** Tier 4 (Generator| SHS | Rechargeable Batteries + appl 4)
	gen elc_cap_tier4 = ( (main_src_elec==3 |main_src_elec==4 |main_src_elec==7) & applpowr==4 ) 
	
	** Tier 3 (Generator| SHS | Rechargeable Batteries + appl 3)
	gen elc_cap_tier3 = ( (main_src_elec==3 |main_src_elec==4 |main_src_elec==7) & applpowr==3 ) 
	
	** Tier 2 (Generator| Rechargeable Batteries + appl 2) OR (SHS <= appl 2)
	gen elc_cap_tier2 = ( (main_src_elec==3 |main_src_elec==7) & applpowr==2 ) | (main_src_elec==4 & applpowr<=2)
	
	** Tier 1 (Generator| Rechargeable Batteries + appl 1 or mobile charging) 
	gen elc_cap_tier1 = ( (main_src_elec==3 |main_src_elec==7) & (applpowr==.|applpowr<2|mob_chrge==1) ) // when no appliance is reported to be used, we assume that the electricity source was for lighting.
			
	** Tier 0 (no elec+dry cell battery) 
	gen elc_cap_tier0=0 
	replace elc_cap_tier0= main_src_elec==8
	
* 3-1. Capacity tier between 0-1

	** 1. SLS (main_src_elec==5)
	gen elc_cap_tier01 = 2/hhsize if main_src_elec==5 
	replace elc_cap_tier01 = 0.7 if main_src_elec==5 & elc_cap_tier01>0.7
	replace elc_cap_tier01 = elc_cap_tier01 + 0.3 if main_src_elec==5 & (mob_chrge==1|radio==1)

	** 2. Solar Lantern (main_src_elec==6)
	replace elc_cap_tier01 = 1/hhsize if main_src_elec==6 
	replace elc_cap_tier01 = 0.7 if main_src_elec==6 & elc_cap_tier01>0.7
	replace elc_cap_tier01 = elc_cap_tier01 + 0.3 if main_src_elec==6 & (mob_chrge==1|radio==1)
	
	sum  elc_cap_tier01 
	
	/* 
		
		Variable |        Obs        Mean    Std. Dev.       Min        Max
	-------------+---------------------------------------------------------
	elc_cap_t~01 |        630    .5725197    .2490958   .0769231          1


	*/
	
	replace elc_cap_tier1=1 if elc_cap_tier01>.5725197 & elc_cap_tier01!=.
	replace elc_cap_tier0=1 if elc_cap_tier01<=.5725197 & elc_cap_tier01!=.
	
* 4. Capacity Tier
	
	gen elc_cap_tier=0 if elc_cap_tier0==1
	replace elc_cap_tier=1 if elc_cap_tier1==1
	replace elc_cap_tier=2 if elc_cap_tier2==1
	replace elc_cap_tier=3 if elc_cap_tier3==1
	replace elc_cap_tier=4 if elc_cap_tier4==1
	replace elc_cap_tier=5 if elc_cap_tier5==1

	label define tier 0 "Tier 0" 1 "Tier 1" 2 "Tier 2" 3 "Tier 3" 4 "Tier 4" 5 "Tier 5", modify
	label values elc_cap_tier tier

	la var elc_cap_tier0 "Electricity: Capacity Tier 0"
	la var elc_cap_tier1 "Electricity: Capacity Tier 1"
	la var elc_cap_tier2 "Electricity: Capacity Tier 2"
	la var elc_cap_tier3 "Electricity: Capacity Tier 3"
	la var elc_cap_tier4 "Electricity: Capacity Tier 4"
	la var elc_cap_tier5 "Electricity: Capacity Tier 5"
	la var elc_cap_tier "Electricity: Capacity Tier: Categorical variable"
	
	ta elc_cap_tier, m // no missing yay
	
	ta elc_cap_tier [aw=HH_WT]
	bys HI04: ta elc_cap_tier [aw=HH_WT]
	bys T: ta elc_cap_tier [aw=HH_WT]
	ta elc_cap_tier if T==2 [aw= ref_weight]
	estpost ta elc_cap_tier C171 [aw=HH_WT] if HI04==2 // technology by capacity tier - rural 
				

*************************** B. AVAILABILITY TIER ******************************					
					
// GRID
	* C29B: How many hours of electricity are available each day and night from the grid? (max 24 hours)
// MINI GRID
	* C70: How many hours of electricity are available each day and night from the mini-grid? (max 24 hours) 
// GENERATOR
	* C97: How many hours can you use this generator each day and nightif you want to? (max 24 hours)
// RECH. BATTERY
	* C116: How many hours can you use the rechargeable batteries for electricity supply each day if you want to? (max 24 hours)
// SOLAR
	* C158: How many hours do you receive electricity from this [DEVICE] each day and night? (max 24 hours)
	
	
* 1. Set up

	lab var C071D "Number of hours of availability of electricity each evening in typical months"

** Defining availability variable for day and night
	gen dur_day=C029B if C171==1 //typical month
	replace dur_day=C070D if C171==2
	replace dur_day=C097 if C171==3
	replace dur_day=C158 if C171==4|C171==5|C171==6
	replace dur_day=C116 if C171==7
	replace dur_day=. if dur_day==888

** Defining availability variable for evening
	gen dur_eve=C030B if C171==1
	replace dur_eve=C071D if C171==2
	replace dur_eve=C098 if C171==3
	replace dur_eve=C159 if C171==4|C171==5|C171==6
	replace dur_eve=C117 if C171==7
	replace dur_eve=. if dur_eve==888

	
* 2. Tier calculation	
	
** Availability tier for day and night (Tier 0 2 3 4 5)
	
	gen elc_durd_tier=5 if dur_day>=23 & dur_day<.
	replace elc_durd_tier=4 if dur_day<23
	replace elc_durd_tier=3 if dur_day<16
	replace elc_durd_tier=2 if dur_day<8 
	replace elc_durd_tier=0 if dur_day<4
	label values elc_durd_tier tier	
	la var elc_durd_tier "Electricity - Availability Tier Day & Night: Categorical"

	ta elc_durd_tier [aw=HH_WT]
	ta elc_durd_tier [aw=ref]
	
	/* Before missing value imputation						
						
		ta elc_durd_tier [aw=HH_WT]

		Electricity |
				  - |
		Availabilit |
		 y Tier Day |
		   & Night: |
		Categorical |      Freq.     Percent        Cum.
		------------+-----------------------------------
			 Tier 0 | 136.796846        4.45        4.45
			 Tier 2 | 148.630444        4.83        9.28
			 Tier 3 | 206.040775        6.70       15.97
			 Tier 4 | 367.242496       11.94       27.91
			 Tier 5 | 2,218.2894       72.09      100.00
		------------+-----------------------------------
			  Total |      3,077      100.00
	  	  
	*/
	
	*** Missing value imputation: imputed using the mode value of each technology except for national grid and minigrid that are imputed with the mode of each cluster; for the cases with multiple modes, we take the minimum value.
	
		egen day_mode_grid = mode(elc_durd_tier) if inlist(C171,1), min by(Cluster)
		egen day_mode_minigrid = mode(elc_durd_tier) if inlist(C171,2), min by(Cluster)
		egen day_mode_solarlantern = mode(elc_durd_tier) if inlist(C171,4), min 
		egen day_mode_SLS = mode(elc_durd_tier)	if inlist(C171,5), min 
		egen day_mode_SHS = mode(elc_durd_tier)	if inlist(C171,6), min 
		egen day_mode_batt = mode(elc_durd_tier) if inlist(C171,7), min
		
		replace elc_durd_tier=day_mode_grid if inlist(C171,1)&elc_durd_tier==.
		replace elc_durd_tier=day_mode_minigrid if inlist(C171,2)&elc_durd_tier==.
		replace elc_durd_tier=day_mode_solarlantern if inlist(C171,4)&elc_durd_tier==.
		replace elc_durd_tier=day_mode_SLS if inlist(C171,5)&elc_durd_tier==.
		replace elc_durd_tier=day_mode_SHS if inlist(C171,6)&elc_durd_tier==.
		replace elc_durd_tier=day_mode_batt if inlist(C171,7)&elc_durd_tier==.
		
		br Cluster hhid elc_durd_tier C171 if !inlist(C171,8,111)&elc_durd_tier==.
	
	ta elc_durd_tier [aw=HH_WT]
	bys HI04: ta elc_durd_tier [aw=HH_WT]
	bys T: ta elc_durd_tier [aw=HH_WT]
	ta elc_durd_tier if T==2 [aw= ref_weight]
	
	/* After imputation				
		
		.  ta elc_durd_tier

		Electricity |
				  - |
		Availabilit |
		 y Tier Day |
		   & Night: |
		Categorical |      Freq.     Percent        Cum.
		------------+-----------------------------------
			 Tier 0 |        179        5.16        5.16
			 Tier 2 |        234        6.74       11.90
			 Tier 3 |        211        6.08       17.97
			 Tier 4 |        353       10.17       28.14
			 Tier 5 |      2,495       71.86      100.00
		------------+-----------------------------------
			  Total |      3,472      100.00


		.  ta elc_durd_tier [aw=HH_W]

		Electricity |
				  - |
		Availabilit |
		 y Tier Day |
		   & Night: |
		Categorical |      Freq.     Percent        Cum.
		------------+-----------------------------------
			 Tier 0 | 148.843028        4.63        4.63
			 Tier 2 |179.5671285        5.59       10.22
			 Tier 3 | 206.260314        6.42       16.64
			 Tier 4 | 371.209811       11.55       28.19
			 Tier 5 | 2,307.1197       71.81      100.00
		------------+-----------------------------------
			  Total |      3,213      100.00

	*/
	
	
** Availability tier for evening (Tier 0 1 2 3 5)

	gen elc_dure_tier=5 if dur_eve==4
	replace elc_dure_tier=3 if dur_eve<4
	replace elc_dure_tier=2 if dur_eve<3
	replace elc_dure_tier=1 if dur_eve<2
	replace elc_dure_tier=0 if dur_eve<1 & dur_eve!=.
	label values elc_dure_tier tier
	la var elc_dure_tier "Electricity - Availability Tier Evening: Categorical"

	
	/*	Before Imputation
				
		. ta elc_dure_tier

		Electricity |
				  - |
		Availabilit |
			 y Tier |
		   Evening: |
		Categorical |      Freq.     Percent        Cum.
		------------+-----------------------------------
			 Tier 1 |         26        0.77        0.77
			 Tier 2 |        159        4.71        5.48
			 Tier 3 |        499       14.79       20.27
			 Tier 5 |      2,690       79.73      100.00
		------------+-----------------------------------
			  Total |      3,374      100.00

		. ta elc_dure_tier [aw=HH_W]

		Electricity |
				  - |
		Availabilit |
			 y Tier |
		   Evening: |
		Categorical |      Freq.     Percent        Cum.
		------------+-----------------------------------
			 Tier 1 | 12.6441961        0.40        0.40
			 Tier 2 | 146.440633        4.68        5.08
			 Tier 3 | 538.865978       17.22       22.30
			 Tier 5 | 2,432.0492       77.70      100.00
		------------+-----------------------------------
			  Total |      3,130      100.00
	
	*/
	
	
	*** Missing value imputation : imputed using the mode value of each technology except for national grid and minigrid that are imputed with the mode of each cluster; for the cases with multiple modes, we take the minimum value.
	
		egen eve_mode_grid = mode(elc_dure_tier) if inlist(C171,1), min by(Cluster)
		egen eve_mode_minigrid = mode(elc_dure_tier) if inlist(C171,2), min by(Cluster)
		egen eve_mode_solarlantern = mode(elc_dure_tier) if inlist(C171,4), min 
		egen eve_mode_SLS = mode(elc_dure_tier)	if inlist(C171,5), min 
		egen eve_mode_SHS = mode(elc_dure_tier)	if inlist(C171,6), min 
		egen eve_mode_batt = mode(elc_dure_tier) if inlist(C171,7), min
		
		replace elc_dure_tier=eve_mode_grid if inlist(C171,1)&elc_dure_tier==.
		replace elc_dure_tier=eve_mode_minigrid if inlist(C171,2)&elc_dure_tier==.
		replace elc_dure_tier=eve_mode_solarlantern if inlist(C171,4)&elc_dure_tier==.
		replace elc_dure_tier=eve_mode_SLS if inlist(C171,5)&elc_dure_tier==.
		replace elc_dure_tier=eve_mode_SHS if inlist(C171,6)&elc_dure_tier==.
		replace elc_dure_tier=eve_mode_batt if inlist(C171,7)&elc_dure_tier==.
		
		br Cluster hhid elc_dure_tier C171 if !inlist(C171,8,111)&elc_dure_tier==.
	
	/*	After Imputation
						
		. ta elc_dure_tier

		Electricity |
				  - |
		Availabilit |
			 y Tier |
		   Evening: |
		Categorical |      Freq.     Percent        Cum.
		------------+-----------------------------------
			 Tier 1 |         26        0.75        0.75
			 Tier 2 |        160        4.61        5.36
			 Tier 3 |        502       14.46       19.82
			 Tier 5 |      2,784       80.18      100.00
		------------+-----------------------------------
			  Total |      3,472      100.00

			  
		. ta elc_dure_tier [aw=HH_WT]

		Electricity |
				  - |
		Availabilit |
			 y Tier |
		   Evening: |
		Categorical |      Freq.     Percent        Cum.
		------------+-----------------------------------
			 Tier 1 | 12.6697533        0.39        0.39
			 Tier 2 | 146.876406        4.57        4.97
			 Tier 3 | 540.901675       16.83       21.80
			 Tier 5 | 2,512.5522       78.20      100.00
		------------+-----------------------------------
			  Total |      3,213      100.00
	
	*/
	
	
	ta elc_dure_tier [aw=HH_WT]
	bys HI04: ta elc_dure_tier [aw=HH_WT]
	bys T: ta elc_dure_tier [aw=HH_WT]
	ta elc_dure_tier if T==2 [aw= ref_weight]
	
	
	
	
** Availability tier - aggregate

	egen elc_avbl_tier=rmin(elc_dure_tier elc_durd_tier) if elc_dure_tier!=.&elc_durd_tier!=.
	label values elc_avbl_tier tier
	la var elc_avbl_tier "Electricity: Availability Tier - Aggregate: Categorical variable"
	
	ta elc_avbl_tier [aw=HH_WT]
	bys HI04: ta elc_avbl_tier [aw=HH_WT]
	bys T: ta elc_avbl_tier [aw=HH_WT]
	ta elc_avbl_tier if T==2 [aw= ref_weight]
	
	
				
**************************** C. RELIABILITY TIER *******************************
					
// GRID				
	* C31: How many unscheduled outages/blackouts occur in a typical week? 
	* C32: What is the total duration of all the unscheduled outages/blackouts in a typical week?

// MINI GRID
	* C72: How many unscheduled outages/blackouts occur in a typical week?
	* C73: What is the total duration of all the unscheduled outages/blackouts in a typical week? 
					
					
* 1. Set up	

	** Create variables
	
	// grid
	gen C032_hr = C032B 
	replace C032_hr = C032B/60 if C032B_UNITY == 2&(C032B!=111& C032B!=888)
	lab var C032_hr "Duration of grid disruption in hour (typical week)"
	
	gen C032_min = C032B
	replace C032_min = C032B*60 if C032B_UNITY ==1&(C032B!=111& C032B!=888)
	lab var C032_min "Duration of grid disruption in minute (typical week)"
	
	// minigrid
	gen C073_hr = C073D 
	replace C073_hr = C073D/60 if C073D_UNITY == 2 & C073D!=888
	lab var C073_hr "Duration of minigrid disruption in hour (typical week)"
	
	gen C073_min = C073D
	replace C073_min = C073D*60 if C073D_UNITY ==1 & C073D!=888
	lab var C073_min "Duration of minigrid disruption in minute (typical week)"
	
	
	** Defining weekly disruption variable

	for var C031B C032_hr C032_min C072D C073_hr C073_min: recode X 888=.
	replace C031B=0 if C031B==111
	replace C032_hr =0 if C032_hr ==111
	replace C032_min =0 if C032_min ==111
	replace C073_hr =0 if C073_hr==111
	replace C073_min =0 if C073_min==111
	replace C072D=0 if C072D==111

	gen dis_frq=C031B if C171==1
	replace dis_frq=C072D if C171==2
					
	** Defining duration of disruption
 	
	gen dis_dur=C032_hr if C171==1
	replace dis_dur=C073_hr if C171==2
	ta dis_dur, m

	
* 2. Tier calculation

	gen elc_rel_tier0 = dis_frq==. 
	gen elc_rel_tier1 = dis_frq==.
	gen elc_rel_tier2 = dis_frq==.
	gen elc_rel_tier3 = dis_frq>14 & dis_frq!=. 
	gen elc_rel_tier4 = (dis_frq>3 & dis_frq<=14)|(dis_frq<=3 & dis_dur>=2)  
	gen elc_rel_tier5 = dis_frq<=3 & dis_dur<2                               
 
	gen elc_rel_tier=3 if elc_rel_tier3==1 
	replace elc_rel_tier=4 if elc_rel_tier4==1
	replace elc_rel_tier=5 if elc_rel_tier5==1
	label values elc_rel_tier tier

	la var elc_rel_tier0 "Electricity: reliability Tier 0"
	la var elc_rel_tier1 "Electricity: reliability Tier 1"
	la var elc_rel_tier2 "Electricity: reliability Tier 2"
	la var elc_rel_tier3 "Electricity: reliability Tier 3"
	la var elc_rel_tier4 "Electricity: reliability Tier 4"
	la var elc_rel_tier5 "Electricity: reliability Tier 5"
	la var elc_rel_tier "Electricity: Reliability Tier: Categorical variable"
	

	** Missing value imputation [Feb 9, not applied] : imputed using the mode value of each cluster by technology; for the cases with multiple modes, we take the minimum value.
	
	//egen rel_mode = mode(elc_rel_tier) if inlist(C171,1,2), min by(Cluster)
	egen rel_mode_grid = mode(elc_rel_tier)	if inlist(C171,1), min by(Cluster)
	egen rel_mode_minigrid = mode(elc_rel_tier)	if inlist(C171,2), min by(Cluster)
		
	//replace elc_rel_tier=rel_mode  if inlist(C171,1,2)&elc_rel_tier==.
	replace elc_rel_tier=rel_mode_grid if inlist(C171,1)&elc_rel_tier==.
	replace elc_rel_tier=rel_mode_minigrid if inlist(C171,2)&elc_rel_tier==.

	ta elc_rel_tier [aw=HH_W]	
	/* 
	Electricity |
			  : |
	Reliability |
		  Tier: |
	Categorical |
	   variable |      Freq.     Percent        Cum.
	------------+-----------------------------------
		 Tier 4 | 1,504.3142       56.64       56.64
		 Tier 5 | 1,151.6858       43.36      100.00
	------------+-----------------------------------
		  Total |      2,656      100.00
	*/
		
	*** ->>>>> 2 obs missing since the entire cluster misses reliability information. The 2 cases were imputed with mode of the entire sample: Tier 4. [ !!! Confirm with Bryan ]
		
	replace elc_rel_tier=4 if Cluster == 261 & inlist(C171,1,2)
	
	** Statistics 
	
	ta elc_rel_tier [aw=HH_WT]
	bys HI04: ta elc_rel_tier [aw=HH_WT]
	bys T: ta elc_rel_tier [aw=HH_WT]
	ta elc_rel_tier if T==2 [aw= ref_weight]
	
	
	
****************************** D. QUALITY TIER ********************************
					
// Main source of electricity
	* C171: Of all the sources that you mentioned above, which is the source that you use the most in your household?					
// GRID
	* C36: In the last 12 months, did any of your appliances get damaged because the voltage from the grid was going up and down?	
// MINI GRID
	* C77: In the last 12 months, did any of your appliances get damaged because the voltage from the mini grid was going up and down? 
	
					
* 1. Set up

	gen appl_dam = (C036==1 & C171==1)|(C077==1 & C171==2) 
	replace appl_dam =. if C171!=1 & C171!=2 

* 2. Tier calculation

	gen elc_qual_tier0 = appl_dam== . 
	gen elc_qual_tier1 = appl_dam== .
	gen elc_qual_tier2 = appl_dam== .
	gen elc_qual_tier3 = appl_dam== 1
	gen elc_qual_tier4 = 0 
	gen elc_qual_tier5 = appl_dam== 0

	gen elc_qual_tier = 3 if elc_qual_tier3 == 1 
	replace elc_qual_tier = 5 if elc_qual_tier5 == 1
	label values elc_qual_tier tier

	la var elc_qual_tier0 "Electricity: Quality Tier 0"
	la var elc_qual_tier1 "Electricity: Quality Tier 1"
	la var elc_qual_tier2 "Electricity: Quality Tier 2"
	la var elc_qual_tier3 "Electricity: Quality Tier 3"
	la var elc_qual_tier4 "Electricity: Quality Tier 4"
	la var elc_qual_tier5 "Electricity: Quality Tier 5"
	la var elc_qual_tier "Electricity: Quality Tier: Categorical variable"
									
	ta elc_qual_tier [aw=HH_WT]
	bys HI04: ta elc_qual_tier [aw=HH_WT]
	bys T: ta elc_qual_tier [aw=HH_WT]
	ta elc_qual_tier if T==2 [aw= ref_weight]			
				
					
***************************** E. FORMALITY TIER ********************************

// GRID
	* C12. Who receives the payment for your electricity service?
// MINI GRID
	* C52: Who receives the payment for your electricity service? 					
		
		
* Cleaning "Other, specify" answers for C12
		
	** cleaning 43 "Other, specify" responses
		
		decode C012_OTHER, gen(C012_Other_dec)
		
		replace C012 = 5 if C012_Other_dec== "Neighbor"
		replace C012_Other_dec="" if C012_Other_dec=="Neighbor"
		
		replace C012=4 if C012_Other_dec=="Head of household"
		replace C012_Other_dec="" if C012_Other_dec=="Head of household"
		
		replace C012 = 11 if C012_Other_dec== "(mobile money) service providing Agent"
		replace C012_Other_dec="" if C012_Other_dec== "(mobile money) service providing Agent"
		
		replace C012 = 11 if C012_Other_dec== "He/She uses mobile money"
		replace C012_Other_dec="" if C012_Other_dec==  "He/She uses mobile money"
		
		replace C012 = 11 if C012_Other_dec== "On Phone"
		replace C012_Other_dec="" if C012_Other_dec== "On Phone"
		
		replace C012 = 12 if C012_Other_dec== "HH not yet installed"
		replace C012_Other_dec="" if C012_Other_dec== "HH not yet installed"
		
		replace C012 = 12 if C012_Other_dec== "They don't use electricity"
		replace C012_Other_dec="" if C012_Other_dec== "They don't use electricity"
		
		replace C012 = 13 if C012_Other_dec== "He buys electricity himself"
		replace C012_Other_dec="" if C012_Other_dec== "He buys electricity himself"
		
		replace C012 = 13 if C012_Other_dec== "He pays electricity himself"
		replace C012_Other_dec="" if C012_Other_dec== "He pays electricity himself"
		
		replace C012 = 13 if C012_Other_dec== "They buy themselves"
		replace C012_Other_dec="" if C012_Other_dec== "They buy themselves"
		
		replace C012 = 13 if C012_Other_dec== "They pay themselves"
		replace C012_Other_dec="" if C012_Other_dec== "They pay themselves"
						
		label define C012  1 "Distribution Company" 2 "Pre-paid meter card seller" 3 "Community/village/municipality" 4 "Relative" 5 "Neighbor" 6 "Landlord" 7 "Local store" 9 "Bank" 10 "Post office" 11 "Service provider using mobile money" 12 "No electricity" 13 "Paying to someone but not specified" 555 "Other", modify
		//label values C012 C012
				
* 1. Set up

	for var C012 C052: recode X . = 0 // check
	gen elec_pay = (C012!=0 & C012!=111 & C171==1)|(C052!=0 & C052!=7 & C171==2) 
	//gen elec_pay = (C012!=0 & C012!=111 & maybemore?? & C171==1)|(C052!=0 & C052!=7 & C171==2) // check
	replace elec_pay=0 if C012==12
	replace elec_pay=. if C171!=1 & C171!=2
	ta elec_pay, m 
					
* 2. Tier calculation

	gen elc_leg_tier0 = elec_pay==. 
	gen elc_leg_tier1 = elec_pay==.
	gen elc_leg_tier2 = elec_pay==.
	gen elc_leg_tier3 = elec_pay==0
	gen elc_leg_tier4 = elec_pay==. 
	gen elc_leg_tier5 = elec_pay==1

	gen elc_leg_tier=3 if elc_leg_tier3==1 
	replace elc_leg_tier=5 if elc_leg_tier5==1
	label values elc_leg_tier tier

	la var elc_leg_tier0 "Electricity: formality Tier 0"
	la var elc_leg_tier1 "Electricity: formality Tier 1"
	la var elc_leg_tier2 "Electricity: formality Tier 2"
	la var elc_leg_tier3 "Electricity: formality Tier 3"
	la var elc_leg_tier4 "Electricity: formality Tier 4"
	la var elc_leg_tier5 "Electricity: formality Tier 5"
	la var elc_leg_tier "Electricity: formality Tier: Categorical variable"
		
	ta elc_leg_tier [aw=HH_WT]
	bys HI04: ta elc_leg_tier [aw=HH_WT]
	bys T: ta elc_leg_tier [aw=HH_WT]
	ta elc_leg_tier if T==2 [aw= ref_weight]
	
	
	
***************************** F. SAFETY TIER *********************************

// GRID
	* C39: In the last 12 months, did any household members die or have permanent limb (bodily injury) damage because of the grid electricity?
// MINI GRID
	* C80
// ELECTRIC GENERATOR
	* C104
// RECHARGEABLE BATTERY
	* C124
					
* 1. Set up
 
	for var C039 C080 C104 C124: recode X . = 0 // check
	gen elec_bdy = (C039==1 & C171==1)|(C080==1 & C171==2)| (C104==1 & C171==3) | (C124==1 & C171==7)  // not asked for solar
	replace elec_bdy =. if C171!=1 & C171!=2 & C171!=3 & C171!=7

* 2. Tier calculation 

	gen elc_sft_tier0 = elec_bdy==. 
	gen elc_sft_tier1 = elec_bdy==.
	gen elc_sft_tier2 = elec_bdy==.
	gen elc_sft_tier3 = elec_bdy==1 
	gen elc_sft_tier4 = 0 
	gen elc_sft_tier5 = elec_bdy==0

	gen elc_sft_tier=3 if elc_sft_tier3==1 
	replace elc_sft_tier=5 if elc_sft_tier5==1
	label values elc_sft_tier tier

	la var elc_sft_tier0 "Electricity: safety Tier 0"
	la var elc_sft_tier1 "Electricity: safety Tier 1"
	la var elc_sft_tier2 "Electricity: safety Tier 2"
	la var elc_sft_tier3 "Electricity: safety Tier 3"
	la var elc_sft_tier4 "Electricity: safety Tier 4"
	la var elc_sft_tier5 "Electricity: safety Tier 5"
	la var elc_sft_tier  "Electricity: Safety Tier: Categorical variable"
	
	ta elc_sft_tier [aw=HH_WT]
	bys HI04: ta elc_sft_tier [aw=HH_WT]
	bys T: ta elc_sft_tier [aw=HH_WT]
	ta elc_sft_tier  if T==2 [aw= ref_weight]
	
	
	
***************************** G. AGGREGATE TIER ********************************				
// affordability tier skipped			

* Tier calculation

	egen elc_aggr_tier=rmin(elc_cap_tier elc_durd_tier elc_dure_tier elc_rel_tier elc_qual_tier elc_leg_tier elc_sft_tier) 
	label values elc_aggr_tier tier

	gen elc_aggr_tier0=elc_aggr_tier==0 
	gen elc_aggr_tier1=elc_aggr_tier==1
	gen elc_aggr_tier2=elc_aggr_tier==2
	gen elc_aggr_tier3=elc_aggr_tier==3
	gen elc_aggr_tier4=elc_aggr_tier==4 
	gen elc_aggr_tier5=elc_aggr_tier==5

	la var elc_aggr_tier0 "Electricity: aggregate Tier 0"
	la var elc_aggr_tier1 "Electricity: aggregate Tier 1"
	la var elc_aggr_tier2 "Electricity: aggregate Tier 2"
	la var elc_aggr_tier3 "Electricity: aggregate Tier 3"
	la var elc_aggr_tier4 "Electricity: aggregate Tier 4"
	la var elc_aggr_tier5 "Electricity: aggregate Tier 5"
	la var elc_aggr_tier "Electricity: aggregate Tier: Categorical variable"
	
	ta elc_aggr_tier [aw=HH_WT]
	bys HI04: ta elc_aggr_tier [aw=HH_WT]
	bys B13: ta elc_aggr_tier [aw=HH_WT]
	bys T: ta elc_aggr_tier [aw=HH_WT]
	ta elc_aggr_tier if T==2 [aw= ref_weight]
	
	// Agg. Tier, by technology
		bys elc_aggr_tier: ta C171 [aw=HH_WT]
		/*//OR
		estpost ta elc_aggr_tier C171 [aw=HH_WT]
		putexcel set "$output\report.xlsx" , sheet("Tier_Dist_Tech") modify
		mata: st_matrix("tier_tech",rowshape(st_matrix("tier_tech"),9))
		mat colnames tier_tech="Tier 0" "Tier 1" "Tier 2" "Tier 3" "Tier 4" "Tier 5" "Total"
		mat rownames tier_tech="Nat grid" "Mini grid" "Solar Lantern" "SLS" "SHS" "Rechargeable battery" "Dry-cell battery" "No electricity" "Total"
		matlist tier_tech
		putexcel A1=matrix(tier_tech),names*/
	
save "$wip_hh\SECTION_C1.dta", replace 






********************************************************************************
************************** COOKING TIER CALCULATION ****************************
********************************************************************************

					*---------------------------------*
					*          Exposure Tier          *  
					*---------------------------------*
* restricting to the main stove
		
	use "$wip_hh\SECTION_G2.dta", clear
	sort hhid
	save "$wip_hh\SECTION_G2.dta", replace
	
	use "$wip_hh\SECTION_G1_onlymainstove.dta", clear		
	sort hhid
	drop _m
	merge hhid using "$wip_hh\SECTION_G2.dta"
					
* A) Emission: Stove Design (all stoves) 

	/* 
	
	// G03: Other cookstove cleaning
	decode G04_OTHER, gen(G04_other_decoded)
	replace G03=1 if G04_other_decoded=="Runonko"
	replace G03=2 if G04_other_decoded=="Rondereza"|G04_other_decoded=="All metal"|G04_other_decoded=="Metal Charcoal"|G04_other_decoded=="Charcoal stove"
	replace G03=3 if G04_other_decoded=="Canarumwe"|G04_other_decoded=="Ecozoom rocket"|G04_other_decoded=="Ecozoom Dura"|G04_other_decoded=="Ecozoom Jik"|G04_other_decoded=="ECOZOOM"|G04_other_decoded=="Ruliba clay"
	
	// G10: "Other" cleaning
	replace G10=6 if G10_O=="all metal"& G04_other_decoded=="All metal"
	replace G10=15 if G10_O=="all metal"& G04_other_decoded=="ECOZOOM"
	replace G10=9 if G10_O=="all metal"& G04_other_decoded=="Canarumwe"
	
	*/
	
	gen mainstove=G03
	lab var mainstove "Type of Main Stove"
	label define mainstove 1 "3-Stones/Open fire stove"   2 "Traditional/Locally built stove"    3   "Manufactured biomass stove"   4 "Kerosene stove"     5 "LPG stove"  6 "Piped Natral Gas stove"           7   "Electric stove"   8 "Solar cooker"   9 "Other cookstove", modify
	label val mainstove mainstove
	save "$wip_hh\SECTION_G1_onlymainstove.dta", replace
	
	use "$wip_hh\SECTION_G1_onlymainstove.dta", clear		

	/* G10: stove model
	
           1   1. Amabuye atatu (3-stones) // tier 0
           2   2. Round mud stove // tier 1
           3   3. Gisafuriya stove // tier 1
           4   4. Rocket stove // tier 1
           5   5. Double place Metal stove // tier 1
           6   6. All Metal charcoal stove // tier 1
           7   7. Darfour stove // tier 1
           8   8. Mimi Moto // tier 4
           9   9. Claded Canarumwe // tier 1
          10   10. Uncladed Canarumwe // tier 1
          11   11. Canamake // tier 1
          12   12. Installed Canarumwe ???? on hold; for now calculated as Tier 1
          13   13. Ruliba Clay // tier 2
          14   14. Save 80 // tier 3
          15   15. Ecozoom // tier 2
          16   16. Jiko Malkia // tier 1
          17   17. Mahwi // tier 1
          18   18. Ecozoom Jiko Bora Mama Yao PNG 40 // tier 2
          19   19. ECOZOOM Dura Rocket stove // tier 2
          20   20. Green Way Jumbo // tier 2
          21   21. Ruliba Clay // tier 2
          22   22. Gisubizo S26-13 // tier 2
          23   23. Ecozoom Jiko fresh // tier 0
          24   24. JIKO Malkia // tier 1
          25   25. SONGA stove // tier 2
          26   26. Gisubizo C28-23 Max // tier 1-2 ; no obs 
          27   27. AJDR Charcoal stove // tier 1
          28   28. Igisubizo // tier 1
          29   29. ZIGAMA stove // tier 2
          30   30. Umurabyo // // tier 0
          31   31. KOKO cooker // tier 5
          32   32. Blu Stove Setar // tier 5
          33   33. Ecoca stove // tier 5
          34   34. All LPG stoves // tier 5
          35   35. All Electric stoves // tier 5
          36   36. All biogas stoves // tier 3
          37   37. Self-built (Twikoreye) // no obs
         555   38. Other (Irindi) // no obs

	*/
	
	gen cs_emis_tier=.
			
	replace cs_emis_tier=0 if inlist(G10,1,23,30)|G03==1
	
	replace cs_emis_tier=1 if inlist(G10,2,3,4,5,6,7,9,10,11,12,16,17,24,27,28) 
	
	replace cs_emis_tier=2 if inlist(G10,13,15,18,19,20,21,22,25,29)
	
	replace cs_emis_tier=3 if inlist(G10,14,36)
	
	replace cs_emis_tier=4 if inlist(G10,8)
	
	replace cs_emis_tier=5 if inlist(G10,31,32,33,34,35)
	
	label var cs_emis_tier "Cooking (Stove): Emission" 
	label define cs_emis_tier 0"Tier 0" 1"Tier 1" 2 "Tier 2" 3"Tier 3" 4"Tier 4" 5 "Tier 5", modify	  					  
	label val cs_emis_tier cs_emis_tier	
	
	ta cs_emis_tier, m 
	
	
* B) Ventilation: 
	
	** Structure

			* Tier 0: no opening except for the door
			* Tier 1: 2 openings
			* Tier 2: 3+ openings
			* Tier 3: Indoor cooking with an exhaust system (chimney)
			* Tier 4: Veranda or Indoor cooking with a forced exhaust system (fan, or hood)
			* Tier 5: open air
			
			*** G13 In the last 12 months, where did you normally cook with [STOVE]?
				   * 1 In dwelling, NOT in sleeping area
				   * 2 In dwelling, in a sleeping area
				   * 3 In a separate kitchen
				   * 4 In a veranda (roofed platform with at least two open sides)
				   * 5 Outdoors
				   * 555 Other, specify
				   
				 /*
				 // G13: "Other, specify" cleaning
				replace G13=3 if G13_OTHER=="In kichen"|G13_OTHER=="In sitting room"|G13_OTHER=="Kitchen"|G13_OTHER=="Plastic sheetin"
				replace G13_O="" if G13_OTHER=="In kichen"|G13_OTHER=="In sitting room"|G13_OTHER=="Kitchen"|G13_OTHER=="Plastic sheetin"
				
				replace G13=5 if G13_OTHER=="Near the kraal"
				replace G14=. if G13_OTHER=="Near the kraal"
				replace G13_O="" if G13_OTHER=="Near the kraal"
				*/
				  				
			*** G14 What exhaust system, in working condition, do you use with [STOVE]? 
					* 1   Chimney
					* 2   Hood
					* 3   Chimney and Hood
					* 4   Chimney and other
					* 5   Hood and other
					* 6   All exhaust system
					* 111   No exhaust system
					* 555   Other, specify
				
				/*
				// G14: "Other, specify" cleaning
				decode G14_OTHER, gen(G14_Other_decoded)
				replace G13=5 if G14_Other_decoded=="It is fully open"|G14_Other_decoded=="Outside"|G14_Other_decoded=="We cook outside"
				replace G14=. if G14_Other_decoded=="It is fully open"|G14_Other_decoded=="Outside"|G14_Other_decoded=="We cook outside"
				replace G14_Other_decoded="" if G14_Other_decoded=="It is fully open"|G14_Other_decoded=="Outside"|G14_Other_decoded=="We cook outside"
				
				replace G14=1 if G14_Other_decoded =="Cheminey"
				replace G14_Other_decoded ="" if G14_Other_decoded =="Cheminey"
				
				replace G14=111 if G14_Other_decoded=="Behind, trough holes"|G14_Other_decoded=="Ceiling"|G14_Other_decoded=="Door"|G14_Other_decoded=="Door & Window"|G14_Other_decoded=="Holes"|G14_Other_decoded=="Holes between bricks"|G14_Other_decoded=="No smoke exit"|G14_Other_decoded=="We cook with open doors"|G14_Other_decoded=="We just take off one brick of the wall"|G14_Other_decoded=="Window"
				replace G14_Other_decoded ="" if G14_Other_decoded=="Behind, trough holes"|G14_Other_decoded=="Ceiling"|G14_Other_decoded=="Door"|G14_Other_decoded=="Door & Window"|G14_Other_decoded=="Holes"|G14_Other_decoded=="Holes between bricks"|G14_Other_decoded=="No smoke exit"|G14_Other_decoded=="We cook with open doors"|G14_Other_decoded=="We just take off one brick of the wall"|G14_Other_decoded=="Window"
			*/
			
			*** G52 How many doors and windows (opening  outside) does the cooking space have? [Assuming that the cooking location is same even though they use multiple stoves]

			gen vent_strc=.
			replace vent_strc=5 if G13==5 | mainstove==5 | mainstove==6 | mainstove==7 | mainstove==8
			replace vent_strc=4 if G13==4
			replace vent_strc=3 if (G13==1 | G13==2 | G13==3) & inlist(G14,1,2,3,4,5,6) 
			replace vent_strc=2 if (G13==1 | G13==2 | G13==3) & G52>=2 & G52!=888 & G52!=. & vent_strc==. // 2 or more openings 
			replace vent_strc=1 if (G13==1 | G13==2 | G13==3) & G52==1 & G52!=888 & vent_strc==. // 1 openings
			replace vent_strc=0 if (G13==1 | G13==2 | G13==3) & G14==111 & G52==0 & G52!=888  & vent_strc==. // zero opening, no chimney or hood
			//replace vent_strc=0 if (G13==1 | G13==2 | G13==3) & G14==111 & G52==. & G52!=888  & vent_strc==.  // no chimney or hood, # of window info missing 

			label var vent_strc "Cooking (Stove): Ventilation Structure"
			label define vent_strc 0"Tier 0" 1"Tier 1" 2"Tier 2" 3"Tier 3" 4"Tier 4" 5"Tier 5", modify		  					  
			label values vent_strc vent_strc
			
			tab vent_strc, m	
						
	** Ventilation Tier without Kitchen

		* Good ventilation: inlist(vent_strc,4,5),vent_strc is vent_tier 
		* Average ventilation: inlist(vent_strc,3)
		* Bad ventilation: inlist(vent_strc,0,1,2)

		gen vent_tier=.
		replace vent_tier=2 if inlist(vent_strc,0,1,2) 
		replace vent_tier=3 if inlist(vent_strc,3) 
		replace vent_tier=5 if inlist(vent_strc,4,5)
		
		label var vent_tier "Cooking (Stove): Ventilation Level"
		label define vent_tier 2"Bad" 3"Average" 5"Good", modify		  					  
		label values vent_tier vent_tier
			
		tab vent_tier	
		
* C) Exposure Tier Calculation

		gen cs_exp_tier=.

		*** When Emission Tier = 5

		replace cs_exp_tier=5 if cs_emis_tier==5

		*** Emission tier 4 

		replace cs_exp_tier=5 if cs_emis_tier==4 & inlist(vent_tier,4,5) 
		replace cs_exp_tier=4 if cs_emis_tier==4 & inlist(vent_tier,0,1,2,3) 
		
		
		*** Emission tier 3 

		replace cs_exp_tier=4 if cs_emis_tier==3 & inlist(vent_tier,4,5) 
		replace cs_exp_tier=3 if cs_emis_tier==3 & inlist(vent_tier,3) 
		replace cs_exp_tier=2 if cs_emis_tier==3 & inlist(vent_tier,0,1,2)

		*** Emission tier 2 

		replace cs_exp_tier=3 if cs_emis_tier==2 & inlist(vent_tier,4,5) 
		replace cs_exp_tier=2 if cs_emis_tier==2 & inlist(vent_tier,3) 
		replace cs_exp_tier=1 if cs_emis_tier==2 & inlist(vent_tier,0,1,2) 

		*** Emission tier 1 

		replace cs_exp_tier=2 if cs_emis_tier==1 & inlist(vent_tier,4,5)
		replace cs_exp_tier=1 if cs_emis_tier==1 & inlist(vent_tier,3) 
		replace cs_exp_tier=0 if cs_emis_tier==1 & inlist(vent_tier,0,1,2)

		*** Emission tier 0 

		replace cs_exp_tier=1 if cs_emis_tier==0 & inlist(vent_tier,4,5) 
		replace cs_exp_tier=0 if cs_emis_tier==0 & inlist(vent_tier,0,1,2,3)
				
		*** For those cs_exp_tier==. (vent_tier==.)
		replace cs_exp_tier=cs_emis_tier if cs_exp_tier==. & vent_tier==. // double check if using this code is right		
				
		label var cs_exp_tier "Cooking (Stove): Exposure Tier"
		label define cs_exp_tier 0"Tier 0" 1"Tier 1" 2"Tier 2" 3"Tier 3" 4"Tier 4" 5"Tier 5", modify	  					  
		label values cs_exp_tier cs_exp_tier
		ta cs_exp_tier,m
		
		/*
		
		ta cs_exp_tier,m

			Cooking |
		   (Stove): |
		   Exposure |
			   Tier |      Freq.     Percent        Cum.
		------------+-----------------------------------
			 Tier 0 |      2,862       50.16       50.16
			 Tier 1 |      1,494       26.18       76.34
			 Tier 2 |        517        9.06       85.40
			 Tier 3 |        141        2.47       87.87
			 Tier 4 |         87        1.52       89.40
			 Tier 5 |        602       10.55       99.95
				  . |          3        0.05      100.00
		------------+-----------------------------------
			  Total |      5,706      100.00

		*/
		
/*	[IMPORTANT] Missing value imputation not conducted for the exposure tier. The 3 households missing the exposure tier will be excluded from the result. (Feb 15) 

	*Missing value imputation (3 obs) : imputed using the mode value of each stove; for the cases with multiple modes, we take the minimum value.
	
	egen exp_traditional_mode = mode(cs_exp_tier)	if inlist(mainstove,2), min 
	egen exp_manufacturedbio_mode = mode(cs_exp_tier)	if inlist(mainstove,3), min 
	//egen exp_kerosene_mode = mode(cs_exp_tier)	if inlist(mainstove,4), min 

	replace cs_exp_tier = exp_traditional_mode if inlist(mainstove,2) & cs_exp_tier==.
	replace cs_exp_tier = exp_manufacturedbio_mode if inlist(mainstove,3) & cs_exp_tier==.
	//replace cs_exp_tier = exp_kerosene_mode if inlist(mainstove,4) & cs_exp_tier==.
	
	*/
	
	save "$wip_hh\SECTION_G1_onlymainstove.dta", replace
	
	ta cs_exp_tier [aw=HH_WT]
	bys HI04: ta  cs_exp_tier [aw=HH_WT]
	bys T: ta  cs_exp_tier [aw=HH_WT]
	ta cs_exp_tier if T==2		
			

					
					*----------------------------------*
					*         Convenience Tier         *  
					*----------------------------------*

	/* 
	I17: In a typical day, how many minutes in total did [PEOPLE] spend gathering, collecting or purchasing fuels for the household and home-based income generating activities including travel time? 
	I19: In a typical day, how many minutes in total do [PEOPLE] spend on fuel preparationi..i.e. combined time ofchopping wood, igniting wood for starter, turning on the stove
	G17: How much time do household members spend preparing the [STOVE] and fuel for each meal on average [including setting up the fuel and turning on the stove but excluding gathering fuel and cooking time]?
	*/
	
	* Fuel acquisition (through collection or purchase) and preparation time (hours per week)
			* fuel collection time + fuel preparation time
			* Tier 1: more than or equal to 7 hours
			* Tier 2: less than 7 hours
			* Tier 3: less than 3 hours
			* Tier 4: less than 1.5 hours
			* Tier 5: less than 0.5 hours
					
	* [SET UP] I17 I19 
	
		// Merge I17,I19 with G dataset
	
		use "$wip_hh\SECTION_I2J.dta", clear // be aware of the change in file name
		keep hhid I*
		sort hhid
		save "$wip_hh\SECTION_I_I17-I19_only.dta", replace
		
		use "$wip_hh\SECTION_G1_onlymainstove.dta", clear
		drop _m
		sort hhid
		merge hhid using "$wip_hh\SECTION_I_I17-I19_only.dta"
		save "$wip_hh\SECTION_G1_onlymainstove.dta", replace
	
		// fuel acquisition
		replace I17A=. if I17A==888
		replace I17B=. if I17B==888
		replace I17C=. if I17C==888
		replace I17D=. if I17D==888
		replace I17A=0 if I17A==.
		replace I17B=0 if I17B==.
		replace I17C=0 if I17C==.
		replace I17D=0 if I17D==.

		gen I17_total=I17A+I17B+I17C+I17D
		lab var I17_total "Fuel acquisiton: sum of I17s"

		// fuel prep
		replace I19A=. if I19A==888
		replace I19B=. if I19B==888
		replace I19C=. if I19C==888
		replace I19D=. if I19D==888
		replace I19A=0 if I19A==.
		replace I19B=0 if I19B==.
		replace I19C=0 if I19C==.
		replace I19D=0 if I19D==.

		gen I19_total=I19A+I19B+I19C+I19D
		lab var I19_total "Fuel prep: sum of I19s"

	* [SET UP] Total time on fuel 
	
		gen fcol_time_week=(I17_total/60)*7  // acquisition hr per week
		gen fprep_time_week=(I19_total/60)*7 // prep hr per week
		lab var fcol_time_week "Household members' total fuel collection hour per week"
		lab var fprep_time_week "Household members' total fuel prep hour per week"
		
		gen cs_fcol_fpre = fcol_time_week + fprep_time_week
		label var cs_fcol_fpre "Fuel collection + preparation time (hours/week)"
		
			
	* [TIER CALC] Fuel time tier calculation
	
		gen cs_fcol_tier=.
		replace cs_fcol_tier=1 if cs_fcol_fpre >= 7 & cs_fcol_fpre!=. 
		replace cs_fcol_tier=2 if cs_fcol_fpre >= 3 & cs_fcol_fpre!=. & cs_fcol_tier==.
		replace cs_fcol_tier=3 if cs_fcol_fpre >= 1.5 & cs_fcol_fpre!=. & cs_fcol_tier==.
		replace cs_fcol_tier=4 if cs_fcol_fpre >= 0.5 & cs_fcol_fpre!=. & cs_fcol_tier==.
		replace cs_fcol_tier=5 if cs_fcol_fpre <0.5 & cs_fcol_fpre!=. & cs_fcol_tier==.
					
		replace cs_fcol_tier=5 if inlist(mainstove,5,6,7,8) 
				
		label var cs_fcol_tier "Cooking (Stove): Fuel Collection + Preparation Time"
		label define cs_fcol_tier 1"Tier 1: >=7 hrs/week" 2"Tier 2: 3=< and <7 hrs/week" 3"Tier 3: 1.5=< and <3 hrs/week"	4 "Tier 4: 0.5=< and <1.5 hrs/week" 5 "Tier 5: < 0.5 hrs/week", modify
		label values cs_fcol_tier cs_fcol_tier
		
		ta cs_fcol_tier [aw=HH_WT]
		bys HI04: ta cs_fcol_tier [aw=HH_WT]
	
	* [SET UP] Stove preparation time (minutes per meal) 
	
		* Tier 1: 10 mins or more
		* Tier 3: less than 10 mins
		* Tier 4: less than 5 mins
		* Tier 5: less than 2 mins
			
		** G17. How much time do household members spend preparing the [STOVE] and fuel for each meal on average [including setting up the fuel and turning on the stove but excluding gathering fuel and cooking time]? 
	
		ta G17 if G04==1&inlist(mainstove, 5,6,7,8) // clean stoves - good
		
	* [TIER CALC] Stove prep tier
	
		gen cs_prep_tier=.
		replace cs_prep_tier=2 if G17>=10 & G17!=. & G17!=888 
		replace cs_prep_tier=3 if G17<10 & G17!=. & G17!=888 
		replace cs_prep_tier=4 if G17<5 & G17!=.  & G17!=888
		replace cs_prep_tier=5 if G17<2 & G17!=. & G17!=888 
		
		// Natural Piped Gas/LPG/electric stoves
		replace cs_prep_tier=5 if inlist(mainstove,5,6,7,8) // check check
		
		label var cs_prep_tier "Cooking (Stove): Stove Preparation Time"
		label define cs_prep_tier 2 "Tier 2: >= 10 mins/meal" 3"Tier 3: 5=< and <10 mins/meal"	///
							4"Tier 4: 2=< and <5 mins/meal" 5 "Tier 5: <2 mins/meal", modify
		label values cs_prep_tier cs_prep_tier
		
		ta cs_prep_tier [aw=HH_WT]
		bys HI04: ta cs_prep_tier [aw=HH_WT]
		
	* [TIER CALC] Convenience tier calcuation
	
	label define cs_fcol_tier 1"Tier 1: >=7 hrs/week" 2"Tier 2: 3=< and <7 hrs/week" 3"Tier 3: 1.5=< and <3 hrs/week"	4 "Tier 4: 0.5=< and <1.5 hrs/week" 5 "Tier 5: < 0.5 hrs/week", modify
		
	/*
		
		Tier 1 – HR_fuel >7 hrs per week & HG7>=10 minutes per meal 
		Tier 2 – 7< HR_fuel <3 hrs/week & 15< HG7<10 mins/meal 
		Tier 3 – 3< HR_fuel <1.5 hrs/week & 10< HG7<5 mins/meal 
		Tier 4 – 1.5< HR_fuel <0.5 hrs/week  & 5< HG7<2 mins/meal 
		Tier 5 – HR_fuel <0.5 hrs/week  & HG7<2  mins/meal 
		
		Tier 1 – cs_fcol_tier==1 & cs_prep_tier==2
		Tier 2 – cs_fcol_tier==2 & cs_prep_tier==3
		Tier 3 – cs_fcol_tier==3 & cs_prep_tier==3
		Tier 4 – cs_fcol_tier==4 & cs_prep_tier==4
		Tier 5 – cs_fcol_tier==5 & cs_prep_tier==5
		
																	*/
		
		egen cs_conv_tier=rowmin(cs_prep_tier cs_fcol_tier) if cs_prep_tier!=.& cs_fcol_tier!=.
	/*	gen cs_conv_tier2=.
		forval i=1/5 {
		replace cs_conv_tier2=`i' if (cs_prep_tier==`i' & cs_fcol_tier>=`i') | (cs_prep_tier>=`i' & cs_fcol_tier==`i') 
				} */
		replace cs_conv_tier=5 if inlist(mainstove,5,6,7,8) 
		
		label var cs_conv_tier "Cooking (Stove): Convenience (fuel acquisition and preparation time + stove preparation time)"
		label define cs_conv_tier 1"Tier 1" 2"Tier 2" 3"Tier 3" 4"Tier 4" 5"Tier 5", modify
		label values cs_conv_tier cs_conv_tier
			
		save "$wip_hh\SECTION_G1_onlymainstove.dta", replace	
			
		ta cs_conv_tier [aw=HH_WT]
		bys HI04: ta  cs_conv_tier [aw=HH_WT]
		bys T: ta  cs_conv_tier [aw=HH_WT]
		ta cs_conv_tier if T==2		[aw=ref_w]
			
					
					*--------------------------------*
					*          Safety Tier           *  
					*--------------------------------*
					
	* based on the main stove
		* Tier 0: Death
		* Tier 2: Serious  
		* Tier 3: Minor 
		* Tier 5: None 
		
		use "$wip_hh\SECTION_G1_onlymainstove.dta", clear

		
	* G24.In the last 12 months, what type of harm/injury/damage did your household experience from [STOVE]? Multiple responses possible.
				
		gen cs_sfty_tier=.
		replace cs_sfty_tier=0 if G24_1==1 // death 
		replace cs_sfty_tier=2 if inlist(1,G24_2,G24_3,G24_4) // poisoning/burns/fire, severe cough and major injury
		replace cs_sfty_tier=3 if inlist(1,G24_5,G24_6,G24_7,G24_8) // minor injury, fire with no injury, itchy/watery eyes, light cough
		replace cs_sfty_tier=5 if cs_sfty_tier==. & inlist(1,G24_9) // none 
		
		
		label var cs_sfty_tier "Cooking (Stove): Safety Tier"
		label define cs_sfty_tier 0 "Tier 0: Death" 2 "Tier 2: Serious" 3 "Tier 3: Minor" 5 "Tier 5: None", modify
		label values cs_sfty_tier cs_sfty_tier
			
		save "$wip_hh\SECTION_G1_onlymainstove.dta", replace
	
			
		ta cs_sfty_tier [aw=HH_WT]
		bys HI04: ta  cs_sfty_tier [aw=HH_WT]
		bys T: ta  cs_sfty_tier [aw=HH_WT]
		ta cs_sfty_tier if T==2	[aw=ref_w]	
					
					
					*-------------------------------------*
					*          Affordability Tier         *  skipped
					*-------------------------------------*
										
					
					*---------------------------------*
					*        Availability Tier        *  
					*---------------------------------*
					
	use "$wip_hh\SECTION_G1_onlymainstove.dta", clear
			
					
* Fuel availability  : Fuel Availability (G16A) - only for primary fuel for the main stove.
** G16A: In the last 12 months, how often was the [FUEL TYPE] available? (asked to mainstove 1,2,3,4)

		* Tier 2: Rarely Available
		* Tier 3: Sometimes Available
		* Tier 4: Mostly Available
		* Tier 5: Always Available
		
	gen cs_avbl_tier=.
	replace cs_avbl_tier=2 if inlist(G16A,4)	//rarely availble
	replace cs_avbl_tier=3 if inlist(G16A,3)	//sometimes available 
	replace cs_avbl_tier=4 if inlist(G16A,2)	//mostly available
	replace cs_avbl_tier=5 if inlist(G16A,1)	//always available
	
	label var cs_avbl_tier "Cooking (Stove): Availability"
	label define cs_avbl_tier 2 "Tier 2: Rarely available" 3 "Tier 3: Sometimes Available" 4 "Tier 4: Mostly Available"	5"Tier 5: Always Available", modify
	label values cs_avbl_tier cs_avbl_tier
	
** Missing value imputation : imputed using the mode value of each stove; for the cases with multiple modes, we take the minimum value.
	
	egen avl_threestone_mode = mode(cs_avbl_tier)	if inlist(mainstove,1), min 
	egen avl_traditional_mode = mode(cs_avbl_tier)	if inlist(mainstove,2), min 
	egen avl_manufacturedbio_mode = mode(cs_avbl_tier)	if inlist(mainstove,3), min 
	//no missing for kerosene

	replace cs_avbl_tier = avl_threestone_mode if inlist(mainstove,1) & cs_avbl_tier==.
	replace cs_avbl_tier = avl_traditional_mode if inlist(mainstove,2) & cs_avbl_tier==.
	replace cs_avbl_tier = avl_manufacturedbio_mode if inlist(mainstove,3) & cs_avbl_tier==.
	
	save "$wip_hh\SECTION_G1_onlymainstove.dta", replace

	
	ta cs_avbl_tier [aw=HH_WT]
	bys HI04: ta  cs_avbl_tier [aw=HH_WT]
	bys T: ta  cs_avbl_tier [aw=HH_WT]
	ta cs_avbl_tier if T==2		[aw=ref_w]
	
	
					*------------------------------*
					*        Aggregate Tier        *  
					*------------------------------*
	
	use "$wip_hh\SECTION_G1_onlymainstove.dta", clear

	
	egen cs_aggr_tier=rmin(cs_exp_tier cs_conv_tier cs_sfty_tier cs_avbl_tier) // affordability tier excluded
	
	label var cs_aggr_tier "Cooking: Aggregate Tier"
	label define cs_aggr_tier 0"Tier 0" 1"Tier 1" 2"Tier 2" 3"Tier 3" 4"Tier 4" 5"Tier 5", modify
	label values cs_aggr_tier cs_aggr_tier
	
	save "$wip_hh\SECTION_G1_onlymainstove.dta", replace


