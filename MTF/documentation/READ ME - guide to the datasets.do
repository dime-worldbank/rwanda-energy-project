* Dataset structures: Most are in household level, but some are not. Please check questionnaires to distinguish them. (For example, the dataset "SECTION_C2" is a roster dataset covering questions from C128 to C151 in the C module.)


* Household ID variable: HH_ID 
	// The raw datasets do not have a household ID variable, so we created one with the following code for the analysis.
	gen long hhid = (HI05*100) + (Cluster*100*1000) + (DISTRICT*100*1000*100) + PROVINCE

	
* Household types variable: TYPEHH
	/* 
		Freq.   Numeric  Label
		4,000       1  Ordinary HH
		700         2  Refugee HH
		1,006       3  HH Surrounding Camp 
	*/ 

	
* Nationally representative sample weight variable: HH_WT 
	// HH_WT is the sample weight. Please disregard the variable "C_hhweight". 
	// For the nationally representative samples, which are ordinary households (TYPEHH==1) and households surrounding the refugee camps (TYPEHH==3), you can use HH_WT for the weight.
	// For the refugee households, they have a separate sample weight. Please see below.

	
* Refugee sample weight: ref_weight
	// Please use the following codes to set up the sample weight only for the refugee samples (TYPEHH==2)

	** Create the refugee settlement variable.
		gen camp = .
		replace camp = 1 if T==2 & DIS== 25
		replace camp = 2 if T==2 & DIS== 31
		replace camp = 3 if T==2 & DIS== 55
		replace camp = 4 if T==2 & DIS== 22
		replace camp = 5 if T==2 & DIS== 53
		lab define camp 1 "Kigeme Camp" 2 "Kiziba Camp" 3 "Mahama Camp" 4 "Mugombwa Camp" 5 "Nyabiheke Camp", modify
		lab val camp camp
		lab var camp "Refugee camp names"
		
	** Create the refugee weight variable.
		gen ref_weight=.
		replace ref_weight= 1 / (140 / 3025) if camp==1 // Kigeme camp
		replace ref_weight= 1 / (144 / 3223) if camp==2 // Kiziba camp
		replace ref_weight= 1 / (146 / 15481) if camp==3 // Mahama camp
		replace ref_weight= 1 / (126 / 2269) if camp==4 // Mugombwa camp
		replace ref_weight= 1 / (144 / 2672) if camp==5 // Nyabiheke camp
		lab var ref_weight "Weight for refugee households"