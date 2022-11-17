********************************************************************************
**	TITLE: 		03_Ethiopia_ESS3_manip.do
**
**	PURPOSE: 	Manipulates variables based on poverty rates
**
**	INPUTS: 	${clean}/MASTER_2016_Ethiopia_full.dta
**
** 	OUTPUTS: 	${clean}/PPI_2016_Ethiopia_full.dta
**
**	AUTHOR: 	Manuel Cardona 
**
**	CREATED:	
********************************************************************************
*Table of Contents
*1. 
*2. 


loc cdate: di %td_CCYY_NN_DD date(c(current_date), "DMY")
loc cdate = subinstr(trim("`cdate'"), " " , "", .)
log using "${logs}/03_Ethiopia_ESS3_manip_`cdate'.smcl", replace
di `cdate'

******************************************
******************************************
************PPI - Indicators**************
******************************************
******************************************
u "$clean/ESS3_2016.dta", clear 

******************************************
*** Manipulate indicators based on NPL ***
******************************************

* Drop variables that are problematic
drop hh_rel hhead_religion hhead_marital hh_jobagri hh_jobbusiness hh_jobcasual hh_jobwage hh_jobunpaid hh_ATMDebit hh_ebanking hh_mobbanking hh_agentbanking hh_enoughfood-hh_fs8

* Region
fre region
gen region_c=region
replace region=1 if region_c==3
replace region=2 if region_c==4
replace region=3 if region_c==7
replace region=4 if region_c==1
replace region=5 if inlist(region_c, 2, 5, 6, 12, 13, 14, 15)
label define reg	1 "Amhara"	///
					2 "Oromia"	///
					3 "SNNP"	///
					4 "Tigray"	///
					5 "Other regions"
label values region reg 
tab region, sum(poor_npl1)
drop region_c

	* Household size
	tab hh_size, sum(poor_npl1)
	gen hh_size_c=hh_size
	replace hh_size=1 if hh_size_c<=3
	replace hh_size=2 if hh_size_c>=4 & hh_size<=5
	replace hh_size=3 if hh_size_c>=6
	label define siz	1 "3 or less"	///
						2 "4 or 5"		///
						3 "6 or more"
	label values hh_size siz
	drop hh_size_c
	tab hh_size, sum(poor_npl1)
	
* Sex of the HH Head
tab hhead_sex, sum(poor_npl1)

	* Age of the HH Head
	tab hhead_age, sum(poor_npl1)
	gen hhead_age_c=hhead_age
	replace hhead_age=1 if hhead_age_c<=35
	replace hhead_age=2 if hhead_age_c>=36 & hhead_age_c<=54
	replace hhead_age=3 if hhead_age_c>=55
	label define ages	1 "35 or below"	///
						2 "36-54"		///
						3 "55 or more"
	label values hhead_age ages
	drop hhead_age_c
	tab hhead_age, sum(poor_npl1)
	
* How many members of the HH can read and write?
tab hh_readwrite, sum(poor_npl1)
replace hh_readwrite=2 if hh_readwrite>=2
label define rwri	0 "None"		///
					1 "Only one"	///
					2 "Two or more"
label values hh_readwrite rwri
tab hh_readwrite, sum(poor_npl1)

	* Can the HH Head read and write
	tab hhead_readwrite, sum(poor_npl1)

* How many members of the HH have ever attended school?
tab hh_attended, sum(poor_npl1) mis
replace hh_attended=0 if hh_attended==1
replace hh_attended=1 if hh_attended>=2
label define atte	0 "0-1"	1 "2 or more"
label values hh_attended atte
tab hh_attended, sum(poor_npl1)

	* Has the HH Head ever attended school?
	tab hhead_attended, sum(poor_npl1) mis

* How many members of the HH completed primary education?
tab hh_primary, sum(poor_npl1) mis
replace hh_primary=1 if hh_primary>=1
label define prim	0 "None" 1 "One or more"
label values hh_primary prim
tab hh_primary, sum(poor_npl1)

	* Did the HH Head completed primary education?
	tab hhead_primary, sum(poor_npl1) mis
	
* Did the HH spend on school fees in the last 12 months?
replace hh_spent_schoolfees=1 if hh_spent_schoolfees!=0
label values hh_spent_schoolfees yesno
tab hh_spent_schoolfees, sum(poor_npl1) mis

	* Did the HH spend on school supplies in the last 12 months?
	replace hh_spent_schoolsup=1 if hh_spent_schoolsup!=0
	label values hh_spent_schoolsup yesno
	tab hh_spent_schoolsup, sum(poor_npl1) mis
	
* Did the HH Head spend at least one hour working on agriculture?
tab hhead_jobagri, sum(poor_npl1) mis
replace hhead_jobagri=1 if hhead_jobagri!=0
label values hhead_jobagri yesno
tab hhead_jobagri, sum(poor_npl1) mis

	* Did the HH Head spend at least one hour working on non-agri business?
	tab hhead_jobbusiness, sum(poor_npl1) mis
	replace hhead_jobbusiness=1 if hhead_jobbusiness!=0
	label values hhead_jobbusiness yesno
	tab hhead_jobbusiness, sum(poor_npl1) mis
	
* Did the HH Head spend at least one hour working on casual labor?
tab hhead_jobcasual, sum(poor_npl1) mis
replace hhead_jobcasual=1 if hhead_jobcasual!=0
label values hhead_jobcasual yesno
tab hhead_jobcasual, sum(poor_npl1) mis

	* Did the HH Head spend at least one hour working on wage labor?
	tab hhead_jobwage, sum(poor_npl1) mis
	replace hhead_jobwage=1 if hhead_jobwage!=0
	label values hhead_jobwage yesno
	tab hhead_jobwage, sum(poor_npl1) mis
	
* Did the HH Head spend at least one hour working on unpaid labor?
tab hhead_jobunpaid, sum(poor_npl1) mis
replace hhead_jobunpaid=1 if hhead_jobunpaid!=0
label values hhead_jobunpaid yesno
tab hhead_jobunpaid, sum(poor_npl1) mis

	* Does any member of the HH have an account?
	tab hh_account, sum(poor_npl1) mis
	
* On what basis does the HH occupy the dwelling?
replace hh_status=2 if hh_status!=1
label define dwe 1 "Privately owned" 2 "Free of rent, Rented, Other"
label values hh_status dwe
tab hh_status, sum(poor_npl1) mis

	* How many rooms does the HH have?
	tab hh_rooms, sum(poor_npl1) mis
	replace hh_rooms=2 if hh_rooms!=1
	label define roomst 1 "Only one" 2 "Two or more"
	label values hh_rooms roomst
	tab hh_rooms, sum(poor_npl1) mis
	
* Material of the walls
fre hh_wall
tab hh_walls, sum(poor_npl1)
gen hh_walls_c=hh_walls
replace hh_walls=0 if hh_walls_c<=3 | hh_walls_c==10 | hh_walls_c>=17
replace hh_walls=1 if (hh_walls_c>=4 & hh_walls_c<=9) | (hh_walls_c>=11 & hh_walls_c<=16)
label define walls 0 "Wood and mud, Wood and Thatch, wood only, Mud Bricks (Traditional), Reed/Bamboo, Other" 1 "Stone only, Stone and mud, Stone and cement, Blocks (plastered with cement), Blocks (unplastered bricks), Steel (Lamera), Cargo Container, Chip Wood, Corrugated Iron Sheet, Asbestos"
label values hh_walls walls
drop hh_walls_c
tab hh_walls, sum(poor_npl1) mis

	*Main material of roof
	tab hh_roof, sum(poor_npl1)
	gen hh_roof_c=hh_roof
	replace hh_roof=0 if (hh_roof_c>=3 | hh_roof_c<=6) | hh_roof_c==9
	replace hh_roof=1 if hh_roof_c<=2 | hh_roof_c==7 | hh_roof_c==8
	label define roof 0 "Thatch, Mud and wood, Bamboo/Reed, Plastic Canvas, Other" 1 "Corrugated iron sheets, Concrete/Cement, Asbestos, Bricks"
	label values hh_roof roof
	drop hh_roof_c 
	
*Main material of floor
tab hh_floor, sum(poor_npl1)
gen hh_floor_c=hh_floor
replace hh_floor=0 if hh_floor_c==1
replace hh_floor=1 if hh_floor_c>=2
label define floor 0 "Mud/Dung" 1 "Bamboo/Reed, Wood Planks, Parquet or polished wood, Cement screed, Plastic tiles, Cement tiles, Brick tiles, Cemramic/Marble tiles, Others"
label values hh_floor floor
drop hh_floor_c

	*Type of kitchen
	tab hh_kitchen, sum(poor_npl1)
	replace hh_kitchen=0 if hh_kitchen==1
	replace hh_kitchen=1 if hh_kitchen>1
	label define kitchen 0 "No kitchen" 1 "A room used for traditional kitchen inside the houseing unit, A roome used for traditional kitchen outside the housing unit, A room used for modern kitchen outside the housing unit, Other"
	label values hh_kitchen kitchen
	
*Oven type
tab hh_oventype, mis sum(poor_npl1)
gen hh_oventype_c=hh_oventype
replace hh_oventype=0 if hh_oventype_c<=2 | hh_oventype_c==5
replace hh_oventype=1 if hh_oventype_c==3 | hh_oventype_c==4
label define oven 0 "Traditional mitad (oven) removable, Traditional mitad (oven) not removable, None" 1 "Improved energy saving mitad (rural technology product), Electric mitad"
label values hh_oventype oven
drop hh_oventype_c

	*Type of toilet
	tab hh_toilet, sum(poor_npl1)
	gen hh_toilet_c=hh_toilet
	replace hh_toilet=0 if hh_toilet_c==5 | hh_toilet_c==4 | hh_toilet_c==7 | hh_toilet_c==8
	replace hh_toilet=1 if hh_toilet_c<=3 | hh_toilet_c==6
	label define toilett 0 "PIT Latrine without slab, Composting Toilet, Field/Forest, Other" 1 "Flush toilet, PIT Latrine (ventilated VIP), PIT latrine with slab, Bucket"
	label values hh_toilet toilett
	drop hh_toilet_c
	
*Is the toilet shared with other households?
tab hh_toiletshared, sum(poor_npl1)
recode hh_toiletshared (3=0) (0=1) (1=2)
label define share 0 "Toilet=Field/Forest" 1 "No" 2 "Yes"
label values hh_toiletshared share

	*Washing location in the household
	tab hh_wash, sum(poor_npl1)
	recode hh_wash (3=0) (2=1) (1=2)
	label define washi 0 "No" 1 "Yes, in yard/plot" 2 "Yes, in dwelling"
	label values hh_wash washi
	
*Water in the washing location
tab hh_washwater, sum(poor_npl1)
recode hh_washwater (3=0) (0=1) (1=2)
label define ww 0 "No washing location" 1 "No" 2 "Yes"
label values hh_washwater ww
drop hh_washwater //Dropping since one of the categories contain less than 10% of the observations.
	
	*Soap in the washing location
	tab hh_washsoap, sum(poor_npl1)
	drop hh_washsoap //Dropping since one of the categories contain less than 10% of the observations.
	
*Solid waste disposal facilities
tab hh_disposal, sum(poor_npl1)
gen hh_disposal_c=hh_disposal
replace hh_disposal=1 if hh_disposal_c<=3 | hh_disposal_c>=6
replace hh_disposal=0 if hh_disposal_c==4 | hh_disposal_c==5
label define dispo 0 "Throw away, Used as fertilizer" 1 "Waste disposable vehicle, Waste disposal container, Dug-out, Burning the waste, Collected by municipality (public pump), Other"
label values hh_disposal dispo
drop hh_disposal_c

	*Main source of water
	tab hh_water, sum(poor_npl1)
	gen hh_water_c=hh_water
	replace hh_water=0 if (hh_water_c>=3 & hh_water_c<=9) | hh_water_c==14
	replace hh_water=1 if hh_water_c<3 | (hh_water_c>=10 & hh_water_c<=13) | hh_water_c==15
	label define wat 0 "Piped water public tap / Standpipe, Tubwell/Borehole, Protected dug well, Unprotected dug well, Protected spring, Unprotected spring, Rainwater collection, " 1 "Piped water into dwelling, Piped water into yard/plot, Piped water kiosk/retailer, Bottled water, Cart with small tank/drum, Tanker/truck, Other"
	label values hh_water wat
	drop hh_water_c
	
*Source of light
tab hh_light, sum(poor_npl1)
gen hh_light_c=hh_light
replace hh_light=0 if hh_light_c==5 | hh_light_c==6 | (hh_light_c>=8 & hh_light_c<=12)
replace hh_light=1 if hh_light_c<=4 | hh_light_c==7 | hh_light_c==13
label define lig 0 "Bio gas, Electrical battery, Light from dry cell with switch, Kerosene light lamp (imported), Local kerosene lamp (Kuraz), Candle/Wax, Fire wood" 1 "Electricity meter- private, Electricity meter- shared, Electricity from generator, Solar energy, Lantern, Other"
label values hh_light lig
drop hh_light_c

	*Energy for cooking
	tab hh_cook, sum(poor_npl1)
	gen hh_cook_c=hh_cook
	replace hh_cook=0 if hh_cook_c==1 | (hh_cook_c>=4 & hh_cook_c<=6) | hh_cook_c==10
	replace hh_cook=1 if hh_cook_c==2 | hh_cook_c==3 | hh_cook_c==7 | hh_cook_c==8 | hh_cook_c==9 | hh_cook_c>10
	label define coo 0 "Collecting fire wood, Crop residue / leaves, Dung/Manure, Saw dust, Solar energy, Biogas, None, Other" 1 "Purchase fire wood, Charcoal, Kerosene, Butane - Gas, Electricity, Solar Energy"
	label values hh_cook coo
	drop hh_cook_c
	
	*How many assets
tab hmany3, sum(poor_npl1)
	replace hmany3=1 if hmany3>=1
	label define cero 0 "0" 1 "1 or more"
	label values hmany3 cero
	ren hmany3 hmany03
tab hmany4, sum(poor_npl1)
	replace hmany4=2 if hmany4>=2
	label define two 0 "0" 1 "1" 2 "2 or more"
	label values hmany4 two
	ren hmany4 hmany04
tab hmany5, sum(poor_npl1)
	replace hmany5=2 if hmany5>=2
	label values hmany5 two
	ren hmany5 hmany05
tab hmany6, sum(poor_npl1)
	replace hmany6=1 if hmany6>=1
	label values hmany6 cero
	ren hmany6 hmany06
tab hmany8, sum(poor_npl1)
	replace hmany8=2 if hmany8>=2
	label values hmany8 two
	ren hmany8 hmany08
tab hmany9, sum(poor_npl1)
	replace hmany9=1 if hmany9>=1
	label values hmany9 cero
	ren hmany9 hmany09
tab hmany10, sum(poor_npl1)
	replace hmany10=1 if hmany10>=1
	label values hmany10 cero
tab hmany11, sum(poor_npl1)
	replace hmany11=1 if hmany11>=1
	label values hmany11 cero
tab hmany12, sum(poor_npl1)
	replace hmany12=1 if hmany12>=1
	label values hmany12 cero
tab hmany13, sum(poor_npl1)
	replace hmany13=1 if hmany13>=1
	label values hmany13 cero
tab hmany20, sum(poor_npl1)
	replace hmany20=1 if hmany20>=1
	label values hmany20 cero
tab hmany21, sum(poor_npl1)
	replace hmany21=1 if hmany21>=1
	label values hmany21 cero
tab hmany22, sum(poor_npl1)
	replace hmany22=1 if hmany22>=1
	label values hmany22 cero 
	drop hmany22 //Dropping because of lack of variation
tab hmany24, sum(poor_npl1)
	drop hmany24 //Dropping because the question may be difficlt to ask. 
tab hmany25, sum(poor_npl1)
	drop hmany25 //Dropping because the question may be difficult to ask. 
tab hmany26, sum(poor_npl1)
	replace hmany26=1 if hmany26>=1
	label values hmany26 cero
tab hmany27, sum(poor_npl1)
	replace hmany27=1 if hmany27>=1
	label values hmany27 cero
tab hmany30, sum(poor_npl1)
	replace hmany30=2 if hmany30>=2
	recode hmany30 (2=0) (0=2)
	label define twoo 0 "2 or more" 1 "1" 2 "0"
	label values hmany30 twoo
tab hmany31, sum(poor_npl1)
	replace hmany31=1 if hmany31>=1
	recode hmany31 (0=1) (1=0)
	label define ceroo 0 "1 or more" 1 "0"
	label values hmany31 ceroo
tab hmany32, sum(poor_npl1)
	replace hmany32=1 if hmany32>=1
	recode hmany32 (0=1) (1=0)
	label values hmany32 ceroo
tab hmany33, sum(poor_npl1)
	replace hmany33=1 if hmany33>=1
	recode hmany33 (0=1) (1=0)
	label values hmany33 ceroo
	
	drop hmany11 hmany12 hmany31 hmany32
	
**C. Save
	* Save
	save "$clean/$masterdb", replace
	
import delimited "$gps/eth_cluster_RWI.csv", clear
keep cluster_lat cluster_lon rwi
merge 1:m cluster_lat cluster_lon using "$clean/$masterdb"

	* Close log
	log c

**EOF**
