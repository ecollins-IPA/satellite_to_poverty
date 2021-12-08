*************************************************************************
*	Purpose: Create dummy variables for Ethiopia PPI				    *
*	Author: Manuel Cardona												*
*   Last modified: 02/20/2020											* 
*   Note: Last edit incorporates feedback from the User Review. To see  *
*         the exact changes just Ctrl+F and search "Change UR". 		*
*************************************************************************

set more off
clear all

*Manuel Cardona Arias	
if "`c(username)'" == "manuelarias" {
	gl path "/Users/manuelarias/Documents/GitHub/satellite_to_poverty_IPA"
}

global dta "$path/02 Data/01 Raw/LSMS/Household"
global clean "$path/02 Data/02 Clean/LSMS"

*Personal global (may change the path)
	*No need to change
	global root "$path/02 Data/01 Raw/LSMS/Household"
	global clean "$path/02 Data/02 Clean/LSMS"

	
*************To customize:*****************
///////////////////////////////////////////
global odb "ESS3_Ethiopia.dta"
global masterdb "MASTER_ESS3_Ethiopia.dta"
global ppidb "PPI_Ethiopia_2015-2016.dta"
global codebook "Codebook_Ethiopia_2015-2016.xls"
global qkey "Ethiopia15-16_QuestionKEY.csv"
///////////////////////////////////////////
	
******************************************
******************************************
************PPI - Indicators**************
******************************************
******************************************
u "$clean/$odb", clear

***************************************
***  Merge with poverty indicators  ***
***************************************
merge 1:m hh_id using "$clean/HICE_Ethiopia_2016_poverty.dta"
keep if _merge==3

	*Household Size
	tab hh_size, sum(poor_npl1)
	gen hh_size_cat=.
	replace hh_size_cat=1 if hh_size<=4
	replace hh_size_cat=2 if hh_size>4 & hh_size<=7
	replace hh_size_cat=3 if hh_size>7
	tab hh_size hh_size_cat
	label var hh_size_cat "Household Size"
	label define sizes 1 "1-4" 2 "5-7" 3 "8 or more"
	label values hh_size_cat sizes
	drop hh_size
	ren hh_size_cat hh_size

*Household head with health problems
tab hhead_hproblem, sum(poor_npl1) mis

	*Household head consulted medical assistance
	tab hhead_medassist, sum(poor_npl1)
	
*Can the household head read and write in any language?
tab hhead_readwrite, sum(poor_npl1)
recode hhead_readwrite (2=0)
label values hhead_readwrite yesno

	*Has the household head ever attended school
	tab hhead_attend, sum(poor_npl1)
	recode hhead_attend (2=0)
	label values hhead_attend yesno
	
*Highest level of education the household head has reached
tab hhead_grade, sum(poor_npl1) mis
levelsof hhead_grade
gen hhead_maxgrade=.
replace hhead_maxgrade=0 if hhead_grade==. | hhead_grade<=4 | hhead_grade==98
replace hhead_maxgrade=1 if hhead_grade>=5 & hhead_grade<=96
tab hhead_maxgrade, sum(poor_npl1)
drop hhead_grade
label define grades 0 "Kindergarten/Nursery/0 Grade, From 1st to 4th grade, Illiterate, Never attended School" 1 "5th grade or above, Informal Education, Adult literacy program, Satellite, Non-regular"
label values hhead_maxgrade grades
tab hhead_maxgrade

	*Sex of household head
	tab hhead_sex, sum (poor_npl1)
	recode hhead_sex (1=0) (2=1)
	label define sex 0 "Male" 1 "Female"
	label values hhead_sex sex

*Age of household head
tab hhead_age, sum(poor_npl1)
gen hhead_age_c=.
replace hhead_age_c=0 if hhead_age<=40
replace hhead_age_c=1 if hhead_age>40
tab hhead_age_c, sum(poor_npl1)
replace hhead_age=0 if hhead_age_c==1
replace hhead_age=1 if hhead_age_c==0
drop hhead_age_c
label define ages 0 "More than 40 years old" 1 "40 years old or less"
label values hhead_age ages
tab hhead_age, sum(poor_npl1)

	*Religion of household head
	tab hhead_religion, sum(poor_npl1)
	gen hhead_religion_c=hhead_religion
	replace hhead_religion=0 if hhead_religion_c==2 | hhead_religion_c==3 | hhead_religion_c==5  | hhead_religion_c==6 | hhead_religion_c==8
	replace hhead_religion=1 if hhead_religion_c==1 | hhead_religion_c==4 | hhead_religion_c==7
	label define religions 0 "Catholic, Protestant, Traditional, Pegan, Other" 1 "Orthodox, Muslem, Wakefeta"
	label values hhead_religion religions
	drop hhead_religion_c
		//Religion will be very sensitive, so dropping. 
		drop hhead_religion
	
*Marital Status of household head
tab hhead_maritals, sum(poor_npl1)
gen hhead_maritals_c=hhead_maritals
replace hhead_maritals=0 if hhead_maritals_c==2 | hhead_maritals_c==3
replace hhead_maritals=1 if hhead_maritals==1 | hhead_maritals>=4
label define marital 0 "Married (Monogamous or Polygamous)" 1 "Never married, Divorced, Separeted, Widowed"
label values hhead_maritals marital
drop hhead_maritals_c
	
	*Anyone in the household can read and write
	tab hh_readwrite, sum(poor_npl1)

*Anyone in the household has ever attended school
tab hh_attend, sum(poor_npl1)

	*Anyone in the household faced any health problem
	tab hh_hproblem, sum(poor_npl1)
	
*Anyone in the household consulted medical assistance
tab hh_medassist, sum(poor_npl1)

*Consumption items
tab consumed1, sum(poor_npl1)
	ren consumed1 consumed01
tab consumed2, sum(poor_npl1)
	ren consumed2 consumed02
tab consumed3, sum(poor_npl1)
	ren consumed3 consumed03
tab consumed4, sum(poor_npl1)
	ren consumed4 consumed04
tab consumed5, sum(poor_npl1)
	ren consumed5 consumed05
*tab consumed6, sum(poor_npl1)
*	ren consumed6 consumed06
tab consumed7, sum(poor_npl1)
	ren consumed7 consumed07
tab consumed8, sum(poor_npl1)
	ren consumed8 consumed08
*tab consumed9, sum(poor_npl1)
*	ren consumed9 consumed09
*tab consumed10, sum(poor_npl1)
tab consumed11, sum(poor_npl1)
*tab consumed12, sum(poor_npl1)
	*recode consumed12 (1=0) (0=1)
	label define noyes 0 "Yes" 1 "No"
	*label values consumed12 noyes
*tab consumed13, sum(poor_npl1)
*tab consumed14, sum(poor_npl1)
*	recode consumed14 (1=0) (0=1)
*	label values consumed14 noyes
tab consumed15, sum(poor_npl1)
tab consumed16, sum(poor_npl1)
	recode consumed16 (1=0) (0=1)
	label values consumed16 noyes
tab consumed17, sum(poor_npl1)
tab consumed18, sum(poor_npl1)
*tab consumed19, sum(poor_npl1)
tab consumed20, sum(poor_npl1)
tab consumed21, sum(poor_npl1)
*tab consumed22, sum(poor_npl1)
*tab consumed23, sum(poor_npl1)
*tab consumed24, sum(poor_npl1)
*tab consumed25, sum(poor_npl1)
tab consumed26, sum(poor_npl1)
	recode consumed26 (1=0) (0=1)
	label values consumed26 noyes
tab consumed27, sum(poor_npl1)
tab consumed28, sum(poor_npl1)
*tab consumed29, sum(poor_npl1)
*tab consumed30, sum(poor_npl1)
*tab consumed31, sum(poor_npl1)
tab consumed32, sum(poor_npl1)

	*Purchases in the past one month
	*tab purchased1, sum(poor_npl1)
	*	ren purchased1 purchased01
	*tab purchased2, sum(poor_npl1)
	*	recode purchased2 (1=0) (0=1)
	*	label values purchased2 noyes
	*	ren purchased2 purchased02
	tab purchased3, sum(poor_npl1)
		ren purchased3 purchased03
	tab purchased5, sum(poor_npl1)
		ren purchased5 purchased05
	*tab purchased6, sum(poor_npl1)
	*	ren purchased6 purchased06
	tab purchased7, sum(poor_npl1)
		ren purchased7 purchased07
	tab purchased8, sum(poor_npl1)
		ren purchased8 purchased08
	tab purchased9, sum(poor_npl1)
		recode purchased9 (1=0) (0=1)
		label values purchased9 noyes
		ren purchased9 purchased09
	*tab purchased11, sum(poor_npl1)
	*tab purchased12, sum(poor_npl1)
	
	//Dropping questions that are sensitive or ambiguous:
	*drop purchased06 
	
*Purchases in the past 12 months
/*
tab purchase1, sum(poor_npl1)
	ren purchase1 purchase01
tab purchase2, sum(poor_npl1)
	ren purchase2 purchase02
tab purchase3, sum(poor_npl1)
	recode purchase3 (1=0) (0=1)
	label values purchase3 noyes
	ren purchase3 purchase03
tab purchase4, sum(poor_npl1)
	recode purchase4 (1=0) (0=1)
	label values purchase4 noyes
	ren purchase4 purchase04
	*/
tab purchase5, sum(poor_npl1)
	ren purchase5 purchase05
tab purchase6, sum(poor_npl1)
	ren purchase6 purchase06
tab purchase8, sum(poor_npl1)
	recode purchase8 (1=0) (0=1)
	label values purchase8 noyes
	ren purchase8 purchase08
tab purchase9, sum(poor_npl1)
	ren purchase9 purchase09
	
/*
tab purchase10, sum(poor_npl1)
tab purchase11, sum(poor_npl1)
tab purchase12, sum(poor_npl1)
	recode purchase12 (1=0) (0=1)
	label values purchase12 noyes
	*/
	//Dropping questions that are sensitive or ambiguous:
	drop purchase09 //purchase10 purchase11 purchase12
	
*Food security question:1
tab hh_fs1, sum(poor_npl1)
	recode hh_fs1 (1=0) (0=1)
	label values hh_fs1 noyes
tab hh_fs2a, sum(poor_npl1)
	replace hh_fs2a=1 if hh_fs2a>=1
	recode hh_fs2a (0=1) (1=0)
	label define days 0 "1 day or more" 1 "0 days"
	label values hh_fs2a days
tab hh_fs2b, sum(poor_npl1)
	replace hh_fs2b=1 if hh_fs2b>=1
	recode hh_fs2b (0=1) (1=0)
	label values hh_fs2b days
tab hh_fs2c, sum(poor_npl1)
	replace hh_fs2c=1 if hh_fs2c>=1
	recode hh_fs2c (0=1) (1=0)
	label values hh_fs2c days
tab hh_fs2d, sum(poor_npl1)
	replace hh_fs2d=1 if hh_fs2d>=1
	recode hh_fs2d (0=1) (1=0)
	label values hh_fs2d days
tab hh_fs2e, sum(poor_npl1)
	replace hh_fs2e=1 if hh_fs2e>=1
	recode hh_fs2e (0=1) (1=0)
	label values hh_fs2e days
	drop hh_fs2e //Dropping because of low variation
tab hh_fs2f, sum(poor_npl1)
	replace hh_fs2f=1 if hh_fs2f>=1
	recode hh_fs2f (0=1) (1=0)
	label values hh_fs2f days
	drop hh_fs2f //Dropping because of low variation
tab hh_fs2g, sum(poor_npl1)
	replace hh_fs2g=1 if hh_fs2g>=1
	recode hh_fs2g (0=1) (1=0)
	label values hh_fs2g days
	drop hh_fs2g //Dropping because of low variation
tab hh_fs2h, sum(poor_npl1)
	replace hh_fs2h=1 if hh_fs2h>=1
	recode hh_fs2h (0=1) (1=0)
	label values hh_fs2h days
	drop hh_fs2h //Dropping because of low variation
tab hh_fs6, sum(poor_npl1)
	recode hh_fs6 (0=1) (1=0)
	label values hh_fs6 noyes
	
*Household tenure status
tab hh_status, sum(poor_npl1)
replace hh_status=0 if hh_status==1
replace hh_status=1 if hh_status>=2
label define status 0 "Privately owned" 1 "Free of rent, Rented, Other"
label values hh_status status

	*Number of rooms
	tab hh_rooms, sum(poor_npl1)
	replace hh_rooms=0 if hh_rooms==1
	replace hh_rooms=1 if hh_rooms>1
	label define rooms 0 "One" 1 "2 or more"
	label values hh_rooms rooms
	
*Main material of walls
tab hh_walls, sum(poor_npl1)
gen hh_walls_c=hh_walls
replace hh_walls=0 if hh_walls_c<=3 | hh_walls_c==10 | hh_walls_c>=17
replace hh_walls=1 if (hh_walls_c>=4 & hh_walls_c<=9) | (hh_walls_c>=11 & hh_walls_c<=16)
label define walls 0 "Wood and mud, Wood and Thatch, wood only, Mud Bricks (Traditional), Reed/Bamboo, Other" 1 "Stone only, Stone and mud, Stone and cement, Blocks (plastered with cement), Blocks (unplastered bricks), Steel (Lamera), Cargo Container, Chip Wood, Corrugated Iron Sheet, Asbestos"
label values hh_walls walls
drop hh_walls_c
	
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
	
	//Dropping assets that may be hard to ask or respond or that might change quickly over time:
	drop hmany08 hmany10

*Representativeness at the regional level.
gen region=.
replace region=1 if saq01==3
replace region=2 if saq01==4
replace region=3 if saq01==7
replace region=4 if saq01==1
replace region=5 if region==.
label define regions 1 "Amhara" 2 "Oromiya" 3 "SNNP" 4 "Tigray" 5 "Other regions"
label values region regions
tab region, sum(poor_npl1)

*ORDER DATABASE
//ren (lat_dd_mod lon_dd_mod) (cluster_lat cluster_lon)
order hh_id ind_id household_id2 cluster_lat cluster_lon hh_weight ind_weight adulteq rural pw_w3 saq01 saq02 saq04 hh_s2q00 ea_id ea_id2 no_conv no_cons food_cons_ann nonfood_cons_ann educ_cons_ann total_cons_ann price_index_hce nom_totcons_aeq cpi_factor_11 cpi_factor_05 ppp_factor_11 ppp_factor_05 abs_pl food_pl cons_exp_pa cons_exp_pa_spa poor_npl1 poor_npl2 poor_150npl1-poor_bottom_80 region hh_size hhead_hproblem hhead_medassist hhead_readwrite hhead_attended hhead_maxgrade
drop hh_s2q00 ea_id ea_id2 no_conv no_cons food_cons_ann nonfood_cons_ann educ_cons_ann total_cons_ann price_index_hce nom_totcons_aeq cpi_factor_11 cpi_factor_05 ppp_factor_11 ppp_factor_05 cons_exp_pa cons_exp_pa_spa _merge saq01-saq04
compress
save "$clean/$masterdb", replace

		************************************************
		************************************************
		*************Arrange for custom PPI*************	After changing the globals: this code should run without any further changes
		************************************************
		************************************************
		u "$clean/$masterdb", clear
		set more off
		
		*************To customize:*****************
		///////////////////////////////////////////
		global vlist "region hh_size hhead_hproblem hhead_medassist hhead_readwrite hhead_attended hhead_maxgrade hhead_sex hhead_age hhead_maritals hh_readwrite hh_attended hh_hproblem hh_medassist consumed01 consumed02 consumed03 consumed04 consumed05 consumed07 consumed08 consumed11 consumed15 consumed16 consumed17 consumed18 consumed20 consumed21 consumed26 consumed27 consumed28 consumed32 purchased03 purchased05 purchased07 purchased08 purchased09 purchase05 purchase06 purchase08 hh_fs1 hh_fs2a hh_fs2b hh_fs2c hh_fs2d hh_fs6 hh_status hh_rooms hh_walls hh_roof hh_floor hh_kitchen hh_oventype hh_toilet hh_toiletshared hh_wash hh_disposal hh_water hh_light hh_cook hmany03 hmany04 hmany05 hmany06 hmany09 hmany11 hmany12 hmany13 hmany20 hmany21 hmany26 hmany27 hmany30 hmany31 hmany32 hmany33"
		global poor poor_npl1 //Change only if we don't want to train our model with the National Poverty Line
		global advars "hh_id ind_id household_id2 cluster_lat cluster_lon hh_weight ind_weight rural poor_npl1 poor_npl2 poor_150npl1 poor_200npl1 poor_2011_100 poor_2011_190 poor_2011_320 poor_2011_550 poor_2011_800 poor_2011_1100 poor_2011_1500 poor_2011_2170 poor_2005_125 poor_2005_250 poor_2005_500 poor_bottom_20 poor_bottom_40 poor_bottom_60 poor_bottom_80" //All variables that are useful in the DB, but we don't want in the CSV key file
		global tobe_dropped "adulteq pw_w3 abs_pl food_pl"
		/////////////////////////////////////////// 
		
			local to_drop = " "
			local count = 1
			gen name = ""
			gen orig_name = ""
			gen label = ""
			gen value = ""
			
		*We plug in all the variables we want to dummify: it creates dummies for each category and drops the one with the highest poverty rate.
		foreach var of varlist $vlist{
			bysort `var': egen mean = mean($poor)  // Poverty rate by category of the variable
			quietly levelsof `var', local(levels)      // Captures all the levels/categories for each variable
			foreach i of local levels {				   // Loop through all the levels
				gen `var'`i'= (`var'==`i')			   // Generates a dummy variable for each level
					*Keep variable names, labels and values for them to be used later
					while (name[`count']!=""){			//Everytime a new variable is looped over the DB re-sorts itself, bc of this we need to make sure that the obs in name that we will replace with the variable name/label/value is in blank. If not, we change the obs.
						local count = `count' + 1		//If the observation that we will replace has a value already, we will change the local so we replace it in an additional obs
						display "While loop replaced count with `count'"
					}
					quietly: replace name = "`var'`i'" in `count' //Var name in observation #`count' is replaced with the variable name
					local x : variable label `var' //Variable label is stored
					quietly: replace label = "`x'" in `count' //Var label in observation #`count' is replaced with the variable label
					quietly: replace value = "`i'" in `count' //Var value in observation #`count' is replaced with the value of that category
					quietly: replace orig_name ="`var'" in `count' //Var orig_name in observation #`count' is replaced with the name of the original variable that created this one
				quietly: su mean					   // Summary stats for the poverty rate of each category variable
				global max `: di %4.3f r(max)'         // Keep in a global (max) the maximum poverty rate for a particular category/level
				quietly su $poor if `var'==`i'     // Summarize the poverty rate for this category/level
				global max2 `: di %4.3f r(mean)'	   // Store the mean poverty rate from this category/level
				if $max == $max2 {					   // If the poverty rate from this category (max2) is the same as the highest poverty rate for any category (max), the code goes ahead and drops that category
					display "Will drop `var'`i'"		   // It displays the category that was dropped
					local to_drop = ("`var'`i'" + " " + "`to_drop'")
				 }
				 local count = `count' + 1
			}
			drop mean `var'                            // Drops variable mean (will be used next time the loop begins) and the original variable that was split into different categories
		}
		drop `to_drop' //Drops all variables that had the highest poverty rates
		display "Variables dropped: `to_drop'" //Lists all the variables that were dropped
		
		*******Create a list of names with labels and values; export it to another DB*******
		preserve
		drop if name==""
		sort name value
		keep name orig_name label value
		save "$clean/PPI_labels.dta",  replace //Save the variable names with it's labels
		restore
		drop name label value orig_name
		
		*******Order DB*******
		order $advars
		drop $tobe_dropped //Drop questions that will not be used by the algorithm: usually MPI, WB, HDDS, WI, among others.
		
		********Drop all variables with low prevalence*******
 		*foreach v of var item* asset* food* { //For all items, food or assets
		*	quietly: su `v' //Summarize 
		*	if (r(mean)<0.1)  { //Drop if less than 10% of hh have them
		*		drop `v' //Drop
		*		display "Dropped `v'" 	//Display dropped
		*	}		
		*}
		
		*******Drop all missing values*******
		*mdesc         //Check for missing values in each variable
 		foreach v of var * { 
			drop if missing(`v') //Drop all observations that have any missing values 
		}

* Create urban indicator
gen urban=(rural!=1)		
drop rural
order hh_id-ind_weight urban poor_npl1-hmany331

* Change UR:
* Keep only relevant indicators:
	* Drop indicators that were not selected by the variable selection algorithm.
	* Drop indicators based on the User Review feedback.
	keep hh_id-region5 hh_size1 hh_size2 hhead_readwrite1 hhead_attended1 hhead_maxgrade1 consumed181 consumed201 purchased071 purchase061 hh_roof1 hh_kitchen1 hh_toilet1 hh_toiletshared1 hh_toiletshared2 hh_disposal1 hh_light1 hh_cook1 hmany041 hmany042 hmany051 hmany052 hmany061 hmany091 hmany311
		//Now, we only have 56 variables in  the DB.
		
saveold "$clean/$ppidb", replace version(12)

				************************************************
				************************************************
				*******Arrange for question key CSV file******** After changing the globals: this code should run without any further changes
				************************************************
				************************************************
				u "$clean/$ppidb", clear
				
				*************To customize:*****************
				///////////////////////////////////////////
				global obs_start = 28 //This is the observation where questions that were split into different categories begin
				global obs_na = 27 //Observations such as poverty rates, weights and urban/rural are not supposed to be included. 
								  //This sets the number of obs that should be dropped before the we output the qkey file
				global minus = 1 //This shouldn't be changed unless one var has more than 10 categories. If this is true, change it to 3.
				///////////////////////////////////////////
					
				*Generate CSV file that numbers every variable and groups the dummies by original variable (all region dummies will be assigned the same number)
					gen varname = ""
					local i = 1  									//Local that equals 1
				foreach var of varlist * {						//Loop that goes through all variables		
					replace varname = "`var'" in `i'			//For observation "i", replace with the name of each variable on the list
					local i = `i' + 1							//Change the local by adding 1: therefore, we will replace next variable name in the following observation
				}
				drop in 1/$obs_na 
				replace varname = "" if varname == "varname"	//Since varname is a variable, we delete it
				
				*Generate another variable that takes away last two characters of our variable names
				generate varname2=substr(varname,1, strlen(varname)-$minus)
				*in $obs_start /1000  //Make sure to change the numbers in each case
				replace varname2= varname if varname2=="" //Remove obs with varname
				drop if varname=="" //Keep only the number of obs = to number of variables
				gen a = 1 if varname2!=varname2[_n-1]  //We tag obs if past varname is not the same as the new one
				gen unique_var_number = _n if a==1 //We make a count variable for each case in which we have a variable genre for the first time
				replace unique_var_number = unique_var_number[_n-1] if unique_var_number==. //Replace all missings with a number that is the same for each var genre
				
				*Unique_var_name has some jumps (e.g. from 60 to 69). We don't want this to happen. So we wil fix it (e.g. so after 60 goes 61).
				local i = 1
				levelsof unique_var_number, local(levels)  //Store all the levels of unique_var_number   
				foreach lev of local levels {			//Loop through all those levels	   
					replace unique_var_number = `i' if unique_var_number == `lev'  //Replace each level with the local i; which will go continuously 1 by 1. 
					local i = `i' + 1	//We add 1 every time we are finished with one level
				 }
				
				rename (unique_var_number varname) (unique_q_number variable_name)
				keep  variable_name unique_q_number
				order unique_q_number
				export delimited using "$clean/$qkey",  replace
