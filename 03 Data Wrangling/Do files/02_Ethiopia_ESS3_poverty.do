********************************************************************************
**	TITLE: 		02_Ethiopia_ESS3_poverty.do
**
**	PURPOSE: 	create poverty indicators
**
**	INPUTS: 	${raw}/SEVERAL MODULES.dta
**
** 	OUTPUTS: 	${clean}/ESS3_2016.dta
**
**	AUTHOR: 	Manuel Cardona 
**
**	CREATED:	August 2022
********************************************************************************
*Table of Contents
*1. 
*2. 


loc cdate: di %td_CCYY_NN_DD date(c(current_date), "DMY")
loc cdate = subinstr(trim("`cdate'"), " " , "", .)
*log using "${logs}/01_Ethiopia_ESS3_selection`cdate'.smcl", replace
di `cdate'

******************************************
************PPI - Indicators**************
******************************************

use "${dta}/cons_agg_w3.dta", clear

*Rename variables
ren pw_w3 hh_weight
gen ind_weight=hh_weight*hh_size
label var ind_weight "Individual weight"

	*Get CPI and PPP conversion factors
	gen cpi_factor_11=(((211.403+226.759)/2)/133.25) //Survey year CPI divided by 2011 CPI found here https://data.worldbank.org/indicator/FP.CPI.TOTL?locations=ID
	gen cpi_factor_05=(((211.403+226.759)/2)/44.846) //Survey year CPI divided by 2005 CPI found here https://data.worldbank.org/indicator/FP.CPI.TOTL?locations=ID
	gen ppp_factor_11=5.439 //2011 ppp conversion factors, found here (change country): https://data.worldbank.org/indicator/PA.NUS.PRVT.PP?locations=ID
	gen ppp_factor_05=2.108 //2005 ppp conversion factors, found here (change country): https://data.worldbank.org/indicator/PA.NUS.PRVT.PP?locations=ID
		label var cpi_factor_11 "Consumper Price Index 2016 / Consumer Price Index 2011"
		label var cpi_factor_05 "Consumper Price Index 2016 / Consumer Price Index 2005"
		label var ppp_factor_11 "2011 PPP Conversion Factor for Ethiopia"
		label var ppp_factor_05 "2005 PPP Conversion Factor for Ethiopia"
		
*National Poverty Lines
gen abs_pl=7184
label var abs_pl "Absolute poverty line (Birr per year per adult person)"
gen food_pl=3772 
label var food_pl "Food poverty line (Birr per year per adult person)"

	//Need to obtain the Real Per Adult Consumption 
	
	//Step 1: Divide the nominal consumption expenditure by nutritional calorie
	//based adult equivalence family size, to arrive to at per adult consumption expenditure.
	gen cons_exp_pa=total_cons_ann/adulteq
	
	//Step 2: Per adult consumption expenditure has been updated by deflating all food
	//and non-food consumption items by spatial price indices and temporal price indices
	// to bring them to December 2010 constant prices. These adjustments result into real
	// per adult food and non-food consumption expenditure measured at December 2015 national
	//average prices.
	gen cons_exp_pa_spa=cons_exp_pa*2.11403/price_index_hce
	label var cons_exp_pa_spa "Real per adult consumption expenditure at Dec 2015 national average prices"

gen poor_npl1=(cons_exp_pa_spa<abs_pl)
label var poor_npl1 "Real per adult consumption is below the national poverty line"
sum poor_npl1 [w=ind_weight]
gen poor_npl2=(cons_exp_pa_spa<food_pl)
label var poor_npl2 "Real per adult consumption is below the food poverty line"
sum poor_npl2 [w=ind_weight]
gen povline_150npl1=abs_pl*1.5
label var povline_150npl1 "150% National Poverty Line"
gen poor_150npl1=(cons_exp_pa_spa<povline_150npl1)
label var poor_150npl1 "Real per adult consumption is below the 150% National Poverty Line"
gen povline_200npl1=abs_pl*2
label var povline_200npl1 "200% National Poverty Line"
gen poor_200npl1=(cons_exp_pa_spa<povline_200npl1)
label var poor_200npl1 "Real per adult consumption is below the 200% National Poverty Line"


	*International Poverty Lines 2011 - Lower-middle income countries
	gen povline_2011_100 = 1.00*ppp_factor_11*cpi_factor_11 //1 USD a day
	gen poor_2011_100 = ((cons_exp_pa_spa/365) < povline_2011_100) //1 USD a day
	gen povline_2011_190 = 1.90*ppp_factor_11*cpi_factor_11  // 1.90 USD a day
	gen poor_2011_190 = ((cons_exp_pa_spa/365) < povline_2011_190) // 1.90 USD a day
	gen povline_2011_320 = 3.20*ppp_factor_11*cpi_factor_11 // 3.20 USD a day
	gen poor_2011_320 = ((cons_exp_pa_spa/365) < povline_2011_320) // 3.20 USD a day
	gen povline_2011_550 = 5.50*ppp_factor_11*cpi_factor_11 // 5.50 USD a day
	gen poor_2011_550 = ((cons_exp_pa_spa/365) < povline_2011_550) // 5.50 USD a day
	gen povline_2011_800 = 8.00*ppp_factor_11*cpi_factor_11 // 8 USD a day
	gen poor_2011_800 = ((cons_exp_pa_spa/365) < povline_2011_800) // 8 USD a day
	gen povline_2011_1100 = 11.00*ppp_factor_11*cpi_factor_11 // 11 USD a day
	gen poor_2011_1100 = ((cons_exp_pa_spa/365) < povline_2011_1100) // 11 USD a day
	gen povline_2011_1500 = 15.00*ppp_factor_11*cpi_factor_11 // 15 USD a day
	gen poor_2011_1500 = ((cons_exp_pa_spa/365) < povline_2011_1500) // 15 USD a day
	gen povline_2011_2170 = 21.70*ppp_factor_11*cpi_factor_11 // 21.70 USD a day
	gen poor_2011_2170 = ((cons_exp_pa_spa/365) < povline_2011_2170) // 21.70 USD a day
			su poor_2011* [weight = ind_weight] // These estimates almost match: http://iresearch.worldbank.org/PovcalNet/povOnDemand.aspx
	label var poor_2011_100 "$1.00 a day (2011 PPP) poverty line: HH is below this line"
	label var poor_2011_190 "$1.90 a day (2011 PPP) poverty line: HH is below this line"
	label var poor_2011_320 "$3.20 a day (2011 PPP) poverty line: HH is below this line"
	label var poor_2011_550 "$5.50 a day (2011 PPP) poverty line: HH is below this line"
	label var poor_2011_800 "$8.00 a day (2011 PPP) poverty line: HH is below this line"
	label var poor_2011_1100 "$11.00 a day (2011 PPP) poverty line: HH is below this line"
	label var poor_2011_1500 "$15.00 a day (2011 PPP) poverty line: HH is below this line"
	label var poor_2011_2170 "$21.70 a day (2011 PPP) poverty line: HH is below this line"

*International pvoerty lines (2005) - Lower-middle income countries 
gen povline_2005_125 = 1.25*ppp_factor_05*cpi_factor_05 // 1.25 USD a day
gen poor_2005_125 = ((cons_exp_pa_spa/365) < povline_2005_125) // 1.25 USD a day
gen povline_2005_250 = 2.50*ppp_factor_05*cpi_factor_05 // 2.50 USD a day
gen poor_2005_250 = ((cons_exp_pa_spa/365) < povline_2005_250) // 2.50 USD a day
gen povline_2005_500 = 5.00*ppp_factor_05*cpi_factor_05 // 5.00 USD a day
gen poor_2005_500 = ((cons_exp_pa_spa/365) < povline_2005_500) // 5.00 USD a day
	su poor_2005* [weight = ind_weight] //No benchmark to compare it to
label var poor_2005_125 "$1.25 a day (2005 PPP) poverty line: HH is below this line"
label var poor_2005_250 "$2.50 a day (2005 PPP) poverty line: HH is below this line"
label var poor_2005_500 "$5.00 a day (2005 PPP) poverty line: HH is below this line"

	*Divide hh by consumption quintiles
	xtile con_quint = cons_exp_pa_spa [w=hh_weight], nq(5) //Consumption quintiles
	egen relative20 = min(cons_exp_pa_spa) if con_quint==2 //Minimum consumption for 2nd quintile - Missings for all except 2nd quintile
	egen povline_relative_20 = max(relative20) //Generate a variable = relative20 but for all observations
	gen poor_bottom_20 = (cons_exp_pa_spa<povline_relative_20) //Generate a dummy for the bottom quintile
	egen relative40 = min(cons_exp_pa_spa) if con_quint ==3 //Minimum consumption for 3rd quintile - Missings for all except 2nd quintile
	egen povline_relative_40 = max(relative40) //Generate a variable = relative40 but for all observations
	gen poor_bottom_40 = (cons_exp_pa_spa < povline_relative_40) //Generate a dummy for the second quintile
	egen relative60 = min(cons_exp_pa_spa) if con_quint ==4 //Minimum consumption for 4th quintile - Missings for all except 2nd quintile
	egen povline_relative_60 = max(relative60)  //Generate a variable = relative60 but for all observations
	gen poor_bottom_60 = (cons_exp_pa_spa < povline_relative_60) //Generate a dummy for the third quintile
	egen relative80 = min(cons_exp_pa_spa) if con_quint ==5 //Minimum consumption for 5th quintile - Missings for all except 2nd quintile
	egen povline_relative_80 = max(relative80)  //Generate a variable = relative80 but for all observations
	gen poor_bottom_80 = (cons_exp_pa_spa < povline_relative_80) //Generate a dummy for the fourth quintile
		su poor_bottom* [weight = hh_weight] //No benchmark to compare it to	
	label var poor_bottom_20 "20th Percentile Poverty Line - Lowest 20% of the consumption distribution" 
	label var poor_bottom_40 "40th Percentile Poverty Line - Lowest 40% of the consumption distribution" 
	label var poor_bottom_60 "60th Percentile Poverty Line - Lowest 60% of the consumption distribution" 
	label var poor_bottom_80 "80th Percentile Poverty Line - Lowest 80% of the consumption distribution" 

	drop povline* con_quint cons_quint relative*
	
drop household_id 

save "${clean}/poverty_ESS3.dta", replace

