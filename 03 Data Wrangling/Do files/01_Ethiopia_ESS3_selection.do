********************************************************************************
**	TITLE: 		01_Ethiopia_ESS3_selection.do
**
**	PURPOSE: 	Select indicators from original survey modules
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

***********************
*** 0. Survey Cover ***
***********************

use "$dta/sect_cover_hh_w3.dta", clear

duplicates report household_id2
unique household_id2	//4,954 unique households

* Keep relevant variables
keep household_id2 rural pw_w3 saq01 hh_saq09

* Rural indicator
fre rural
gen urban=(rural!=1) if !mi(rural)
label var urban "Urban==1"
label define urb 0 "Rural" 1 "Urban"
label values urban urb
drop rural
fre urban

	* Household weights
	ren pw_w3 hh_wgt
	
* Household Size
fre hh_saq09
ren hh_saq09 hh_size

	* Region
	fre saq01
	ren saq01 region
	
sort household_id2 
order household_id2 hh_wgt urban region hh_size
save "${temp}/hh_temp.dta", replace

***************************
*** 1. Household Roster ***
***************************

use "$dta/sect1_hh_w3.dta", clear

unique household_id2	//Information about 27,990 individuals from 4,954 households

*Keep relevant variables
keep household_id2 individual_id2 hh_s1q02 hh_s1q03 hh_s1q04a hh_s1q04b hh_s1q04h hh_s1q04d hh_s1q04e hh_s1q07 hh_s1q08

*Any mebers below certain age of the non-adult age interval
tab hh_s1q04a hh_s1q04b, mis
ren (hh_s1q04a hh_s1q04b) (age_yrs age_months)
replace age_yrs=0 if age_yrs==. & age_months!=.

ren hh_s1q04h age_2
replace age_yrs=age_2 if age_2!=.  //Even replacing with all the possible age vars, we have 14.33% of missing values. 

	*Household Size
	bysort household_id2: gen number=_n
	bysort household_id2: egen hh_size2=max(number)
	drop number
	label var hh_size2 "Household Size Calculated"

*Sex of household members
	//5% of missing values
	
	*Variables for the Household Head
	ren hh_s1q02 hh_rel
	keep if hh_rel==1 //Keep if household head

	keep household_id2 individual_id2 hh_rel hh_s1q03 age_yrs hh_s1q07 hh_s1q08
	ren (hh_s1q03 age_yrs hh_s1q07 hh_s1q08) (hhead_sex hhead_age hhead_religion hhead_maritals)

	tab hhead_sex, mis			//1 missing value
	tab hhead_age, mis			//7 missing value
	tab hhead_religion, mis		//24 missing values
	tab hhead_maritals, mis		//22 missing values

	sort household_id2
	quietly by household_id2: gen dup = cond(_N==1,0,_n)
	drop if dup>1
	drop dup

merge 1:1 household_id2 using "${temp}/hh_temp.dta"
drop _merge
sort household_id2 
order household_id2 individual_id2 hh_wgt-hh_size
save "${temp}/hh_temp.dta", replace


********************
*** 2. Education ***
********************

* This section is only for members 5 years or older
use "$dta/sect2_hh_w3.dta", clear

unique household_id2	//Information about 23,393 individuals from 4,954 households

* Keep relevant variables
keep household_id2 individual_id2 hh_s2q01 hh_s2q02 hh_s2q03 hh_s2q05 hh_s2q16 hh_s2q17
drop if hh_s2q01==""	//Member is below 5 years of age
drop hh_s2q01

	* How many members can read and write in any language?
	fre hh_s2q02
	replace hh_s2q02=0 if hh_s2q02==2
	bysort household_id2: egen hh_readwrite=sum(hh_s2q02)
	label var hh_readwrite "How many members of the HH can read and write in any language?"
	
	* Can the HHead read and write in any language?
	ren hh_s2q02 hhead_readwrite
	label var hhead_readwrite "Can the HHead read and write in any language?"
	label define yesno 0 "No" 1 "Yes"
	label values hhead_readwrite yesno
	
* How many members of the HH have ever attended school?
fre hh_s2q03
replace hh_s2q03=0 if hh_s2q03==2
bysort household_id2: egen hh_attended=sum(hh_s2q03)
label var hh_attended "How many members of the HH have ever attended school?"

* Has the HHead ever attended school?
ren hh_s2q03 hhead_attended
label var hhead_attended "Has the HHead ever attended school?"
label values hhead_attended yesno

	* How many members of the HH completed primary education?
	fre hh_s2q05
	gen primary=inlist(hh_s2q05, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 93, 94)
	bysort household_id2: egen hh_primary=sum(primary)
	label var hh_primary "How many members of the HH completed primary education?"
	drop hh_s2q05
	
	* Did the HHead completed primary education?
	fre primary
	ren primary hhead_primary
	label var hhead_primary "Did the HHead completed primary education?"
	label values hhead_primary yesno
	
* How much did the HH spend on school fees this year?
replace hh_s2q16=0 if hh_s2q16==.	//Did not attend school
bysort household_id2: egen hh_spent_schoolfees=sum(hh_s2q16)
label var hh_spent_schoolfees "How much did the HH spend on school fees the past 12 months?"

	* How much did the HH spend on school supplies this year?
	replace hh_s2q17=0 if hh_s2q17==.	//Did not attend school
	bysort household_id2: egen hh_spent_schoolsup=sum(hh_s2q17)
	label var hh_spent_schoolsup "How much did the HH spend on school supplies the past 12 months?"
	drop hh_s2q16 hh_s2q17
	
merge 1:1 household_id2 individual_id2 using "${temp}/hh_temp.dta"
keep if _merge==3	//No information for those hheads
drop _merge
sort household_id2
order household_id2 individual_id2 hh_wgt-hhead_maritals hh_readwrite hhead_readwrite hh_attended hhead_attended hh_primary hhead_primary hh_spent_schoolfees hh_spent_schoolsup
save "${temp}/hh_temp.dta", replace

	
*****************************
*** 4. Time Use and Labor ***
*****************************

use "${dta}/sect4_hh_w3.dta", clear
unique household_id2	//Information about 23,393 individuals from 4,954 households
fre hh_s4q01
drop if hh_s4q01==""	//Member is below 7 years of age


* Keep relevant variables
keep household_id2 individual_id2 hh_s4q04 hh_s4q05 hh_s4q06 hh_s4q07 hh_s4q08

* How many members spent some time in [ACTIVITY]?
foreach act of varlist hh_s4q04 hh_s4q05 hh_s4q06 hh_s4q07 hh_s4q08 {
	gen any_`act'=(`act'!=0)
	bysort household_id2: egen hh_`act'=sum(any_`act')
	drop any_`act'
}
label var hh_hh_s4q04 "How many members spent at least an hour in the last 7 days on HH agricultural activities?"
label var hh_hh_s4q05 "How many members spent at least an hour in the last 7 days running/helping with non-agri HH business?"
label var hh_hh_s4q06 "How many members spent at least an hour in the last 7 days engaged in casual/part-time labor?"
label var hh_hh_s4q07 "How many members spent at least an hour in the last 7 days on any work for salary/wage/comission?"
label var hh_hh_s4q08 "How many members spent at least an hour in the last 7 days on unpaid apprenticeship?"
ren (hh_hh_s4q04 hh_hh_s4q05 hh_hh_s4q06 hh_hh_s4q07 hh_hh_s4q08) (hh_jobagri hh_jobbusiness hh_jobcasual hh_jobwage hh_jobunpaid)
ren (hh_s4q04 hh_s4q05 hh_s4q06 hh_s4q07 hh_s4q08) (hhead_jobagri hhead_jobbusiness hhead_jobcasual hhead_jobwage hhead_jobunpaid)
label var hhead_jobagri "Did the HHead spend at least one hour in the last 7 days on HH agricultural activities?"
label var hhead_jobbusiness "Did the HHead spend at least one hour in the last 7 days running/helping with non-agri HH business?"
label var hhead_jobcasual "Did the HHead spend at least one hour in the last 7 days engaged in casual/part-time labor?"
label var hhead_jobwage "Did the HHead spend at least one hour in the last 7 days on any work for salary/wage/comission?"
label var hhead_jobunpaid "Did the HHead spend at least one hour in the last 7 days on unpaid apprenticeship?"

merge 1:1 household_id2 individual_id2 using "${temp}/hh_temp.dta"
keep if _merge==3
drop _merge
sort household_id2
order household_id2 individual_id2 hh_wgt-hh_spent_schoolsup
save "${temp}/hh_temp.dta", replace

 
*******************
*** 4B. Savings ***
*******************

use "${dta}/sect4b_hh_w3.dta", clear
unique household_id2	//Information about 23,397 individuals from 4,954 households
fre hh_s4bq01
drop if hh_s4bq01!=1	//Member is below 18 years of age

* Keep relevant variables
keep household_id2 individual_id2 hh_s4bq02 hh_s4bq04a hh_s4bq04b hh_s4bq04c hh_s4bq04d hh_s4bq07 hh_s4bq15 hh_s4q16a hh_s4q16b hh_s4q16c hh_s4q16d hh_s4q16e hh_s4bq21

* Does any member of the household have a registration or account at a bank, MFI, SACCO, etc?
fre hh_s4bq02
replace hh_s4bq02=0 if hh_s4bq02==2
bysort household_id2: egen hh_account=max(hh_s4bq02)
drop hh_s4bq02
label var hh_account "Does any member of the HH have an account with a Bank/MFI/SACCO/...?"
label define yesno 0 "No" 1 "Yes"
label values hh_account yesno
fre hh_account

* Has any member of the HH used the following products?
foreach prod of varlist hh_s4bq04a hh_s4bq04b hh_s4bq04c hh_s4bq04d {
	gen any_`prod'=`prod'==1
	bysort household_id2: egen hh_`prod'=max(any_`prod')
	drop any_`prod' `prod' 
	label values hh_`prod' yesno
}
ren (hh_hh_s4bq04a hh_hh_s4bq04b hh_hh_s4bq04c hh_hh_s4bq04d) (hh_ATMDebit hh_ebanking hh_mobbanking hh_agentbanking)
label var hh_ATMDebit "In the last 12 months, has any member of the HH used ATM/Debit Card?"
label var hh_ebanking "In the last 12 months, has any member of the HH used Online Banking?"
label var hh_mobbanking "In the last 12 months, has any member of the HH used Mobile Banking?"
label var hh_agentbanking "In the last 12 months, has any member of the HH used Agent Banking?"

	* Has any member of the HH dsaved in any way in the last 12 months?
	fre hh_s4bq07
	replace hh_s4bq07=0 if hh_s4bq07==2
	bysort household_id2: egen hh_savings=max(hh_s4bq07)
	drop hh_s4bq07
	label var hh_savings "Has any member of the HH dsaved in any way in the last 12 months?"
	label values hh_savings yesno
	fre hh_savings
	
* Has any member of the GG owned/used any formal insurance?
fre hh_s4bq15
drop hh_s4bq15 hh_s4q16a-hh_s4q16e //Too little variance

	* Does any member of the HH know how to open an account?
	fre hh_s4bq21
	replace hh_s4bq21=0 if hh_s4bq21==2
	bysort household_id2: egen hh_openaccount=max(hh_s4bq21)
	drop hh_s4bq21
	label var hh_openaccount "Does any member of the HH know how to open an account?"
	label values hh_openaccount yesno
	fre hh_openaccount
	
merge 1:1 household_id2 individual_id2 using "${temp}/hh_temp.dta"
keep if _merge==3
drop _merge
sort household_id2
order household_id2 individual_id2 hh_wgt-hh_jobunpaid
save "${temp}/hh_temp.dta", replace

****************************
*** 5A. Food Last 7 days ***
****************************

use "${dta}/sect5a_hh_w3.dta", clear
unique household_id2	//Information about 4,954 households

keep household_id2 hh_s5aq0a hh_s5aq01
ren (hh_s5aq0a hh_s5aq01) (item consumed)
recode consumed (2=0)
label values consumed yesno
label define yesno 0 "No" 1 "Yes"
label values consumed yesno
save "$temp/temp_food.dta", replace
collapse (mean) consumed, by(item)
gen food=consumed
drop consumed
keep if food>=.1 & food<=.9
gen code_item=_n
merge 1:m item using "$temp/temp_food.dta"
sort household_id2
keep if _merge==3
drop food _merge item
quietly bysort household_id2 code_item: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
reshape wide consumed, i(household_id2) j(code_item)
foreach var in consumed1-consumed32{
	label values `var' yesno
	}
	label var consumed1 "Consumed Banana"
	label var consumed2 "Consumed Barley"
	label var consumed3 "Consumed Beef"
	label var consumed4 "Consumed Butter/ghee"
	label var consumed5 "Consumed Chat/kat"
	label var consumed6 "Consumed Chick Pea"
	label var consumed7 "Consumed Coffee"
	label var consumed8 "Consumed Eggs"
	label var consumed9 "Consumed Field Pea"
	label var consumed10 "Consumed Green chili pepper (kariya)"
	label var consumed11 "Consumed Greens (kale, cabbage, etc.)"
	label var consumed12 "Consumed Haricot Beans"
	label var consumed13 "Consumed Horsebeans"
	label var consumed14 "Consumed Kocho"
	label var consumed15 "Consumed Lentils"
	label var consumed16 "Consumed Maize"
	label var consumed17 "Consumed Milk"
	label var consumed18 "Consumed Oils (processed)"
	label var consumed19 "Consumed Onion"
	label var consumed20 "Consumed Pasta/Maccaroni"
	label var consumed21 "Consumed Potato"
	label var consumed22 "Consumed Purchased Injera"
	label var consumed23 "Consumed Purchased Bread or Biscuit"
	label var consumed24 "Consumed Red pepper (berbere)"
	label var consumed25 "Consumed Soft drinks/soda"
	label var consumed26 "Consumed Sorghum"
	label var consumed27 "Consumed Sugar"
	label var consumed28 "Consumed Tea"
	label var consumed29 "Consumed Teff"
	label var consumed30 "Consumed Tella"
	label var consumed31 "Consumed Tomato"
	label var consumed32 "Consumed Wheat"
	
	foreach con of varlist consumed* {
		ren `con' hh_`con'
	}
	
merge 1:1 household_id2 using "${temp}/hh_temp.dta"
keep if _merge==3
drop _merge
sort household_id2
order household_id2 individual_id2 hh_wgt-hh_openaccount
save "${temp}/hh_temp.dta", replace

*******************************************************
*** 6A. Non-food expenditures in the LAST ONE MONTH ***
*******************************************************

use "${dta}/sect6a_hh_w3.dta", clear
unique household_id2	//Information about 4,954 households
keep household_id2 hh_s6aq00 hh_s6aq01
ren (hh_s6aq00 hh_s6aq01) (item purchased)
recode purchased (2=0)
label values purchased yesno
label define yesno 0 "No" 1 "Yes"
label values purchased yesno
save "$temp/temp_items.dta", replace
collapse (mean) purchased, by(item)
gen food=purchased
drop purchased
keep if food>=.1 & food<=.9
merge 1:m item using "$temp/temp_items.dta"
keep if _merge==3
drop food _merge
quietly bysort household_id2 item: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
reshape wide purchased, i(household_id2) j(item)
	label var purchased1 "Purchased Matches"
	label var purchased2 "Purchased Batteries"
	label var purchased3 "Purchased Candles (tua'af), incense"
	label var purchased5 "Purchased Hand/body soap"
	label var purchased6 "Purchased Other personal care goods (incl. sendel, mantent,...)"
	label var purchased7 "Purchased Charcoal"
	label var purchased8 "Purchased Firewood"
	label var purchased9 "Purchased Kerosene"
	label var purchased11 "Purchased Transport"
	label var purchased12 "Purchased House Rent"
	
	foreach pur of varlist purchased* {
		ren `pur' hh_`pur'
	}
	
merge 1:1 household_id2 using "${temp}/hh_temp.dta"
keep if _merge==3
drop _merge
sort household_id2
order household_id2 individual_id2 hh_wgt-hh_consumed32
save "${temp}/hh_temp.dta", replace

**************************************************
*** 6B. Non-food expenditures in the LAST YEAR ***
**************************************************

use "${dta}/sect6b_hh_w3.dta", clear
unique household_id2	//Information about 4,954 households

keep household_id2 hh_s6bq00 hh_s6bq03
ren (hh_s6bq00 hh_s6bq03) (item purchase)
recode purchase (2=0)
label values purchase yesno
label define yesno 0 "No" 1 "Yes"
label values purchase yesno
save "$temp/temp_items2.dta", replace
collapse (mean) purchase, by(item)
gen food=purchase
drop purchase
keep if food>=.1 & food<=.9
merge 1:m item using "$temp/temp_items2.dta"
keep if _merge==3
drop food _merge
quietly bysort household_id2 item: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
reshape wide purchase, i(household_id2) j(item)
	label var purchase1 "Purchased Clothes/shoes/fabric for MEN (18 years and older)"
	label var purchase2 "Purchased Clothes/shoes/fabric for WOMEN (18 years and older)"
	label var purchase3 "Purchased Clothes/shoes/fabric for BOYS (less than 18 years)"
	label var purchase4 "Purchased Clothes/shoes/fabric for GIRLS (less than 18 years)"
	label var purchase5 "Purchased Kitchen equipment (cooking pots, etc.)"
	label var purchase6 "Purchased Linens (sheets, towels, blankets)"
	label var purchase8 "Purchased Lamp/torch"
	label var purchase9 "Purchased Ceremonial expeses"
	label var purchase10 "Purchased Contributions to IDDIR"
	label var purchase11 "Purchased Donations to the churches and mosques"
	label var purchase12 "Purchased Taxes and levies"
	
	foreach ite of varlist purchase* {
		ren `ite' hh_`ite'
	}
	
merge 1:1 household_id2 using "${temp}/hh_temp.dta"
keep if _merge==3
drop _merge
sort household_id2
order household_id2 individual_id2 hh_wgt-hh_purchased12
save "${temp}/hh_temp.dta", replace	

************************
*** 7. Food Security ***
************************

use "${dta}/sect7_hh_w3.dta", clear
unique household_id2	//Information about 4,954 households

* Keep relevant variables
keep household_id2 hh_s7q01 hh_s7q02_a hh_s7q02_b hh_s7q02_c hh_s7q02_d hh_s7q02_e hh_s7q02_f hh_s7q02_g hh_s7q02_h

	ren (hh_s7q01 hh_s7q02_a hh_s7q02_b hh_s7q02_c hh_s7q02_d hh_s7q02_e hh_s7q02_f hh_s7q02_g hh_s7q02_h)	///
	(hh_enoughfood hh_fs1 hh_fs2 hh_fs3 hh_fs4 hh_fs5 hh_fs6 hh_fs7 hh_fs8)

merge 1:1 household_id2 using "${temp}/hh_temp.dta"
keep if _merge==3
drop _merge
sort household_id2
order household_id2 individual_id2 hh_wgt-hh_purchase12
save "${temp}/hh_temp.dta", replace	

**********************************
*** 9. Housing characteristics ***
**********************************

use "${dta}/sect9_hh_w3.dta", clear
unique household_id2 //Information about 4,954 households

* Keep relevant variables 
keep household_id2 household_id2 hh_s9q03 hh_s9q04 hh_s9q05 hh_s9q06 hh_s9q07 hh_s9q08 hh_s9q09 hh_s9q10 hh_s9q10b hh_s9q10c hh_s9q10d hh_s9q10e hh_s9q12 hh_s9q13 hh_s9q13_e hh_s9q14 hh_s9q19_a hh_s9q21

*Household Tenure Status
ren hh_s9q03 hh_status
tab hh_status, mis

*Number of rooms
ren hh_s9q04 hh_rooms
tab hh_rooms, mis

*Wall material
ren hh_s9q05 hh_walls
tab hh_walls, mis

*Roof material
ren hh_s9q06 hh_roof
tab hh_roof, mis

*Floor material
ren hh_s9q07 hh_floor
tab hh_floor, mis

*Type of kitchen
ren hh_s9q08 hh_kitchen
tab hh_kitchen, mis

*Type of oven
ren hh_s9q09 hh_oventype
tab hh_oventype, mis

*Toilet facilities
ren hh_s9q10 hh_toilet
tab hh_toilet, mis

*Toilet shared
ren hh_s9q10b hh_toiletshared
tab hh_toiletshared, mis
recode hh_toiletshared (2=0)
replace hh_toiletshared=3 if hh_toiletshared==.
label define toil 0 "No" 1 "Yes" 3 "Toilet=Field/Forest"
label values hh_toiletshared toil

*Washing
ren hh_s9q10c hh_wash
tab hh_wash, mis

*Water at washing location
ren hh_s9q10d hh_washwater
tab hh_washwater, mis
recode hh_washwater (2=0)
replace hh_washwater=3 if hh_washwater==.
label define washwater 0 "No" 1 "Yes" 3 "No washing location"
label values hh_washwater washwater

*Soap at washing location
ren hh_s9q10e hh_washsoap
tab hh_washsoap, mis
replace hh_washsoap=5 if hh_washsoap==.
label define washsoap 1 "Yes, soap/detergent" 2 "Yes, ash" 3 "Yes, mud/sand" 4 "No" 5 "No washing location"
label values hh_washsoap washsoap

*Type of disposal facilities
ren hh_s9q12 hh_disposal
tab hh_disposal, mis

*Source of drinking water
ren hh_s9q13 hh_water
tab hh_water, mis

*Different source of drinking water during the dry season
ren hh_s9q13_e hh_waterdry
tab hh_waterdry, mis //Few heterogeneity, so dropping
drop hh_waterdry hh_s9q14

*Source of light
ren hh_s9q19 hh_light
tab hh_light, mis

*Source for cooking
ren hh_s9q21 hh_cook
tab hh_cook, mis

merge 1:1 household_id2 using "${temp}/hh_temp.dta"
keep if _merge==3
drop _merge
sort household_id2
order household_id2 individual_id2 hh_wgt-hh_fs8
save "${temp}/hh_temp.dta", replace	

****************************
*** 10. Assets ownership ***
****************************

use "${dta}/sect10_hh_w3.dta", clear
unique household_id2 //Information about 4,954 households

keep household_id2 hh_s10q00 hh_s10q01
ren (hh_s10q00 hh_s10q01) (item hmany)
quietly bysort household_id2 item: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
reshape wide hmany, i(household_id2) j(item)
	label var hmany1 "How many KEROSENE STOVE does the household have?" //Drop
	label var hmany2 "How many CYLINDER GASSTOVE does the household have?" //Drop
	label var hmany3 "How many ELECTRIC STOVE does the household have?" 
	label var hmany4 "How many BLANKET/GABI does the household have?" 
	label var hmany5 "How many MATTRESS AND/OR BED does the household have?"
	label var hmany6 "How many WRIST WATCH/CLOCK does the household have?"
	label var hmany7 "How many FIXED LINE TELEPHONE does the household have?" //Drop
	label var hmany8 "How many MOBILE TELEPHONE does the household have?"
	label var hmany9 "How many RADIO/TAPE RECORDER does the household have?"
	label var hmany10 "How many TELEVISION does the household have?"
	label var hmany11 "How many CD/VCD/DVD/VIDEO DECK does the household have?"
	label var hmany12 "How many SATELITE DISH does the household have?"
	label var hmany13 "How many SOFA SET does the household have?"
	label var hmany14 "How many BICYCLE does the household have?" //Dropping
	label var hmany15 "How many MOTOR CYCLE does the household have?" //Dropping
	label var hmany16 "How many CART (HAND PUSHED) does the household have?" //Dropping
	label var hmany17 "How many CART (ANIMAL DRAWN)- FOR TRANSPORTING PEOPLE AND GOODS does the household have?" // Dropping
	label var hmany18 "How many SEWING MACHINE does the household have?" //Dropping
	label var hmany19 "How many WEAVING EQUIPMENT does the household have?" //Dropping
	label var hmany20 "How many MITAD-ELECTRIC does the household have?" 
	label var hmany21 "How many ENERGY SAVING STOVE (LAKECH, MIRT ETC) does the household have?"
	label var hmany22 "How many REFRIGERATOR does the household have?"
	label var hmany23 "How many PRIVATE CAR does the household have?" //Dropping
	label var hmany24 "How many JEWELS GOLD (IN GRAMS)does the household have?"
	label var hmany25 "How many JEWELS SILVER (IN GRAMS) does the household have?"
	label var hmany26 "How many WARDROBE does the household have?"
	label var hmany27 "How many HELF FOR STORING GOODS does the household have?"
	label var hmany28 "How many BIOGAS PIT does the household have?" //Dropping
	label var hmany29 "How many WATER STORAGE PIT does the household have?" //Dropping
	label var hmany30 "How many SICKLE (MACHID) does the household have?"
	label var hmany31 "How many AXE (GEJERA) does the household have?"
	label var hmany32 "How many PICK AXE (GESO) does the household have?"
	label var hmany33 "How many PLOUGH (TRADITIONAL) does the household have?"
	label var hmany34 "How many PLOUGH (MODERN) does the household have?" //Dropping
	label var hmany35 "How many WATER PUMP does the household have?" //Dropping
	//Most of the assets have less than 10% of ownershiop. Assets will more than 90% in the 0 category will be dropped.
drop hmany1 hmany2 hmany7 hmany14 hmany15 hmany16 hmany17 hmany18 hmany19 hmany23 hmany28 hmany29 hmany34 hmany35

merge 1:1 household_id2 using "${temp}/hh_temp.dta"
keep if _merge==3
drop _merge
sort household_id2
order household_id2 individual_id2 hh_wgt-hh_cook
save "${temp}/hh_temp.dta", replace	

merge 1:1 household_id2 using "${clean}/poverty_ESS3.dta", keepusing(household_id2 poor_npl1-poor_bottom_80)
keep if _merge==3
drop _merge
sort household_id2
order household_id2 individual_id2 hh_wgt-urban poor* 

save "${clean}/ESS3_2016.dta", replace
