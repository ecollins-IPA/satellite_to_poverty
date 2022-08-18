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
