*********************************
*   -Purpose: Clean databases   *
*   -Author: Manuel Cardona		*
*********************************

set more off
clear all

*Manuel Cardona Arias	
if "`c(username)'" == "manuelarias" {
	gl path "/Users/manuelarias/Documents/GitHub/satellite_to_poverty_IPA"
}


global dta "$path/02 Data/01 Raw/LSMS/Household"
global clean "$path/02 Data/02 Clean/LSMS"
global temp "$path/02 Data/01 Raw/Temp"
global geo "$path/02 Data/01 Raw/LSMS/Geovariables"



*************
* Section 1 *
*************

import delimited "$dta/sect1_hh_w3.csv", clear

duplicates report household_id individual_id
sort household_id individual_id
drop household_id2 individual_id2 saq08 hh_s1q00 hh_s1q01
ren (household_id individual_id) (hh_id ind_id) 

*Any mebers below certain age of the non-adult age interval
tab hh_s1q04a hh_s1q04b, mis
ren (hh_s1q04a hh_s1q04b) (age_yrs age_months)
replace age_yrs=0 if age_yrs==. & age_months!=.

ren hh_s1q04h age_2
replace age_yrs=age_2 if age_2!=.  //Even replacing with all the possible age vars, we have 14.33% of missing values. 

*Household Size
bysort hh_id: gen number=_n
bysort hh_id: egen hh_size=max(number)
drop number
label var hh_size "Household Size"

*Sex of household members
	//5% of missing values
	
*Variables for the Household Head
ren hh_s1q02 hh_sex
keep if hh_sex==1 //Keep if household head

keep hh_id ind_id hh_s1q03 age_yrs hh_s1q07 hh_s1q08 hh_s1q32_a hh_s1q32_b
ren (hh_s1q03 age_yrs hh_s1q07 hh_s1q08 hh_s1q32_a hh_s1q32_b) (hhead_sex hhead_age hhead_religion hhead_maritals hhead_occup hhead_occup2)

	//Occupation is full of missings, so dropping
	drop hhead_occup*

tab hhead_sex, mis
tab hhead_age, mis
tab hhead_religion, mis
tab hhead_maritals, mis

quietly by hh_id ind_id: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup

save "$temp/Ethiopia_temp1.dta", replace

*************
* Section 2 *
*************
import delimited "$dta/sect2_hh_w3.csv", clear

duplicates report household_id individual_id
quietly bysort household_id individual_id: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
ren (household_id individual_id) (hh_id ind_id) 


merge 1:1 hh_id ind_id using "$temp/Ethiopia_temp1.dta"
keep if _merge==3  //Keep if Household Head
keep hh_id ind_id rural pw_w3 saq01 saq02 saq04 hh_s2q00 hh_s2q02 hh_s2q03 hh_s2q05 hhead_sex hhead_age hhead_religion hhead_maritals
ren (hh_s2q02 hh_s2q03 hh_s2q05) (hhead_readwrite hhead_attended hhead_grade)

*Household Head can read and write
tab hhead_readwrite, mis

*Household Head attended school
tab hhead_attended, mis

*Household Head highest grade
tab hhead_grade, mis

save "$temp/Ethiopia_temp1.dta", replace

import delimited "$dta/sect2_hh_w3.csv", clear

duplicates report household_id individual_id
quietly bysort household_id individual_id: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
ren (household_id individual_id) (hh_id ind_id) 

merge 1:1 hh_id ind_id using "$temp/Ethiopia_temp1.dta"
keep hh_id ind_id rural pw_w3 saq01 saq02 saq04 hh_s2q00 hh_s2q02 hh_s2q03 hh_s2q05 hhead_readwrite-hhead_maritals _merge

*Can anyone in the household read and write in any language?
	//This question applies just for individuals 5 years or older
bysort hh_id: egen hh_readwrite=min(hh_s2q02)
label var hh_readwrite "Can anyone in the household read and write in any language?"
replace hh_readwrite=0 if hh_readwrite==2
label define yesno 0 "No" 1 "Yes"
label values hh_readwrite yesno
tab hh_readwrite
drop hh_s2q02

*Has anyone in the household ever attended school?
	//This question applies just for individuals 5 years or older
bysort hh_id: egen hh_attended=min(hh_s2q03)
label var hh_attended "Has anyone in the household ever attended school?"
replace hh_attended=0 if hh_attended==2
label values hh_attended yesno
tab hh_attended

*What is the highest grade that anyone in your household has completed?
	//This question applies just for individuals 5 years or older
bysort hh_id: egen hh_grade=min(hh_s2q05)
replace hh_s2q05=999 if hh_s2q03==2
	//Since we cannot order the levels of education from lowest to highest, we are not using this variable for any member.
drop hh_grade hh_s2q05 hh_s2q03

keep if _merge==3 //Keep one observation per household
drop _merge
save "$temp/Ethiopia_temp1.dta", replace

*************
* Section 3 *
*************

import delimited "$dta/sect3_hh_w3.csv", clear

duplicates report household_id individual_id
quietly bysort household_id individual_id: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
ren (household_id individual_id) (hh_id ind_id) 

keep hh_id ind_id hh_s3q03 hh_s3q08a 

merge 1:1 hh_id ind_id using "$temp/Ethiopia_temp1.dta"

*Health questions for anyone in the household
*Health problems
bysort hh_id: egen hh_hproblem=min(hh_s3q03)
tab hh_hproblem, mis
label var hh_hproblem "Has anyone in your household faced any health problems during the last 4 weeks?"
replace hh_hproblem=0 if hh_hproblem==2
label values hh_hproblem yesno

*Medical assistance
bysort hh_id: egen hh_medassist=min(hh_s3q08a)
tab hh_medassist, mis
label var hh_medassist "Has anyone in your household consulted any medical assistance during the last 12 months?"
replace hh_medassist=0  if hh_medassist==2
label values hh_medassist yesno

*Health questions foor the Household Head
keep if _merge==3 //Keep if Household Head (1 obs per household)
*Health problems
ren hh_s3q03 hhead_hproblem
label var hhead_hproblem "Has the Household Head faced any health problems during the last 4 weeks?"
replace hhead_hproblem=0 if hhead_hproblem==2
label values hhead_hproblem yesno

*Medical assistance
ren hh_s3q08a hhead_medassist
label var hhead_medassist "Has the Household Head consulted any medical assitance during the last 12 months?"
replace hhead_medassist=0 if hhead_medassist==2
label values hhead_medassist yesno

quietly bysort hh_id: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup

drop _merge
save "$temp/Ethiopia_temp1.dta", replace

**************
* Section 5a *
**************
import delimited "$dta/sect5a_hh_w3.csv", clear
drop household_id2 
ren household_id hh_id
keep hh_id hh_s5aq0a hh_s5aq01
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
sort hh_id
keep if _merge==3
drop food _merge item
quietly bysort hh_id code_item: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
reshape wide consumed, i(hh_id) j(code_item)
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
	
	//Drop items that might be seasonal/problematic
	drop consumed6 consumed9 consumed10 consumed12 consumed13 consumed14 consumed19 consumed22 consumed23 consumed24 consumed25 consumed29 consumed30 consumed31
	
merge 1:1 hh_id using "$temp/Ethiopia_temp1.dta"
keep if _merge==3
drop _merge
save "$temp/Ethiopia_temp1.dta", replace

**************
* Section 6a *
**************
import delimited "$dta/sect6a_hh_w3.csv", clear
drop household_id2 
ren household_id hh_id
keep hh_id hh_s6aq00 hh_s6aq01
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
quietly bysort hh_id item: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
reshape wide purchased, i(hh_id) j(item)
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
	
	drop purchased1 purchased2 purchased6 purchased11 purchased12
	
merge 1:1 hh_id using "$temp/Ethiopia_temp1.dta"
keep if _merge==3
drop _merge
save "$temp/Ethiopia_temp1.dta", replace

**************
* Section 6b *
**************
import delimited "$dta/sect6b_hh_w3.csv", clear
drop household_id2 
ren household_id hh_id
keep hh_id hh_s6bq00 hh_s6bq03
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
quietly bysort hh_id item: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
reshape wide purchase, i(hh_id) j(item)
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
	
	drop purchase1 purchase2 purchase3 purchase4 purchase10 purchase11 purchase12
	
merge 1:1 hh_id using "$temp/Ethiopia_temp1.dta"
keep if _merge==3
drop _merge
save "$temp/Ethiopia_temp1.dta", replace

*************
* Section 7 *
*************
import delimited "$dta/sect7_hh_w3.csv", clear
drop household_id2 

duplicates report household_id
quietly bysort household_id: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
ren (household_id) (hh_id) 

keep hh_id hh_s7q01 hh_s7q06 hh_s7q02_a-hh_s7q02_h
ren (hh_s7q01 hh_s7q02_a hh_s7q02_b hh_s7q02_c hh_s7q02_d hh_s7q02_e hh_s7q02_f hh_s7q02_g hh_s7q02_h hh_s7q06) (hh_fs1 hh_fs2a hh_fs2b hh_fs2c hh_fs2d hh_fs2e hh_fs2f hh_fs2g hh_fs2h hh_fs6)

*Food Security Questions
recode hh_fs1 (2=0)
recode hh_fs6 (2=0)
label define yesno 0 "No" 1"Yes"
label values hh_fs1 yesno
label values hh_fs6 yesno

merge 1:1 hh_id using "$temp/Ethiopia_temp1.dta"
keep if _merge==3
drop _merge
save "$temp/Ethiopia_temp1.dta", replace


************
* Section 9*
************
import delimited "$dta/sect9_hh_w3.csv", clear

*Dwelling Characteristics
duplicates report household_id
quietly bysort household_id: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
ren household_id hh_id
keep hh_id household_id2 hh_s9q03 hh_s9q04 hh_s9q05 hh_s9q06 hh_s9q07 hh_s9q08 hh_s9q09 hh_s9q10 hh_s9q10b hh_s9q10c hh_s9q10d hh_s9q10e hh_s9q12 hh_s9q13 hh_s9q13_e hh_s9q14 hh_s9q19_a hh_s9q21

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

merge 1:1 hh_id using "$temp/Ethiopia_temp1.dta"
keep if _merge==3
drop _merge
save "$temp/Ethiopia_temp1.dta", replace

*************
* Section 10*
*************
import delimited "$dta/sect10_hh_w3.csv", clear

drop household_id2
ren household_id hh_id

keep hh_id hh_s10q00 hh_s10q01
ren (hh_s10q00 hh_s10q01) (item hmany)
quietly bysort hh_id item: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
reshape wide hmany, i(hh_id) j(item)
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

merge 1:1 hh_id using "$temp/Ethiopia_temp1.dta"
keep if _merge==3
drop _merge
save "$temp/Ethiopia_temp1.dta", replace

*Order variables
order hh_id ind_id-hh_medassist consumed1-consumed32 purchased3-purchased9 purchase5-purchase9 hh_fs1-hh_fs6 hh_status-hh_cook hmany3-hmany33
compress
save "$clean/ESS3_Ethiopia.dta", replace

****************
* Geovariables *
****************

import delimited "$geo/ETH_HouseholdGeovars_y3.csv", clear
keep household_id2 lat_dd_mod lon_dd_mod
quietly bysort household_id2: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
merge 1:m household_id2 using "$clean/ESS3_Ethiopia.dta"
keep if _merge==3
drop _merge

ren (lat_dd_mod lon_dd_mod) (cluster_lat cluster_lon)

order hh_id ind_id household_id2 cluster_lat cluster_lon pw_w3 saq01 saq02 saq04 hh_s2q00 consumed1-consumed32 purchased3-purchased9 purchase5-purchase9 hh_fs1-hh_fs6 hh_status-hh_cook hmany3-hmany33
compress
save "$clean/ESS3_Ethiopia.dta", replace
