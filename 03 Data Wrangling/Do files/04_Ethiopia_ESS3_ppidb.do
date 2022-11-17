********************************************************************************
**	TITLE: 		04_Ethiopia_ESS3_ppidb.do
**
**	PURPOSE: 	Creates the PPI database only with relevant binary variables
**
**	INPUTS: 	${clean}/MASTER_2016_Ethiopia_full.dta
**
** 	OUTPUTS: 	${clean}/PPI_2016_Ethiopia_full.dta
**
**	AUTHOR: 	Manuel Cardona 
**
**	CREATED:	2021/12/22
********************************************************************************
*Table of Contents
*1. 
*2. 


loc cdate: di %td_CCYY_NN_DD date(c(current_date), "DMY")
loc cdate = subinstr(trim("`cdate'"), " " , "", .)
log using "${logs}/04_Ethiopia_ESS3_ppidb_`cdate'.smcl", replace
di `cdate'		
		
		************************************************
		************************************************
		*************Arrange for custom PPI*************	
		************************************************
		************************************************
		u "$clean/$masterdb", clear
		set more off
		
		*************To customize:*****************
		
		///////////////////////////////////////////
		
				ren hh_consumed1 hh_consumed01
				ren hh_consumed2 hh_consumed02
				ren hh_consumed3 hh_consumed03
				ren hh_purchased1 hh_purchased01
				ren hh_purchase1 hh_purchase01

				
		global vlist "region hh_size hhead_sex hhead_age hh_readwrite hhead_readwrite hh_attended hhead_attended hh_primary hhead_primary hh_spent_schoolfees hh_spent_schoolsup hhead_jobagri hhead_jobbusiness hhead_jobcasual hhead_jobwage hhead_jobunpaid hh_account hh_savings hh_openaccount hh_consumed01 hh_consumed02 hh_consumed03 hh_consumed4 hh_consumed5 hh_consumed6 hh_consumed7 hh_consumed8 hh_consumed9 hh_consumed10 hh_consumed11 hh_consumed12 hh_consumed13 hh_consumed14 hh_consumed15 hh_consumed16 hh_consumed17 hh_consumed18 hh_consumed19 hh_consumed20 hh_consumed21 hh_consumed22 hh_consumed23 hh_consumed24 hh_consumed25 hh_consumed26 hh_consumed27 hh_consumed28 hh_consumed29 hh_consumed30 hh_consumed31 hh_consumed32 hh_purchased01 hh_purchased2 hh_purchased3 hh_purchased5 hh_purchased6 hh_purchased7 hh_purchased8 hh_purchased9 hh_purchased11 hh_purchased12 hh_purchase01 hh_purchase2 hh_purchase3 hh_purchase4 hh_purchase5 hh_purchase6 hh_purchase8 hh_purchase9 hh_purchase10 hh_purchase11 hh_purchase12 hh_status hh_rooms hh_walls hh_roof hh_floor hh_kitchen hh_oventype hh_toilet hh_toiletshared hh_wash hh_disposal hh_water hh_light hh_cook hmany03 hmany04 hmany05 hmany06 hmany08 hmany09 hmany10 hmany13 hmany20 hmany21 hmany26 hmany27 hmany30 hmany33"
		global poor poor_npl1
		global advars "household_id2 hh_wgt urban poor_npl1 poor_npl2 poor_150npl1 poor_200npl1 poor_2011_100 poor_2011_190 poor_2011_320 poor_2011_550 poor_2011_800 poor_2011_1100 poor_2011_1500 poor_2011_2170 poor_2005_125 poor_2005_250 poor_2005_500 poor_bottom_20 poor_bottom_40 poor_bottom_60 poor_bottom_80"
		global tobe_dropped "individual_id2"
		
		
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
		
		
**C. Save
	* Save
	saveold "$clean/$ppidb", replace version(12)

	* Close log
	log c

**EOF**


