********************************************************************************
**	TITLE: 		05_Ethiopia_ESS3_qkey.do
**
**	PURPOSE: 	Creates PPI Question Key
**
**	INPUTS: 	${clean}/PPI_2016_Ethiopia_full.dta
**
** 	OUTPUTS: 	${clean}/Ethiopia_ESS3_QuestionKEY_full.csv
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
log using "${logs}/05_Ethiopia_ESS3_qkey_`cdate'.smcl", replace
di `cdate'		
	

				************************************************
				************************************************
				*******Arrange for question key CSV file******** After changing the globals: this code should run without any further changes
				************************************************
				************************************************
				u "$clean/$ppidb", clear
				
				*************To customize:*****************
				///////////////////////////////////////////
				global obs_start = 23 //This is the observation where questions that were split into different categories begin
				global obs_na = 22 //Observations such as poverty rates, weights and urban/rural are not supposed to be included. 
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
				generate varname2=substr(varname,1, strlen(varname)-$minus)   //Make sure to change the numbers in each case
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
				
		
**C. Save
	* Save
	export delimited using "$clean/$qkey",  replace

	* Close log
	log c

**EOF**




		
