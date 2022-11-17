********************************************************************************
** 	TITLE: 		00_master.do
**
**	PURPOSE: 	Run all do files to clean and organize household-level data
**				
**	NOTES:
**
**	AUTHOR: 	Manuel Cardona	
**
**	CREATED:  	August 2022
**
**	MODFIIED: Manuel Cardona (August 17, 2022)
********************************************************************************
*Table of Contents:
*0. Set Directories
*1. Select indicators
*2. Create poverty indicators

clear all
cap log close

*version 16.1
set more off
pause on


**************************************
*	0. Setting directories and locals
**************************************
*A. Set lcoals
*B. Locations
*C. Globals


**A. Set Locals
	/* 
**#
	These locals control which dofiles are run
		Each one takes 	0 = Do not run or 1 = Run 
	*/
	loc select 		0 // Select indicators from original survey modules
	loc poverty		0 // Create Poverty Indicators
	loc manip		0 // Manipulates variables to convert them into dummy vars

**B. Locations 
	*Set content directory relatively based on location of do file
	if "`c(username)'" == "manuelarias" {
		loc path 		= "/Users/manuelarias/Documents/GitHub/satellite_to_poverty_IPA"	
	}
	else { 
		loc cdloc 		= subinstr("`c(pwd)'", "\", "/" ,.)
		loc path 		= subinstr("`cdloc'", "/GitHub/satellite_to_poverty_IPA", "", .)
	}
	
	*Save root directory for all users
	dis "`path''"
	
	*Folder globals
	gl dta 		"`path'/02 Data/01 Raw/LSMS/Household"
	gl clean 	"`path'/02 Data/02 Clean/LSMS/Full datasets"
	gl dos 		"`path'/03 Data Wrangling/Do files"
	gl temp 	"`path'/02 Data/01 Raw/Temp"
	gl logs		"`path'/03 Data Wrangling/Do files/log_files"
	gl fig 		"`path'/06 Figures/01 LSMS"
	gl gps		"`path'/02 Data/01 Raw/GPS"
	gl geo 		"`path'/02 Data/01 Raw/LSMS/Geovariables"
	
	*************To customize:*****************
	///////////////////////////////////////////
	global odb		"HBS_2016_Ethiopia_full.dta"
	global masterdb "MASTER_2016_Ethiopia_full.dta"
	global ppidb	"PPI_2016_Ethiopia_full.dta"
	global qkey		"Ethiopia_ESS3_QuestionKEY_full.csv"
	///////////////////////////////////////////

***************************************************
* 1. Select indicators from original survey modules
***************************************************

**A. Select indicators
	/*
	PURPOSE: 	Select indicators from original survey modules		

	INPUTS:		${raw}/SEVERAL MODULES

	OUTPUTS: 	${clean}/ESS3_2016.dta
	
	NOTES:
	*/
	if `select'	do "${dos}/01_Ethiopia_ESS3_selection.do"

***************************************
* 2. Create Poverty Indicators
***************************************

**B. Poverty Indicators
	/*
	PURPOSE: 	Create Poverty Indicators based on National, International and
				Relative Poverty Lines

	INPUTS:		${raw}/.dta

	OUTPUTS: 	${clean}/poverty_ESS3.dta

	NOTES:	 	
	*/
	if `poverty' 	do "${dos}/02_Ethiopia_ESS3_poverty.do"
	
***************************************
* 3. Manipulates Indicators
***************************************

**C. PPI Database
	/*
	PURPOSE: 	Converts indicators into categories

	INPUTS:		${raw}/MASTER_2016_Ethiopia_full.dta

	OUTPUTS: 	${clean}/PPI_2016_Ethiopia_full.dta

	NOTES:	 	
	*/
	if `manip' 	do "${dos}/03_Ethiopia_ESS3_manip.do"

