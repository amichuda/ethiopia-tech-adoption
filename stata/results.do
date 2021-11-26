clear all 

global root = "C:\Users\obarriga\Documents\GitHub\ethiopia-tech-adoption\"
global root_data = "${root}/data"
global root_results = "${root}/results"


global root_processed_data= "D:\Box Sync\ethiopia_data\data"



use "${root_data}/full_panel.dta", clear

* Get panel
duplicates tag household_id wave,gen(panel2)
keep if panel2 == 0 

ta wave, m 
duplicates tag household_id ,gen(panel)
ta panel
keep if panel == 2
ta wave


* **********************************************************************
* 1 - Generate variables that indicate trajectories (groups)
* **********************************************************************

preserve

    * Keep only panel indicator, panel variable indicator & main selection var
    * Selection variable should be 0/1
	
    keep            household_id  wave impmaize
	reshape         wide impmaize*, i(household_id  ) j(wave)

    drop if missing(impmaize1) | missing(impmaize2) | missing(impmaize3)

	* Generate a string variable to contain the trajectories
	gen             strL string_traj = ""

    * Expand variable list and put in local macro choice_vars
	unab            choice_vars: impmaize*


	* Turn choices into strings & collate
	foreach         var of varlist `choice_vars' {
		tostring    `var', gen(`var'S)
		replace     string_traj = string_traj + `var'S
	}

/* pause on
pause */

    * Create encoded version
	encode          string_traj, gen(trajectory)
	lab var         trajectory "Trajectory group indicators"
	drop            impmaize*
    drop            string_traj

	tab             trajectory      /* hopefully looks good */
	tempfile        trajectory
	save            `trajectory'

    restore

* Merge the trajectories to the rest of the dataset
merge           m:1  household_id using `trajectory', nogen


* Keep only trajectory exists (we lose 84,368)
drop if missing(trajectory)

* Create log yield variable
foreach var of varlist YIELD_cropcutfresh_tr YIELD_cropcutdry_tr YIELD_selfr_tr {
    gen ln_`var' = ln(`var')
}


global          never 1
tab             trajectory

global          always `r(r)'
global          lastswitcher = $always-1

numlist         "2(1)$lastswitcher"
global          switchers `r(numlist)'

numlist         "1(1)$lastswitcher"
global          noalways `r(numlist)'

/* We have to define a local with all the mu parameters
	needed to identify \phi. */
local           switcherpars ({mu:3.trajectory} - ///
					{mu:2.trajectory})*(3.trajectory#1.impmaize)

foreach num of numlist $switchers {
	if `num'>3 {
		local   switcherpars `switcherpars' + ({mu:`num'.trajectory} ///
		- {mu:2.trajectory})*(`num'.trajectory#1.impmaize)
	di "`switcherpars'"
	}
}

matrix define initval = (1,-1,1,-1,1,-1,1,-1,1,-1)

loc controls = "yrseduc age_head sex_head  title parcesizeHA hhlabor hiredlabor fertcosts dirrigation"

local controls_gmm_exp - {xb:`controls'}

reg ln_YIELD_cropcutdry_tr i(${noalways}).trajectory 1.impmaize i(${switchers}).trajectory ${always}.trajectory#1.impmaize `controls'
matrix          ols_init = e(b)

keep if e(sample)	== 1 
reg ln_YIELD_cropcutdry_tr i(${noalways}).trajectory 1.impmaize i(${switchers}).trajectory ${always}.trajectory#1.impmaize 
matrix          ols_init = e(b)


foreach var of varlist YIELD_cropcutfresh_tr YIELD_cropcutdry_tr YIELD_selfr_tr {
	gmm     	(ln_`var' - {mu: i($noalways).trajectory} ///              	
				 - {Delta}*(1.impmaize)  /// 
				- {phi}*(`switcherpars')            ///
				- ({mu_always} + {phi}*({mu_always}                     ///
				- {mu:2.trajectory}))*($always.trajectory#1.impmaize))    ///
				, instruments(i($noalways).trajectory 1.impmaize         ///
				i($switchers $always).trajectory#1.impmaize , nocons)     ///
				vce(cluster holder_id)  winitial(identity) from(ols_init)

	estat overid


}






sadddddddddddddddd
* Append

use "${root_processed_data}/processed/r1_croproster.dta"
gen round = 1

append using "${root_processed_data}/processed/r2_croproster.dta"
replace round = 2 if round == .

append using "${root_processed_data}/processed/r3_croproster.dta"
replace round = 3 if round == .

// append using "${root_processed_data}/processed/r2_croproster.dta"
// replace round = 2 if round == .


keep if cropname == "MAIZE"
drop if  household_id  == ""



* Panel 
duplicates tag household_id round, gen(panel)
ta panel

sort household_id round 

br household_id round panel
adsasd
collapse (sum) drycropcutfreshweight_kg cropcutfreshweight_kg harvest_kg, by(household_id round)

br