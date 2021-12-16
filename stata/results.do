clear all 

if "`c(username)'" == "lordflaron" {
	global root = "/home/lordflaron/Documents/ethiopia-tech-adoption/"
}

else {
	global root = "C:\Users\obarriga\Documents\GitHub\ethiopia-tech-adoption\"

}

global root_data = "${root}/data"
global root_results = "${root}/results"


global root_processed_data= "D:\Box Sync/ethiopia_data\data"



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
* 0 - reshape weather variables and merge back
* **********************************************************************

tempfile weather

preserve



* make sure weather is in all obs
local weather_vars h2011_eviarea h2011_evimax h2011_grn h2011_sen h2011_tot h2011_wetQ h2011_wetQstart h2013_eviarea h2013_evimax h2013_grn h2013_sen h2013_tot h2013_wetQ h2013_wetQstart h2015_eviarea h2015_evimax h2015_grn h2015_sen h2015_tot h2015_wetQ h2015_wetQstart

foreach var of local weather_vars {
	bys household_id: egen `var'_aux = mean(`var')
	replace `var' = `var'_aux
	drop `var'_aux
}

duplicates drop household_id, force

reshape long h@_tot h@_wetQ h@_wetQstart h@_eviarea h@_evimax h@_grn h@_sen, i(household_id) j(year)

drop wave

gen wave = year==2011
replace wave = 2 if year==2013
replace wave = 3 if year==2015

keep household_id wave h_tot h_wetQ h_wetQstart h_eviarea h_evimax h_grn h_sen

label variable h_tot "12-month total rainfall (mm) in Jan-Dec, starting January"
label variable h_wetQ "Total rainfall in wettest quarter (mm) within 12-month period starting January 1"
label variable h_wetQstart "Start of wettest quarter in dekads 1-36, where first week of January  = 1"
label variable h_eviarea "Total change in greenness(integral of daily EVI values)w/in growing season,"
label variable h_evimax "EVI value at peak of greenness within growing season,starting 1/1"
label variable h_grn "Onset of greenness increase in day of year 1-356,starting 1/1,avg by zone"
label variable h_sen "Onset of greenness decrease in day of year 1-356,starting 1/1,avg by zone"

save "`weather'", replace


restore

merge 1:1 household_id wave using "`weather'"
keep if _merge==3


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

* Save for later
save "${root_data}/full_panel_rain_traj.dta", replace

global          never 1
tab             trajectory

global          always `r(r)'
global          lastswitcher = $always-1

numlist         "2(1)$lastswitcher"
global          switchers `r(numlist)'

numlist         "0(1)$lastswitcher"
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

* Keep only trajectory exists (we lose 84,368)
drop if missing(trajectory)

* Assume that missing fertcosts

* Create log yield variable
foreach var of varlist YIELD_cropcutfresh_tr YIELD_cropcutdry_tr YIELD_selfr_tr {
    gen ln_`var' = ln(`var')
}

loc controls = "yrseduc age_head sex_head  title parcesizeHA hhlabor hiredlabor"
loc interactions = "1.impmaize#c.(`controls')"


capture program drop "${root}/stata/grc_weak_id_inference"
include "${root}/stata/grc_weak_id_inference.ado"

tokenize "000 001 010 011 100 101 110"

forval i=1/7 {
	local j = `j' + 1
	local coeflabels `coeflabels' `i'.trajectory  "$\mu_{``j''}$" `i'.trajectory#1.impmaize "$\Delta_{``j''}$"
}

local coeflabels `coeflabels' 8.trajectory#1.impmaize "$\kappa_{111}$"


foreach var of varlist YIELD_cropcutdry_tr YIELD_selfr_tr {

	if "`var'" == "YIELD_cropcutdry_tr" {
		matrix define initval = (0, 6,6,6,6, 6,6,5,-1,1,-1,2,3,5,7,4,3,3)
		
		gmm     	(ln_`var' - {mu: i($noalways).trajectory} ///              	
					- {Delta}*(1.impmaize)  /// 
					- {phi}*(`switcherpars')            ///
					- ({mu_always} + {phi}*({mu_always}                     ///
					- {mu:2.trajectory}))*($always.trajectory#1.impmaize) -{xb: `controls' })    ///
					, instruments(i($noalways).trajectory 1.impmaize         ///
					i($switchers $always).trajectory#1.impmaize `controls' , nocons)     ///
					vce(cluster household_id)  winitial(identity) from(initval) 
		estimates store theta_model

	}


	local i = `i' + 1

	local short_var_name = substr("`var'", 7,9) 

	grc_weak_id_inference ln_`var', h(impmaize) min(-10) max(10) inc(0.01) hhid(household_id) test("base") base(2) progress(1) store(grc_`short_var_name')
	mat grc_`short_var_name'_mat = real(r(min_phi23)),real(r(max_phi23)) \ real(r(min_phi24)),real(r(max_phi24)) \ real(r(min_phi25)),real(r(max_phi25)) \ real(r(min_phi26)),real(r(max_phi26)) \ real(r(min_phi27)),real(r(max_phi27)) \ real(r(min_phi_joint)),real(r(max_phi_joint))

	estadd local controls "No" : grc_`short_var_name'
	estadd local interacted = "No" : grc_`short_var_name'

	grc_weak_id_inference ln_`var', h(impmaize) min(-10) max(10) inc(0.01) hhid(household_id) test("base") base(2) controls(`controls') progress(1) store(grc_`short_var_name'_controls)
	mat grc_`short_var_name'_mat_controls = real(r(min_phi23)),real(r(max_phi23)) \ real(r(min_phi24)),real(r(max_phi24)) \ real(r(min_phi25)),real(r(max_phi25)) \ real(r(min_phi26)),real(r(max_phi26)) \ real(r(min_phi27)),real(r(max_phi27)) \ real(r(min_phi_joint)),real(r(max_phi_joint))

	estadd local controls "Yes" : grc_`short_var_name'_controls
	estadd local interacted = "No" : grc_`short_var_name'_controls

	grc_weak_id_inference ln_`var', h(impmaize) min(-10) max(10) inc(0.01) hhid(household_id) test("base") base(2) controls(`controls' `interactions') progress(1) store(grc_`short_var_name'_controls_int)
	mat grc_`short_var_name'_mat_controls_int = real(r(min_phi23)),real(r(max_phi23)) \ real(r(min_phi24)),real(r(max_phi24)) \ real(r(min_phi25)),real(r(max_phi25)) \ real(r(min_phi26)),real(r(max_phi26)) \ real(r(min_phi27)),real(r(max_phi27)) \ real(r(min_phi_joint)),real(r(max_phi_joint))

	estadd local controls "Yes" : grc_`short_var_name'_controls_int
	estadd local interacted = "Yes" : grc_`short_var_name'_controls_int

	mat grc_mat_`short_var_name' = grc_`short_var_name'_mat, grc_`short_var_name'_mat_controls, grc_`short_var_name'_mat_controls_int

}

esttab grc_cropcutdr grc_cropcutdr_controls grc_cropcutdr_controls_int grc_selfr_tr grc_selfr_tr_controls grc_selfr_tr_controls_int using "$root/results/tables/grc.tex", ///
keep(*trajectory*) drop(0.trajectory) mtitles("Log Dry Cropcuts" "Log Dry Cropcuts" "Log Dry Cropcuts" "Log Self-Report" "Log Self-Report" "Log Self-Report") star(* 0.10 ** 0.05 *** 0.01) tex replace s(N controls interacted, label("Observations" "Controls" "Interact w/ Hybrid")) ///
coeflabels(`coeflabels') substitute(\_ _) se nogaps compress title(Unrestricted Model of Dry Cropcuts and Self-reported Yields \label{tbl:unres})

mata 

grc_mat_YIELD_cropcutdry_tr = st_matrix("grc_mat_cropcutdr")
grc_mat_YIELD_selfr_tr = st_matrix("grc_mat_selfr_tr")

cropcut = J(6,3,"")
self = J(6,3,"")

for (i=1; i<=6; i++) {
	k=1
	for (j=1; j<=3; j++) {
		cropcut[i,j] = "(" + strofreal(grc_mat_YIELD_cropcutdry_tr[i,k]) + ", " + strofreal(grc_mat_YIELD_cropcutdry_tr[i,k+1]) + ")"
		self[i,j] = "(" + strofreal(grc_mat_YIELD_selfr_tr[i,k]) + ", " + strofreal(grc_mat_YIELD_selfr_tr[i,k+1]) + ")"
		k = k+2
	}
}

end

python 

import pandas as pd
from sfi import Mata, Macro

cropcut = (
	pd.DataFrame(Mata.get("cropcut"),
	index = ["Restriction 001-010" ,
	"Restriction 001-011",
	"Restriction 001-100",
	"Restriction 001-101",
	"Restriction 001-110",
	"\\hline\\\Joint Test"],
	columns = ['Base', 'Controls', 'Controls w/ Int.'])
	).to_latex(escape=False)
	
self = (
	pd.DataFrame(Mata.get("self"),
	index = ["Restriction 001-010" ,
	"Restriction 001-011",
	"Restriction 001-100",
	"Restriction 001-101",
	"Restriction 001-110",
	"\\hline\\\Joint Test"],
	columns = ['Base', 'Controls', 'Controls w/ Int.'])
	).to_latex(escape=False)


self = self.replace("(-10", "(-$\infty$").replace("10)", "$\infty$)").replace("(., .)", "$\emptyset$")
cropcut = cropcut.replace("(-10", "(-$\infty$").replace("10)", "$\infty$)").replace("(., .)", "$\emptyset$")
with open("${root}/results/tables/phi_test_self.tex", 'w') as f:
	f.write(self)

with open("${root}/results/tables/phi_test_cropcut.tex", 'w') as f:
	f.write(cropcut)

end


// Generate Figure for Comparative Advantage

python

from sfi import Matrix, Data
import numpy as np
import matplotlib.pyplot as plt
plt.style.use('seaborn')

end

estimates restore theta_model

ereturn list

tab trajectory , matcell(theta_weights)

mata

theta_weights = st_matrix("theta_weights")
b = st_matrix("e(b)")

mu = b[2..8]

theta_weights = theta_weights/sum(theta_weights)

mu = b[2..8], b[11]
e_theta = J(1, 8, mu*theta_weights)

theta_i = mu - e_theta

st_matrix("theta_i", theta_i)

end

mat theta_i_t = theta_i'

mat rownames theta_i_t = "000" "001" "010" "011" "100" "101" "110" "111"

mat colnames theta_i_t = "theta"

mat returns  = J(1,8,.)
forval i=1/8 {
if `i'==2 {
mat returns[1,2] = _b[Delta:_cons]
continue
}
if `i' == 8 {
nlcom (_b[phi:_cons]*(_b[mu_always:_cons] - _b[mu:2.trajectory]) - _b[Delta:_cons])
mat returns[1,8] = r(b)
}
else{
nlcom (_b[phi:_cons]*(_b[mu:`i'.trajectory] - _b[mu:2.trajectory]) - _b[Delta:_cons])
mat returns[1,`i'] = r(b)
}
}


python

import matplotlib

matplotlib.use('TkAgg')

df = pd.DataFrame(Matrix.get("theta_i_t"), index = Matrix.getRowNames("theta_i_t"), columns = Matrix.getColNames("theta_i_t"))

# make some nice aggregations
pdf = df.assign(when_adopt_1 = lambda df: (df.index.str[0].astype(int)==1).astype(int),
	when_adopt_2 = lambda df: (df.index.str[1].astype(int)==1).astype(int),
	when_adopt_3 = lambda df: (df.index.str[2].astype(int)==1).astype(int),
	traj_sum = lambda df: df.index.map(lambda x: np.array([int(i) for i in list(x)]).sum())
)

returns   = Matrix.get("returns")
pdf_returns = pdf.assign(returns = np.array(returns[0]))

# Raw theta plot

fig, ax = plt.subplots(2,1, sharex=True)

pdf_returns['theta'].plot.bar(ax=ax[0])
pdf_returns['returns'].plot.bar(ax=ax[1])

ax[0].axhline(0, color='black')
ax[1].axhline(0, color='black')

ax[0].set_ylabel(r"Comparative Advantage ($\theta$)")
ax[1].set_ylabel("Returns ($\Delta$)")
ax[1].set_xlabel("Trajectory")


plt.tight_layout()

plt.savefig("${root}/results/figures/theta.png", dpi=160)

# Adoption year plot

fig, ax = plt.subplots(2,3, sharey=True, sharex=True)

for when, a_col in zip(['when_adopt_1', 'when_adopt_2', 'when_adopt_3'], ax.transpose()):
	pdf_returns.groupby(when).mean()['theta'].plot.bar(ax=a_col[0],  color=['tab:green', 'tab:blue'])
	pdf_returns.groupby(when).mean()['returns'].plot.bar(ax=a_col[1],  color=['tab:green', 'tab:blue'])


for a in ax.flatten():
	a.axhline(0, color='black')
	a.set_xticklabels(["No", "Yes"])

ax[0,0].set_ylabel(r"Comparative Advantage ($\theta$)")
ax[1,0].set_ylabel("Returns ($\Delta$)")

ax[1,0].set_xlabel("Did HH adopt in Wave 1?")
ax[1,1].set_xlabel("Did HH adopt in Wave 2?")
ax[1,2].set_xlabel("Did HH adopt in Wave 3?")

plt.tight_layout()

plt.savefig("${root}/results/figures/when_adopt_cropcut.png", dpi=160)

# Adoption Sum Plot

fig, ax = plt.subplots(2,1, sharex=True)

pdf_returns.groupby('traj_sum').mean()['theta'].plot.bar(ax=ax[0])
pdf_returns.groupby('traj_sum').mean()['returns'].plot.bar(ax=ax[1])

ax[1].set_xlabel("Number of Times Adopted")
ax[0].axhline(0, color='black')
ax[1].axhline(0, color='black')

ax[0].set_ylabel(r"Comparative Advantage ($\theta$)")
ax[1].set_ylabel("Returns ($\Delta$)")

for label in ax[1].get_xticklabels():
    label.set_rotation(0) 

plt.tight_layout()

plt.savefig("${root}/results/figures/traj_sum.png", dpi=160) 

end