clear all 

global root = "C:\Users\obarriga\Documents\GitHub\ethiopia-tech-adoption\"
global root_data = "${root}/data"
global root_results = "${root}/results"


*figures

import excel "${root_data}/FAO_STATS/FAOSTAT_data.xls", sheet("Sheet1") firstrow

drop Domain DomainCode AreaCodeFAO Area ElementCode ItemCodeFAO YearCode Flag
drop if Item == "Cereals nes"
keep if Value != .
replace Value = Value/10 if Unit == "hg/ha"
replace Value = Value/1000 if Unit == "ha"

keep if Year == 2019
adasd
*** Get Ha of Maize
keep if Item == "Maize"
	


twoway (line Value Year if Unit == "ha", yaxis(2) ytitle("Ha (x1000)", axis(2))) ///
(line Value Year if Unit == "hg/ha", yaxis(1) ytitle("Kg/Ha", axis(1))) , ///
legend(order(1 "Area Harvested" 2 "Yields"))

graph export "${root_results}/figures\Maize_yields.pdf", as(pdf) name("Graph") replace

asdas


 , ///
o

de
asd

keep hhid dist div prov year
drop if year == 2000
drop if div == .

duplicates drop hhid year, force

ta year

duplicates tag hhid, gen(aux)
keep if aux == 1


merge m:1 hhid using "D:\Dropbox\Suri_replication\paper_econometrica\preparing_files\data\raw\data_original_suri\econometrica_data.dta" , keepusing(z66  z26 hhid )


use "D:\Dropbox\Suri_replication\paper_econometrica\preparing_files\data\raw\data_original_suri\econometrica_data.dta", clear

merge 1:1 hhid using "D:\Dropbox\Suri_replication\paper_econometrica\preparing_files\data\working_dataset\intermediate_data_1997\HH_basic.dta"

keep hhid prov dist z26 z66 div vil aez aezsmall zone year _merge

keep if _merge == 3

sort hhid
bys vil : egen sd = sd(z26)
su sd

bys dist: egen sd_dist = sd(z26)
su sd_dist

bys aez: egen sd_aez = sd(z26)
su sd_aez 
