


		*******************************************************************************
		* Project: SPIA - Improved seeds Ethiopia
		* This dofiles attempts to estimate the switching regression
		* Look at Drought tolerant varieties in detail
		* Date: June, 2021
		*******************************************************************************
		
		
		clear all
		mata: mata clear
		cap log close
		set more off
		set trace off
		set linesize 140
		set memory 200m
		set matsize 1000
		set maxvar 25000
		// set scheme plotplain
		// set			scheme MRC 
		set	scheme plottig 
		set seed 123456789
		timer clear
		estimates clear
		
		*-Set Base directory

		if "`c(os)'"=="Windows" {								
		local drive: env HOMEDRIVE
		local path : env HOMEPATH
			global 	BASE_DIR "`drive'`path'/Dropbox"
			}
		if "`c(os)'"=="MacOSX" {
		local path : env HOMEPATH
			global BASE_DIR "~//`path'/Dropbox"
		}

		
		* Define file directories
		global		ORIG	"$BASE_DIR/OTHER RESEARCH/HYV adoption/data/panel"
		global		ALEKS	"$BASE_DIR/OTHER RESEARCH/HYV adoption/dofiles/aleks/intermediate"
		global		OUT		"$ORIG/processed"
		global		TABLES	"$BASE_DIR/OTHER RESEARCH/HYV adoption/tables"
		
		
		use			"$OUT\r4_all", clear
		
		recode		sex_head (2=0), gen(female_head)
		recode		title (.=0)
		recode		dirrigation* (2 . = 0)
		
		foreach		var of varlist YIELD_cropcutfresh*_tr YIELD_cropcutdry*_tr YIELD_selfr*_tr {
			gen		log_`var' = ln(`var')
		}
		
		/* RELEVANT VARIABLES FOR ESTIMATION
		// ---------------------------------
		// yields
		YIELD_cropcutfresh_tr YIELD_cropcutdry_tr YIELD_selfr_tr
		// choices
		impmaize
		// demographics
		yrseduc age_head sex_head religion_head marital_head title depend_ratio fadult madult  hhsize_ae   asset_index hh_s12q02
		// labor
		hhlabor othhlabor hhlabor_harv othhlabor_harv // hh labor planting, ohter hh labor, harvest, other hh labor harvest 
		hiredlabor hiredlabor_harv // hired labor planting, hravest
		laborcosts laborcosts_harv // labor costs
		// inputs
		durea ddap dcompost dorganicfert  // dummies of use
		urea_kg dap_kg  chemical_kg // inputs quantity
		urea_birr dap_birr chemical_birr nps_birr // input costs
		// other inputs
		irrigationsource dirrigation     		
		// distance     
		dist_road dist_popcenter dist_market 
		// rainfall, elevation 
		af_bio_12 srtm   
		*/
		
		
		gen				year2000 = 0 if year_release_dna1!=.
		replace			year2000 = 1 if year_release_dna1>=2000 & year_release_dna1!=.
		gen				year2010 = 0 if year_release_dna1!=.
		replace			year2010 = 1 if year_release_dna1>=2010 & year_release_dna1!=.
		
		// SEED TYPES, COMPARING DEFINITIONS AT HH LEVEL
		// the real comparison should be done at parcel level though
		sum				imp_sr1 imp_sr2 imp_dna1 imp_dna2 imp_dna3 imp_dna4 imp_dna5 imp_dna6 imp_dna7 year2000 year2010 [aw=pw_w4]
		corr			imp_sr1 imp_sr2 imp_dna1 imp_dna2 imp_dna3 imp_dna4 imp_dna5 imp_dna6 imp_dna7 year2000 year2010 [aw=pw_w4]
		
		tab				imp_sr1 imp_dna1 [aw=pw_w4], row
		tab				imp_sr1 imp_dna2 [aw=pw_w4], row
		tab				imp_sr1 imp_dna3 [aw=pw_w4], row
		tab				imp_sr1 imp_dna4 [aw=pw_w4], row
		tab				imp_sr1 imp_dna5 [aw=pw_w4], row
		tab				imp_sr1 imp_dna6 [aw=pw_w4], row
		tab				imp_sr1 imp_dna7 [aw=pw_w4], row
		tab				imp_sr1 year2000 [aw=pw_w4], row
		tab				imp_sr1 year2010 [aw=pw_w4], row
		
		tab				imp_sr2 imp_dna1 [aw=pw_w4], row
		tab				imp_sr2 imp_dna2 [aw=pw_w4], row
		tab				imp_sr2 imp_dna3 [aw=pw_w4], row
		tab				imp_sr2 imp_dna4 [aw=pw_w4], row
		tab				imp_sr2 imp_dna5 [aw=pw_w4], row
		tab				imp_sr2 imp_dna6 [aw=pw_w4], row
		tab				imp_sr2 imp_dna7 [aw=pw_w4], row
		tab				imp_sr2 year2000 [aw=pw_w4], row
		tab				imp_sr2 year2010 [aw=pw_w4], row
		
		// for Aleks:
		// cross-tabulations of self-reported VS improved (let's use: SR2 (only new) AND purity 90%, DTM and year of release 2010
		tab				imp_sr2 imp_dna3 [aw=pw_w4], row
		tab				imp_sr2 imp_dna5 [aw=pw_w4], row
		tab				imp_sr2 year2010 [aw=pw_w4], row
		
		
		tab				asset_index, gen(asset)
		// ren				cs4q15 dist_market 
		// dist to markets is not available for 693 hhs. shall we input an average at the minimal political level?
		// cant be at kebele level, bc distance is set at this level. let's do it then at subcity code
		// bys				saq01 saq02 saq03 saq04 saq05: egen avg_dist = mean(dist_market)
		// this adds very few additions!
		// replace			dist_market = avg_dist if dist_market==.
		// doesn't work. it decreases sample size too much!
		
		
		// ADD WEIGHTS!! pw_w4	dorganicfert
		
		// ESTIMATIONS
		// -----------
		
		// it is giving weird results when switching from dry to fresh, but they're quite similar. possibly bc of number of observations
		// replace		log_YIELD_cropcutfresh_sr1_tr = log_YIELD_cropcutdry_sr1_tr if log_YIELD_cropcutfresh_sr1_tr==. & log_YIELD_cropcutdry_sr1_tr!=.
		// replace		log_YIELD_cropcutdry_sr1_tr = log_YIELD_cropcutfresh_sr1_tr if log_YIELD_cropcutdry_sr1_tr==. & log_YIELD_cropcutfresh_sr1_tr!=.
		
		// We'll be adding controls, comparing estimations using different improved seed definitions and yields
		
		// dry sale al revés
		// y fresh no corre con sr2 !!
		local		s = "_sr1"
		movestay 	(log_YIELD_cropcutdry`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' othinputcosts`s'), ///
					select(imp`s' = yrseduc age_head female_head title asset_index)
		
		program		results
		cap 		drop y11 y10 y01 y00
		mspredict	y11 if e(sample), yc1_1 
		mspredict	y10 if e(sample), yc1_2		
		mspredict	y01 if e(sample), yc2_1 
		mspredict	y00 if e(sample), yc2_2
		
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y11==y10, unp
		mat			MT=r(table)
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y01==y00, unp
		mat			MU=r(table)
		estadd 		matrix atet=MT[1,1]
		estadd 		matrix ateu=MU[1,1]
		estadd 		matrix set=MT[2,1]
		estadd 		matrix seu=MU[2,1]
		estadd 		matrix pvalt=MT[4,1]
		estadd 		matrix pvalu=MU[4,1]
		
		foreach 	var in y11 y10 y01 y00 {
		sum			`var'
		estadd 		matrix `var'=r(mean)
		}
		
		esttab 		using "$TABLES/switch`2'.tex", cells("y11(fmt(a2)) y10(fmt(a2)) atet(fmt(a2)) set(fmt(3)) pvalt(fmt(4))")  /// 
		collabels(none) eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATT") refcat(c1 "`1'", nola) ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append
		
		esttab 		using "$TABLES/switch`2'.tex", cells("y01(fmt(a2)) y00(fmt(a2)) ateu(fmt(a2)) seu(fmt(3)) pvalu(fmt(4))")  /// 
		collabels(none) eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATU") ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append
		end
		
		
		
		// SIMPLEST REGRESSION. do the ones that run
		// it is getting complicated with such few DNA fingerprints. so work only with self-reported first
		foreach		s in _sr1  {
		movestay 	(log_YIELD_selfr`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' othinputcosts`s') [pw=pw_w4], ///
					select(imp`s' = yrseduc age_head female_head  title )
		
		// FOR BEING THE FIRST RESULT IN TABLE, COPY THE WHOLE COMMAND
		cap 		drop y11 y10 y01 y00
		mspredict	y11 if e(sample), yc1_1 
		mspredict	y10 if e(sample), yc1_2		
		mspredict	y01 if e(sample), yc2_1 
		mspredict	y00 if e(sample), yc2_2
		
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y11==y10, unp
		mat			MT=r(table)
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y01==y00, unp
		mat			MU=r(table)

		estadd 		matrix atet=MT[1,1]
		estadd 		matrix set=MT[2,1]
		estadd 		matrix pvalt=MT[4,1]		
		estadd 		matrix ateu=MU[1,1]
		estadd 		matrix seu=MU[2,1]
		estadd 		matrix pvalu=MU[4,1]
		
		foreach 	var in y11 y10 y01 y00 {
		sum			`var'
		estadd 		matrix `var'=r(mean)
		}
		
		esttab 		using "$TABLES/switch1.tex", cells("y11(fmt(a2)) y10(fmt(a2)) atet(fmt(a2)) set(fmt(3)) pvalt(fmt(4))")  /// 
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATT") refcat(c1 "\textit{SR1, SR yields}", nola) ///
		collabels ("Adopters yields" "Non-adopters yields" "ATE" "SE" "p-value") ///
		postfoot(%) 	///
		prehead( `"\begin{table}[H]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\caption{Effects on yields (log) from adopting improved maize varieties (self-reported)}"' ///
		`"\label{tab:switch1}"' ///
		`"\begin{tabular}{l cccccc}"' ///
		`"\hline"' `"\hline"') replace 
		
		
		esttab 		using "$TABLES/switch1.tex", cells("y01(fmt(a2)) y00(fmt(a2)) ateu(fmt(a2)) seu(fmt(3)) pvalu(fmt(4))")  /// 
		collabels(none) eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATU") ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append

		
		
		movestay 	(log_YIELD_cropcutdry`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' othinputcosts`s') [pw=pw_w4], ///
					select(imp`s' = yrseduc age_head female_head  title )
		results		"\textit{SR1, cropcut yields}" 1
		}
		
		// ADD CONTROLS
		/* // report straight the full set of controls
		// fertilizer costs
		foreach		s in _sr1  {
		movestay 	(log_YIELD_selfr`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s' othinputcosts`s') [pw=pw_w4], ///
					select(imp`s' = yrseduc age_head female_head  title )
		results		SR1, SR yields, controlling by fertilizer costs			
		movestay 	(log_YIELD_cropcutdry`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s' othinputcosts`s') [pw=pw_w4], ///
					select(imp`s' = yrseduc age_head female_head  title )
		results		SR1, cropcut yields, controlling by fertilizer costs
		}
		*/
		/*
		// asset index to selection equation
		foreach		s in _sr1  {
		movestay 	(log_YIELD_selfr`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s'  othinputcosts`s' ) [pw=pw_w4], ///
					select(imp`s' = yrseduc age_head female_head  title asset_index)
		movestay 	(log_YIELD_cropcutdry`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s'  othinputcosts`s') [pw=pw_w4], ///
					select(imp`s' = yrseduc age_head female_head  title asset_index)
		}
		*/
		// irrigation, mechanization
		foreach		s in _sr1    {
		movestay 	(log_YIELD_selfr`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s' othinputcosts`s' dirrigation`s' ///
					dmechanization`s' dorganicfert`s') [pw=pw_w4], ///
					select(imp`s' = yrseduc age_head female_head  title asset_index)
		results		"\textit{SR1, SR yields, full set of controls}" 1
		movestay 	(log_YIELD_cropcutdry`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s' othinputcosts`s' dirrigation`s' ///
					dorganicfert`s') [pw=pw_w4], ///
					select(imp`s' = yrseduc age_head female_head  title asset_index)
		results		"\textit{SR1, cropcut yields, full set of controls}" 1
		}
		
		// same with SR2
		foreach		s in _sr2    {
		movestay 	(log_YIELD_selfr`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s' othinputcosts`s' dirrigation`s' ///
					dmechanization`s' dorganicfert`s') [pw=pw_w4], ///
					select(imp`s' = yrseduc age_head female_head  title asset_index)
		results		"\textit{SR2, SR yields}" 1
		movestay 	(log_YIELD_cropcutdry`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s' othinputcosts`s' dirrigation`s' ///
					dorganicfert`s') [pw=pw_w4], ///
					select(imp`s' = yrseduc age_head female_head  title asset_index)
		
		// LET'S DO THE PROGRAM AGAIN FOR BEING THE LAST ONE
		cap 		drop y11 y10 y01 y00
		mspredict	y11 if e(sample), yc1_1 
		mspredict	y10 if e(sample), yc1_2		
		mspredict	y01 if e(sample), yc2_1 
		mspredict	y00 if e(sample), yc2_2
		
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y11==y10, unp
		mat			MT=r(table)
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y01==y00, unp
		mat			MU=r(table)
		estadd 		matrix atet=MT[1,1]
		estadd 		matrix ateu=MU[1,1]
		estadd 		matrix set=MT[2,1]
		estadd 		matrix seu=MU[2,1]
		estadd 		matrix pvalt=MT[4,1]
		estadd 		matrix pvalu=MU[4,1]
		
		foreach 	var in y11 y10 y01 y00 {
		sum			`var'
		estadd 		matrix `var'=r(mean)
		}
		
		esttab 		using "$TABLES/switch1.tex", cells("y11(fmt(a2)) y10(fmt(a2)) atet(fmt(a2)) set(fmt(a2)) pvalt(fmt(a2))")  /// 
		collabels(none) eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATT") refcat(c1 "\textit{SR2, cropcut yields}", nola) ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append
		
		esttab 		using "$TABLES/switch1.tex", cells("y01(fmt(a2)) y00(fmt(a2)) ateu(fmt(a2)) seu(fmt(a2)) pvalu(fmt(a2))")  /// 
		collabels(none) eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATU") ///
		prehead (%)posthead(%) ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Note: Full set of controls include: parcel size (in HA), household labor, hired labor, fertilizer costs, other input costs irrigation (dummy), mechanization (dummy), organic fertilizer (dummy). Instruments for the adoption equation include: years of education of hh head, age of hh head, female head, land title, asset index, seed costs. Results for SR2 include full set of controls. SR1=1 if New, 2nd gen or recycled,=0 if Traditional; SR2=1 if New, =0 2nd gen, recycled or traditional}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') 	append
		}
		
		
		// THEN TRY OUT THE DNA DEFINITIONS
		// NOTE: bc we had too few observations with DNA information, we are replacing as zero those that self-report not using improved seeds
		
		local		s = "dna1"
		movestay 	(log_YIELD_selfr_`s'_tr parcesizeHA_`s' hhlabor_t_`s' hiredlabor_t_`s' fertcosts_`s' othinputcosts_`s' dirrigation_`s' ///
					dmechanization_`s' ) [pw=pw_w4], ///
					select(imp_`s' = yrseduc age_head female_head  title seed_cost_`s')
		
		// FOR BEING THE FIRST RESULT IN TABLE, COPY THE WHOLE COMMAND
		cap 		drop y11 y10 y01 y00
		mspredict	y11 if e(sample), yc1_1 
		mspredict	y10 if e(sample), yc1_2		
		mspredict	y01 if e(sample), yc2_1 
		mspredict	y00 if e(sample), yc2_2
		
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y11==y10, unp
		mat			MT=r(table)
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y01==y00, unp
		mat			MU=r(table)

		estadd 		matrix atet=MT[1,1]
		estadd 		matrix set=MT[2,1]
		estadd 		matrix pvalt=MT[4,1]		
		estadd 		matrix ateu=MU[1,1]
		estadd 		matrix seu=MU[2,1]
		estadd 		matrix pvalu=MU[4,1]
		
		foreach 	var in y11 y10 y01 y00 {
		sum			`var'
		estadd 		matrix `var'=r(mean)
		}
		
		esttab 		using "$TABLES/switch2.tex", cells("y11(fmt(a2)) y10(fmt(a2)) atet(fmt(a2)) set(fmt(3)) pvalt(fmt(4))")  /// 
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATT") refcat(c1 "\textit{DNA 70, SR yields}", nola) ///
		collabels ("Adopters yields" "Non-adopters yields" "ATE" "SE" "p-value") ///
		postfoot(%) 	///
		prehead( `"\begin{table}[H]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\caption{Effects on yields (log) from adopting improved maize varieties (DNA fingerprinting)}"' ///
		`"\label{tab:switch2}"' ///
		`"\begin{tabular}{l cccccc}"' ///
		`"\hline"' `"\hline"') replace 
		
		
		esttab 		using "$TABLES/switch2.tex", cells("y01(fmt(a2)) y00(fmt(a2)) ateu(fmt(a2)) seu(fmt(3)) pvalu(fmt(4))")  /// 
		collabels(none) eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATU") ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append
		
		movestay 	(log_YIELD_cropcutdry_`s'_tr parcesizeHA_`s' hhlabor_t_`s' hiredlabor_t_`s' fertcosts_`s' othinputcosts_`s') [pw=pw_w4], ///
					select(imp_`s' = yrseduc age_head female_head  title seed_cost_`s')
		results		"\textit{DNA 70, cropcut yields}" 2

		
		local		dna2 = "DNA 90"
		local		dna3 = "DNA 95"
		local		dna4 = "HYB"
		foreach		s in   dna2 dna3 dna4 {
		movestay 	(log_YIELD_selfr_`s'_tr parcesizeHA_`s' hhlabor_t_`s' hiredlabor_t_`s' fertcosts_`s' othinputcosts_`s' dirrigation_`s' ///
					dmechanization_`s' ) [pw=pw_w4], ///
					select(imp_`s' = yrseduc age_head female_head  title seed_cost_`s')
		results		"\textit{``s'', SR yields}" 2
		movestay 	(log_YIELD_cropcutdry_`s'_tr parcesizeHA_`s' hhlabor_t_`s' hiredlabor_t_`s' fertcosts_`s' othinputcosts_`s') [pw=pw_w4], ///
					select(imp_`s' = yrseduc age_head female_head  title seed_cost_`s')
		results		"\textit{``s'', cropcut yields}" 2
		}

		
		// DTMZ
		// ----
		local		s = "_dna5"			
		movestay 	(log_YIELD_selfr`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s' othinputcosts`s' dirrigation`s') [pw=pw_w4], ///
					select(imp`s' = yrseduc age_head female_head  title seed_cost`s')
		// when we add seed costs to selection equation, it gives us the opposite sign
		results		"\textit{DTMZ, SR yields}" 2
		movestay 	(log_YIELD_cropcutdry`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s' othinputcosts`s') [pw=pw_w4], ///
					select(imp`s' = yrseduc age_head female_head  title seed_cost`s')		
		// LET'S DO THE PROGRAM AGAIN FOR BEING THE LAST ONE
		cap 		drop y11 y10 y01 y00
		mspredict	y11 if e(sample), yc1_1 
		mspredict	y10 if e(sample), yc1_2		
		mspredict	y01 if e(sample), yc2_1 
		mspredict	y00 if e(sample), yc2_2
		
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y11==y10, unp
		mat			MT=r(table)
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y01==y00, unp
		mat			MU=r(table)
		estadd 		matrix atet=MT[1,1]
		estadd 		matrix ateu=MU[1,1]
		estadd 		matrix set=MT[2,1]
		estadd 		matrix seu=MU[2,1]
		estadd 		matrix pvalt=MT[4,1]
		estadd 		matrix pvalu=MU[4,1]
		
		foreach 	var in y11 y10 y01 y00 {
		sum			`var'
		estadd 		matrix `var'=r(mean)
		}
		
		esttab 		using "$TABLES/switch2.tex", cells("y11(fmt(a2)) y10(fmt(a2)) atet(fmt(a2)) set(fmt(a2)) pvalt(fmt(a2))")  /// 
		collabels(none) eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATT") refcat(c1 "\textit{DTMZ, cropcut yields}", nola) ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append
		
		esttab 		using "$TABLES/switch2.tex", cells("y01(fmt(a2)) y00(fmt(a2)) ateu(fmt(a2)) seu(fmt(a2)) pvalu(fmt(a2))")  /// 
		collabels(none) eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATU") ///
		prehead (%)posthead(%) ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Note: Full set of controls include: parcel size (in HA), household labor, hired labor, fertilizer costs, other input costs irrigation (dummy), mechanization (dummy), organic fertilizer (dummy). Instruments for the adoption equation include: years of education of hh head, age of hh head, female head, land title, asset index, seed costs. All results include full set of controls. DNA 70, 90 and 95 refer to improved varieties defined by DNA finerprinting with purity levels of 70, 90 and 95\%, respectively. HYB equals 1 for a hybrid variety, 0 for an open-pollinated variety. DTMZ equals 1 for a Drought-tolerant maize variety}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') 	append
		
		
		// What about source and year of release??
		// Source: 6-CGIAR 7-EXOTIC
		// switching results 3
		foreach		s in dna6 {
		movestay 	(log_YIELD_selfr_`s'_tr parcesizeHA_`s' hhlabor_t_`s' hiredlabor_t_`s' fertcosts_`s' othinputcosts_`s' dirrigation_`s' ///
					dmechanization_`s' ) [pw=pw_w4], ///
					select(imp_`s' = yrseduc age_head female_head  title seed_cost_`s')
		
		// FOR BEING THE FIRST RESULT IN TABLE, COPY THE WHOLE COMMAND
		cap 		drop y11 y10 y01 y00
		mspredict	y11 if e(sample), yc1_1 
		mspredict	y10 if e(sample), yc1_2		
		mspredict	y01 if e(sample), yc2_1 
		mspredict	y00 if e(sample), yc2_2
		
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y11==y10, unp
		mat			MT=r(table)
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y01==y00, unp
		mat			MU=r(table)

		estadd 		matrix atet=MT[1,1]
		estadd 		matrix set=MT[2,1]
		estadd 		matrix pvalt=MT[4,1]		
		estadd 		matrix ateu=MU[1,1]
		estadd 		matrix seu=MU[2,1]
		estadd 		matrix pvalu=MU[4,1]
		
		foreach 	var in y11 y10 y01 y00 {
		sum			`var'
		estadd 		matrix `var'=r(mean)
		}
		
		esttab 		using "$TABLES/switch3.tex", cells("y11(fmt(a2)) y10(fmt(a2)) atet(fmt(a2)) set(fmt(3)) pvalt(fmt(4))")  /// 
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATT") refcat(c1 "\textit{CGIAR source, SR yields}", nola) ///
		collabels ("Adopters yields" "Non-adopters yields" "ATE" "SE" "p-value") ///
		postfoot(%) 	///
		prehead( `"\begin{table}[H]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\caption{Effects on yields (log) from adopting improved maize varieties (DNA fingerprinting)}"' ///
		`"\label{tab:switch3}"' ///
		`"\begin{tabular}{l cccccc}"' ///
		`"\hline"' `"\hline"') replace 
		
		
		esttab 		using "$TABLES/switch3.tex", cells("y01(fmt(a2)) y00(fmt(a2)) ateu(fmt(a2)) seu(fmt(3)) pvalu(fmt(4))")  /// 
		collabels(none) eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATU") ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append
		
		
		
		movestay 	(log_YIELD_cropcutdry_`s'_tr parcesizeHA_`s' hhlabor_t_`s' hiredlabor_t_`s' fertcosts_`s' othinputcosts_`s') [pw=pw_w4], ///
					select(imp_`s' = yrseduc age_head female_head  title seed_cost_`s')
		results		"\textit{CGIAR source, cropcut yields}" 3
		}
		
		foreach		s in dna7  {
		movestay 	(log_YIELD_selfr_`s'_tr parcesizeHA_`s' hhlabor_t_`s' hiredlabor_t_`s' fertcosts_`s' othinputcosts_`s' dirrigation_`s' ///
					dmechanization_`s' ) [pw=pw_w4], ///
					select(imp_`s' = yrseduc age_head female_head  title seed_cost_`s')
		results		"\textit{Exotic source, SR yields}" 3
		movestay 	(log_YIELD_cropcutdry_`s'_tr parcesizeHA_`s' hhlabor_t_`s' hiredlabor_t_`s' fertcosts_`s' othinputcosts_`s') [pw=pw_w4], ///
					select(imp_`s' = yrseduc age_head female_head  title seed_cost_`s')
		results		"\textit{Exotic source, cropcut yields}" 3
		}
							
		// year of release (according to DNA1)
		replace		year2000 = 0 if imp_sr1==0
		replace		year2010 = 0 if imp_sr1==0
		
		local		s = "_dna1"	
		movestay 	(log_YIELD_selfr`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s' othinputcosts`s') [pw=pw_w4], ///
					select(year2000 = yrseduc age_head female_head  title seed_cost`s')
		results		"\textit{Year 2000+, SR yields}" 3
		movestay 	(log_YIELD_cropcutdry`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s' othinputcosts`s') [pw=pw_w4], ///
					select(year2000 = yrseduc age_head female_head  title seed_cost`s')
		results		"\textit{Year 2000+, cropcut yields}" 3
		movestay 	(log_YIELD_selfr`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s' othinputcosts`s'  dirrigation`s') [pw=pw_w4], ///
					select(year2010 = yrseduc age_head female_head  title seed_cost`s')
		results		"\textit{Year 2010+, SR yields}" 3
		movestay 	(log_YIELD_cropcutdry`s'_tr parcesizeHA`s' hhlabor_t`s' hiredlabor_t`s' fertcosts`s' othinputcosts`s'), ///
					select(year2010 = yrseduc age_head female_head  title seed_cost`s')			
		// LET'S DO THE PROGRAM AGAIN FOR BEING THE LAST ONE
		cap 		drop y11 y10 y01 y00
		mspredict	y11 if e(sample), yc1_1 
		mspredict	y10 if e(sample), yc1_2		
		mspredict	y01 if e(sample), yc2_1 
		mspredict	y00 if e(sample), yc2_2
		
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y11==y10, unp
		mat			MT=r(table)
		bootstrap 	(r(mu_1) - r(mu_2)), reps(1000) nowarn nodots :  ttest y01==y00, unp
		mat			MU=r(table)
		estadd 		matrix atet=MT[1,1]
		estadd 		matrix ateu=MU[1,1]
		estadd 		matrix set=MT[2,1]
		estadd 		matrix seu=MU[2,1]
		estadd 		matrix pvalt=MT[4,1]
		estadd 		matrix pvalu=MU[4,1]
		
		foreach 	var in y11 y10 y01 y00 {
		sum			`var'
		estadd 		matrix `var'=r(mean)
		}
		
		esttab 		using "$TABLES/switch3.tex", cells("y11(fmt(a2)) y10(fmt(a2)) atet(fmt(a2)) set(fmt(a2)) pvalt(fmt(a2))")  /// 
		collabels(none) eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATT") refcat(c1 "\textit{Year 2010+, cropcut yields}", nola) ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append
		
		esttab 		using "$TABLES/switch3.tex", cells("y01(fmt(a2)) y00(fmt(a2)) ateu(fmt(a2)) seu(fmt(a2)) pvalu(fmt(a2))")  /// 
		collabels(none) eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "ATU") ///
		prehead (%)posthead(%) ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Note: Full set of controls include: parcel size (in HA), household labor, hired labor, fertilizer costs, other input costs irrigation (dummy), mechanization (dummy), organic fertilizer (dummy). Instruments for the adoption equation include: years of education of hh head, age of hh head, female head, land title, asset index, seed costs. All results include full set of controls. }"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') 	append	
		
		
		
		
		
		
		
				
		