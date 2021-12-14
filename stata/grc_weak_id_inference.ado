program define grc_weak_id_inference, rclass
    version 16.0
    syntax varname [if], hhid(varname) Hybrid(varname) MIn(integer) MAx(integer) INCrement(real) [STORE_results(name) progress(integer 1) path(string) TEST_type(string) type_one(real 0.05) ADD_post(string) controls(varlist fv) BASEgroup(integer 2)]
    /*
        Program that carries out a brute force search for phi for an outcomes `varname', with household ID `hhid', and
        hybrid variable `hybrid', and optionally, controls `controls'.

        The program creates a grid from `min' to `max' with an increment of `increment'. Optionally you can set the 
        Type-I error with `type_one'.

        This program saves p-values to a postfile and you can add another variable or local, with `add_post'.

        There are three types of tests that are implemented (set with `test_type'):
        1. "joint": This tests all restrictions as a joint test. This is the default setting.
        2. "separate": This tests all restrictions separately and p-values are saved separately. It this is chosen for T=2,
            it switches to "joint" as those are equivalent in the T=2 case.
        3. "base" : This test separate restrictions against a base group `base_group', set by the number of the trajectory.
            (run tab trajectory, to see which number to use)

        Optionally you can save to `path'
    */

    * Get post file for saving p-values
	tempname memhold

    if "`path'" != "" {
        local results_data "`path'"
    }
    else {
        tempfile results_phi
        local results_data "`results_phi'"
    }

    if "`test_type'" == "base" {
        postfile `memhold' phi p_val`basegroup'3 p_val`basegroup'4 p_val`basegroup'5 p_val`basegroup'6 p_val`basegroup'7 p_val_joint str20(test_type add) using `results_data', replace
    }
    else {
	    postfile `memhold' phi p_val32 p_val43 p_val54 p_val65 p_val76 p_val_joint str20(test_type add) using `results_data', replace
    }


    local          never 1
    tab             trajectory

    local         always `r(r)'
    local          lastswitcher = `always'-1

    numlist         "2(1)`lastswitcher'"
    local          switchers `r(numlist)'

    numlist         "0(1)`lastswitcher'"
    local          noalways `r(numlist)'

    * expand factor variables if needed
    local fvops = "`s(fvops)'" == "true" | _caller() >= 11 & "`controls'" != ""  
    if `fvops' {
        fvexpand `controls'
        local controls =  r(varlist)
    }
    else {
        local controls
    }
    di "`controls'"
    * Check if controls exist
    if "`controls'" != "" {
        local controls_gmm_exp - {xb:`controls'}
    }
        * First run OLS to get initial values for GMM
        reg             `varlist' i(`noalways').trajectory                ///
                    i(`switchers' `always').trajectory#1.`hybrid' 	         ///
                    `controls' ///
                    `if', vce(cluster `hhid') nocons

        estimates store `store_results'
        * Grab matrix of parameter estimates
        matrix          ols_init = e(b)

        /* * Run gmm
        gmm     	(`varlist' - {mu: i(`noalways').trajectory}       ///
                    - {Delta: i(`switchers').trajectory#1.`hybrid'} ///
                    - {mu_111: i(`always').trajectory#1.`hybrid'}
                       `controls_gmm_exp') ///
                    `if', instruments(i(`noalways').trajectory 			///
                    i(`switchers').trajectory#1.`hybrid' `controls', nocons) 		///
                    vce(cluster `hhid') from(ols_init) winitial(identity)  conv_maxiter(2000)

	
    matrix          gmm_init = e(b) */
	
	
    // pause on
    // pause 
    qui ereturn list
    return add
    
	/*
	We test for a plethora of phi values
	*/
    if `progress' == 1 {
        _dots 0, title("Running Tests...")
    }

	forvalues phi_potential = `=`min''(`=`increment'')`=`max'' {

        if `progress' == 1 {
            local i = `i' + 1
            _dots `i' 0
            local qui quietly
        }

        qui tab trajectory
        local test_string ""
        if r(r) == 4 {
            local test_type = "joint" 
            * If T=2, joint and separate are the same
        }
        if "`test_type'" == "joint" {
            forvalues i = `=`r(r)'-1'(-1)3 {
                local test_string ((`i'.trajectory#1.`hybrid'- `=`i'-1'.trajectory#1.`hybrid') = `phi_potential'*(`i'.trajectory-`=`i' -1'.trajectory)) `test_string'
            }
            `qui' test `test_string'
            qui return list
            loc p_v = r(p)
            post `memhold' (`phi_potential') (`p_v') (.) (.) (.) (.) (.) ("`test_type'") ("`add_post'") 

        }
        else if "`test_type'" == "separate" {

            forvalues i = `=`r(r)'-1'(-1)3 {
                `qui' test ((`i'.trajectory#1.`hybrid'-`=`i'-1'.trajectory#1.`hybrid') = `phi_potential'*(`i'.trajectory-`=`i' -1'.trajectory))
                scalar p_v_`i' = r(p)
            }
            post `memhold' (`phi_potential') (p_v_3) (p_v_4) (p_v_5) (p_v_6) (p_v_7) ("`test_type'") ("`add_post'")
        }

        else if "`test_type'" == "base" {
            if "basegroup" == "" {
                di "Base group not chosen. Choosing trajectory 1 (001)"
                exit
            }
            local test_string ""

            forvalues i = 2(1)`=`r(r)'-1' {
                if `i' == `basegroup' {
                    continue
                }
                /* di `basegroup' */
                local test_string ((`i'.trajectory#1.`hybrid'-`basegroup'.trajectory#1.`hybrid') = `phi_potential'*(`i'.trajectory-`basegroup'.trajectory)) `test_string'
                `qui' test ((`i'.trajectory#1.`hybrid'-`basegroup'.trajectory#1.`hybrid') = `phi_potential'*(`i'.trajectory-`basegroup'.trajectory))
                scalar p_v_`i' = r(p)
            }

            `qui' test `test_string'
            qui return list
            scalar p_val_joint = r(p)
            post `memhold' (`phi_potential') (p_v_3) (p_v_4) (p_v_5) (p_v_6) (p_v_7) (p_val_joint) ("`test_type'") ("`add_post'")

        }

	}
    tab trajectory
    
    postclose `memhold'

    preserve
        use `results_data', clear

        destring *, replace	

        if test_type == "joint" | test_type == "separate" {
            gen not_sig32 = (p_val32>=`type_one')
            su phi if not_sig32 == 1

            gen min_phi32 = round(r(min),0.001)
            gen max_phi32 = round(r(max),0.001)

            loc min_phi32 = round(r(min),0.001)
            loc max_phi32 = round(r(max),0.001)
            loc mean_phi32 = r(mean)

            return local min_phi32 = `min_phi32'
            return local max_phi32 = `max_phi32'
        }

        if test_type == "separate" {
            forval i=4/7 {
                gen not_sig`i'`=`i'-1' = (p_val`i'`=`i'-1' >= `type_one')
                su phi if not_sig`i'`=`i'-1' ==1
                loc min_phi`i'`=`i'-1' = round(r(min), 0.001)
                loc max_phi`i'`=`i'-1' = round(r(max), 0.001)

                * Also create variables for those terms
                gen min_phi`i'`=`i'-1' = round(r(min), 0.001)
                gen max_phi`i'`=`i'-1' = round(r(max), 0.001)
                
                return local min_phi`i'`=`i'-1' = `min_phi`i'`=`i'-1''
                return local max_phi`i'`=`i'-1' = `max_phi`i'`=`i'-1''
            }
        }

        else if test_type == "base" {
            forval i=3/7 {
                gen not_sig2`i' = (p_val2`i' >= `type_one')
                su phi if not_sig2`i' ==1
                loc min_phi2`i' = round(r(min), 0.001)
                loc max_phi2`i' = round(r(max), 0.001)

                * Also create variables for those terms
                gen min_phi2`i' = round(r(min), 0.001)
                gen max_phi2`i' = round(r(max), 0.001)

                
                return local min_phi2`i' = `min_phi2`i''
                return local max_phi2`i' = `max_phi2`i''
            }
            gen not_sig_joint = (p_val_joint >= `type_one')
            su phi if not_sig_joint ==1
            gen min_phi_joint = round(r(min), 0.001)
            gen max_phi_joint = round(r(max), 0.001)

            loc min_phi_joint = round(r(min), 0.001)
            loc max_phi_joint = round(r(max), 0.001)

            return local min_phi_joint = `min_phi_joint'
            return local max_phi_joint = `max_phi_joint'

        }

        if "`path'" != "" {
        save "`path'", replace
        }

    restore

    
end