***** 
* Clustered std error analysis, minimum wage, nonstandard work, with HHI index
*****
cls
clear	
set more off
set trace off
set matsize 10000
set seed 42

if c(username)=="bngla" {
	gl data "\Users\bngla\Dropbox\PhD requirements\Nonemployer data\Data"
	gl output "\Users\bngla\Dropbox\PhD requirements\Minimum wage and alt\output\RR"
    gl do "\Users\bngla\Dropbox\PhD requirements\Minimum wage and alt"
	gl home "\Users\bngla\Dropbox\PhD requirements\Minimum wage and alt"
}
if c(username)=="bglasner" {
	gl data "\Users\bglasner\Dropbox\PhD requirements\Nonemployer data\Data"
	gl output "\Users\bglasner\Dropbox\PhD requirements\Minimum wage and alt\output\RR"
    gl do "\Users\bglasner\Dropbox\PhD requirements\Minimum wage and alt"
	gl home "\Users\bglasner\Dropbox\PhD requirements\Minimum wage and alt"
}

/***********************
ssc install did_multiplegt, replace
ssc install ftools, replace
ssc install moremata, replace
ssc install boottest, replace
ftools, compile
*/
****************************************************
****		What should the code run? 			****
****************************************************
gl allnonemployers_event = 1
gl transport_event = 1
gl allnonemployers_notaxi_event = 1
gl allnonemployers_local = 0
gl transport_local = 0

* only select one of these at a time when doint the event study. Deselect both when using local option
gl pre2007 = 0
gl post2009 = 1


* only select one
gl treat_mw = 0 // Need to use the nominal minimum wage to establish a treatment instead of letting the deflation count as changes
gl local = 0 // Need to use the nominal minimum wage to establish a treatment instead of letting the deflation count as changes
gl treat_mw_log = 1

gl breps = 1000  // specify the number of bootstraps
local controls hhi pop_density population // specify the controls to be used

****************************************************
****		Data load and cleaning for DIDM 	
****************************************************

import delimited "$data/primary_analysis_RR.csv", clear // load the data
xtset id years // check for panel balance

gen mw_round = round(minimum_wage,.15) // use the rounded version at 15 cents to create quasi-control groups of the continuous variable

gen local_mw = 0
replace local_mw = 1 if local_minimum_wage>0

gen recat_mw = minimum_wage // create a bundled version of the nominal minimum wage
replace recat_mw = 5 if minimum_wage>5 & minimum_wage<5.5
replace recat_mw = 5.5 if minimum_wage>5.5 & minimum_wage<6

replace recat_mw = 6 if minimum_wage>6 & minimum_wage<6.5
replace recat_mw = 6.5 if minimum_wage>6.5 & minimum_wage<7

replace recat_mw = 7 if minimum_wage>7 & minimum_wage<7.5
replace recat_mw = 7.5 if minimum_wage>7.5 & minimum_wage<8

replace recat_mw = 8 if minimum_wage>8 & minimum_wage<8.5
replace recat_mw = 8.5 if minimum_wage>8.5 & minimum_wage<9

replace recat_mw = 9 if minimum_wage>9 & minimum_wage<9.5
replace recat_mw = 9.5 if minimum_wage>9.5 & minimum_wage<10

replace recat_mw = 10 if minimum_wage>10 & minimum_wage<11
replace recat_mw = 11 if minimum_wage>11 & minimum_wage<12
replace recat_mw = 12 if minimum_wage>12 & minimum_wage<13
replace recat_mw = 13 if minimum_wage>13 & minimum_wage<14
replace recat_mw = 14 if minimum_wage>14 & minimum_wage<15
replace recat_mw = 15 if minimum_wage>15 & minimum_wage<16

****************************************************
****		Check Treatment distribution		
****************************************************
preserve 
drop if naics!="00"
tab recat_mw years  if years<2007

tab recat_mw years if years>2009
restore

****************************************************
****		Model Specification 				
****************************************************


if $local ==1{
gen treat = local_mw // define the treatment variable as the nominal min wage
gl placebo = 5
gl dynamic = 5
}
if $pre2007 ==1{
drop if years>2006 // from 2000 to 2006, so seven years to work with
gl placebo = 3
gl dynamic = 4
}
if $post2009 ==1{
drop if years<2010 // from 2010 to 2018, so nine years to work with
gl placebo = 4
gl dynamic = 5
}
if $treat_mw ==1{
gen treat = mw_round // define the treatment variable as the nominal min wage
gen recat = recat_mw // use the recat_treatment option to discretize the treatment, it muddies the waters but allows for more units to be included
}

if $treat_mw_log ==1{
gen treat = log(mw_round) // define the treatment variable as the nominal min wage
gen recat = log(recat_mw) // use the recat_treatment option to discretize the treatment, it muddies the waters but allows for more units to be included
}

****************************************************
****		Run the Analysis					
****************************************************

if $allnonemployers_event ==1{
preserve
drop if naics!="00"
 did_multiplegt estab id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				recat_treatment(recat) ///
				cluster(id)
	ereturn list
if $pre2007 ==1{
		graph export "$output\eventpre_chaise_estab.pdf", replace
		graph close
}
if $post2009 ==1{
		graph export "$output\eventpost_chaise_estab.pdf", replace
		graph close
}

 did_multiplegt log_estab id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				recat_treatment(recat) ///
				cluster(id)
	ereturn list
if $pre2007 ==1{
		graph export "$output\eventpre_chaise_log_estab.pdf", replace
		graph close
}
if $post2009 ==1{
		graph export "$output\eventpost_chaise_log_estab.pdf", replace
		graph close
}

did_multiplegt estab_lf id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				recat_treatment(recat) ///
				cluster(id)
	ereturn list
if $pre2007 ==1{
		graph export "$output\eventpre_chaise_estab_pop.pdf", replace
		graph close
}
if $post2009 ==1{
		graph export "$output\eventpost_chaise_estab_pop.pdf", replace
		graph close
}

 did_multiplegt log_avg_rcp id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				recat_treatment(recat) ///
				cluster(id)
	ereturn list
if $pre2007 ==1{
		graph export "$output\eventpre_chaise_log_avg_rcp.pdf", replace
		graph close
}
if $post2009 ==1{
		graph export "$output\eventpost_chaise_log_avg_rcp.pdf", replace
		graph close
}

*/
restore
}
if $transport_event ==1{
preserve
drop if naics!="48-49"
 did_multiplegt estab id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				recat_treatment(recat) ///
				cluster(id)
	ereturn list
if $pre2007 ==1{
		graph export "$output\eventpre_chaise_estab_taxi.pdf", replace
		graph close
}
if $post2009 ==1{
		graph export "$output\eventpost_chaise_estab_taxi.pdf", replace
		graph close
}

 did_multiplegt log_estab id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				recat_treatment(recat) ///
				cluster(id)
	ereturn list
if $pre2007 ==1{
		graph export "$output\eventpre_chaise_log_estab_taxi.pdf", replace
		graph close
}
if $post2009 ==1{
		graph export "$output\eventpost_chaise_log_estab_taxi.pdf", replace
		graph close
}

did_multiplegt estab_lf id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				recat_treatment(recat) ///
				cluster(id)
	ereturn list
if $pre2007 ==1{
		graph export "$output\eventpre_chaise_estab_pop_taxi.pdf", replace
		graph close
}
if $post2009 ==1{
		graph export "$output\eventpost_chaise_estab_pop_taxi.pdf", replace
		graph close
}

 did_multiplegt log_avg_rcp id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				recat_treatment(recat) ///
				cluster(id)
	ereturn list
if $pre2007 ==1{
		graph export "$output\eventpre_chaise_log_avg_rcp_taxi.pdf", replace
		graph close
}
if $post2009 ==1{
		graph export "$output\eventpost_chaise_log_avg_rcp_taxi.pdf", replace
		graph close
}

restore
}


if $allnonemployers_notaxi_event ==1{
preserve
drop if naics!="00-48"
 did_multiplegt estab id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				recat_treatment(recat) ///
				cluster(id)
	ereturn list
if $pre2007 ==1{
		graph export "$output\eventpre_chaise_estab_notaxi.pdf", replace
		graph close
}
if $post2009 ==1{
		graph export "$output\eventpost_chaise_estab_notaxi.pdf", replace
		graph close
}

 did_multiplegt log_estab id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				recat_treatment(recat) ///
				cluster(id)
	ereturn list
if $pre2007 ==1{
		graph export "$output\eventpre_chaise_log_estab_notaxi.pdf", replace
		graph close
}
if $post2009 ==1{
		graph export "$output\eventpost_chaise_log_estab_notaxi.pdf", replace
		graph close
}

did_multiplegt estab_lf id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				recat_treatment(recat) ///
				cluster(id)
	ereturn list
if $pre2007 ==1{
		graph export "$output\eventpre_chaise_estab_pop_notaxi.pdf", replace
		graph close
}
if $post2009 ==1{
		graph export "$output\eventpost_chaise_estab_pop_notaxi.pdf", replace
		graph close
}

 did_multiplegt log_avg_rcp id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				recat_treatment(recat) ///
				cluster(id)
	ereturn list
if $pre2007 ==1{
		graph export "$output\eventpre_chaise_log_avg_rcp_notaxi.pdf", replace
		graph close
}
if $post2009 ==1{
		graph export "$output\eventpost_chaise_log_avg_rcp_notaxi.pdf", replace
		graph close
}

*/
restore
}
if $allnonemployers_local ==1{
preserve
drop if naics!="00"

 did_multiplegt log_estab id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				cluster(id)
	ereturn list
		graph export "$output\eventpost_chaise_local_log_estab.pdf", replace
		graph close

did_multiplegt estab_lf id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				cluster(id)
	ereturn list
		graph export "$output\eventpost_chaise_local_estab_pop.pdf", replace
		graph close

 did_multiplegt log_avg_rcp id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				cluster(id)
	ereturn list
		graph export "$output\eventpost_chaise_local_log_avg_rcp.pdf", replace
		graph close

*/
restore
}
if $transport_local ==1{
preserve
drop if naics!="48-49"

 did_multiplegt log_estab id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				cluster(id)
	ereturn list
	
		graph export "$output\eventpost_chaise_local_log_estab_taxi.pdf", replace
		graph close

did_multiplegt estab_lf id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				cluster(id)
	ereturn list

		graph export "$output\eventpost_chaise_local_estab_pop_taxi.pdf", replace
		graph close

 did_multiplegt log_avg_rcp id years treat, ///
				controls(`controls') ///
				breps($breps) ///
				placebo($placebo) ///
				dynamic($dynamic) ///
				robust_dynamic ///
				covariances ///
				average_effect ///
				jointtestplacebo ///
				longdiff_placebo ///
				weight(pop_avg) ///
				cluster(id)
	ereturn list

		graph export "$output\eventpost_chaise_local_log_avg_rcp_taxi.pdf", replace
		graph close

restore
}






