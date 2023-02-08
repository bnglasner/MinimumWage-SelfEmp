***** 
* Clustered std error analysis, minimum wage, nonstandard work, with HHI index
*****
cls
clear	
set more off
set trace off
set matsize 10000

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
/**********************************************
* Install ftools (remove program if it existed previously)
cap ado uninstall ftools
net install ftools, from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/")

* Install reghdfe 6.x
cap ado uninstall reghdfe
net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/")

ssc install ppmlhdfe
ssc install estout
*/

*********************************************
gl equation_1 = 1
gl equation_2 = 0
gl equation_3 = 0
gl equation_4 = 0

* Select only one of these
gl total = 0
gl taxi = 0
gl no_taxi = 1

* Select only one of these
gl realmw = 1
gl nommw = 0

* only select one of these at a time when doint the event study. Deselect both when using local option
gl pre2007 = 0
gl post2009 = 1

**************************************************
import delimited "$data/primary_analysis_RR.csv", clear
xtset id years
if $taxi ==1{
drop if naics!="48-49"
}
if $total ==1{
drop if naics!="00"
}
if $no_taxi ==1{
drop if naics!="00-48"
}
if $pre2007 ==1{
drop if years>2006 // from 2000 to 2006, so seven years to work with
}
if $post2009 ==1{
drop if years<2010 // from 2010 to 2018, so nine years to work with
}

*Interaction Effects with Centering
gen log_mw_inf = log(minimum_wage_inf)
sum log_mw_inf, meanonly
replace log_mw_inf = log_mw_inf - r(mean)

gen log_mw = log(minimum_wage)
sum log_mw, meanonly
replace log_mw = log_mw - r(mean)

sum minimum_wage_inf, meanonly
replace minimum_wage_inf = minimum_wage_inf - r(mean)

sum minimum_wage, meanonly
replace minimum_wage = minimum_wage - r(mean)

rename quantile county_quant

/*
twowayfeweights log_estab id years minimum_wage_inf, ///
				controls(county_quant pop_density uber_active) ///
				type(feTR) ///
				test_random_weights(county_quant pop_density uber_active) ///
				weight(population)
*/

if $realmw ==1{
gen treat = log_mw_inf
}

if $nommw ==1{
gen treat = log_mw
}


if $equation_1 ==1{

reghdfe log_estab ///
			c.treat ///
			c.county_quant ///
			c.pop_density ///
			c.uber_active ///
			[aw = pop_avg], ///
			absorb(id years) ///
			vce(cluster cty_st)
			
qui margins, dydx(treat)
marginsplot

reghdfe estab_lf ///
			c.treat ///
			c.county_quant ///
			c.pop_density ///
			c.uber_active ///
			[aw = pop_avg], ///
			absorb(id years) ///
			vce(cluster cty_st)
		
qui margins, dydx(treat)
marginsplot

reghdfe log_avg_rcp ///
			c.treat ///
			c.county_quant ///
			c.pop_density ///
			c.uber_active ///
			[aw = pop_avg], ///
			absorb(id years) ///
			vce(cluster cty_st)
		
qui margins, dydx(treat)
marginsplot
}
if $equation_2 ==1{

reghdfe log_estab ///
			c.treat##c.county_quant ///
			c.pop_density ///
			 c.uber_active ///
			[aw = pop_avg], ///
			absorb(id years) ///
			vce(cluster cty_st)
		
qui margins, dydx(treat) at(county_quant=(5(40)85))
marginsplot

reghdfe estab_lf ///
			c.treat##c.county_quant ///
			c.pop_density ///
			 c.uber_active ///
			[aw = pop_avg], ///
			absorb(id years) ///
			vce(cluster cty_st)
		
qui margins, dydx(treat) at(county_quant=(5(40)85))
marginsplot

reghdfe log_avg_rcp ///
			c.treat##c.county_quant ///
			c.pop_density ///
			 c.uber_active ///
			[aw = pop_avg], ///
			absorb(id years) ///
			vce(cluster cty_st)
		
qui margins, dydx(treat) at(county_quant=(5(40)85))
marginsplot
}
if $equation_3 ==1{

reghdfe log_estab ///
			c.treat##c.uber_active ///
			c.pop_density ///
			c.county_quant ///
			[aw = pop_avg], ///
			absorb(id years) ///
			vce(cluster cty_st)
		
qui margins, dydx(treat) at(uber_active=(0(1)1))
marginsplot

reghdfe estab_lf ///
			c.treat##c.uber_active ///
			c.pop_density ///
			c.county_quant ///
			[aw = pop_avg], ///
			absorb(id years) ///
			vce(cluster cty_st)
		
qui margins, dydx(treat) at(uber_active=(0(1)1))
marginsplot

reghdfe log_avg_rcp ///
			c.treat##c.uber_active ///
			c.pop_density ///
			c.county_quant ///
			[aw = pop_avg], ///
			absorb(id years) ///
			vce(cluster cty_st)
		
qui margins, dydx(treat) at(uber_active=(0(1)1))
marginsplot
}
if $equation_4 ==1{

reghdfe log_estab ///
			c.treat##c.uber_active##c.county_quant##c.pop_density ///
			[aw = pop_avg], ///
			absorb(id years) ///
			vce(cluster cty_st)
		
qui estpost margins, dydx(treat) at(county_quant=(0(10)100) uber_active=(0(1)1))
marginsplot, yline(0) ///
	title("Average Marginal Effects of the Real Min. Wage (95% CIs)") ///
	ytitle("Effects on Linear Prediction") ///
	xtitle("HHI Quantile") ///
	yscale(r(-.2 .2)) ///
	legend(order(3 "Uber Inactive" 4 "Uber Active") pos(5)) 
		if $total ==1{
				esttab using twfe_log_estab_total.csv, cells("b se ci_l ci_u p") replace
				graph export "$output\twfe_log_estab_total_cty.png", width(800) height(800) replace
		}
		if $taxi ==1{
				esttab using twfe_log_estab_taxi.csv, cells("b se ci_l ci_u p") replace
				graph export "$output\twfe_log_estab_taxi_cty.png", width(800) height(800) replace
		}
		graph close

reghdfe estab_lf ///
			c.treat##c.uber_active##c.county_quant##c.pop_density ///
			[aw = pop_avg], ///
			absorb(id years) ///
			vce(cluster cty_st)
		
qui estpost margins, dydx(treat) at(county_quant=(0(10)100) uber_active=(0(1)1))
marginsplot, yline(0) ///
	title("Average Marginal Effects of the Real Min. Wage (95% CIs)") ///
	ytitle("Effects on Linear Prediction") ///
	xtitle("HHI Quantile") ///
	legend(order(3 "Uber Inactive" 4 "Uber Active") pos(5)) 
		if $total ==1{
				esttab using twfe_estabpop_total.csv, cells("b se ci_l ci_u p") replace
				graph export "$output\twfe_estabpop_total_cty.png", width(800) height(800) replace
		}
		if $taxi ==1{
				esttab using twfe_estabpop_taxi.csv, cells("b se ci_l ci_u p") replace
				graph export "$output\twfe_estabpop_taxi_cty.png", width(800) height(800) replace
		}
		graph close

reghdfe log_avg_rcp ///
			c.treat##c.uber_active##c.county_quant##c.pop_density ///
			[aw = pop_avg], ///
			absorb(id years) ///
			vce(cluster cty_st)
		
qui estpost margins, dydx(treat) at(county_quant=(0(10)100) uber_active=(0(1)1))
marginsplot, yline(0) ///
	title("Average Marginal Effects of the Real Min. Wage (95% CIs)") ///
	ytitle("Effects on Linear Prediction") ///
	xtitle("HHI Quantile") ///
	yscale(r(-.2 .2)) ///
	legend(order(3 "Uber Inactive" 4 "Uber Active") pos(5)) 
		if $total ==1{
				esttab using twfe_log_avg_rcp_total.csv, cells("b se ci_l ci_u p") replace
				graph export "$output\twfe_log_avg_rcp_total_cty.png", width(800) height(800) replace
		}
		if $taxi ==1{
				esttab using twfe_log_avg_rcp_taxi.csv, cells("b se ci_l ci_u p") replace
				graph export "$output\twfe_log_avg_rcp_taxi_cty.png", width(800) height(800) replace
		}
		graph close

}


*/
