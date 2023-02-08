***** 
* Clustered std error analysis, minimum wage, nonstandard work, with HHI index
*****
cls
clear all
set more off
set trace off
set matsize 10000
set seed 42

if c(username)=="bngla" {
        gl data "\Users\bngla\Dropbox\Post_Doc\Understanding America Study\Data\Subset"
        gl output "\Users\bngla\Dropbox\Post_Doc\Understanding America Study\Output"
		gl do "\Users\bngla\Dropbox\Post_Doc\Understanding America Study\Code"
        gl home "\Users\bngla\Dropbox\Post_Doc\Understanding America Study"
}
if c(username)=="bglasner" {
        gl data "\Users\bglasner\Dropbox\Post_Doc\Understanding America Study\Data\Subset"
        gl output "\Users\bglasner\Dropbox\Post_Doc\Understanding America Study\Output"
		gl do "\Users\bglasner\Dropbox\Post_Doc\Understanding America Study\Code"
        gl home "\Users\bglasner\Dropbox\Post_Doc\Understanding America Study"
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
gl full = 0
gl female = 0 
gl race = 0
gl income = 0
****************************************
****		Fixed Effect Regression	****
****************************************
***
import delimited "$data/modelready_gad7.csv", clear
encode statereside, gen(statereside_n)
encode education, gen(education_n)
destring race_hispanic, replace force
destring income_bin, replace force
destring ctc_receipt, replace force
keep if has_children ==1
keep if income_bin<4

gen wg001_binary = (wg001>4)
*replace mean_net_mon =0 if ctc_receipt==0
*drop if ctc_receipt==.

did_multiplegt wg001 uasid wave mean_net_mon, ///
				breps(50) ///
				controls(age income_bin) ///
				cluster(statereside_n)
				
reghdfe wg001 c.mean_net_mon##post, absorb(i.statereside_n i.start_month) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000)) post vsquish noestimcheck
marginsplot
/*
reghdfe wg001 c.mean_net_mon##post i.female age i.race_hispanic i.income_bin, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000)) post vsquish noestimcheck
marginsplot
*/
reghdfe gad7 c.mean_net_mon##post, absorb(i.statereside_n i.start_month) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000)) post vsquish noestimcheck
marginsplot
/*
reghdfe gad7 c.mean_net_mon##post i.female age i.race_hispanic i.income_bin, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000)) post vsquish noestimcheck
marginsplot

reghdfe wg001_binary c.mean_net_mon##post i.female age i.race_hispanic i.income_bin, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000)) post vsquish noestimcheck
marginsplot

reghdfe gad7_binary c.mean_net_mon##post i.female age i.race_hispanic i.income_bin, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000)) post vsquish noestimcheck
marginsplot
*/
import delimited "$data/modelready_CESD8.csv", clear
encode statereside, gen(statereside_n)
encode education, gen(education_n)
destring race_hispanic, replace force
destring income_bin, replace force
destring ctc_receipt, replace force
keep if has_children ==1
keep if income_bin<4

* replace mean_net_mon =0 if ctc_receipt==0
* drop if ctc_receipt==.
reghdfe cesd c.mean_net_mon##post, absorb(i.statereside_n i.start_month) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000)) post vsquish noestimcheck
marginsplot

reghdfe cesd c.mean_net_mon##post i.female age i.race_hispanic i.income_bin, absorb(i.statereside_n i.start_month i.education_n i.start_year) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000)) post vsquish noestimcheck
marginsplot

reghdfe cesd_binary c.mean_net_mon##post i.female age i.race_hispanic i.income_bin, absorb(i.statereside_n i.start_month i.education_n i.start_year) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000)) post vsquish noestimcheck
marginsplot

*/
************************************************************
****		Fixed Effect Regression - Full interaction	****
************************************************************
***
if $full ==1 {
import delimited "$data/modelready_gad7.csv", clear
encode statereside, gen(statereside_n)
encode education, gen(education_n)
destring race_hispanic, replace force
destring income_bin, replace force
keep if has_children ==1
keep if income_bin<4

reghdfe wg001 c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)

/***
qui reghdfe wg001 c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) female = (0 1)) post vsquish noestimcheck

qui reghdfe wg001 c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) race_hispanic = (1 2 3)) post vsquish noestimcheck

qui reghdfe wg001 c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) income_bin = (1 2 3)) post vsquish noestimcheck

qui reghdfe wg001 c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) female = (0 1) income_bin = (1 2 3)) post vsquish noestimcheck

qui reghdfe wg001 c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) female = (0 1) race_hispanic = (1 2 3)) post vsquish noestimcheck

*/
reghdfe gad7 c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)

/***

qui reghdfe gad7 c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) female = (0 1)) post vsquish noestimcheck

qui reghdfe gad7 c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) race_hispanic = (1 2 3)) post vsquish noestimcheck

qui reghdfe gad7 c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) income_bin = (1 2 3)) post vsquish noestimcheck

qui reghdfe gad7 c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) female = (0 1) income_bin = (1 2 3)) post vsquish noestimcheck

qui reghdfe gad7 c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) female = (0 1) race_hispanic = (1 2 3)) post vsquish noestimcheck

************/
import delimited "$data/modelready_CESD8.csv", clear
encode statereside, gen(statereside_n)
encode education, gen(education_n)
destring race_hispanic, replace force
destring income_bin, replace force
keep if has_children ==1
keep if income_bin<4

reghdfe cesd c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)

/***
set more off

qui reghdfe cesd c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) female = (0 1)) post vsquish noestimcheck

qui reghdfe cesd c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) race_hispanic = (1 2 3)) post vsquish noestimcheck

qui reghdfe cesd c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) income_bin = (1 2 3)) post vsquish noestimcheck

qui reghdfe cesd c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) female = (0 1) income_bin = (1 2 3)) post vsquish noestimcheck

qui reghdfe cesd c.mean_net_mon##post##female##i.race_hispanic##i.income_bin age, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)
margins, dydx(post) at(mean_net_mon = (0(100)1000) female = (0 1) race_hispanic = (1 2 3)) post vsquish noestimcheck
************/

}

****************************************************
****		Fixed Effect Regression - Subset	****
****************************************************
***
if $female ==1 {
import delimited "$data/modelready_gad7.csv", clear
encode statereside, gen(statereside_n)
encode education, gen(education_n)
destring race_hispanic, replace force
destring income_bin, replace force
keep if has_children ==1
keep if female ==1
keep if income_bin<4

reghdfe wg001 c.mean_net_mon##post age i.race_hispanic i.income_bin, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)

reghdfe gad7 c.mean_net_mon##post age i.race_hispanic i.income_bin, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)

import delimited "$data/modelready_CESD8.csv", clear
encode statereside, gen(statereside_n)
encode education, gen(education_n)
destring race_hispanic, replace force
destring income_bin, replace force
keep if has_children ==1
keep if female ==0
keep if income_bin<4

reghdfe cesd c.mean_net_mon##post age i.race_hispanic i.income_bin, absorb(i.statereside_n i.start_month i.education_n i.start_year) cl(statereside_n)
}
if $race ==1 {
import delimited "$data/modelready_gad7.csv", clear
encode statereside, gen(statereside_n)
encode education, gen(education_n)
destring race_hispanic, replace force
destring income_bin, replace force
keep if has_children ==1
keep if income_bin<4
keep if race_hispanic==3

reghdfe wg001 c.mean_net_mon##post i.female age i.income_bin, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)

reghdfe gad7 c.mean_net_mon##post i.female age i.income_bin, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)

import delimited "$data/modelready_CESD8.csv", clear
encode statereside, gen(statereside_n)
encode education, gen(education_n)
destring race_hispanic, replace force
destring income_bin, replace force
keep if has_children ==1
keep if income_bin<4
keep if race_hispanic==3

reghdfe cesd c.mean_net_mon##post i.female age i.income_bin, absorb(i.statereside_n i.start_month i.education_n i.start_year) cl(statereside_n)

}

if $income ==1 {
import delimited "$data/modelready_gad7.csv", clear
encode statereside, gen(statereside_n)
encode education, gen(education_n)
destring race_hispanic, replace force
destring income_bin, replace force
destring ctc_receipt, replace force
keep if has_children ==1
keep if income_bin==3
keep if female==1 

replace mean_net_mon =0 if ctc_receipt==0
drop if ctc_receipt==.

reghdfe wg001 c.mean_net_mon##post i.female age i.race_hispanic, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)

reghdfe gad7 c.mean_net_mon##post i.female age i.race_hispanic, absorb(i.statereside_n i.start_month i.education_n) cl(statereside_n)


import delimited "$data/modelready_CESD8.csv", clear
encode statereside, gen(statereside_n)
encode education, gen(education_n)
destring race_hispanic, replace force
destring income_bin, replace force
destring ctc_receipt, replace force
keep if has_children ==1
keep if income_bin==3
keep if female==1 


reghdfe cesd c.mean_net_mon##post i.female age i.race_hispanic, absorb(i.statereside_n i.start_month i.education_n i.start_year) cl(statereside_n)

}
*
