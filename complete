//All code in this file is produced or adapted and cited by Jonas Weinert for their dissertation as part of their Msc. Global Health Policy in the LSE department of health policy
//Dataset available upon request to Dr. Mylene Lagarde: M.Lagarde@lse.ac.uk

***************************** #1: Data Cleaning and Manipulation**************************************


dis "Dataset available upon request to Dr. Mylene Lagarde: M.Lagarde@lse.ac.uk"

********************************************************************************
*                                                                              
* Setup                                                                       
*
********************************************************************************

capture log close
log using CovidRisk, text replace

gen control = 1 if informationarm == 0
replace control = 0 if informationarm >=1


gen treat_generic = 1 if informationarm == 1
replace treat_generic =0 if informationarm >1 | informationarm <1

gen treat_indiv = 1 if informationarm == 2
replace treat_indiv =0 if informationarm <2

********************************************************************************
*
* Controls & Subgroups: recoding of variables 
*
********************************************************************************

// create covid history (1= have/had | 0= no infection)
gen covidhist = 1 if health2_covid_fu == 1 | health2_covid_fu == 2
replace covidhist = 0 if health2_covid_fu == 3 | health2_covid_fu == 4
replace covidhist=. if health2_covid_fu==.

// create availabilitybias indicator (1= knows so who had virus | does not know anyone)
gen exposuretovirus = 1 if health3 < 5
replace exposuretovirus = 0 if health3 >=5 
replace exposuretovirus =. if health3 ==.

//create overall covid proximity
gen proximity = 0
replace proximity = 1 if exposuretovirus == 1
replace proximity = 1 if covidhist == 1


// create risk factor control (hasrf = binary; nrf = number of risk factors)
gen hasrf = 1 if diabetes == 1 | hypertension == 1 | age > 59 |  obesity == 1 | health1_1_covid_fu == 1 | health1_2_covid_fu == 1 | health1_3_covid_fu == 1
replace hasrf = 0 if diabetes == 0 & hypertension == 0 & age <= 59 &  obesity == 0 & health1_1_covid_fu == 0 & health1_2_covid_fu == 0 & health1_3_covid_fu == 0

gen agerf = 1 if age > 59
replace agerf = 0 if age <= 59 
gen nrf = diabetes + hypertension + agerf + obesity + health1_1_covid_fu + health1_2_covid_fu + health1_3_covid_fu

// create covid knowledge gap / knowledge of covid risk factors (0= neither knowledge of rf, nor wrong believes, neg=wrong believes, pos= number of known rf)
gen knowledgeofrf = know4_2_covid_fu + know4_10_covid_fu + know4_7_covid_fu + know4_4_covid_fu + know4_9_covid_fu + know4_6_covid_fu - know4_1_covid_fu - know4_3_covid_fu - know4_5_covid_fu
//////binary knowledgegap version (1= well informed - knowledge of 2 or more risk factors, 0= wrong or no knowledge of rf)
gen knowledgable = 1 if knowledgeofrf >=2
replace knowledgable = 0 if knowledgeofrf <= 1

// create pandemic impact on health seeking dummy (1= impact, 0= no impact)
gen healthimpact = 1 if health4 == 1 | health6 == 1
replace healthimpact = 0 if health4 == 0 & health6 ==0
replace healthimpact =. if health4 ==. & health6 ==.

// create pandemic impact on economic situation
gen econimpact = eco1_1_covid_fu + eco1_2_covid_fu + eco1_3_covid_fu + eco1_4_covid_fu + eco1_5_covid_fu + eco1_7_covid_fu - eco1_6_covid_fu - eco1_8_covid_fu
gen incomeloss = dincome - eco3_covid_fu
swindex econimpact incomeloss ppiscore, generate(econimpactindex) normby(control) displayw  replace
sum econimpactindex, d
mat medianecimpind = r(p50)
local medianecimpindl = medianecimpind[1,1]

//egen ecdummymed = `$medianecimpindl'
gen econimpactdummy = 0
replace econimpactdummy = 1 if econimpactindex  > .0169085
// percieved vs actual risk

// dummy overestimate risk
gen overestimate = 1 if hasrf == 0 & covid_severidad == 1
replace overestimate = 0 if covid_severidad == 0
replace overestimate = 0 if hasrf == 1 & covid_severidad !=.

// dummy underestimate risk
gen underestimate = 1 if hasrf == 1 & covid_severidad == 0
replace underestimate = 0 if covid_severidad == 1 
replace underestimate = 0 if hasrf == 0 & covid_severidad !=.

//dummy realistic
gen realistic = 0
replace realistic = 1 if overestimate ==0 & underestimate == 0
// categorical combination for control in regressions
gen pervsact = 0 if overestimate ==0 & underestimate == 0
replace pervsact = 1 if overestimate == 1
replace pervsact = 2 if underestimate ==1
label define perceplabel 0 "realistic" 1 "overestimator" 2 "underestimator", replace
label values pervsact perceplabel

// create control list macro //
global controlvlist male age basiced ppiscore riskl hinsurance lincome covidhist exposuretovirus knowledgable healthimpact underestimate overestimate


********************************************************************************
*
* Balance Check
*
********************************************************************************

global descrip  male dincome age basiced nrf exposuretovirus hinsurance risk covid_severidad_covid_fu econimpactdummy healthimpact 

mat fstat = [.]
foreach var in $descrip {
	eststo:reg `var' control treat_generic treat_indiv
	test control treat_generic treat_indiv
	local F = r(p)
	mat fstattemp = [`F']
	mat fstat  = nullmat(fstat)  \ fstattemp
}


mat list fstat

********************************************************************************
*
* Outcomes: recoding of variables & Index construction
*
********************************************************************************


************* Policy support *************

global policy2 policy2_1_covid_fu policy2_2_covid_fu policy2_3_covid_fu policy2_4_covid_fu policy2_5_covid_fu policy2_6_covid_fu policy2_7_covid_fu policy2_8_covid_fu policy2_9_covid_fu policy2_10_covid_fu 

// Create policy support summary index
swindex $policy2 , generate(policyindex) normby(control) displayw  replace

swindex $policy2 , generate(policyindex2) nostd displayw  replace
// swindex $policy2 , generate(policyindex) normby(control) norescale displayw  replace

************* economic risk perception *************

//Create economic prospect 1 month
gen ecriskmonth = dincome - (ecrisk1_covid_fu * ecrisk2_covid_fu) if ecrisk2_covid_fu != -999 
gen LNecriskmonth = log(ecriskmonth)

//Create economic prospect 1 yea r
gen LNecriskyear = log(dincome - ecrisk4_covid_fu) if ecrisk4_covid_fu != -999 

//Macro containing both
global econrisk LNecriskmonth LNecriskyear

// Create economic risk summary index
swindex LNecriskyear LNecriskmonth , generate(econindex) normby(control) flip(LNecriskyear LNecriskmonth) displayw  replace


************* health risk perception *************

// Create macro varlist of all hrisk*all
global healtriskall hrisk1_all_covid_fu hrisk2_all_covid_fu hrisk3_all_covid_fu hrisk4_all_covid_fu

// set Don't know/ inapplicable to missing
foreach v in $healtriskall {
	replace `v' =. if `v' < 0
}

// Create macro varlist of all hrisk*you
global healthriskyou hrisk1_you_covid_fu hrisk2_you_covid_fu hrisk3_you_covid_fu hrisk4_you_covid_fu

// set Don't know/ inapplicable to missing
foreach v in $healthriskyou {
	replace `v' =. if `v' < 0
}

// Create health risk summary indices
swindex $healtriskall $healthriskyou, generate(health_index_general) normby(control) displayw  replace 
swindex $healtriskall, generate(health_index_all) normby(control)  displayw  replace 
swindex $healthriskyou, generate(health_index_you) normby(control) displayw  replace 


************* demand for information *************
replace information_demand_covid_fu = 0 if information_demand_covid_fu == 2
label define demandlabel 0 "No demand" 1 "Demand", replace
label values information_demand_covid_fu demandlabel


***************************** #2a: Basic Analysis: Graphs and tables**************************************

do "[insert your file path]/Covid Risk Manipulation.do"

********************************************************************************
* 																			   *								
**# 					Tables										   		   *
*																			   *
********************************************************************************

********************************************************************************
*																			  
* 							 Risk perceptions									   
*																			   
********************************************************************************

cap drop _est*
local x = 1
local qc = 0
foreach outc in $healtriskall $healthriskyou $econrisk {
	eststo:reg `outc' i.informationarm , vce(robust) 
}
//export results
esttab using "output/Appendix Regressionresults Risk Perception w Controls.csv", se star(* .10 ** .05 *** .01) r2 


********************************************************************************
*																			  
* 							Policy Preferences									   
*																			   
********************************************************************************

mat policy = [.,.,.,.]
cap drop _est*
local x = 1
local qc = 1
foreach outc in $policy2 {  //items
	logit `outc' i.informationarm $controlvlist, vce(robust)
	eststo:margins, dydx(i.informationarm)
	mat `outc' = r(table)
				local bg =`outc'[1,2]
				local sg =`outc'[2,2]
				local bi =`outc'[1,3]
				local si =`outc'[2,3]
				sum `outc' if control == 1
				local cmean = `r(mean)'
				local csd = `r(sd)'
				mat policytemp = [`bg',`sg',`cmean',`csd']
				mat policy  = nullmat(policy)  \ policytemp
				local x = `x' + 1
				//mat policytemp = [`bi',`si',`cmean',`csd']
				//mat policy  = nullmat(policy)  \ policytemp
	
}

reg policyindex i.informationarm, vce(robust)   //index
margins, dydx(i.informationarm)
esttab using "output/Regressionresults Policy33333.csv", se star(* .10 ** .05 *** .01) pr2 

mat list policy

********************************************************************************
*																			  
* 							Demand for information						   
*																			   
********************************************************************************

logit information_demand_covid_fu i.informationarm#i.priceinf
margins, dydx(i.informationarm) post

capture tabulate priceinf, generate(g)		
global pricelevel g1 g2 g3

// Produce and store estimates
local x = 1	
local  y = 1
mat wtp			
foreach  prl in $pricelevel {
			logit information_demand_covid_fu i.informationarm if priceinf == 1
			eststo:margins, dydx(i.informationarm) 
}
//export results
esttab using "output/Regressionresults DemandInformation.csv", se star(* .10 ** .05 *** .01) pr2


********************************************************************************
* 																			   *								
**# 					Graphs										   		   *
*																			   *
********************************************************************************


********************************************************************************
*																			  
* 							Graph WTP 
*																			   
********************************************************************************

capture tabulate priceinf, generate(g)		
global pricelevel g1 g2 g3
global treatments control treat_generic treat_indiv 

// Produce and store estimates
mat wtp = [.,.,.,.,.]
local x = 1	
local  y = 1			
foreach  prl in $pricelevel {
		foreach treat in $treatments {	
			ci proportions information_demand_covid_fu if `treat' == 1 &  `prl' == 1
			local proport = r(proportion)
			local lb = r(lb)
			local ub = r(ub)
			mat wtptemp = [`proport',`lb',`ub',`x',`y']
			mat wtp = nullmat(wtp) \ wtptemp
			local x = `x' + 1
			local y = `y' + 1
		}
		local y = 1
		mat wtptemp = [.,.,.,`x',`y']
		mat wtp = nullmat(wtp) \ wtptemp
		local x = `x' + 1
}
					
// prepare datset for graph	
		mat colnames wtp = "Proportion" "lb" "ub" "counter" "treat"
		mat list wtp
		cap drop  Proportion  
		cap drop  lb 
		cap drop  ub 
		cap drop  counter
		cap drop  treat
		svmat wtp, names(col)
	
twoway ///
(rcap lb ub counter, vert lc(grey)) /// code for 95% CI
(scatter Proportion counter if treat ==1, ms(o) mc(grey) msize(large) connect(l) lp(shortdash)) /// dot for group 1
(scatter Proportion counter if treat ==2, ms(t) mc(navy) msize(large)   connect(l) lc(navy) lp(shortdash)) /// dot for group 2
(scatter Proportion counter if treat ==3,  mcolor(cranberry) xline(4 8, lc(gs14) ) connect(l) lc(cranberry) lp(shortdash ) ) /// dot for group 1
, legend(row(1) order(2 "Control Group" 3 "General Information" 4 "Tailored Information") pos(6) ) /// legend at 6 o'clock position
	xlabel(2.5 "1$" 6.5 "5$" 10.5 "10$", angle(0) noticks) ///
	title("Willingness to pay for Information") ///
	xtitle(" ") ///
	scale(.8) ///
	
	//ytitle("Y axis") ///
	
graph export "Willingness to pay.pdf"

********************************************************************************
*																			  
* 							Graph policy 
*																			   
********************************************************************************

// create varlist macros	
global policyoutcomes policy2_10_covid_fu policy2_1_covid_fu policy2_2_covid_fu policy2_3_covid_fu policy2_4_covid_fu policy2_5_covid_fu policy2_6_covid_fu policy2_7_covid_fu policy2_8_covid_fu policy2_9_covid_fu 
global treatments treat_generic treat_indiv

// Produce and store estimates
local x = 1				
foreach outc in $policyoutcomes {
**# Bookmark #4
		foreach treatvar in $treatments {	
			logit `outc' `treatvar' $treatments, or vce(robust)
			margins, dydx(`treatvar')
				mat `outc' = r(table)
				local b =`outc'[1,1]
				local ll =`outc'[5,1]
				local ul =`outc'[6,1]
				local p = `outc'[2,1]
				local outcome =`outc'
				local treatmentmatrix =`treatvar'
				if `x' == 1 {
				mat results = [`b',`ll',`ul',`treatmentmatrix',`x']
				}
				else {
				mat matresultstemp = [`b',`ll',`ul',`treatmentmatrix',`x']	
				mat results = nullmat(results) \ matresultstemp	
				mat qtemp = [`p',`x']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				} 				
				local x = `x' + 1
			}
			mat empty = [.,.,.,.,`x']
			mat results = nullmat(results) \ empty
			local x = `x' + 1
		}

// prepare datset for graph	
		mat colnames results = "Effectsize" "lb" "ub" "Treatment" "counter"
		mat list results
		cap drop  Effectsize lb ub Treatment counter
		svmat results, names(col)
		
// build graph
set scheme s1mono // black and white

twoway ///
(rcap lb ub counter, horizontal) /// code for 95% CI
(scatter  counter  Effectsize if Treatment ==0, mcolor(navy) xline(0) lstyle(grid)) /// dot for group 1
(scatter  counter Effectsize if Treatment ==1, mcolor(cranberry)) /// dot for group 2
, legend(row(1) order(2 "Generic Treatment" 3 " Individualized Treatment") pos(6)region(lwidth(none))) /// legend at 6 o'clock position
ylabel(1.5 "Restrictions on leaves" 4.5 "School closure" 7.5 "Closure NE" 10.5 "Closure E" 13.5 "Closure Pub. Transp." 16.5 "No meetings of 5+" 19.5 "Quarantine everyone*" 22.5 "Isolation of p at risk" 25.5 "Containment Center" 28.5 "Fines", angle(0) noticks) ///
title("Policy Support") ///
ytitle("") /// 
xline(5.0, lpattern(dash) lcolor(gs8)) //
//aspect(.2) //

********************************************************************************
*																			  
* 							Graph health risk 1: Indices 
*																			   
********************************************************************************

// create varlist macros	
global indiceoutcomes health_index_you health_index_all policyindex econindex
global treatments treat_generic treat_indiv

// Produce and store estimates
local x = 1				
foreach outc in $indiceoutcomes {
		foreach treatvar in $treatments {	
			reg `outc' `treatvar' $controlvlist $treatments, vce(robust) 
				mat `outc' = r(table)
				local b =`outc'[1,1]
				local ll =`outc'[5,1]
				local ul =`outc'[6,1]
				local outcome =`outc'
				local treatmentmatrix =`treatvar'
				if `x' == 1 {
				mat results = [`b',`ll',`ul',`treatmentmatrix',`x']
				}
				else {
				mat matresultstemp = [`b',`ll',`ul',`treatmentmatrix',`x']	
				mat results = nullmat(results) \ matresultstemp	
				} 				
				local x = `x' + 1
			}
			mat empty = [.,.,.,.,`x']
			mat results = nullmat(results) \ empty
			local x = `x' + 1
		}

// prepare datset for graph	
		mat colnames results = "Effectsize" "lb" "ub" "Treatment" "counter"
		mat list results
		cap drop  Effectsize lb ub Treatment counter
		svmat results, names(col)
		
// build graph
set scheme s1mono // black and white

twoway ///
(rcap lb ub counter, horizontal) /// code for 95% CI
(scatter  counter  Effectsize if Treatment ==0, mcolor(cranberry) xline(0) lstyle(grid)) /// dot for group 1
(scatter  counter Effectsize if Treatment ==1, mcolor(navy)) /// dot for group 2
, legend(row(1) order(2 "Generic Treatment" 3 " Individualized Treatment") pos(6)region(lwidth(none))) /// legend at 6 o'clock position
ylabel(1.5 "Personal Health Risk" 4.5 "Population Health risk" 7.5 "Combined Health risk" 10.5 "policyindex" 13.5 "econindex" /*16.5 "No meetings of 5+" 19.5 "Quarantine everyone*" 22.5 "Isolation of p at risk" 25.5 "Containment Center" 28.5 "Fines"*/, angle(0) noticks) ///
/// note that the labels are 1.5, 4.5, etc so they are between rows 1&2, 4&5, etc.
/// also note that there is a space in between different rows by leaving out rows 3, 6, 9, and 12 
//xlabel(-.035 " " .023 "1.0" , angle(0)) /// no 1.6 label
title("Title") ///
xtitle("X axis") /// 
ytitle("Y axis") /// 
yscale(reverse) /// y axis is flipped
xline(5.0, lpattern(dash) lcolor(gs8)) ///
/// aspect (next line) is how tall or wide the figure is
aspect(.5)


********************************************************************************
*																			  
* 							Graph health risk 1: Items 
*																			   
********************************************************************************

*** Population Health risk perception

// create varlist macros	
global healthriskoutcomes $healtriskall
global treatments treat_generic treat_indiv

// Produce and store estimates
local x = 1				
foreach outc in $healthriskoutcomes {
		foreach treatvar in $treatments {	
			reg `outc' `treatvar' $controlvlist $treatments, vce(robust) 
				mat `outc' = r(table)
				local b =`outc'[1,1]
				local ll =`outc'[5,1]
				local ul =`outc'[6,1]
				local outcome =`outc'
				local treatmentmatrix =`treatvar'
				if `x' == 1 {
				mat results = [`b',`ll',`ul',`treatmentmatrix',`x']
				}
				else {
				mat matresultstemp = [`b',`ll',`ul',`treatmentmatrix',`x']	
				mat results = nullmat(results) \ matresultstemp	
				} 				
				local x = `x' + 1
			}
			mat empty = [.,.,.,.,`x']
			mat results = nullmat(results) \ empty
			local x = `x' + 1
		}

// prepare datset for graph	
		mat colnames results = "Effectsize" "lb" "ub" "Treatment" "counter"
		mat list results
		cap drop  Effectsize lb ub Treatment counter
		svmat results, names(col)
		
// build graph
set scheme s1mono // black and white

twoway ///
(rcap lb ub counter, horizontal) /// code for 95% CI
(scatter  counter  Effectsize if Treatment ==0, mcolor(navy) xline(0) lstyle(grid)) /// dot for group 1
(scatter  counter Effectsize if Treatment ==1, mcolor(cranberry)) /// dot for group 2
, legend(row(1) order(2 "General Treatment" 3 " Tailored Treatment") pos(6)) /// legend at 6 o'clock position
	ylabel(1.5 "Previous Infection in %" 4.5 "Future Infection in %" 7.5 "Severe Cases in %" 10.5 "Mortality in %", angle(0) noticks) ///
	title("Population Health Risk Perception") ///
	ytitle(" ")
graph export "output/Health risk all items reults.pdf", as(pdf) name("Health risk all items results")

*** Personal Health risk perception

// create varlist macros	
global healthriskoutcomes $healthriskyou
global treatments treat_generic treat_indiv

// Produce and store estimates
local x = 1				
foreach outc in $healthriskoutcomes {
		foreach treatvar in $treatments {	
			reg `outc' `treatvar' $controlvlist $treatments, vce(robust) 
				mat `outc' = r(table)
				local b =`outc'[1,1]
				local ll =`outc'[5,1]
				local ul =`outc'[6,1]
				local outcome =`outc'
				local treatmentmatrix =`treatvar'
				if `x' == 1 {
				mat results = [`b',`ll',`ul',`treatmentmatrix',`x']
				}
				else {
				mat matresultstemp = [`b',`ll',`ul',`treatmentmatrix',`x']	
				mat results = nullmat(results) \ matresultstemp	
				} 				
				local x = `x' + 1
			}
			mat empty = [.,.,.,.,`x']
			mat results = nullmat(results) \ empty
			local x = `x' + 1
		}

// prepare datset for graph	
		mat colnames results = "Effectsize" "lb" "ub" "Treatment" "counter"
		mat list results
		cap drop  Effectsize lb ub Treatment counter
		svmat results, names(col)
		
// build graph
set scheme s1mono // black and white

twoway ///
(rcap lb ub counter, horizontal) /// code for 95% CI
(scatter  counter  Effectsize if Treatment ==0, mcolor(navy) xline(0) lstyle(grid)) /// dot for group 1
(scatter  counter Effectsize if Treatment ==1, mcolor(cranberry)) /// dot for group 2
, legend(row(1) order(2 "General Treatment" 3 " Tailored Treatment") pos(6)) /// legend at 6 o'clock position
	ylabel(1.5 "Previous Infection in % LY" 4.5 "Future Infection in % LY" 7.5 "Severe Cases in % LY" 10.5 "Mortality in % LY", angle(0) noticks) ///
	title("Personal Health Risk Perception") ///
	ytitle(" ")
graph export "output/Health risk ly items reults.pdf", as(pdf) name("Health risk items results")


********************************************************************************
*																			  
* 						Graph economic risks: Variables
*																			   
********************************************************************************

// create varlist macros	
global econriskoutcomes $econrisk
global treatments treat_generic treat_indiv

// Produce and store estimates
local x = 1				
foreach outc in $econriskoutcomes {
		foreach treatvar in $treatments {	
			reg `outc' `treatvar' $treatments /*$controlvlist*/, vce(robust)
				mat `outc' = r(table)
				local b =`outc'[1,1]
				local ll =`outc'[5,1]
				local ul =`outc'[6,1]
				local outcome =`outc'
				local treatmentmatrix =`treatvar'
				if `x' == 1 {
				mat results = [`b',`ll',`ul',`treatmentmatrix',`x']
				}
				else {
				mat matresultstemp = [`b',`ll',`ul',`treatmentmatrix',`x']	
				mat results = nullmat(results) \ matresultstemp	
				} 				
				local x = `x' + 1
			}
			mat empty = [.,.,.,.,`x']
			mat results = nullmat(results) \ empty
			local x = `x' + 1
		}

// prepare datset for graph	
		mat colnames results = "Effectsize" "lb" "ub" "Treatment" "counter"
		mat list results
		cap drop  Effectsize lb ub Treatment counter
		svmat results, names(col)
		
// build graph
set scheme s1mono // black and white

twoway ///
(rcap lb ub counter, horizontal) /// code for 95% CI
(scatter  counter  Effectsize if Treatment ==0, mcolor(cranberry) xline(0) lstyle(grid)) /// dot for group 1
(scatter  counter Effectsize if Treatment ==1, mcolor(navy)) /// dot for group 2
, legend(row(1) order(2 "Generic Treatment" 3 " Individualized Treatment") pos(6)region(lwidth(none))) /// legend at 6 o'clock position
ylabel(1.5 "Econ. prospect 1 mo" 4.5 "Econ. prospect 1 y" /*7.5 "Severe Cases in % LY" 10.5 "Lethality in % LY" 13.5 "Previous Infection in %" 16.5 "Future Infection in %" 19.5 "Severe Cases in %" 22.5 "Lethality in %" 25.5 "Containment Center" 28.5 "Fines"*/, angle(0) noticks) ///
title("Title") ///
xtitle("X axis") /// 
ytitle("Y axis") /// 
yscale(reverse) /// y axis is flipped
xline(5.0, lpattern(dash) lcolor(gs8)) ///
aspect(.2)
graph export "output/Health risk items reults.pdf", as(pdf) name("Health risk items results")


********************************************************************************
*																			  
* 						Graph Econ Health
*																			   
********************************************************************************

twoway (scatter econindex health_index_you) (lfit econindex health_index_you), ytitle(Economicrisk perception) xtitle(Health Risk Perception) legend(order(1 "Risk perception" 2 "Predicted economic risk perception"))



***************************** #2b: Basic Analysis: Subgroup**************************************


do "[insert your file path]/Covid Risk Manipulation.do"

********************************************************************************
*
**# Covid 19 Risk and Risk perception
*
********************************************************************************

**************** Graph Subgroups: General ****************
		
global subgroups covid_severidad_covid_fu riskl overestimate underestimate hasrf
global outcomes policyindex econindex health_index_all health_index_you 

// Produce and store estimates
local x = 1	
local  y = 1	
local qc = 111		
foreach  sg in $subgroups {
		foreach  outc in $outcomes {	
			reg `outc' i.informationarm#`sg', vce(robust)
				mat `outc' = r(table)
				local b =`outc'[1,4]
				local ll =`outc'[5,4]
				local ul =`outc'[6,4]				
				local p = `outc'[4,4]
				local outcome =`outc'
				local sgmatrix =`sg'
				if `x' == 1 {
				mat results = [`b',`ll',`ul',`y',`x']
				mat qtemp = [`b',`p',`x',`qc']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				}
				else {
				mat matresultstemp = [`b',`ll',`ul',`y',`x']	
				mat results = nullmat(results) \ matresultstemp	
				mat qtemp = [`b',`p',`x',`qc']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				} 				
				local x = `x' + 1
				local q = `q' + 1
				local y = `y' + 1
			}
			local y = 1
			mat empty = [.,.,.,.,`x']
			mat results = nullmat(results) \ empty
			local x = `x' + 1
		}

// prepare datset for graph	
		mat colnames results = "Effectsize" "lb" "ub" "Outcome" "counter"
		mat list results
		cap drop  Effectsize  
		cap drop  lb 
		cap drop  ub 
		cap drop  Outcome 
		cap drop  counter
		svmat results, names(col)
	
twoway ///
(rcap lb ub counter, vert lc(dknavy)) /// code for 95% CI
(scatter Effectsize counter if Outcome ==1, ms(o) mc(blue) msize(large) ) /// dot for group 1
(scatter Effectsize counter if Outcome ==2, ms(t) mc(lavender ) msize(large)  yline(0 , lc(gs1) lw(vthin) lp(shortdash))) /// dot for group 2
(scatter Effectsize counter if Outcome ==3, mcolor(cranberry) xline(5 10 15 20 25 30 , lc(gs14) ) ) /// dot for group 1
(scatter Effectsize counter if Outcome ==4, ms(d) mcolor(navy)) /// dot for group 2
, legend(row(1) order(2 "Policy Support" 3 "Economic RP" 4 "Health RP" 5 "Health RP (LY)") pos(6)) /// legend at 6 o'clock position
	xlabel(2.5 "High RP" 7.5 "R-Lover" 12.5 "Overest." 17.5 "Underest" 22.5 "At Risk", angle(0) noticks) ///
	title("Generic Info Covid Risk") ///
	xtitle(" ") ///
	//ytitle("Y axis") ///
	

**************** Graph Subgroups: Tailored ****************
		
global subgroups covid_severidad_covid_fu riskl overestimate underestimate hasrf
global outcomes policyindex econindex health_index_all health_index_you 

// Produce and store estimates
local x = 1	
local  y = 1			
foreach  sg in $subgroups {
		foreach  outc in $outcomes {	
			reg `outc' i.informationarm#`sg' , vce(robust)
				mat `outc' = r(table)
				local b =`outc'[1,6]
				local ll =`outc'[5,6]
				local ul =`outc'[6,6]
				local p =`outc'[4,6]
				local outcome =`outc'
				local sgmatrix =`sg'
				if `x' == 1 {
				mat results = [`b',`ll',`ul',`y',`x']
				mat qtemp = [`b',`p',`x']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				}
				else {
				mat matresultstemp = [`b',`ll',`ul',`y',`x']	
				mat results = nullmat(results) \ matresultstemp	
				mat qtemp = [`b',`p',`x']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				} 				
				local x = `x' + 1
				local y = `y' + 1
			} 
			local y = 1
			mat empty = [.,.,.,.,`x']
			mat results = nullmat(results) \ empty
			local x = `x' + 1
		}

// prepare datset for graph	
		mat colnames results = "Effectsize" "lb" "ub" "Outcome" "counter"
		mat list results
		cap drop  Effectsize  
		cap drop  lb 
		cap drop  ub 
		cap drop  Outcome 
		cap drop  counter
		svmat results, names(col)
	
twoway ///
(rcap lb ub counter, vert lc(dknavy)) /// code for 95% CI
(scatter Effectsize counter if Outcome ==1, ms(o) mc(blue) msize(large)) /// dot for group 1
(scatter Effectsize counter if Outcome ==2, ms(t) mc(lavender ) msize(large)  yline(0 , lc(gs1) lw(vthin) lp(shortdash))) ///	) /// dot for group 2
(scatter Effectsize counter if Outcome ==3, mcolor(cranberry) xline(5 10 15 20 25 30 , lc(gs14) ) ) /// dot for group 1
(scatter Effectsize counter if Outcome ==4, ms(d) mcolor(navy)) /// dot for group 2
, legend(row(1) order(2 "Policy Support" 3 "Economic RP" 4 "Health RP" 5 "Health RP (LY)") pos(6)) /// legend at 6 o'clock position
	xlabel(2.5 "High RP" 7.5 "R-Lover" 12.5 "Overest." 17.5 "Underest" 22.5 "At Risk", angle(0) noticks) ///
	title("Individualised Info Covid Risk") ///
	xtitle(" ") ///
	

********************************************************************************
*
**# Health and economic vulnurability
*
********************************************************************************

**************** Graph Subgroups: General ****************
		
global subgroups healthimpact econimpactdummy proximity
global outcomes policyindex econindex health_index_all health_index_you 

// Produce and store estimates
local x = 1	
local  y = 1			
foreach  sg in $subgroups {
		foreach  outc in $outcomes {	
			reg `outc' i.informationarm#`sg', vce(robust)
				mat `outc' = r(table)
				local b =`outc'[1,4]
				local ll =`outc'[5,4]
				local ul =`outc'[6,4]
				local p =`outc'[4,4]
				local outcome =`outc'
				local sgmatrix =`sg'
				if `x' == 1 {
				mat results = [`b',`ll',`ul',`y',`x']
				
				}
				else {
				mat matresultstemp = [`b',`ll',`ul',`y',`x']	
				mat results = nullmat(results) \ matresultstemp	
				
				} 				
				local x = `x' + 1
				local y = `y' + 1
			}
			local y = 1
			mat empty = [.,.,.,.,`x']
			mat results = nullmat(results) \ empty
			local x = `x' + 1
		}

// prepare datset for graph	
		mat colnames results = "Effectsize" "lb" "ub" "Outcome" "counter"
		mat list results
		cap drop  Effectsize  
		cap drop  lb 
		cap drop  ub 
		cap drop  Outcome 
		cap drop  counter
		svmat results, names(col)
	
twoway ///
(rcap lb ub counter, vert lc(dknavy)) /// code for 95% CI
(scatter Effectsize counter if Outcome ==1, ms(o) mc(blue) msize(large)) /// dot for group 1
(scatter Effectsize counter if Outcome ==2, ms(t) mc(lavender ) msize(large)  yline(0 , lc(gs1) lw(vthin) lp(shortdash))) /// dot for group 2
(scatter Effectsize counter if Outcome ==3, mcolor(cranberry) xline(5 10 15 20 25 30 , lc(gs14) ) ) /// dot for group 1
(scatter Effectsize counter if Outcome ==4, ms(d) mcolor(navy)) /// dot for group 2
, legend(row(1) order(2 "Policy Support" 3 "Economic RP" 4 "Health RP" 5 "Pers. Health RP (LY)") pos(6)) /// legend at 6 o'clock position
	xlabel(2.5 "Medically vulnerable" 7.5 "Economically vulnerable" 12.5 "Covid Exposure", angle(0) noticks) ///
	title("General Information Pandemic Exposure") ///
	xtitle(" ") ///
	//ytitle("Y axis") ///
	
	
**************** Graph Subgroups: Tailored ****************
		
global subgroups healthimpact econimpactdummy  proximity
global outcomes policyindex econindex health_index_all health_index_you 

// Produce and store estimates
local x = 1	
local  y = 1			
foreach  sg in $subgroups {
		foreach  outc in $outcomes {	
			reg `outc' i.informationarm#`sg', vce(robust)
				mat `outc' = r(table)
				local b =`outc'[1,6]
				local ll =`outc'[5,6]
				local ul =`outc'[6,6]
				local p =`outc'[4,6]
				local outcome =`outc'
				local sgmatrix =`sg'
				if `x' == 1 {
				mat results = [`b',`ll',`ul',`y',`x']
				
				}
				else {
				mat matresultstemp = [`b',`ll',`ul',`y',`x']	
				mat results = nullmat(results) \ matresultstemp	
				
				} 				
				local x = `x' + 1
				local y = `y' + 1
			}
			local y = 1
			mat empty = [.,.,.,.,`x']
			mat results = nullmat(results) \ empty
			local x = `x' + 1
		}
// prepare datset for graph	
		mat colnames results = "Effectsize" "lb" "ub" "Outcome" "counter"
		mat list results
		cap drop  Effectsize  
		cap drop  lb 
		cap drop  ub 
		cap drop  Outcome 
		cap drop  counter
		svmat results, names(col)
	
twoway ///
(rcap lb ub counter, vert lc(dknavy)) /// code for 95% CI
(scatter Effectsize counter if Outcome ==1, ms(o) mc(blue) msize(large)) /// dot for group 1
(scatter Effectsize counter if Outcome ==2, ms(t) mc(lavender ) msize(large)  yline(0 , lc(gs1) lw(vthin) lp(shortdash))) /// dot for group 2
(scatter Effectsize counter if Outcome ==3, mcolor(cranberry) xline(5 10 15 20 25 30 , lc(gs14) ) ) /// dot for group 1
(scatter Effectsize counter if Outcome ==4, ms(d) mcolor(navy)) /// dot for group 2
, legend(row(1) order(2 "Policy Support" 3 "Economic RP" 4 "Health RP" 5 "Pers. Health RP (LY)") pos(6)) /// legend at 6 o'clock position
	xlabel(2.5 "Medically vulnerable " 7.5 "Economically vulnerable" 12.5 "Covid proximity", angle(0) noticks) ///
	title("Tailored Information Pandemic Exposure") ///
	xtitle(" ") ///
	//ytitle("Y axis") ///
	
graph export "Individualised Info Pandemic Exposure.pdf"

/*	SES SG
**# Socioeconomic and Demographic
**************** Graph Subgroups: Generic ****************
		
global subgroups agerf male basiced 
global outcomes policyindex econindex health_index_all health_index_you 
// Produce and store estimates
local x = 1	
local  y = 1			
foreach  sg in $subgroups {
		foreach  outc in $outcomes {	
			reg `outc' i.informationarm#`sg', vce(robust)
				mat `outc' = r(table)
				local b =`outc'[1,4]
				local ll =`outc'[5,4]
				local ul =`outc'[6,4]
				local p =`outc'[4,4]
				local outcome =`outc'
				local sgmatrix =`sg'
				if `x' == 1 {
				mat results = [`b',`ll',`ul',`y',`x']
				mat qtemp = [`b',`p',`x']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				}
				else {
				mat matresultstemp = [`b',`ll',`ul',`y',`x']	
				mat results = nullmat(results) \ matresultstemp	
				mat qtemp = [`b',`p',`x']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				} 				
				local x = `x' + 1
				local y = `y' + 1
			}
			local y = 1
			mat empty = [.,.,.,.,`x']
			mat results = nullmat(results) \ empty
			local x = `x' + 1
		}
		mat colnames results = "Effectsize" "lb" "ub" "Outcome" "counter"
		mat list results
		cap drop  Effectsize  
		cap drop  lb 
		cap drop  ub 
		cap drop  Outcome 
		cap drop  counter
		svmat results, names(col)
	
twoway ///
(rcap lb ub counter, vert lc(dknavy)) /// code for 95% CI
(scatter Effectsize counter if Outcome ==1, ms(o) mc(blue) msize(large)) /// dot for group 1
(scatter Effectsize counter if Outcome ==2, ms(t) mc(lavender ) msize(large)  yline(0 , lc(gs1) lw(vthin) lp(shortdash))) /// dot for group 2
(scatter Effectsize counter if Outcome ==3, mcolor(cranberry) xline(5 10 15 20 25 30 , lc(gs14) ) ) /// dot for group 1
(scatter Effectsize counter if Outcome ==4, ms(d) mcolor(navy)) /// dot for group 2
, legend(row(1) order(2 "Policy Support" 3 "Economic RP" 4 "Health RP" 5 "Health RP (LY)") pos(6)) /// legend at 6 o'clock position
	xlabel(2.5 "Over 60" 7.5 "Male" 12.5 "Completed basic education", angle(0) noticks) ///
	title("Generic Info Demographic and Socioeconomic") ///
	xtitle(" ") ///
	//ytitle("Y axis") ///
	
graph export "Generic Info Demographic and Socioeconomic.pdf"
	
**************** Graph Subgroups:Individualised ****************
		
global subgroups agerf male basiced 
global outcomes policyindex econindex health_index_all health_index_you 
// Produce and store estimates
local x = 1	
local  y = 1			
foreach  sg in $subgroups {
		foreach  outc in $outcomes {	
			reg `outc' i.informationarm#`sg', vce(robust)
				mat `outc' = r(table)
				local b =`outc'[1,4]
				local ll =`outc'[5,6]
				local ul =`outc'[6,6]
				local p =`outc'[4,6]
				local outcome =`outc'
				local sgmatrix =`sg'
				if `x' == 1 {
				mat results = [`b',`ll',`ul',`y',`x']
				mat qtemp = [`b',`p',`x']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				}
				else {
				mat matresultstemp = [`b',`ll',`ul',`y',`x']	
				mat results = nullmat(results) \ matresultstemp	
				mat qtemp = [`b',`p',`x']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				} 				
				local x = `x' + 1
				local y = `y' + 1
			}
			local y = 1
			mat empty = [.,.,.,.,`x']
			mat results = nullmat(results) \ empty
			local x = `x' + 1
		}
// prepare datset for graph	
		mat colnames results = "Effectsize" "lb" "ub" "Outcome" "counter"
		mat list results
		cap drop  Effectsize  
		cap drop  lb 
		cap drop  ub 
		cap drop  Outcome 
		cap drop  counter
		svmat results, names(col)
	
twoway ///
(rcap lb ub counter, vert lc(dknavy)) /// code for 95% CI
(scatter Effectsize counter if Outcome ==1, ms(o) mc(blue) msize(large)) /// dot for group 1
(scatter Effectsize counter if Outcome ==2, ms(t) mc(lavender ) msize(large)  yline(0 , lc(gs1) lw(vthin) lp(shortdash))) /// dot for group 2
(scatter Effectsize counter if Outcome ==3, mcolor(cranberry) xline(5 10 15 20 25 30 , lc(gs14) ) ) /// dot for group 1
(scatter Effectsize counter if Outcome ==4, ms(d) mcolor(navy)) /// dot for group 2
, legend(row(1) order(2 "Policy Support" 3 "Economic RP" 4 "Health RP" 5 "Health RP (LY)") pos(6)) /// legend at 6 o'clock position
	xlabel(2.5 "Over 60" 7.5 "Male" 12.5 "Completed basic education", angle(0) noticks) ///
	title("Individualised Info Demographic and Socioeconomic") ///
	xtitle(" ") ///
	//ytitle("Y axis") ///
	
graph export "Individualised Info Demographic and Socioeconomic.pdf"
// 
svmat qvalues, names(col)
*/ 



********************************************************************************
*
**# WTP all SGs
*
********************************************************************************

global subgroups covid_severidad_covid_fu riskl overestimate underestimate hasrf healthimpact econimpactdummy proximity 
mat wtpsg = [.,.,.,.,.]

// Produce and store estimates
local x = 1	
local  y = 1			
foreach  sg in $subgroups {
			logit information_demand_covid_fu priceinf i.informationarm#`sg' $controlvlist, vce(robust)
			margins, dydx(i.informationarm)
				mat results = r(table)
				local bg = results[1,2]
				local serg = results[2,2]
				local pg =results[4,2]
				local zg = results[3,2]
				local bi = results[1,3]
				local seri = results[2,3]
				local pi =results[4,3]
				local zi = results[3,3]
				mat wtpsgtemp = [`bg',`zg',`pg',`serg',`x']
				mat wtpsg  = nullmat(wtpsg)  \ wtpsgtemp	
				local x = `x' + 1
				mat wtpsgtemp = [`bi',`zi',`pi',`seri',`x']
				mat wtpsg  = nullmat(wtpsg)  \ wtpsgtemp	
				local x = `x' + 1		
			}

logit information_demand_covid_fu priceinf i.informationarm $controlvlist, vce(robust)
margins, dydx(i.informationarm)
				mat results = r(table)
				global bg = results[1,2]
				global serg = results[2,2]
				global pg =results[4,2]
				global zg = results[3,2]
				global bi = results[1,3]
				global seri = results[2,3]
				global pi =results[4,3]
				global zi = results[3,3]
				mat wtpsgtemp = [`bg',`zg',`pg',`serg',`x']
				mat wtpsg  = nullmat(wtpsg)  \ wtpsgtemp	
				mat wtpsgtemp = [`bi',`zi',`pi',`seri',`x']
				mat wtpsg  = nullmat(wtpsg)  \ wtpsgtemp	


mat colnames wtpsg = "Effectsize" "Z score" "P > z" "SE" "counter"
mat list wtpsg

***************************** #2c: FDR adjustment: Anderson's sharpened q values **************************************

do "[insert your file path]/Covid Risk Manipulation.do"

********************************************************************************
*
* Q Values
*
********************************************************************************

mat qvalues = [.,.,.,.]
mat colnames qvalues = "coeff" "pval" "pvacountint" "sectioncount"

*** Risk perceptions
local x = 1
local qc = 0
global outcomes econindex $econrisk health_index_you $healthriskyou health_index_all $healtriskall policyindex
foreach outc in $outcomes{
	reg `outc' i.informationarm, vce(robust)
				mat `outc' = r(table)
				local bg =`outc'[1,2]
				local bi =`outc'[1,3]
				local pg = `outc'[4,2]
				local pi = `outc'[4,3]
				mat qtemp = [`bg',`pg',`x',`qc']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				local x = `x' + 1
				local qc = 0
				mat qtemp = [`bi',`pi',`x',`qc']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				local x = `x' + 1
				local qc = 0

	//eststo: margins, dydx(i.informationarm)// 
}


*** Policy preferences
local x = 1
local qc = 1
foreach outc in $policy2 {
	logit `outc' i.informationarm, vce(robust)
	margins, dydx(i.informationarm)
	mat `outc' = r(table)
				local bg =`outc'[1,2]
				local bi =`outc'[1,3]
				local pg =`outc'[4,2]
				local pi = `outc'[4,3]
				mat qtemp = [`bg',`pg',`x',`qc']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				local x = `x' + 1
				local qc = 1
				mat qtemp = [`bi',`pi',`x',`qc']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				local x = `x' + 1
				local qc = 1
	
}


**** SG

global subgroups covid_severidad_covid_fu riskl overestimate underestimate hasrf healthimpact econimpactdummy proximity agerf male basiced 
global outcomes policyindex econindex health_index_all health_index_you 

// Produce and store estimates
local x = 1	
local  y = 1	
local qc = 111		
foreach  sg in $subgroups {
		foreach  outc in $outcomes {	
			reg `outc' i.informationarm#`sg', vce(robust)
				mat `outc' = r(table)
				local bg =`outc'[1,4]
				local bi =`outc'[1,6]
				local pg = `outc'[4,4]
				local pi = `outc'[4,6]
				mat qtemp = [`bg',`pg',`x',`qc']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				local x = `x' + 1
				//local qc = 111
				mat qtemp = [`bi',`pi',`x',`qc']
				mat qvalues  = nullmat(qvalues)  \ qtemp
				local x = `x' + 1
			}
local qc = `qc' + 1

		}

		
cap drop coeff  bky06_qval pval pvacountint sectioncount 


svmat qvalues, names(col)
sort sectioncount pvacountint

/////// The following Code is adapted from  M. Anderson, 11/20/08. See the original code here: https://are.berkeley.edu/~mlanderson/downloads/fdr_sharpened_qvalues.do.zip


quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

sort sectioncount pvacountint
bro coeff  bky06_qval pval pvacountint sectioncount 


******************************** #3: Appendix: Other tables and graphs **********************

do "[insert your file path]/Covid Risk Manipulation.do"


********************************************************************************
*
* Omitted Controls 
*
********************************************************************************

********* Items ***********
cap drop _est*
foreach outc in $healtriskall $healthriskyou $econrisk{
	eststo:reg `outc' i.informationarm , vce(robust) 
}
//export results
esttab using "output/Appendix Regressionresults Risk Perception w Controls.csv", se star(* .10 ** .05 *** .01) r2 

********* Inidces ***********
///////// Population Health
clear all
use "[insert your file path]/DESSAP COVID copy.dta"
do "[insert your file path]/Covid Risk Manipulation.do"
global subgroups covid_severidad_covid_fu riskl overestimate underestimate hasrf healthimpact econimpactdummy proximity 
foreach sg in $subgroups {
			eststo:reg health_index_all i.informationarm#`sg', vce(robust)
}
esttab using "output/Heterogeneous/population health woc Heterogeneous.csv", se star(* .10 ** .05 *** .01) r2 
clear all
use "[insert your file path]/DESSAP COVID copy.dta"
do "[insert your file path]/Covid Risk Manipulation.do"
///////// Personal Health
global subgroups covid_severidad_covid_fu riskl overestimate underestimate hasrf healthimpact econimpactdummy proximity 
foreach sg in $subgroups {
			eststo:reg health_index_you i.informationarm#`sg', vce(robust)
}
esttab using "output/Heterogeneous/personal health woc Heterogeneous.csv", se star(* .10 ** .05 *** .01) r2
clear all
use "[insert your file path]/DESSAP COVID copy.dta"
do "[insert your file path]/Covid Risk Manipulation.do"
///////// Econ 
global subgroups covid_severidad_covid_fu riskl overestimate underestimate hasrf healthimpact econimpactdummy proximity 
foreach sg in $subgroups {
			eststo:reg econindex i.informationarm#`sg', vce(robust)
}
esttab using "output/Heterogeneous/econ risk woc Heterogeneous.csv", se star(* .10 ** .05 *** .01) r2 
clear all
use "[insert your file path]/DESSAP COVID copy.dta"
do "[insert your file path]/Covid Risk Manipulation.do"
///////// Policy 
global subgroups covid_severidad_covid_fu riskl overestimate underestimate hasrf healthimpact econimpactdummy proximity 
foreach sg in $subgroups {
			eststo:reg policyindex i.informationarm#`sg', vce(robust)
}
esttab using "output/Heterogeneous/policy woc Heterogeneous.csv",se star(* .10 ** .05 *** .01) r2

********************************************************************************
*
* Included Controls 
*
********************************************************************************

********* Items ***********
cap drop _est*
foreach outc in $healtriskall $healthriskyou $econrisk{
	eststo:reg `outc' i.informationarm $controlvlist, vce(robust) 
}
//export results
esttab using "output/Appendix Regressionresults Risk Perception w Controls.csv", drop($controlvlist) se star(* .10 ** .05 *** .01) r2 

********* Inidces ***********
///////// Population Health
clear all
use "[insert your file path]/DESSAP COVID copy.dta"
do "[insert your file path]/Covid Risk Manipulation.do"global subgroups covid_severidad_covid_fu riskl overestimate underestimate hasrf healthimpact econimpactdummy proximity 
foreach sg in $subgroups {
			eststo:reg health_index_all i.informationarm#`sg' $controlvlist, vce(robust)
}
esttab using "output/Heterogeneous/population health wc Heterogeneous.csv", se star(* .10 ** .05 *** .01) r2 drop($controlvlist)
clear all
use "[insert your file path]/DESSAP COVID copy.dta"
do "[insert your file path]/Covid Risk Manipulation.do"
///////// Personal Health
global subgroups covid_severidad_covid_fu riskl overestimate underestimate hasrf healthimpact econimpactdummy proximity 
foreach sg in $subgroups {
			eststo:reg health_index_you i.informationarm#`sg' $controlvlist, vce(robust)
}
esttab using "output/Heterogeneous/personal health wc Heterogeneous.csv", se star(* .10 ** .05 *** .01) r2 drop($controlvlist)
clear all
use "[insert your file path]/DESSAP COVID copy.dta"
do "[insert your file path]/Covid Risk Manipulation.do"
///////// Econ 
global subgroups covid_severidad_covid_fu riskl overestimate underestimate hasrf healthimpact econimpactdummy proximity 
foreach sg in $subgroups {
			eststo:reg econindex i.informationarm#`sg' $controlvlist, vce(robust)
}
esttab using "output/Heterogeneous/econ risk wc Heterogeneous.csv", se star(* .10 ** .05 *** .01) r2 drop($controlvlist)
clear all
use "[insert your file path]/DESSAP COVID copy.dta"
do "[insert your file path]/Covid Risk Manipulation.do"
///////// Policy 
global subgroups covid_severidad_covid_fu riskl overestimate underestimate hasrf healthimpact econimpactdummy proximity 
foreach sg in $subgroups {
			eststo:reg policyindex i.informationarm#`sg' $controlvlist, vce(robust) 
}
esttab using "output/Heterogeneous/policy wc Heterogeneous.csv",se star(* .10 ** .05 *** .01) r2 drop($controlvlist)



///////////////////////////////END/OF/FILE//////////////////////////////////////

