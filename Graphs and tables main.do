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

