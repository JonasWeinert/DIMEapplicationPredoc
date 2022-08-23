
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



///////////////////////////////END/OF/FILE//////////////////////////////////////
