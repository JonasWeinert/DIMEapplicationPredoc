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


********* Appendix indices
global outcomes health_index_all health_index_you econindex policyindex

foreach var in $outcomes {
	eststo:reg `var' i.informationarm
}

esttab using "output/appendix indices woc.csv",se star(* .10 ** .05 *** .01) r2

