
clear all
cls
cd "C:\Users\thom0\Desktop\Thomas Stuff\Polit KU\Speciale\\Programmering\Stata Output"
import excel "C:\Users\thom0\Desktop\Thomas Stuff\Polit KU\Speciale\\Programmering\Stata Input\DHS_move_full.xlsx", sheet("Sheet1") firstrow

*Make table coefficents interpretable as percentage changes
replace lost_owner = lost_owner *100
*Save unaltered dataset
save DHS, replace


*******
*Logit

logit lost_owner age male married_lag retired_lag student_lag education_lag i.regio_lag i.year income_lag divorced got_members lost_members got_retired got_job lost_job, robust

logit lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
*Probit
probit lost_owner age male married_lag retired_lag student_lag education_lag i.regio_lag i.year income_lag divorced got_members lost_members got_retired got_job lost_job, robust

probit lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
*




***************** Main Analysis - Table 3
use DHS, clear

reg lost_owner divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransition.doc, replace dec(3) ctitle(OLS) keep(divorced got_members lost_members got_retired got_job lost_job) addtext(Controls, NO, Data, 1994+, Expectation, NA)

reg lost_owner age male married_lag retired_lag student_lag education_lag i.regio_lag i.year income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransition.doc, append dec(3) ctitle(OLS) keep(divorced got_members lost_members got_job lost_job got_retired ) addtext(Controls, YES, Data, 1994+, Expectation, NA)
outreg2 using TableA1.doc, replace dec(3) ctitle(OLS) keep(divorced got_members lost_members got_job lost_job got_retired) addtext(Period, 1994-2019, ColumnNumber, 1)

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_job lost_job got_retired, robust
outreg2 using TableTransition.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Controls, YES, Data, 2004+, Expectation, NA)
*Table 4 - Frictions
outreg2 using TableTransFriction.doc, replace dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, NA)
*Table 7 - Inattention
outreg2 using Table7.doc, replace dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+)
*Table A1 - Corona Robustness
outreg2 using TableA1.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Period, 2004-2019, ColumnNumber, 3)
*Table A2 - Winsorizaotin Robustness
outreg2 using TableA2.doc, replace dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Outliers, Win. Original Cutoffs)

drop if missing(exp_home_price_lag)
drop if missing(exp_1y_mortgage_lag)
reg lost_owner age male married_lag retired_lag student_lag education_lag i.regio_lag i.year income_lag divorced got_members lost_members got_job lost_job got_retired, robust
outreg2 using TableTransition.doc, append dec(3) ctitle(OLS) keep(divorced got_members lost_members got_retired got_job lost_job) addtext(Controls, YES, Data, 2004+, Expectation, NA)


***** Inattention - Table 7
*interaciton term

use DHS, clear
gen inter = 0
replace inter = 1 if divorced == 1
replace inter = 1 if got_members == 1
replace inter = 1 if lost_members == 1
replace inter = 1 if got_job == 1
replace inter = 1 if lost_job == 1
replace inter = 1 if got_retired == 1

*gen interhome = exp_home_price_lag*inter
*gen intermort = exp_1y_mortgage_lag*inter
gen interhome = exp_home_price_lag*life_event
gen intermort = exp_1y_mortgage_lag*life_event


reg lost_owner exp_home_price_lag exp_1y_mortgage_lag interhome intermort age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using Table7.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag interhome intermort) addtext(Controls, YES, Data, 2004+)

*Cut dataset
keep if life_event ==1 

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag, robust
outreg2 using Table7.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+)



******************* FRICTRIONS  *******************
/*
************************************** Baseline
use DHS, clear

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust

outreg2 using TableTransFriction2.doc, replace dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, NA)
*/


************************************** Confidence
use DHS, clear

keep if confident_lag == 1

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransFriction.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, Confidence)

drop if kunde == 3

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransFriction2.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, Confidence=4)

************************************** kids
use DHS, clear


drop if kids_indicator_lag == 1


reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransFriction.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, kids)



************************************** Retired/Age
use DHS, clear

drop if retired_lag ==0

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransFriction.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, retired)

use DHS, clear

drop if age<65

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransFriction2.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, age>64)




************************************** education
use DHS, clear

keep if smart==1

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransFriction.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, College)


/*
************************************** Life events
use DHS, clear

drop if divorced == 1
drop if got_members == 1
drop if lost_members == 1
drop if got_retired == 1
drop if got_job == 1
drop if lost_job == 1


gen inter = 0
replace inter = 1 if divorced == 1
replace inter = 1 if got_members == 1
replace inter = 1 if lost_members == 1
replace inter = 1 if got_job == 1
replace inter = 1 if lost_job == 1
replace inter = 1 if got_retired == 1
keep if inter ==1 


reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag, robust
outreg2 using TableTransFriction.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, -events)

************************************** income_cat - virker fint men lidt for roddet
use DHS, clear

drop if income_cat_lag<5

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransFriction.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, income<5)


************************************** large_hh
use DHS, clear

drop if large_hh_lag == 1

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransFriction.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, largehh)

use DHS, clear

drop if larger_hh_lag == 1

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransFriction2.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, largerhh)
*/


******************* Appendix - Corona Robustness  *******************
*save reduced dataset
use DHS, clear
save DHS, replace

*load corona dataset
clear all
import excel "C:\Users\thom0\Desktop\Thomas Stuff\Polit KU\Speciale\Programmering\Stata Input\DHS_move_corona.xlsx", sheet("Sheet1") firstrow 

*Make table coefficents interpretable as percentage changes
replace lost_owner = lost_owner *100

*Without Expectations
reg lost_owner age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_job lost_job got_retired, robust
*Column 2 in actual table
outreg2 using TableA1.doc, append dec(3) ctitle(OLS) keep(divorced got_members lost_members got_job lost_job got_retired) addtext(Period, 1994-2022, ColumnNumber, 2)

*With Expectations
reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_job lost_job got_retired, robust
outreg2 using TableA1.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Period, 2004-2022, ColumnNumber, 4)

******************* Appendix - Winsorize Robustness  *******************
*load reduced dataset
use DHS, clear

* Winsorizaotin +/- 99 percentile
replace exp_home_price_lag = exp_home_price_lag_alt
replace exp_1y_mortgage_lag = exp_1y_mortgage_lag_alt

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_job lost_job got_retired, robust
outreg2 using TableA2.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Outliers, Win 99 pct.)


*Trimming +/- 99 percentile
replace exp_home_price_lag = exp_home_price_lag_trim
replace exp_1y_mortgage_lag = exp_1y_mortgage_lag_trim

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_job lost_job got_retired, robust
outreg2 using TableA2.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Outliers, Trim 99 pct.)



******************* FRICTRIONS - replication with winsorization  *******************

* Winsorizaotin +/- 99 percentile
replace exp_home_price_lag = exp_home_price_lag_alt
replace exp_1y_mortgage_lag = exp_1y_mortgage_lag_alt
save DHS, replace

************************************** Baseline
use DHS, clear

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust

outreg2 using TableA3.doc, replace dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, NA, Win, +/-99)


************************************** Confidence
use DHS, clear

keep if confident_lag == 1

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableA3.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, Confidence, Win, +/-99)

************************************** two or more kids
use DHS, clear

drop if kids_indicator_lag == 1

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableA3.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, kids, Win, +/-99)
************************************** Retired/Age
use DHS, clear

drop if retired_lag ==0

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableA3.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, retired, Win, +/-99)
************************************** Life events
use DHS, clear

drop if divorced == 1
drop if got_members == 1
drop if lost_members == 1
drop if got_retired == 1
drop if got_job == 1
drop if lost_job == 1

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag, robust
outreg2 using TableA3.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, -events, Win, +/-99)

*Eksperiment med Lead events
***************** Main Analysis - Table 3
use DHS, clear

replace divorced = divorced_lead
replace got_members = got_members_lead
replace lost_members = lost_members_lead
replace got_job = got_job_lead
replace lost_job = lost_job_lead
replace got_retired = got_retired_lead

reg lost_owner divorced_lead got_members_lead lost_members_lead got_job_lead got_retired_lead lost_job_lead, robust
outreg2 using TableTransition.doc, replace dec(3) ctitle(OLS) keep(divorced got_members lost_members got_retired got_job lost_job) addtext(Controls, NO, Data, 1994+, Expectation, NA)

reg lost_owner age male married_lag retired_lag student_lag education_lag i.regio_lag i.year income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransition.doc, append dec(3) ctitle(OLS) keep(divorced got_members lost_members got_job lost_job got_retired ) addtext(Controls, YES, Data, 1994+, Expectation, NA)

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_job lost_job got_retired, robust
outreg2 using TableTransition.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Controls, YES, Data, 2004+, Expectation, NA)

drop if missing(exp_home_price_lag)
drop if missing(exp_1y_mortgage_lag)
reg lost_owner age male married_lag retired_lag student_lag education_lag i.regio_lag i.year income_lag divorced got_members lost_members got_job lost_job got_retired, robust
outreg2 using TableTransition.doc, append dec(3) ctitle(OLS) keep(divorced got_members lost_members got_retired got_job lost_job) addtext(Controls, YES, Data, 2004+, Expectation, NA)




*************************************************************************************************
*********************** Old shit 
***********************************************************************************************

*Inattention
*interaciton terms IGEN
*GIVER SIGNIF

use DHS, clear
gen inter = 0
replace inter = 1 if divorced == 1
replace inter = 1 if got_members == 1
replace inter = 1 if lost_members == 1

gen exp_home_dif = exp_home_price_w - exp_home_price_lag
gen exp_mort_dif = exp_1y_mortgage_w - exp_1y_mortgage_lag

gen interhome = exp_home_dif*inter
gen intermort = exp_mort_dif*inter


reg lost_owner interhome intermort exp_home_dif exp_mort_dif age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust

*interaciton terms 

use DHS, clear
gen inter = 0
replace inter = 1 if divorced == 1
replace inter = 1 if got_members == 1
replace inter = 1 if lost_members == 1
replace inter = 1 if got_job == 1
replace inter = 1 if lost_job == 1
replace inter = 1 if got_retired == 1
gen interhome = exp_home_price_w*inter	
gen intermort = exp_1y_mortgage_w*inter


drop if threepluskids_lag == 1

reg lost_owner interhome intermort exp_home_price_w exp_1y_mortgage_w age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust



*interaciton terms - gammel der laver tabel
*ALT
use DHS, clear
gen inter = 0
replace inter = 1 if divorced == 1
replace inter = 1 if got_members == 1
replace inter = 1 if lost_members == 1

replace inter = 1 if got_job == 1
replace inter = 1 if lost_job == 1
replace inter = 1 if got_retired == 1

gen interhome = exp_home_price_w*inter	
gen intermort = exp_1y_mortgage_w*inter


reg lost_owner interhome intermort exp_home_price_w exp_1y_mortgage_w age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableInattention.doc, replace dec(3) ctitle(OLS) keep(interhome intermort exp_home_price_w exp_1y_mortgage_w) addtext(Controls, YES, Data, 2004+)

keep if inter ==1 

reg lost_owner exp_home_price_lag exp_home_price_w exp_1y_mortgage_w male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag, robust
outreg2 using TableInattention.doc, append dec(3) ctitle(OLS) keep(exp_home_price_w exp_1y_mortgage_w ) addtext(Controls, YES, Data, 2004+)





*individuelle
use DHS, clear
gen inter1h = divorced*exp_home_price_lag
gen inter2h = got_members*exp_home_price_lag
gen inter3h = lost_members*exp_home_price_lag
gen inter4h = got_job*exp_home_price_lag
gen inter5h = lost_job*exp_home_price_lag
gen inter6h = got_retired*exp_home_price_lag
gen inter1m = divorced*exp_1y_mortgage_lag
gen inter2m = got_members*exp_1y_mortgage_lag
gen inter3m = lost_members*exp_1y_mortgage_lag
gen inter4m = got_job*exp_1y_mortgage_lag
gen inter5m = lost_job*exp_1y_mortgage_lag
gen inter6m = got_retired*exp_1y_mortgage_lag




reg lost_owner inter1h inter2h inter3h inter4h inter5h inter6h inter1m inter2m inter3m inter4m inter5m inter6m exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust



*kun dem der har plans
keep if move_indicator_lag == 100

gen inter_hp = rent_indicator_lag * exp_home_price_lag
gen inter_mr = rent_indicator_lag* exp_1y_mortgage_lag

reg lost_owner inter_hp inter_mr exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust



******************* Interaction terms  Friction *******************
**** NB, har ikke kigget på appendix tabel, skal genskabes med kode ovenfor
************************************** Baseline
use DHS, clear

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust

outreg2 using TableTransInteraction.doc, replace dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+, Friction, NA)
outreg2 using TableTransFriction2.doc, replace dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Interaction, NA)


************************************** Confidence
use DHS, clear

gen indicator = 0
replace indicator = 1 if confident_lag == 1
gen inter_price = exp_home_price_lag*indicator
gen inter_mortgage = exp_1y_mortgage_lag*indicator

reg lost_owner inter_price exp_home_price_lag inter_mortgage exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransInteraction.doc, append dec(3) ctitle(OLS) keep(inter_price exp_home_price_lag inter_mortgage exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Interaction, Confidence)

drop if kunde == 3

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransFriction2.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, Confidence=4)

************************************** income_cat
use DHS, clear

gen indicator = 0
replace indicator = 1 if income_cat_lag<5
gen inter_price = exp_home_price_lag*indicator
gen inter_mortgage = exp_1y_mortgage_lag*indicator

reg lost_owner inter_price exp_home_price_lag inter_mortgage exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransInteraction.doc, append dec(3) ctitle(OLS) keep(inter_price exp_home_price_lag inter_mortgage exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Interaction, income<5)


************************************** large_hh
use DHS, clear

gen indicator = 1
replace indicator = 0 if large_hh_lag == 1
gen inter_price = exp_home_price_lag*indicator
gen inter_mortgage = exp_1y_mortgage_lag*indicator

reg lost_owner inter_price exp_home_price_lag inter_mortgage exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransInteraction.doc, append dec(3) ctitle(OLS) keep(inter_price exp_home_price_lag inter_mortgage exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Interaction, large_hh)


use DHS, clear

drop if larger_hh_lag == 1

reg lost_owner exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransFriction2.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Friction, largerhh)

************************************** Retired/Age
use DHS, clear

gen indicator = 0
replace indicator = 1 if retired_lag ==0
gen inter_price = exp_home_price_lag*indicator
gen inter_mortgage = exp_1y_mortgage_lag*indicator

reg lost_owner inter_price exp_home_price_lag inter_mortgage exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransInteraction.doc, append dec(3) ctitle(OLS) keep(inter_price exp_home_price_lag inter_mortgage exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Interaction, retired)

use DHS, clear

gen indicator = 0
replace indicator = 1 if age>64
gen inter_price = exp_home_price_lag*indicator
gen inter_mortgage = exp_1y_mortgage_lag*indicator


reg lost_owner inter_price exp_home_price_lag inter_mortgage exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransInteraction.doc, append dec(3) ctitle(OLS) keep(inter_price exp_home_price_lag inter_mortgage exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Interaction, age>64)

************************************** Life events
use DHS, clear

gen indicator = 1
replace indicator = 0 if divorced ==1
replace indicator = 0 if got_members ==1
replace indicator = 0 if lost_members ==1
replace indicator = 0 if got_retired ==1
replace indicator = 0 if got_job ==1
replace indicator = 0 if lost_job ==1

gen inter_price = exp_home_price_lag*indicator
gen inter_mortgage = exp_1y_mortgage_lag*indicator

reg lost_owner inter_price exp_home_price_lag inter_mortgage exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using TableTransInteraction.doc, append dec(3) ctitle(OLS) keep(inter_price exp_home_price_lag inter_mortgage exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2004+, Interaction, events)




************************************** Sale prob
clear all
cls
cd "C:\Users\thom0\Desktop\Thomas Stuff\Polit KU\Speciale\\Programmering\Stata Output"
import excel "C:\Users\thom0\Desktop\Thomas Stuff\Polit KU\Speciale\\Programmering\Stata Input\DHS_move_full.xlsx", sheet("Sheet1") firstrow

drop if owner == 0
drop if year>2019

gen divorced_ = divorced
gen got_members_ = got_members
gen lost_members_ = lost_members
gen got_retired_ = got_retired
gen got_job_ = got_job
gen lost_job_ = lost_job


*uden expectations
*Den går fra 1995+ fordi 1995 har information om married nu og 1994 til divorced
*Derfor er reference category 1995, 1996 er den første du kan se
reg rent_indicator age male married retired student education i.year i.regio income divorced_ got_members_ lost_members_ got_retired_ got_job_ lost_job_, robust
outreg2 using TableDHS.doc, replace ctitle(OLS) keep(divorced_ got_members_ lost_members_ got_retired_ got_job_ lost_job_) addtext(Controls, YES, Data, 1994+, Life Events, Same Period)

replace divorced_ = divorced_lead
replace got_members_ = got_members_lead
replace lost_members_ = lost_members_lead
replace got_retired_ = got_retired_lead
replace got_job_ = got_job_lead
replace lost_job_ = lost_job_lead

*uden expectations - lead
reg rent_indicator age male married retired student education i.year i.regio income divorced_ got_members_ lost_members_ got_retired_ got_job_ lost_job_, robust
outreg2 using TableDHS.doc, append dec(3) ctitle(OLS) keep(divorced_ got_members_ lost_members_ got_retired_ got_job_ lost_job_) addtext(Controls, YES, Data, 1994+, Life Events, Next Period)

replace divorced_ = divorced
replace got_members_ = got_members
replace lost_members_ = lost_members
replace got_retired_ = got_retired
replace got_job_ = got_job
replace lost_job_ = lost_job

*med expectations
reg rent_indicator exp_home_price exp_1y_mortgage age male married retired student education i.year i.regio income divorced_ got_members_ lost_members_ got_retired_ got_job_ lost_job_, robust
outreg2 using TableDHS.doc, append dec(3) ctitle(OLS) keep(exp_home_price exp_1y_mortgage divorced_ got_members_ lost_members_ got_retired_ got_job_ lost_job_) addtext(Controls, YES, Data, 2004+, Life Events, Same Period)

replace divorced_ = divorced_lead
replace got_members_ = got_members_lead
replace lost_members_ = lost_members_lead
replace got_retired_ = got_retired_lead
replace got_job_ = got_job_lead
replace lost_job_ = lost_job_lead

*med expectations - lead
reg rent_indicator exp_home_price exp_1y_mortgage age male married retired student education i.year i.regio income divorced_ got_members_ lost_members_ got_retired_ got_job_ lost_job_, robust
outreg2 using TableDHS.doc, append dec(3) ctitle(OLS) keep(exp_home_price exp_1y_mortgage divorced_ got_members_ lost_members_ got_retired_ got_job_ lost_job_) addtext(Controls, YES, Data, 2004+, Life Events, Next Period)

drop if missing(exp_home_price)
drop if missing(exp_1y_mortgage)

replace divorced_ = divorced
replace got_members_ = got_members
replace lost_members_ = lost_members
replace got_retired_ = got_retired
replace got_job_ = got_job
replace lost_job_ = lost_job

*med expectations
reg rent_indicator age male married retired student education i.year i.regio income divorced_ got_members_ lost_members_ got_retired_ got_job_ lost_job_, robust
outreg2 using TableDHS.doc, append dec(3) ctitle(OLS) keep(exp_home_price exp_1y_mortgage divorced_ got_members_ lost_members_ got_retired_ got_job_ lost_job_) addtext(Controls, YES, Data, 2004+, Life Events, Same Period)

replace divorced_ = divorced_lead
replace got_members_ = got_members_lead
replace lost_members_ = lost_members_lead
replace got_retired_ = got_retired_lead
replace got_job_ = got_job_lead
replace lost_job_ = lost_job_lead

*med expectations - lead
reg rent_indicator age male married retired student education i.year i.regio income divorced_ got_members_ lost_members_ got_retired_ got_job_ lost_job_, robust
outreg2 using TableDHS.doc, append dec(3) ctitle(OLS) keep(exp_home_price exp_1y_mortgage divorced_ got_members_ lost_members_ got_retired_ got_job_ lost_job_) addtext(Controls, YES, Data, 2004+, Life Events, Next Period)





