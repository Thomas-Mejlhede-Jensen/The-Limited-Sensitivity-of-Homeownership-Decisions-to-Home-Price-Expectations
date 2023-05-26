
****************************************************************************************
*Dofile for masters thesis
*Thomas Mejlhede Jensen
*2023
*This dofile creates all tables that use data presented in my thesis
*Need to run the R Script first to perform data manipulation and create the DHS and SCE panels
*****************************************************************************************

* -------------------------------------- Load Data --------------------------------------
clear all
cls
cd "C:\Users\thom0\Desktop\Thomas Stuff\Polit KU\Speciale\\Programmering\Stata Output"
*load DHS panel
import excel "C:\Users\thom0\Desktop\Thomas Stuff\Polit KU\Speciale\\Programmering\Stata Input\DHS_panel.xlsx", sheet("Sheet1") firstrow

*save dataset for faster code
save DHS, replace

*load SCE panel
import excel "C:\Users\thom0\Desktop\Thomas Stuff\Polit KU\Speciale\\Programmering\Stata Input\SCE_panel.xlsx", sheet("Sheet1") firstrow clear

*save dataset for faster code
save SCE, replace
* -------------------------------------- Section 2 --------------------------------------

***************** Table 2 – Summary Statistics DHS
*load DHS data
use DHS, clear
*keep only relevant variablres
keep male age married current_members smart employed income mortgage_indicator fixed_indicator tenure transition divorced got_members lost_members got_job lost_job got_retired
*set order for table
order male age married current_members smart employed income mortgage_indicator fixed_indicator tenure transition divorced got_members lost_members got_job lost_job got_retired
*output
outreg2 using Table2, sum(detail) eqkeep(mean sd) word replace dec(3) nolabel addtext(N, `=_N', Years, 1995-2019)

*load unaltered dataset
use DHS, clear

*restrict to observations in expectatoin analysis (2005+)
drop if missing(exp_home_price_lag)
drop if missing(exp_1y_mortgage_lag)

*keep only relevant variablres
keep male age married current_members smart employed income mortgage_indicator fixed_indicator tenure transition divorced got_members lost_members got_job lost_job got_retired exp_home_price_lag exp_1y_mortgage_lag
*set order for table
order male age married current_members smart employed income mortgage_indicator fixed_indicator tenure transition divorced got_members lost_members got_job lost_job got_retired exp_home_price_lag exp_1y_mortgage_lag
*output
outreg2 using Table2, sum(detail) eqkeep(mean sd) word append dec(3) nolabel addtext(N, `=_N', Years, 2005-2019) 


***************** Table 3 – Summary Statistics SCE
*load SCE data
use SCE, clear

*keep only relevant variablres
keep male age married hh_members college employed income_cat recommend_fixed tenure tenure sale_prob buy_instead_of_rent_prob divorced got_members lost_members got_job lost_job got_retired exp_home_price_point_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w
*set order for table
order male age married hh_members college employed income_cat recommend_fixed tenure tenure sale_prob buy_instead_of_rent_prob divorced got_members lost_members got_job lost_job got_retired exp_home_price_point_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w
*output
outreg2 using Table3, sum(detail) eqkeep(mean sd) word replace dec(3) nolabel addtext(N, `=_N', Years, 2014-2020)


* -------------------------------------- Section 3 --------------------------------------

*load DHS data
use DHS, clear
*Make table coefficents interpretable as percentage point changes
replace transition = transition *100
*Save unaltered dataset
save DHS, replace

***************** Table 4 – Main Regression Results DHS
*Column 1
reg transition divorced got_members lost_members got_retired lost_job, robust
outreg2 using Table4.doc, replace dec(3) ctitle(OLS) keep(divorced got_members lost_members got_retired got_job lost_job) addtext(Controls, NO, Data, 1995+)

*Column 2, - main analysis of events, so also saved for a number of other tables
reg transition age male married_lag retired_lag student_lag education_lag i.regio_lag i.year income_lag divorced got_members lost_members got_retired lost_job, robust
*Table 4
outreg2 using Table4.doc, append dec(3) ctitle(OLS) keep(divorced got_members lost_members got_job lost_job got_retired ) addtext(Controls, YES, Data, 1995+)
*Table A1 - Corona Robustness
outreg2 using TableA1.doc, replace dec(3) ctitle(OLS) keep(divorced got_members lost_members got_job lost_job got_retired) addtext(Period, 1995-2019, ColumnNumber, 1)
*Table A3 - Logit
outreg2 using TableA3.doc, replace dec(3) ctitle(OLS) keep(divorced got_members lost_members got_job lost_job got_retired) addtext(Controls, YES, Data, 1995+, ColumnNumber, 1)

*Column 3, - main analysis of economic expecations, so also saved for a number of other tables
reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members lost_job got_retired, robust
*Table 4
outreg2 using Table4.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Controls, YES, Data, 2005+)
*Table 5 - Frictions
outreg2 using Table5.doc, replace dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+, Friction, NA)
*Table 8 - Inattention
outreg2 using Table8.doc, replace dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+)
*Table A1 - Corona Robustness
outreg2 using TableA1.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Period, 2005-2019, ColumnNumber, 3)
*Table A2 - Winsorizaotin Robustness
outreg2 using TableA2.doc, replace dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Outliers, Win. Original Cutoffs)
*Table A3 - Logit
outreg2 using TableA3.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Controls, YES, Data, 2005+, ColumnNumber, 3)

*Column 4
drop if missing(exp_home_price_lag)
drop if missing(exp_1y_mortgage_lag)
reg transition age male married_lag retired_lag student_lag education_lag i.regio_lag i.year income_lag divorced got_members lost_members lost_job got_retired, robust
outreg2 using Table4.doc, append dec(3) ctitle(OLS) keep(divorced got_members lost_members got_retired got_job lost_job) addtext(Controls, YES, Data, 2005+)


***************** Table 5 – Regressions Based on Frictions DHS 
* No kids
use DHS, clear

drop if kids_indicator_lag == 1

reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired lost_job, robust
outreg2 using Table5.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+, Friction, kids)

* Retired
use DHS, clear

drop if retired_lag ==0

reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired lost_job, robust
outreg2 using Table5.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+, Friction, retired)

* Confidence
use DHS, clear

keep if confident_lag == 1

reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired lost_job, robust
outreg2 using Table5.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+, Friction, Confidence)

* education
use DHS, clear

*smart = college degree or higher
keep if smart_lag==1

reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired lost_job, robust
outreg2 using Table5.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+, Friction, College)

* -------------------------------------- Section 4 --------------------------------------

*load dataset
use SCE, clear

*change region to numeric class
encode region_char, gen(region)

***************** Table 7 – Main Regression Results SCE
*include controls
reg sale_prob exp_home_price_point_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w age male race married retired student some_college college i.income_cat i.date i.region divorced got_members lost_members got_job lost_job got_retired, robust
outreg2 using Table7.doc, replace dec(3) ctitle(OLS) keep(exp_home_price_point_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w divorced got_members lost_members got_job lost_job got_retired) addtext(Future Homeownership, N/A)

*IV 2sls
ivregress 2sls sale_prob (exp_home_price_point_w = exp_home_price_mean_w) exp_1y_mortgage_rate_w percieved_mortgage_rate_w age male race married retired student some_college college i.income_cat i.date i.region divorced got_members lost_members got_retired got_job lost_job, robust
outreg2 using Table7.doc, append dec(3) ctitle(2SLS) keep(exp_home_price_point_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w divorced got_members lost_members got_job lost_job got_retired) addtext(Future Homeownership, N/A)

*ORIV
*create first temporary dataset (regress x1 on x2)
save tmp, replace
keep userid sale_prob exp_home_price_point_w exp_home_price_mean_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w  age male race married retired student years_residence some_college college income_cat date region  divorced got_members lost_members got_retired got_job lost_job buyer renter
rename exp_home_price_point_w home_price_instrument
rename exp_home_price_mean_w exp_home_price_point_w
gen control1 = 1
save tmp1, replace
*create second temporary dataset (regress x2 on x1)
use tmp, clear
keep userid sale_prob exp_home_price_point_w exp_home_price_mean_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w  age male race married retired student years_residence some_college college income_cat date region divorced got_members lost_members got_retired got_job lost_job buyer renter
rename exp_home_price_mean_w home_price_instrument
*dont exectue second rename, as already same name
*rename exp_home_price_point_w exp_home_price_point_w
gen control2 = 1
*full dataset and 2sls
append using tmp1
replace control1 = 0 if control1 == .
replace control2 = 0 if control2 == .
save oriv, replace
ivregress 2sls sale_prob (exp_home_price_point_w = home_price_instrument) exp_1y_mortgage_rate_w percieved_mortgage_rate_w age male race married retired student some_college college i.income_cat i.date i.region divorced got_members lost_members got_retired got_job lost_job, cluster(userid) nocons
outreg2 using Table7.doc, append dec(3) ctitle(ORIV) keep(exp_home_price_point_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w divorced got_members lost_members got_job lost_job got_retired) addtext(Future Homeownership, N/A)

*Reduced sample
drop if missing(buyer)
ivregress 2sls sale_prob (exp_home_price_point_w = home_price_instrument) exp_1y_mortgage_rate_w percieved_mortgage_rate_w age male race married retired student some_college college i.income_cat i.date i.region divorced got_members lost_members got_retired got_job lost_job, cluster(userid) nocons
outreg2 using Table7.doc, append dec(3) ctitle(ORIV) keep(exp_home_price_point_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w divorced got_members lost_members got_job lost_job got_retired) addtext(Future Homeownership, All)
*Only (Future) Owners
keep if buyer ==1
ivregress 2sls sale_prob (exp_home_price_point_w = home_price_instrument) exp_1y_mortgage_rate_w percieved_mortgage_rate_w age male race married retired student some_college college i.income_cat i.date i.region divorced got_members lost_members got_retired got_job lost_job, cluster(userid) nocons
outreg2 using Table7.doc, append dec(3) ctitle(ORIV) keep(exp_home_price_point_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w divorced got_members lost_members got_job lost_job got_retired) addtext(Future Homeownership, Owners)

*Only renters
use oriv, clear
keep if renter == 1
ivregress 2sls sale_prob (exp_home_price_point_w = home_price_instrument) exp_1y_mortgage_rate_w percieved_mortgage_rate_w age male race married retired student some_college college i.income_cat i.date i.region divorced got_members lost_members got_retired got_job lost_job, cluster(userid) nocons
outreg2 using Table7.doc, append dec(3)  ctitle(ORIV) keep(exp_home_price_point_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w divorced got_members lost_members got_job lost_job got_retired) addtext(Future Homeownership, Renters)

* -------------------------------------- Section 5 --------------------------------------
***************** Table 8 – Regressions Investigating Rational Inattention - DHS

*load dataset
use DHS, clear

*create interaction term
gen inter = 0
replace inter = 1 if divorced == 1
replace inter = 1 if got_members == 1
replace inter = 1 if lost_members == 1
replace inter = 1 if lost_job == 1
replace inter = 1 if got_retired == 1
gen interhome = exp_home_price_lag*inter
gen intermort = exp_1y_mortgage_lag*inter

reg transition exp_home_price_lag exp_1y_mortgage_lag interhome intermort age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired lost_job, robust
outreg2 using Table8.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag interhome intermort) addtext(Controls, YES, Data, 2005+)

*Cut dataset
keep if inter ==1

reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag, robust
outreg2 using Table8.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+)

* -------------------------------------- Appendix --------------------------------------
***************** Table A.1 – Main Regression Replication with COVID-19 Years
*save reduced dataset
use DHS, clear
save DHS, replace

*load corona dataset
import excel "C:\Users\thom0\Desktop\Thomas Stuff\Polit KU\Speciale\Programmering\Stata Input\DHS_move_corona.xlsx", sheet("Sheet1") firstrow clear

*Make table coefficents interpretable as percentage point changes
replace transition = transition *100

*Without Expectations
reg transition age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members lost_job got_retired, robust
*Column 2 in actual table
outreg2 using TableA1.doc, append dec(3) ctitle(OLS) keep(divorced got_members lost_members got_job lost_job got_retired) addtext(Period, 1995-2022, ColumnNumber, 2)

*With Expectations
reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members lost_job got_retired, robust
outreg2 using TableA1.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Period, 2005-2022, ColumnNumber, 4)

***************** Table A.2 – Main Regression Replication with Different Winsorization Cutoffs
*load reduced dataset
use DHS, clear

* Winsorizaotin +/- 99 percentile
replace exp_home_price_lag = exp_home_price_lag_99
replace exp_1y_mortgage_lag = exp_1y_mortgage_lag_99

reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members lost_job got_retired, robust
outreg2 using TableA2.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Outliers, Win 99 pct.)

*Trimming +/- 99 percentile
replace exp_home_price_lag = exp_home_price_lag_95
replace exp_1y_mortgage_lag = exp_1y_mortgage_lag_95

reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members lost_job got_retired, robust
outreg2 using TableA2.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members got_job lost_job got_retired) addtext(Outliers, Win 95 pct.)

***************** Table A.3 – Main Regression Replication with Logit Model
*load reduced dataset
use DHS, clear

logit transition age male married_lag retired_lag student_lag education_lag i.regio_lag i.year income_lag divorced got_members lost_members  lost_job got_retired, robust
outreg2 using TableA3.doc, append dec(3) ctitle(Logit) keep(divorced got_members lost_members lost_job got_retired ) addtext(Controls, YES, Data, 1995+, ColumnNumber, 2)
logit transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year education_lag i.regio_lag income_lag divorced got_members lost_members lost_job got_retired, robust
outreg2 using TableA3.doc, append dec(3) ctitle(Logit) keep(exp_home_price_lag exp_1y_mortgage_lag divorced got_members lost_members lost_job got_retired ) addtext(Controls, YES, Data, 2005+, ColumnNumber, 4)

***************** Table A.4 – Frictions Analysis Replication with Various Handling of Outliers
*load reduced dataset
use DHS, clear

* Winsorizaotin +/- 99 percentile
replace exp_home_price_lag = exp_home_price_lag_99
replace exp_1y_mortgage_lag = exp_1y_mortgage_lag_99
save DHS_w, replace

*Baseline
reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members lost_job got_retired, robust
outreg2 using TableA4.doc, replace dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+, Friction, NA)

*No kids
use DHS_w, clear

drop if kids_indicator_lag == 1

reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired lost_job, robust
outreg2 using TableA4.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+, Friction, kids)

*Retired
use DHS_w, clear

drop if retired_lag ==0

reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired lost_job, robust
outreg2 using TableA4.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+, Friction, retired)

*Confidence
use DHS_w, clear

keep if confident_lag == 1

reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_member lost_members got_retired lost_job, robust
outreg2 using TableA4.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+, Friction, Confidence)

*Education
use DHS_w, clear

*(smart_lag is education_lag==6|eudcation_lag==7)
keep if smart_lag==1

reg transition exp_home_price_lag exp_1y_mortgage_lag age male married_lag retired_lag i.year student_lag education_lag i.regio_lag income_lag divorced got_members lost_members got_retired lost_job, robust
outreg2 using TableA4.doc, append dec(3) ctitle(OLS) keep(exp_home_price_lag exp_1y_mortgage_lag) addtext(Controls, YES, Data, 2005+, Friction, College)

***************** Table A.5 – Results of First Stage Regression
*load dataset
use SCE, clear

*change region to numeric class
encode region_char, gen(region)

*2sls manually - for Appendix
*1st stage
reg exp_home_price_point_w exp_home_price_mean_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w age male race married retired student some_college college i.income_cat i.date i.region divorced got_members lost_members got_job lost_job got_retired, robust
outreg2 using TableA5.doc, replace dec(3) ctitle(1st Stage) keep(exp_home_price_mean_w exp_1y_mortgage_rate_w percieved_mortgage_rate_w divorced got_members lost_members got_job lost_job got_retired) addtext(Additional Controls, YES,  Winsorized, YES, Type of Expectation, Point, Dataset, Full)
predict exp_home_price_point_w_hat, xb
*2nd stage Std errors are NOT valid, - refer to ivregress above/in Table 7
reg sale_prob exp_home_price_point_w_hat exp_1y_mortgage_rate_w percieved_mortgage_rate_w age male race married retired student some_college college i.income_cat i.date i.region divorced got_members lost_members got_retired got_job lost_job
