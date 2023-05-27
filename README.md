# The Limited Sensitivity of Homeownership Decisions to Home Price Expectations

Master's thesis in economics by Thomas Mejlhede Jensen

Copenhagen University

Supervisor: Johannes Wohlfart

Submitted on 31/05/2023

## Script Overview:
**Script Data & Figures.R**

This (R script) uses the raw data from the DHS and the SCE to create the panel datafiles that are used for regressions.
Furthermore, it creates all figures presented in the thesis.

**Script Tables.do**

This (STATA dofile) uses the panels datafiles created with the R script to perform all regressions and create all tables presented in the thesis.

## Data Overview:
### DHS data:
Not available for public download. Can be requested for reserach purposes from https://www.centerdata.nl/

### SCE data:

**FRBNY-SCE-Housing-Survey-Public-Microdata-Complete.xlsx**

**frbny-sce-public-microdata-latest.rar (.xlsx file)**

**FRBNY-SCE-Public-Microdata-Complete-17-19.rar** (saved as .xlb due to large size, convert to .xlsx to replicate analysis)

**FRBNY-SCE-Public-Microdata-Complete-13-16.rar** (saved as .xlb due to large size, convert to .xlsx to replicate analysis)

Source: https://www.newyorkfed.org/microeconomics/sce#/


### Additional data:

**Data Figures.rar**

Sources:

CBS https://opendata.cbs.nl/#/CBS/en/dataset/83906ENG/table

Zillow https://www.zillow.com/research/data/

FED https://fred.stlouisfed.org/series/MORTGAGE30US

Contains:

**Existing_own_homes__purchase_prices__price_indices_2015_100__12032023_204925.xlsx**

**Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.xlsx**

**MORTGAGE30US.xlsx**

## Thesis Informatoin:

### Abstract:

This thesis investigates the relationship between homeowners' expectations regarding future home prices and their decision to transition from owning to renting. Using data from the DNB Household Survey, I do not find a significant relationship between these variables. This result is surprising, as it contradicts standard economic theory. Therefore, I investigate if the insignificant relationship can be explained by households' family composition, financial confidence, or their level of education, but find no such evidence. Next, I reduce the impact of measurement error by conducting instrumental variable regressions using data from the New York Fed Survey of Consumer Expectations, but this also cannot explain the insignificance. Finally, I explore whether rational inattention can account for the insignificant relationship and find some evidence supporting this hypothesis. These findings leave room for future research studying rational inattention on the housing market and for using large-scale surveys combined with administrative data.

***JEL Classification:*** A22, C36, C83, D14, D84.

***Keywords:*** Homeownership Choice, Expectations, Household Survey.
