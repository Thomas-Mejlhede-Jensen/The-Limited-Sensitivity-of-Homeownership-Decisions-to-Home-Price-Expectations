
#Script for Masters thesis
#Thomas Mejlhede Jensen
#2023
#This Script performs all data manipulation of the DHS and SCE rawdata
#It also creates all figures for the thesis 
#And calculates all numbers included in the main text not already presented in figures/tables
#DHS data is confidential and thus not uploaded to github. Request the data from Centerdata if you wish to replicate the DHS analysis
#SCE data is available publically at FED's website

####################################################################
###############################
# Setup 
###############################
####################################################################

#Clear all in enviorment
rm(list = ls())

#packages
library("haven") #For importing .dta files
library("tidyverse") #For everything
library("openxlsx") #For reading excel files
library("readxl") #for reading excel files
library("writexl") #For writing excel files
library("lubridate") #For working with dates
library("devEMF") #For exporting plots as  emf files
library("scales") #For plotting axis text with .1 accuracy

#set directories
dataDHS <- "C:/Users/thom0/Desktop/Thomas Stuff/Polit KU/Speciale/Programmering/Data DHS"
dataSCE <- "C:/Users/thom0/Desktop/Thomas Stuff/Polit KU/Speciale/Programmering/Data SCE"
dataDescriptive <- "C:/Users/thom0/Desktop/Thomas Stuff/Polit KU/Speciale/Programmering/Data Descriptive"
output <- "C:/Users/thom0/Desktop/Thomas Stuff/Polit KU/Speciale/Programmering/Stata Input"
outputPlots <- "C:/Users/thom0/Desktop/Thomas Stuff/Polit KU/Speciale/Programmering/Output Plots"

####################################################################
###############################
# Data Manipulation -  DNB Household Survey (DHS) Panel
###############################
####################################################################

#wd
setwd(dataDHS)

#Load DHS data, by module, and create year variable
#Done individually rather than with a function, due to varying file and variable names in DHS rawdata


#HHI - Houshold Information
HHI1994 <- read_dta("hhi1994en_2.0.dta") %>%
  mutate(year = 1994) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, onderw, bezigbel, aantalhh, aantalki, regio) %>%
  rename(bezighei = bezigbel, oplmet2=onderw)
HHI1995 <- read_dta("hhi1995en_2.0.dta") %>%   
  mutate(year = 1995) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, scholing, bezigbel, aantalhh, aantalki, regio) %>%
  rename(bezighei = bezigbel, oplmet2=scholing)
HHI1996 <- read_dta("hhi1996en_2.0.dta") %>%   
  mutate(year = 1996) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, scholing, bezigbel, aantalhh, aantalki, regio) %>%
  rename(bezighei = bezigbel, oplmet2=scholing)
HHI1997 <- read_dta("hhi1997en_2.0.dta") %>%   
  mutate(year = 1997) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, scholing, bezigbel, aantalhh, aantalki, regio) %>%
  rename(bezighei = bezigbel, oplmet2=scholing)
HHI1998 <- read_dta("hhi1998en_2.0.dta") %>%   
  mutate(year = 1998) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, scholing, bezigbel, aantalhh, aantalki, regio) %>%
  rename(bezighei = bezigbel, oplmet2=scholing)
HHI1999 <- read_dta("hhi1999en_2.0.dta") %>%   
  mutate(year = 1999) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, scholing, bezigbel, aantalhh, aantalki, regio) %>%
  rename(bezighei = bezigbel, oplmet2=scholing)
HHI2000 <- read_dta("hhi2000en_2.0.dta") %>%   
  mutate(year = 2000) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, scholing, bezigbel, aantalhh, aantalki, regio) %>%
  rename(bezighei = bezigbel, oplmet2=scholing)
#uniqie to 2001, scholing has 40 observations which respond "99" (which shouldnt be possible, since it isnt listed in options)
#I will assume this is no response, and categorize it as such
HHI2001 <- read_dta("hhi2001en_2.0.dta") %>%   
  mutate(year = 2001) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, scholing, bezigbel, aantalhh, aantalki, regio) %>%
  rename(bezighei = bezigbel, oplmet2=scholing) %>%
  mutate(oplmet2 = ifelse(oplmet2==99,NA,oplmet2))
HHI2002 <- read_dta("hhi2002en_2.0.dta") %>%   
  mutate(year = 2002) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezigbel, aantalhh, aantalki, regio, woning) %>%
  rename(bezighei = bezigbel)
HHI2003 <- read_dta("hhi2003en_2.0.dta") %>%   
    mutate(year = 2003) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezigbel, aantalhh, aantalki, regio, woning) %>%
  rename(bezighei = bezigbel)
HHI2004 <- read_dta("hhi2004en_2.0.dta") %>%   
  mutate(year = 2004) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, belbezig, aantalhh, aantalki, regio, woning) %>%
  rename(bezighei = belbezig)
HHI2005 <- read_dta("hhi2005en_2.0.dta") %>%   
  mutate(year = 2005) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, belbezig, aantalhh, aantalki, regio, woning) %>%
  rename(bezighei = belbezig)
HHI2006 <- read_dta("hhi2006en_2.0.dta") %>%  
  mutate(year = 2006) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, belbezig, aantalhh, aantalki, regio, woning) %>%
  rename(bezighei = belbezig)
HHI2007 <- read_dta("hhi2007en_2.0.dta") %>% 
  mutate(year = 2007) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2008 <- read_dta("hhi2008en_2.0.dta") %>% 
  mutate(year = 2008) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2009 <- read_dta("hhi2009en_2.0.dta") %>% 
  mutate(year = 2009) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2010 <- read_dta("hhi2010en_2.0.dta") %>% 
  mutate(year = 2010) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2011 <- read_dta("hhi2011en_2.0.dta") %>%
  mutate(year = 2011) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2012 <- read_dta("hhi2012en_2.0.dta") %>% 
  mutate(year = 2012) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2013 <- read_dta("hhi2013en_2.0.dta") %>%
  mutate(year = 2013) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2014 <- read_dta("hhi2014en_2.0.dta") %>% 
  mutate(year = 2014) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2015 <- read_dta("hhi2015en_2.1.dta") %>% 
  mutate(year = 2015) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2016 <- read_dta("hhi2016en_1.0.dta") %>%
  mutate(year = 2016) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2017 <- read_dta("hhi2017en_1.0.dta") %>% 
  mutate(year = 2017) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2018 <- read_dta("hhi2018en_1.0.dta") %>% 
  mutate(year = 2018) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2019 <- read_dta("hhi2019en_1.0.dta") %>%   
  mutate(year = 2019) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2020 <- read_dta("hhi2020en_1.0.dta") %>%   
  mutate(year = 2020) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2021 <- read_dta("hhi2021en_1.0.dta") %>%
  mutate(year = 2021) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)
HHI2022 <- read_dta("hhi2022en_1.0.dta") %>%
  mutate(year = 2022) %>%
  select(nohhold, nomem, year, gebjaar, geslacht, positie, oplmet, bezighei, aantalhh, aantalki, regio, woning)

#WRK - Work & Pension Data
WRK1994 <- read_dta("wrk1994en_2.0.dta") %>%
  mutate(year = 1994) %>%
  select(nohhold, nomem, year, burgst)
WRK1995 <- read_dta("wrk1995en_2.0.dta") %>%  
  mutate(year = 1995) %>%
  select(nohhold, nomem, year, burgst)
WRK1996 <- read_dta("wrk1996en_2.0.dta") %>%  
  mutate(year = 1996) %>%
  select(nohhold, nomem, year, burgst)
WRK1997 <- read_dta("wrk1997en_2.0.dta") %>% 
  mutate(year = 1997) %>%
  select(nohhold, nomem, year, burgst)
WRK1998 <- read_dta("wrk1998en_2.0.dta") %>%  
  mutate(year = 1998) %>%
  select(nohhold, nomem, year, burgst)
WRK1999 <- read_dta("wrk1999en_2.0.dta") %>%  
  mutate(year = 1999) %>%
  select(nohhold, nomem, year, burgst)
WRK2000 <- read_dta("wrk2000en_2.0.dta") %>%  
  mutate(year = 2000) %>%
  select(nohhold, nomem, year, burgst)
WRK2001 <- read_dta("wrk2001en_2.0.dta") %>%  
  mutate(year = 2001) %>%
  select(nohhold, nomem, year, burgst)
WRK2002 <- read_dta("wrk2002en_2.0.dta") %>%  
  mutate(year = 2002) %>%
  select(nohhold, nomem, year, burgst)
WRK2003 <- read_dta("wrk2003en_2.0.dta") %>%  
  mutate(year = 2003) %>%
  select(nohhold, nomem, year, burgst)
WRK2004 <- read_dta("wrk2004en_2.0.dta") %>%  
  mutate(year = 2004) %>%
  select(nohhold, nomem, year, burgst)
WRK2005 <- read_dta("wrk2005en_2.0.dta") %>%  
  mutate(year = 2005) %>%
  select(nohhold, nomem, year, burgst)
WRK2006 <- read_dta("wrk2006en_2.0.dta") %>%  
  mutate(year = 2006) %>%
  select(nohhold, nomem, year, burgst)
WRK2007 <- read_dta("wrk2007en_2.0.dta") %>% 
  mutate(year = 2007) %>%
  select(nohhold, nomem, year, burgst)
WRK2008 <- read_dta("wrk2008en_2.0.dta") %>%
  mutate(year = 2008) %>%
  select(nohhold, nomem, year, burgst)
WRK2009 <- read_dta("wrk2009en_2.0.dta") %>% 
  mutate(year = 2009) %>%
  select(nohhold, nomem, year, burgst)
WRK2010 <- read_dta("wrk2010en_2.0.dta") %>% 
  mutate(year = 2010) %>%
  select(nohhold, nomem, year, burgst)
WRK2011 <- read_dta("wrk2011en_2.0.dta") %>% 
  mutate(year = 2011) %>%
  select(nohhold, nomem, year, burgst)
WRK2012 <- read_dta("wrk2012en_2.0.dta") %>% 
  mutate(year = 2012) %>%
  select(nohhold, nomem, year, burgst)
WRK2013 <- read_dta("wrk2013en_2.0.dta") %>% 
  mutate(year = 2013) %>%
  select(nohhold, nomem, year, burgst)
WRK2014 <- read_dta("wrk2014en_2.0.dta") %>%
  mutate(year = 2014) %>%
  select(nohhold, nomem, year, burgst)
WRK2015 <- read_dta("wrk2015en_1.0.dta") %>% 
  mutate(year = 2015) %>%
  select(nohhold, nomem, year, burgst)
WRK2016 <- read_dta("wrk2016en_1.0.dta") %>%  
  mutate(year = 2016)  %>%
  select(nohhold, nomem, year, burgst)
WRK2017 <- read_dta("wrk2017en_1.0.dta") %>%  
  mutate(year = 2017)  %>%
  select(nohhold, nomem, year, burgst)
WRK2018 <- read_dta("wrk2018en_1.0.dta") %>%  
  mutate(year = 2018) %>%
  select(nohhold, nomem, year, burgst)
WRK2019 <- read_dta("wrk2019en_1.2.dta") %>%  
  mutate(year = 2019) %>%
  select(nohhold, nomem, year, burgst)
WRK2020 <- read_dta("wrk2020en_1.0.dta") %>%   
  mutate(year = 2020)  %>%
  select(nohhold, nomem, year, burgst)
WRK2021 <- read_dta("wrk2021en_1.0.dta") %>%
  mutate(year = 2021) %>%
  select(nohhold, nomem, year, burgst)
WRK2022 <- read_dta("wrk2022en_1.0.dta") %>%
  mutate(year = 2022) %>%
  select(nohhold, nomem, year, burgst)


#HSE - Accommodation Data
#The questions about economic expectations, wod44p, wod44q, wod52a,and wod52b
#are missing from 2003 and earlier, making it impossible to conduct expectations based regresions
HSE1994 <- read_dta("hse1994en_2.0.dta") %>%
  mutate(year = 1994) %>%
  select(nohhold, nomem, year, wo1, wo34, wo41)
HSE1995 <- read_dta("hse1995en_2.0.dta") %>%
  mutate(year = 1995) %>%
  select(nohhold, nomem, year, wo1, wo34, wo41, hy71)
HSE1996 <- read_dta("hse1996en_2.0.dta") %>%
  mutate(year = 1996) %>%
  select(nohhold, nomem, year, wo1, wo34, wo41, hy71)
HSE1997 <- read_dta("hse1997en_2.0.dta") %>%
  mutate(year = 1997) %>%
  select(nohhold, nomem, year, wo1, wo34, wo41, hy71)
HSE1998 <- read_dta("hse1998en_2.0.dta") %>%
  mutate(year = 1998) %>%
  select(nohhold, nomem, year, wo1, wo34, wo41, hy71)
HSE1999 <- read_dta("hse1999en_2.0.dta") %>%
  mutate(year = 1999) %>%
  select(nohhold, nomem, year, wo1, wo34, wo41, hy71)
HSE2000 <- read_dta("hse2000en_2.0.dta") %>%
  mutate(year = 2000) %>%
  select(nohhold, nomem, year, wo1, wo34, wo41, hy71)
HSE2001 <- read_dta("hse2001en_2.0.dta") %>%
  mutate(year = 2001)%>%
  select(nohhold, nomem, year, wo1, wo34, wo41, hy71)
HSE2002 <- read_dta("hse2002en_2.0.dta") %>% 
  mutate(year = 2002) %>%
  select(nohhold, nomem, year, wo1, wo34, wo41, hy71)
HSE2003 <- read_dta("hse2003en_3.0.dta") %>%  
  mutate(year = 2003) %>%
  select(nohhold, nomem, year, wo1, wo34, wo41, hy71)
HSE2004 <- read_dta("hse2004en_2.0.dta") %>%  
  mutate(year = 2004) %>%
  select(nohhold, nomem, year, wo1, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2005 <- read_dta("hse2005en_2.0.dta") %>%  
  mutate(year = 2005) %>%
  select(nohhold, nomem, year, wo1, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2006 <- read_dta("hse2006en_2.0.dta") %>%  
  mutate(year = 2006) %>%
  select(nohhold, nomem, year, wo1, wod44p, wod44q, wo34, wo41, wod52a, wod52b, HY71, wo48) %>%
  #HY71 with capital letters in 2006
  rename(hy71 = HY71)
HSE2007 <- read_dta("hse2007en_2.0.dta") %>%  
  mutate(year = 2007) %>%
  select(nohhold, nomem, year, wo1, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2008 <- read_dta("hse2008en_3.0.dta") %>%  
  mutate(year = 2008) %>%
  select(nohhold, nomem, year, wo1, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2009 <- read_dta("hse2009en_2.1.dta") %>% 
  mutate(year = 2009) %>%
  select(nohhold, nomem, year, wo1, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2010 <- read_dta("hse2010en_2.0.dta") %>%  
  mutate(year = 2010) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2011 <- read_dta("hse2011en_2.0.dta") %>%  
  mutate(year = 2011) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2012 <- read_dta("hse2012en_2.0.dta") %>%  
  mutate(year = 2012) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2013 <- read_dta("hse2013en_2.0.dta") %>%  
  mutate(year = 2013) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2014 <- read_dta("hse2014en_2.0.dta") %>%  
  mutate(year = 2014) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2015 <- read_dta("hse2015en_1.0.dta") %>%  
  mutate(year = 2015) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2016 <- read_dta("hse2016en_1.0.dta") %>%   
  mutate(year = 2016) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2017 <- read_dta("hse2017en_1.0.dta") %>%   
  mutate(year = 2017) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2018 <- read_dta("hse2018en_1.0.dta") %>%  
  mutate(year = 2018) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2019 <- read_dta("hse2019en_1.0.dta") %>% 
  mutate(year = 2019) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2020 <- read_dta("hse2020en_2.0.dta") %>%   
  mutate(year = 2020) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2021 <- read_dta("hse2021en_1.0.dta") %>%  
  mutate(year = 2021) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)
HSE2022 <- read_dta("hse2022en_1.0.dta") %>%  
  mutate(year = 2022) %>%
  select(nohhold, nomem, year, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, hy71, wo48)

#PSY -  Psychological concepts data
PSY1994 <- read_dta("psy1994en_2.0.dta") %>% 
  mutate(year = 1994) %>%
  select(nohhold, nomem, inkhh, year)
PSY1995 <- read_dta("psy1995en_2.0.dta") %>%  
  mutate(year = 1995) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY1996 <- read_dta("psy1996en_2.0.dta") %>% 
  mutate(year = 1996) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY1997 <- read_dta("psy1997en_2.0.dta") %>%  
  mutate(year = 1997) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY1998 <- read_dta("psy1998en_2.0.dta") %>%  
  mutate(year = 1998) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY1999 <- read_dta("psy1999en_2.0.dta") %>% 
  mutate(year = 1999) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2000 <- read_dta("psy2000en_2.0.dta") %>%  
  mutate(year = 2000) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2001 <- read_dta("psy2001en_2.0.dta") %>%   
  mutate(year = 2001) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2002 <- read_dta("psy2002en_2.0.dta") %>%  
  mutate(year = 2002) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2003 <- read_dta("psy2003en_2.0.dta") %>% 
  mutate(year = 2003) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2004 <- read_dta("psy2004en_2.0.dta") %>% 
  mutate(year = 2004) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2005 <- read_dta("psy2005en_2.0.dta") %>% 
  mutate(year = 2005) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2006 <- read_dta("psy2006en_2.0.dta") %>% 
  mutate(year = 2006) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2007 <- read_dta("psy2007en_2.0.dta") %>% 
  mutate(year = 2007) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2008 <- read_dta("psy2008en_2.0.dta") %>% 
  mutate(year = 2008) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2009 <- read_dta("psy2009en_2.0.dta") %>% 
  mutate(year = 2009) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2010 <- read_dta("psy2010en_2.0.dta") %>%  
  mutate(year = 2010) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2011 <- read_dta("psy2011en_2.0.dta") %>% 
  mutate(year = 2011) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2012 <- read_dta("psy2012en_3.0.dta") %>% 
  mutate(year = 2012) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2013 <- read_dta("psy2013en_2.0.dta") %>% 
  mutate(year = 2013) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2014 <- read_dta("psy2014en_2.0.dta") %>% 
  mutate(year = 2014) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2015 <- read_dta("psy2015en_1.0.dta") %>%  
  mutate(year = 2015) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2016 <- read_dta("psy2016en_1.0.dta") %>% 
  mutate(year = 2016) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2017 <- read_dta("psy2017en_1.0.dta") %>% 
  mutate(year = 2017) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2018 <- read_dta("psy2018en_1.0.dta") %>%  
  mutate(year = 2018) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2019 <- read_dta("psy2019en_1.0.dta") %>%
  mutate(year = 2019) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2020 <- read_dta("psy2020en_1.1.dta") %>%
  mutate(year = 2020) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2021 <- read_dta("psy2021en_1.0.dta") %>%   
  mutate(year = 2021) %>%
  select(nohhold, nomem, inkhh, year, kunde)
PSY2022 <- read_dta("psy2022en_1.0.dta") %>%   
  mutate(year = 2022) %>%
  select(nohhold, nomem, inkhh, year, kunde)

#AGI -  Income data
AGI1994 <- read_dta("agi1994en_2.0.dta") %>% 
  mutate(year = 1994) %>%
  select(nohhold, nomem, year, btot)
AGI1995 <- read_dta("agi1995en_2.0.dta") %>%  
  mutate(year = 1995) %>%
  select(nohhold, nomem, year, btot)
AGI1996 <- read_dta("agi1996en_2.0.dta") %>% 
  mutate(year = 1996) %>%
  select(nohhold, nomem, year, btot)
AGI1997 <- read_dta("agi1997en_2.0.dta") %>%  
  mutate(year = 1997) %>%
  select(nohhold, nomem, year, btot)
AGI1998 <- read_dta("agi1998en_3.0.dta") %>%  
  mutate(year = 1998) %>%
  select(nohhold, nomem, year, btot)
AGI1999 <- read_dta("agi1999en_3.0.dta") %>% 
  mutate(year = 1999) %>%
  select(nohhold, nomem, year, btot)
AGI2000 <- read_dta("agi2000en_2.0.dta") %>%  
  mutate(year = 2000) %>%
  select(nohhold, nomem, year, btot)
AGI2001 <- read_dta("agi2001en_2.0.dta") %>%   
  mutate(year = 2001) %>%
  select(nohhold, nomem, year, btot)
AGI2002 <- read_dta("agi2002en_2.0.dta") %>%  
  mutate(year = 2002) %>%
  select(nohhold, nomem, year, btot)
AGI2003 <- read_dta("agi2003en_2.0.dta") %>% 
  mutate(year = 2003) %>%
  select(nohhold, nomem, year, btot)
AGI2004 <- read_dta("agi2004en_3.0.dta") %>% 
  mutate(year = 2004) %>%
  select(nohhold, nomem, year, btot)
AGI2005 <- read_dta("agi2005en_3.0.dta") %>% 
  mutate(year = 2005) %>%
  select(nohhold, nomem, year, btot)
AGI2006 <- read_dta("agi2006en_3.0.dta") %>% 
  mutate(year = 2006) %>%
  select(nohhold, nomem, year, btot)
AGI2007 <- read_dta("agi2007en_2.0.dta") %>% 
  mutate(year = 2007) %>%
  select(nohhold, nomem, year, btot)
AGI2008 <- read_dta("agi2008en_3.0.dta") %>% 
  mutate(year = 2008) %>%
  select(nohhold, nomem, year, btot)
AGI2009 <- read_dta("agi2009en_3.0.dta") %>% 
  mutate(year = 2009) %>%
  select(nohhold, nomem, year, btot)
AGI2010 <- read_dta("agi2010en_2.0.dta") %>%  
  mutate(year = 2010) %>%
  select(nohhold, nomem, year, btot)
AGI2011 <- read_dta("agi2011en_2.0.dta") %>% 
  mutate(year = 2011) %>%
  select(nohhold, nomem, year, btot)
AGI2012 <- read_dta("agi2012en_2.0.dta") %>% 
  mutate(year = 2012) %>%
  select(nohhold, nomem, year, btot)
AGI2013 <- read_dta("agi2013en_2.0.dta") %>% 
  mutate(year = 2013) %>%
  select(nohhold, nomem, year, btot)
AGI2014 <- read_dta("agi2014en_2.0.dta") %>% 
  mutate(year = 2014) %>%
  select(nohhold, nomem, year, btot)
AGI2015 <- read_dta("agi2015en_1.0.dta") %>%  
  mutate(year = 2015) %>%
  select(nohhold, nomem, year, btot)
AGI2016 <- read_dta("agi2016en_1.0.dta") %>% 
  mutate(year = 2016) %>%
  select(nohhold, nomem, year, btot)
AGI2017 <- read_dta("agi2017en_1.0.dta") %>% 
  mutate(year = 2017) %>%
  select(nohhold, nomem, year, btot)
AGI2018 <- read_dta("agi2018en_1.0.dta") %>%  
  mutate(year = 2018) %>%
  select(nohhold, nomem, year, btot)
AGI2019 <- read_dta("agi2019en_1.0.dta") %>%
  mutate(year = 2019) %>%
  select(nohhold, nomem, year, btot)
AGI2020 <- read_dta("agi2020en_1.0.dta") %>%
  mutate(year = 2020) %>%
  select(nohhold, nomem, year, btot)
AGI2021 <- read_dta("agi2021en_1.0.dta") %>%   
  mutate(year = 2021) %>%
  select(nohhold, nomem, year, btot)
AGI2022 <- read_dta("agi2022en_1.0.dta") %>%   
  mutate(year = 2022) %>%
  select(nohhold, nomem, year, btot)

#bind modules together 
HHI <- HHI1994 %>%
  bind_rows(HHI1995, HHI1996, HHI1997, HHI1998, HHI1999, HHI2000, HHI2001, HHI2002, HHI2003, HHI2004,
            HHI2005, HHI2006, HHI2007, HHI2008, HHI2009, HHI2010, HHI2011, HHI2012, HHI2013, HHI2014,
            HHI2015, HHI2016, HHI2017, HHI2018, HHI2019, HHI2020, HHI2021, HHI2022)
WRK <- WRK1994 %>%
  bind_rows(WRK1995, WRK1996, WRK1997, WRK1998, WRK1999, WRK2000, WRK2001, WRK2002, WRK2003, WRK2004,
            WRK2005, WRK2006, WRK2007, WRK2008, WRK2009, WRK2010, WRK2011, WRK2012, WRK2013, WRK2014,
            WRK2015, WRK2016, WRK2017, WRK2018, WRK2019, WRK2020, WRK2021, WRK2022)
HSE <- HSE1994 %>%
  bind_rows(HSE1995, HSE1996, HSE1997, HSE1998, HSE1999, HSE2000, HSE2001, HSE2002, HSE2003, HSE2004,
            HSE2005, HSE2006, HSE2007, HSE2008, HSE2009, HSE2010, HSE2011, HSE2012, HSE2013, HSE2014,
            HSE2015, HSE2016, HSE2017, HSE2018, HSE2019, HSE2020, HSE2021, HSE2022)
PSY <- PSY1994 %>%
  bind_rows(PSY1995, PSY1996, PSY1997, PSY1998, PSY1999, PSY2000, PSY2001, PSY2002, PSY2003, PSY2004,
            PSY2005, PSY2006, PSY2007, PSY2008, PSY2009, PSY2010, PSY2011, PSY2012, PSY2013, PSY2014,
            PSY2015, PSY2016, PSY2017, PSY2018, PSY2019, PSY2020, PSY2021, PSY2022)
AGI <- AGI1994 %>%
  bind_rows(AGI1995, AGI1996, AGI1997, AGI1998, AGI1999, AGI2000, AGI2001, AGI2002, AGI2003, AGI2004,
            AGI2005, AGI2006, AGI2007, AGI2008, AGI2009, AGI2010, AGI2011, AGI2012, AGI2013, AGI2014,
            AGI2015, AGI2016, AGI2017, AGI2018, AGI2019, AGI2020, AGI2021, AGI2022)

#Join Moduels
#Done individually due to df sizes
HSE_HHI <- HSE %>%
  left_join(HHI, by = c("nohhold", "nomem", "year"))
HSE_HHI_WRK <- HSE_HHI %>%
  left_join(WRK, by = c("nohhold", "nomem", "year"))
HSE_HHI_WRK_PSY <- HSE_HHI_WRK %>%
  left_join(PSY, by = c("nohhold", "nomem", "year"))
DHS_merge <- HSE_HHI_WRK_PSY %>%
  left_join(AGI, by = c("nohhold", "nomem", "year"))

#housekeeping
rm(HHI1994, HHI1995, HHI1996, HHI1997, HHI1998, HHI1999, HHI2000, HHI2001, HHI2002, HHI2003, HHI2004,
   HHI2005, HHI2006, HHI2007, HHI2008, HHI2009, HHI2010, HHI2011, HHI2012, HHI2013, HHI2014, HHI2015,
   HHI2016, HHI2017, HHI2018, HHI2019, HHI2020, HHI2021, HHI2022,
   WRK1994, WRK1995, WRK1996, WRK1997, WRK1998, WRK1999, WRK2000, WRK2001, WRK2002, WRK2003, WRK2004,
   WRK2005, WRK2006, WRK2007, WRK2008, WRK2009, WRK2010, WRK2011, WRK2012, WRK2013, WRK2014, WRK2015,
   WRK2016, WRK2017, WRK2018, WRK2019, WRK2020, WRK2021, WRK2022,
   HSE1994, HSE1995, HSE1996, HSE1997, HSE1998, HSE1999, HSE2000, HSE2001, HSE2002, HSE2003, HSE2004,
   HSE2005, HSE2006, HSE2007, HSE2008, HSE2009, HSE2010, HSE2011, HSE2012, HSE2013, HSE2014, HSE2015,
   HSE2016, HSE2017, HSE2018, HSE2019, HSE2020, HSE2021, HSE2022,
   AGI1994, AGI1995, AGI1996, AGI1997, AGI1998, AGI1999, AGI2000, AGI2001, AGI2002, AGI2003, AGI2004,
   AGI2005, AGI2006, AGI2007, AGI2008, AGI2009, AGI2010, AGI2011, AGI2012, AGI2013, AGI2014, AGI2015,
   AGI2016, AGI2017, AGI2018, AGI2019, AGI2020, AGI2021, AGI2022,
   PSY1994, PSY1995, PSY1996, PSY1997, PSY1998, PSY1999, PSY2000, PSY2001, PSY2002, PSY2003, PSY2004,
   PSY2005, PSY2006, PSY2007, PSY2008, PSY2009, PSY2010, PSY2011, PSY2012, PSY2013, PSY2014, PSY2015,
   PSY2016, PSY2017, PSY2018, PSY2019, PSY2020, PSY2021, PSY2022,
   HSE, HHI, WRK, PSY, AGI, HSE_HHI, HSE_HHI_WRK, HSE_HHI_WRK_PSY)

#Find first year
#Before finding first year, drop observations that are missing information for variables which need to be lagged
#these are: employment, household members, married, and income information
#These would not be included in regression anyway, but need to be dropped now to find first year with full info
DHS_drop <- DHS_merge %>%
  select(-inkhh) %>%
  #(-9) represents NA
  mutate(bezighei = ifelse(bezighei==-9,NA,bezighei)) %>%
  rename(inkhh = btot) %>%
  drop_na(inkhh, bezighei, aantalhh, burgst)

#create indicator counting first participation year (needed for later variables)
DHS_first <- DHS_drop %>%
  arrange(nohhold, year) %>%
  filter(duplicated(nohhold) == FALSE) %>%
  mutate(first_year = year) %>%
  select(nohhold, first_year)

#define lists of employed and unemployed
employed <- c(1,2,3)
unemployed <- c(4,5,9,10,11)

#create new variables
#always set (-9) to NA, as it corressponds to "I don't know"
DHS_enrich <- DHS_drop %>%
  #merge first year onto dataset
  arrange(nohhold, year) %>%
  #arrange by userid so that lag() works
  left_join(DHS_first, by = "nohhold") %>%
  
  ### Dependet Variable (transition) and homeowner indicator
  #create owner
  mutate(owner = ifelse(wo1==3,1,ifelse(wo1==-9,NA,0))) %>%
  #transition from ownership to renting
  mutate(owner_lag = ifelse(year==first_year,NA,lag(owner)),
         transition = ifelse(owner<owner_lag,1,0)) %>%
  ### Economic Expectatoins
  #Create exp_home_price
  mutate(wod44q = ifelse(wod44q==-9,NA,wod44q),
         wod44p = ifelse(wod44p==-9,NA,wod44p),
         exp_home_price_1 =  ifelse(wod44p==2,0,
                                    ifelse(is.na(wod44q),NA,
                                           ifelse(wod44p==1,wod44q,
                                                  ifelse(wod44p==3,wod44q*-1,NA))))) %>%
  mutate(wod205 = ifelse(wod205==-9,NA,wod205),
         wod206 = ifelse(wod206==-9,NA,wod206),
         exp_home_price_2 =    ifelse(wod205==2,0,
                                      ifelse(is.na(wod206),NA,
                                             ifelse(wod205==1,wod206,
                                                    ifelse(wod205==3,wod206*-1,NA))))) %>%
  mutate(exp_home_price = ifelse(is.na(exp_home_price_1), exp_home_price_2,exp_home_price_1),
         exp_home_price = exp_home_price/2) %>% #divide by 2 because 2 year ahead
  #create exp_1y_mortgage
  mutate(wod52a = ifelse(wod52a==-9,NA,wod52a),
         wod52b = ifelse(wod52b==-9,NA,wod52b),
         exp_2y_mortgage = ifelse(wod52a==2,0,
                                  ifelse(is.na(wod52b), NA,
                                         ifelse(wod52a==1,wod52b,
                                                ifelse(wod52a==3,wod52b*-1,NA))))) %>%
  mutate(exp_1y_mortgage = exp_2y_mortgage/2) %>% #divide by 2 because 2 year ahead
  
  ### Life Events
  #create Members
  mutate(current_members = aantalhh,
         #If first period, assume no change
         lagged_members = ifelse(year==first_year,NA,lag(aantalhh)),
         got_members = ifelse(current_members>lagged_members,1,0),
         lost_members = ifelse(current_members<lagged_members,1,0)) %>%
  #create married, and divorced
  mutate(married = ifelse(burgst<3,1,0),
         lagged_married = ifelse(year==first_year,NA,lag(married)),
         divorced = ifelse(is.na(lagged_married)|is.na(married),NA,
                           ifelse(lagged_married==1 & married==0,1,0))) %>%
  #employment
  #drop observations missing first year job employment information
  mutate(job = bezighei,
         #If first period, assume no change (MIGHT CHANGE LATER, NOT OBVIOUS)
         lagged_job = ifelse(year==first_year,NA,lag(job)),
         got_job = ifelse(year==first_year,NA,
                          ifelse(job %in% employed & lagged_job %in% unemployed,1,0)),
         lost_job = ifelse(year==first_year,NA,
                           ifelse(job %in% unemployed & lagged_job %in% employed,1,0))) %>%
  #got retired
  mutate(got_retired = ifelse(job==8 & lagged_job!= 8,1,0)) %>%
  #current job
  mutate(employed = ifelse(job %in% employed,1,0)) %>%
  
  ### Control Variables and other variables for summary statistics
  #Education 
  #changes not only name, but also answers form 2001-2002
  mutate(education = ifelse(year>2001,oplmet,
                            ifelse(oplmet2== 10,1,
                                   ifelse(oplmet2== 1,2,
                                          ifelse(oplmet2==2|oplmet2==3,3,
                                                 ifelse(oplmet2==4,4,
                                                        ifelse(oplmet2==5|oplmet2==6|oplmet2==11,5,
                                                               ifelse(oplmet2==7|oplmet2==8,6,
                                                                      ifelse(oplmet2==9,7,
                                                                ifelse(oplmet2==12,9,NA)))))))))) %>%
  #retired/student
  mutate(retired = ifelse(bezighei==8,1,0),
         student = ifelse(bezighei==6,1,0)) %>%
  #age and gender
  mutate(age = year-gebjaar) %>%
  mutate(male = ifelse(geslacht == 1,1,0)) %>%
  #mortgage indicators
  mutate(fixed_indicator = hy71*(-1)+2) %>%
  mutate(mortgage_indicator = ifelse(wo48==1,1,NA),
         mortgage_indicator = ifelse(wo48==2,0,mortgage_indicator),
         mortgage_indicator = ifelse(!is.na(fixed_indicator),1,mortgage_indicator)) %>%
  #Keep only fixed if has a mortgage
  mutate(fixed_indicator = ifelse(is.na(mortgage_indicator),NA,
                                  ifelse(mortgage_indicator==0,0,fixed_indicator))) %>%
  #Children
  mutate(kids_indicator = ifelse(aantalki>0,1,0)) %>%
  #any life event
  mutate(life_event = ifelse(divorced==1|got_members==1|lost_members==1|
                               lost_job==1|got_retired==1,1,0)) %>%
  #income
  #remove missing
  mutate(inkhh = ifelse(inkhh==-9,NA,inkhh)) %>%
  #log income
  mutate(income = ifelse(inkhh==0,NA,log(inkhh)))



#Lagged Variables
DHS_enrich_time <- DHS_enrich %>%
  arrange(nohhold, year) %>%
  mutate(income_lag = ifelse(year==first_year,NA,lag(income))) %>%
         #lagged expectations
  mutate(exp_home_price_lag_no_w = ifelse(year==first_year,NA,lag(exp_home_price)),
         exp_1y_mortgage_lag_no_w = ifelse(year==first_year,NA,lag(exp_1y_mortgage))) %>%
  #lagged controls
  mutate(married_lag = ifelse(year==first_year,NA,lag(married)),
         retired_lag = ifelse(year==first_year,NA,lag(retired)),
         student_lag = ifelse(year==first_year,NA,lag(student)),
         education_lag = ifelse(year==first_year,NA,lag(education)),
         regio_lag = ifelse(year==first_year,NA,lag(regio))) %>%
  #lagged frictions
  #No Kids
  mutate(kids_indicator_lag = ifelse(year==first_year,NA,lag(kids_indicator))) %>%
  #Confidence
  mutate(confident = ifelse(kunde>2,1,0),
         confident_lag =ifelse(year==first_year,NA,lag(confident))) %>%
  #College Degree (smart)
  mutate(smart = ifelse(education==7|education==6,1,0),
         smart_lag = ifelse(education_lag==7|education_lag==6,1,0))

#Limit dataset
DHS_limit <- DHS_enrich_time %>%
  #Drop NA's so that only respondent used for regresions are present in summary statistics datasets
  drop_na(transition, age, male, married_lag, retired_lag, student_lag, education_lag, regio_lag, income_lag,
          divorced, got_members, lost_members, got_retired, got_job, lost_members) %>%
  # keep only t-1 owner
  filter(owner_lag==1) %>%
  #Keep 1 member from each household, prioritizing newer observations
  group_by(nohhold) %>%
  arrange(desc(year)) %>%
  filter(nomem == first(nomem)) %>%
  #calculate tenure for remaning member
  mutate(tenure = n()) %>%
  ungroup() %>%
  #remove unused variables
  select(-nomem, -wo1, -wo34, -wo41, -hy71, -wod44p, -wod44q, -wod52a, -wod52b, -wo48, -wod205, -wod206, -gebjaar, 
         -geslacht, -positie, -oplmet2, -bezighei, -aantalhh, -aantalki, -regio, -oplmet, -woning, -burgst, -kunde,
         -inkhh, exp_home_price_1, exp_home_price_2, exp_2y_mortgage)

#WINSORIZE
DHS_enrich_w <- DHS_limit %>%
  mutate(exp_home_price_w = ifelse(exp_home_price<(-15),-15, ifelse(exp_home_price>15,15,exp_home_price)),
         exp_1y_mortgage_w = ifelse(exp_1y_mortgage<(-10),-10, ifelse(exp_1y_mortgage>10,10,exp_1y_mortgage)),
         exp_home_price_lag = ifelse(exp_home_price_lag_no_w<(-15),-15, ifelse(exp_home_price_lag_no_w>15,15,exp_home_price_lag_no_w)),
         exp_1y_mortgage_lag = ifelse(exp_1y_mortgage_lag_no_w<(-10),-10, ifelse(exp_1y_mortgage_lag_no_w>10,10,exp_1y_mortgage_lag_no_w))) %>%
         #alternative cutoffs for replication - 99th/95th percentile
  mutate(q1 = quantile(exp_home_price, 0.01, na.rm = T),
         q99 = quantile(exp_home_price, 0.99,  na.rm = T),
         q5 = quantile(exp_home_price, 0.05, na.rm = T),
         q95 = quantile(exp_home_price, 0.95,  na.rm = T),
         exp_home_price_w_99 = ifelse(exp_home_price > q99, q99, ifelse(exp_home_price < q1, q1, exp_home_price)),
         exp_home_price_w_95 = ifelse(exp_home_price > q95, q95, ifelse(exp_home_price < q5, q5, exp_home_price)),
         q1_lag = quantile(exp_home_price_lag_no_w, 0.01, na.rm = T),
         q99_lag = quantile(exp_home_price_lag_no_w, 0.99,  na.rm = T),
         q5_lag = quantile(exp_home_price_lag_no_w, 0.05, na.rm = T),
         q95_lag = quantile(exp_home_price_lag_no_w, 0.95,  na.rm = T),
         exp_home_price_lag_99 = ifelse(exp_home_price_lag_no_w > q99_lag, q99_lag, ifelse(exp_home_price_lag_no_w < q1_lag, q1_lag, exp_home_price_lag_no_w)),
         exp_home_price_lag_95 = ifelse(exp_home_price_lag_no_w > q95_lag, q95_lag, ifelse(exp_home_price_lag_no_w < q5_lag, q5_lag, exp_home_price_lag_no_w)),
         q1_mr = quantile(exp_1y_mortgage, 0.01,  na.rm = T),
         q99_mr = quantile(exp_1y_mortgage, 0.99,  na.rm = T),
         q5_mr = quantile(exp_1y_mortgage, 0.05,  na.rm = T),
         q95_mr = quantile(exp_1y_mortgage, 0.95,  na.rm = T),
         exp_1y_mortgage_w_99 = ifelse(exp_1y_mortgage > q99_mr, q99_mr, ifelse(exp_1y_mortgage < q1_mr, q1_mr, exp_1y_mortgage)),
         exp_1y_mortgage_w_95 = ifelse(exp_1y_mortgage > q95_mr, q95_mr, ifelse(exp_1y_mortgage < q5_mr, q5_mr, exp_1y_mortgage)),
         q1_mr_lag = quantile(exp_1y_mortgage_lag_no_w, 0.01,  na.rm = T),
         q99_mr_lag = quantile(exp_1y_mortgage_lag_no_w, 0.99,  na.rm = T),
         q5_mr_lag = quantile(exp_1y_mortgage_lag_no_w, 0.05,  na.rm = T),
         q95_mr_lag = quantile(exp_1y_mortgage_lag_no_w, 0.95,  na.rm = T),
         exp_1y_mortgage_lag_99 = ifelse(exp_1y_mortgage_lag_no_w > q99_mr_lag, q99_mr_lag, ifelse(exp_1y_mortgage_lag_no_w < q1_mr_lag, q1_mr_lag, exp_1y_mortgage_lag_no_w)),
         exp_1y_mortgage_lag_95 = ifelse(exp_1y_mortgage_lag_no_w > q95_mr_lag, q95_mr_lag, ifelse(exp_1y_mortgage_lag_no_w < q5_mr_lag, q5_mr_lag, exp_1y_mortgage_lag_no_w))) %>%
  select(-q1, -q1_mr, -q99, -q99_mr, -q1_lag, -q99_lag, -q1_mr_lag, -q99_mr_lag,
         -q5, -q5_mr, -q95, -q95_mr, -q5_lag, -q95_lag, -q5_mr_lag, -q95_mr_lag)
#99th percentiles are currently:
#hp = -5/+5,mr = -2.5/+1
#95th percentiles are currently:
#hp = -2.5/+2.5, mr= -1.4/+0.25
#found by dropping select and investigating data

#Panel with corona
DHS_corona <- DHS_enrich_w

#Panel WITHOUT corona
DHS_panel <- DHS_enrich_w %>%
  filter(year<2020)

#Output both
setwd(output)
write_xlsx(DHS_panel, "DHS_panel.xlsx")
write_xlsx(DHS_corona, "DHS_move_corona.xlsx")

#remove temporary dataframes
rm(DHS_drop, DHS_enrich, DHS_enrich_time, DHS_enrich_w, DHS_first, DHS_limit, DHS_merge)


####################################################################
###############################
# Data Manipulation -  New York Fed Survey of Consumer Expectations (SCE)
###############################
####################################################################

#set wd
setwd(dataSCE)

#load data fra FED Source: https://www.newyorkfed.org/microeconomics/sce#/
SCE20 = read_excel("frbny-sce-public-microdata-latest.xlsx", skip=1, guess_max = 10000)
SCE13to16 = read_excel("FRBNY-SCE-Public-Microdata-Complete-13-16.xlsx", skip=1, guess_max = 10000)
SCE17to19 = read_excel("FRBNY-SCE-Public-Microdata-Complete-17-19.xlsx", skip=1, guess_max = 10000)
Housing2014  = read_excel("FRBNY-SCE-Housing-Survey-Public-Microdata-Complete.xlsx", 3)
Housing2015  = read_excel("FRBNY-SCE-Housing-Survey-Public-Microdata-Complete.xlsx", 5)
Housing2016  = read_excel("FRBNY-SCE-Housing-Survey-Public-Microdata-Complete.xlsx", 7)
Housing2017  = read_excel("FRBNY-SCE-Housing-Survey-Public-Microdata-Complete.xlsx", 9)
Housing2018  = read_excel("FRBNY-SCE-Housing-Survey-Public-Microdata-Complete.xlsx", 11)
Housing2019  = read_excel("FRBNY-SCE-Housing-Survey-Public-Microdata-Complete.xlsx", 13)
Housing2020  = read_excel("FRBNY-SCE-Housing-Survey-Public-Microdata-Complete.xlsx", 15)
Housing2021  = read_excel("FRBNY-SCE-Housing-Survey-Public-Microdata-Complete.xlsx", 17)

#Create total main survey df
SCE_total <- SCE13to16 %>%
  bind_rows(SCE17to19, SCE20)

#Join individual housing module years with general survey, to link same-date expectations and trim data
#2014
SCE2014 <- SCE_total %>%
  filter(date==201402)
data2014 <- SCE2014 %>%  
  inner_join(Housing2014, by = "userid") %>%
  select(userid, date, HQH3b_1, HQ6c_1, "_EDU_CAT", D6, Q3, Q8v2part2, Q9_mean, Q10_7, Q10_8, tenure,
         Q3, Q8v2part2, Q9_mean, Q31v2part2, C1_mean, HQ5b_1, HQ5c_1, HQ5c_2, Q25v2part2, DQ38, "_REGION_CAT",
         C1_var, Q23v2part2, Q24_mean, HQ5dd, HQH5)

#2015
SCE2015 <- SCE_total %>%
  filter(date==201502)
data2015 <- SCE2015 %>%  
  inner_join(Housing2015, by = "userid") %>%
  select(userid, date, HQH3b_1, HQ6c_1, "_EDU_CAT", D6, Q3, Q8v2part2, Q9_mean, Q10_7, Q10_8, tenure,
         Q3, Q8v2part2, Q9_mean, Q31v2part2, C1_mean, HQ5b_1, HQ5c_1, HQ5c_2, Q25v2part2, DQ38, "_REGION_CAT",
         C1_var, Q23v2part2, Q24_mean, HQ5dd, HQH5)

#2016
SCE2016 <- SCE_total %>%
  filter(date==201602)
data2016 <- SCE2016 %>%  
  inner_join(Housing2016, by = "userid") %>%
  select(userid, date, HQH3b_1, HQ6c_1, "_EDU_CAT", D6, Q3, Q8v2part2, Q9_mean, Q10_7, Q10_8, tenure,
         Q3, Q8v2part2, Q9_mean, Q31v2part2, C1_mean, HQ5b_1, HQ5c_1, HQ5c_2, Q25v2part2, DQ38, "_REGION_CAT",
         C1_var, Q23v2part2, Q24_mean, HQ5dd, HQH5)

#2017
SCE2017 <- SCE_total %>%
  filter(date==201702)
data2017 <- SCE2017 %>%  
  inner_join(Housing2017, by = "userid") %>%
  select(userid, date, HQH3b_1, HQ6c_1, "_EDU_CAT", D6, Q3, Q8v2part2, Q9_mean, Q10_7, Q10_8, tenure,
         Q3, Q8v2part2, Q9_mean, Q31v2part2, C1_mean, HQ5b_1, HQ5c_1, HQ5c_2, Q25v2part2, DQ38, "_REGION_CAT",
         C1_var, Q23v2part2, Q24_mean, HQ5dd, HQH5)

#2018
SCE2018 <- SCE_total %>%
  filter(date==201802)
data2018 <- SCE2018 %>%  
  inner_join(Housing2018, by = "userid") %>%
  select(userid, date, HQH3b_1, HQ6c_1, "_EDU_CAT", D6, Q3, Q8v2part2, Q9_mean, Q10_7, Q10_8, tenure,
         Q3, Q8v2part2, Q9_mean, Q31v2part2, C1_mean, HQ5b_1, HQ5c_1, HQ5c_2, Q25v2part2, DQ38, "_REGION_CAT",
         C1_var, Q23v2part2, Q24_mean, HQ5dd, HQH5)

#2019
SCE2019 <- SCE_total %>%
  filter(date==201902)
data2019 <- SCE2019 %>%  
  inner_join(Housing2019, by = "userid") %>%
  select(userid, date, HQH3b_1, HQ6c_1, "_EDU_CAT", D6, Q3, Q8v2part2, Q9_mean, Q10_7, Q10_8, tenure,
         Q3, Q8v2part2, Q9_mean, Q31v2part2, C1_mean, HQ5b_1, HQ5c_1, HQ5c_2, Q25v2part2, DQ38, "_REGION_CAT",
         C1_var, Q23v2part2, Q24_mean, HQ5dd, HQH5)

#2020
SCE2020 <- SCE_total %>%
  filter(date==202002)
data2020 <- SCE2020 %>%  
  inner_join(Housing2020, by = "userid") %>%
  select(userid, date, HQH3b_1, HQ6c_1, "_EDU_CAT", D6, Q3, Q8v2part2, Q9_mean, Q10_7, Q10_8, tenure,
         Q3, Q8v2part2, Q9_mean, Q31v2part2, C1_mean, HQ5b_1, HQ5c_1, HQ5c_2, Q25v2part2, DQ38, "_REGION_CAT",
         C1_var, Q23v2part2, Q24_mean, HQ5dd, HQH5)

#2021
#NOTE: The variables in the housing model change names from 2020 to 2021 (lower case).
#See the rename funciton for the changes.
#Note that HQ5dd, QH5 (question on recommend fixed or floating rate mortgage is missing, but only used for descriptives)
SCE2021 <- SCE_total %>%
  filter(date==202102)
data2021 <- SCE2021 %>%  
  inner_join(Housing2021, by = "userid") %>%
  select(userid, date, hqh3bnew, hq6c_1, "_EDU_CAT", D6, Q3, Q8v2part2, Q9_mean, Q10_7, Q10_8, tenure,
         Q3, Q8v2part2, Q9_mean, Q31v2part2, C1_mean, hq5b_1, hq5cr1, hq5cr2, Q25v2part2, DQ38, "_REGION_CAT",
         C1_var, Q23v2part2, Q24_mean, hqh5) %>%
  rename(HQH3b_1=hqh3bnew, HQ6c_1=hq6c_1, HQ5b_1=hq5b_1, HQ5c_1=hq5cr1, HQ5c_2=hq5cr2, HQH5=hqh5)


#bind years together
SCE_bind <- data2014 %>%
  bind_rows(data2015, data2016, data2017, data2018, data2019, data2020, data2021) %>%
  #retired and student indicators
  mutate(retired = ifelse(Q10_7==1,1,0),
         student = ifelse(Q10_8==1,1,0)) %>%
  select(-Q10_7, -Q10_8) %>%
  #rename variables for more readable code
  rename(sale_prob = HQH3b_1, buy_instead_of_rent_prob = HQ6c_1, move_prob = Q3, exp_inflation_point=Q8v2part2,
         exp_inflation_mean = Q9_mean, exp_home_price_point = Q31v2part2, exp_home_price_mean = C1_mean,
         education = "_EDU_CAT", income_cat = D6, exp_income_point=Q25v2part2,
         percieved_mortgage_rate = HQ5b_1, married_at_date = DQ38, region_char = "_REGION_CAT",
         exp_3y_mortgage_rate = HQ5c_2, exp_1y_mortgage_rate = HQ5c_1, home_price_var=C1_var,
         exp_earnings_point = Q23v2part2, exp_earnings_mean = Q24_mean, mortgage_indicator = HQH5)


#Individual charecteristics are only measured on first survey date, so needs seperate data manipulation
#Save all userids in housing surveys
userid_list = as.list(SCE_bind$userid)

#find original no. of members, original employment status, and general charecteristics only measured 
#in first survey.
#in Appendix C, refered to as "past elicited varialbes"
SCE_first <- SCE_total %>%
  #save only reposndetns in analysis
  filter(userid %in% userid_list) %>%
  #find first observations for each
  arrange(userid, date) %>%
  filter(duplicated(userid) == FALSE) %>%
  #original job
  mutate(original_job = ifelse(Q10_1==1,1, ifelse(Q10_2==1,2, ifelse(Q10_3==1,3, ifelse(Q10_4==1,4,
                                                                                        ifelse(Q10_5==1,5, ifelse(Q10_6==1,6, ifelse(Q10_7==1,7, ifelse(Q10_8==1,8,
                                                                                                                                                        ifelse(Q10_9==1,9, ifelse(Q10_10==1,10,NA))))))))))) %>%
  #originla members
  mutate(original_members = Q45new_1+Q45new_2++Q45new_3+Q45new_4+Q45new_5+Q45new_6+Q45new_7+Q45new_8) %>%
  #keep needed viarables
  select(userid, Q32, Q33, Q34, Q38, Q41, Q47, original_job, original_members) %>%
  #rename for more readable code
  rename(age = Q32, gender = Q33, race = Q34, married_original = Q38, years_residence = Q41,
         income_cat_original = Q47) 

#Find date of housing survey participation
SCE_year <- SCE_bind %>%
  rename(max_date = date) %>%
  select(userid, max_date)
#Remove dates from SCE_total that are after housing survey date
SCE_total_max_date <- SCE_total %>%
  left_join(SCE_year, by="userid") %>%
  filter(date <= max_date)

#Find latest change in members
SCE_current_members <- SCE_total_max_date %>%
  mutate(current_members = D2new_1+D2new_2+D2new_3+D2new_4+D2new_5+D2new_6+D2new_7+D2new_8) %>%
  #remove periods without change in members
  select(date, userid, current_members) %>% 
  drop_na() %>%
  #Save latest number of members only
  arrange(userid, -date) %>%
  filter(duplicated(userid) == FALSE) %>%
  select(-date)

#Find latest employment for respondent
SCE_current_jobs <- SCE_total_max_date %>%
  filter(date == max_date) %>%
  mutate(current_job = ifelse(Q10_1==1,1, ifelse(Q10_2==1,2, ifelse(Q10_3==1,3, ifelse(Q10_4==1,4,
                                                                                       ifelse(Q10_5==1,5, ifelse(Q10_6==1,6, ifelse(Q10_7==1,7, ifelse(Q10_8==1,8,
                                                                                                                                                       ifelse(Q10_9==1,9, ifelse(Q10_10==1,10,NA))))))))))) %>%
  select(userid, current_job) 

#define lists of employed and unemployed
employed <- c(1)
unemployed <- c(3,4,5,9)

#Bind new variables on SCE_first, and find change in members from original and change in employment
SCE_new_variables <- SCE_first %>%
  #members
  left_join(SCE_current_members, by="userid") %>%
  mutate(members_change = current_members-original_members,
         #if no change found, set change to 0
         members_change = ifelse(is.na(members_change),0,members_change)) %>%
  #employment
  left_join(SCE_current_jobs, by="userid") %>%
  mutate(lost_job = ifelse(is.na(current_job)|is.na(original_job),NA,
                           ifelse(current_job %in% unemployed &
                                    original_job %in% employed,1,0)),
         got_job = ifelse(is.na(current_job)|is.na(original_job),NA,
                          ifelse(current_job %in% employed &
                                   original_job %in% unemployed,1,0)),
         job_change_indicator = ifelse(got_job==1|lost_job==1,1,0)) %>%
  mutate(employed = ifelse(!is.na(current_job) & current_job %in% employed,1,0)) %>%
  mutate(got_retired = ifelse(current_job==7 & original_job != 7,1,0)) %>%
  #no. of housheold members, add 1, because questions ask about memebers other than respondent
  mutate(hh_members = 1+ifelse(is.na(current_members),original_members,current_members))

#Join new variables on main panel
SCE_temp = SCE_bind %>%
  left_join(SCE_new_variables, by="userid")

#Final adjustments to dataset
SCE_final <- SCE_temp %>%
  #Create dummy variables
  mutate(some_college = ifelse(education=="Some College",1,0), college= ifelse(education=="College",1,0),
         #if newest  is empty, insert first income_cat
         income_cat = ifelse(is.na(income_cat),income_cat_original,income_cat),
         #members change
         lost_members= ifelse(is.na(original_members),NA,
                              ifelse(members_change>0,1,0)),
         got_members= ifelse(is.na(original_members),NA,
                             ifelse(members_change<0,1,0)),
         #if newest marriage indicator is empty, insert first marriage indicator
         married=ifelse(is.na(married_at_date),married_original,married_at_date),
         #change married == 2 (means not married) to == 0 for summary statistics
         married=ifelse(married==2,0,married),
         #Divorced
         married_at_date=ifelse(is.na(married_at_date),married_original,married_at_date),
         divorced = ifelse(married_at_date==2 & married_original==1,1,0)) %>%
  #recommend fixed if 
  mutate(recommend_fixed = ifelse(HQ5dd==1|HQ5dd==2,1,0)) %>%
  select(-HQ5dd) %>%
  #Variable that defines "future owners" and "future renters"
  mutate(buyer = ifelse(buy_instead_of_rent_prob>50,1,0),
         renter = ifelse(buy_instead_of_rent_prob<=50,1,0)) %>%
  #Drop observations missing values for variables included in regresions later
  drop_na(sale_prob, exp_home_price_point, income_cat, income_cat_original, college, race,
          age, years_residence, exp_1y_mortgage_rate, exp_home_price_mean,
          percieved_mortgage_rate) %>%
  #change some indicator variables to 0-1 instead of 1-2 (for descriptive statistics, irrelevant for regresions)
  mutate(male = ifelse(gender==1,0,ifelse(gender==2,1,NA))) %>%
  #remove unsued variables
  select(-education, -income_cat_original, -married_at_date, -married_original, -members_change, -gender) %>%
  mutate(rent_prob = ifelse(buy_instead_of_rent_prob<50,sale_prob,0)) #if either variable is NA, rent_prob is also NA

#WINSORIZE
SCE_final_w <- SCE_final %>%
  mutate(exp_home_price_point_w = ifelse(exp_home_price_point<(-15),-15, ifelse(exp_home_price_point>15,15,exp_home_price_point)),
         exp_home_price_mean_w = ifelse(exp_home_price_mean<(-15),-15, ifelse(exp_home_price_mean>15,15,exp_home_price_mean)),
         percieved_mortgage_rate_w = ifelse(percieved_mortgage_rate<0,0, ifelse(percieved_mortgage_rate>10,10,percieved_mortgage_rate)),
         exp_1y_mortgage_rate_w = ifelse(exp_1y_mortgage_rate<0,0, ifelse(exp_1y_mortgage_rate>10,10,exp_1y_mortgage_rate)))


#Remove COVID-19
SCE_panel <- SCE_final_w %>%
  filter(date<202102) 
#dataset including corona
SCE_corona <- SCE_final_w

#housekeeping
rm(data2014, data2015, data2016, data2017, data2018, data2019, data2020, data2021,
   SCE2014, SCE2015, SCE2016, SCE2017, SCE2018, SCE2019, SCE2020, SCE2021,
   Housing2014, Housing2015, Housing2016, Housing2017, Housing2018, Housing2019, 
   Housing2020, Housing2021, SCE20, SCE13to16, SCE17to19, SCE_bind, SCE_current_jobs, 
   SCE_current_members, SCE_total, SCE_year, SCE_new_variables, SCE_first, 
   SCE_total_max_date, SCE_temp, employed, unemployed, userid_list, SCE_final, SCE_final_w)

#change wd
setwd(output)

#SAVE / LOAD data
write_xlsx(SCE_panel, "SCE_panel.xlsx")
write_xlsx(SCE_corona, "SCE_corona.xlsx")


####################################################################
###############################
# Figures in main text
###############################
####################################################################

#Read SCE and DHS panels
#OPTINAL
#Clear all in environment (so code below can be run individually) 
#(have to set wd's and laod packages afterwards)
# rm(list = ls())

setwd(output)
DHS <- read_excel("DHS_move_corona.xlsx")
SCE <- read_excel("SCE_corona.xlsx")

DHS_temp <- DHS %>%
  select(transition, exp_home_price_lag, exp_1y_mortgage_lag)

SCE_temp <- SCE %>%
  select(sale_prob, exp_home_price_point_w, exp_1y_mortgage_rate_w, percieved_mortgage_rate_w)

##############################  Set theme for plots
# theme_master_2() depends on theme_master()
theme_master_2=function(base_size = 10.5, base_family = "",base_line_size = base_size / 22,base_rect_size = base_size / 22)
{
  theme_master()+theme(plot.title = element_text(family="Times New Roman",size=10.5),
                       legend.text = element_text(margin=margin(l=-3)))
}
theme_master <- function(base_size = 10.5, base_family = "",base_line_size = base_size / 22,base_rect_size = base_size / 22) 
{
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # no background and no grid
      panel.border     = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line( size=.1, color="black",linetype="dashed" ),
      panel.grid.major.x = element_blank(),
      # show axes
      axis.line      = element_line(colour = "black", size = .1),
      axis.title.x = element_text(size=base_size, family="Times New Roman", angle = 0, margin = margin(t = 5, r = 0, b = 0, l = 0)),
      axis.text.y = element_text(size=base_size, family="Times New Roman", color="black", angle=0, margin = margin(t = 0, r = 2, b = 0, l = 0)),
      axis.text.x = element_text(size=base_size, family="Times New Roman", color="black", angle=0, margin = margin(t = 2, r = 0, b = 0, l = 0)),
      plot.title.position = "plot", 
      plot.tag.position = c(1,0.99),
      plot.tag = element_text(size=9,family="Times New Roman",hjust=1),
      axis.ticks =  element_line( size=.1, color="black"),
      legend.key       = element_blank(),
      legend.title=element_text(size=base_size, family="Times New Roman"), 
      legend.text=element_text(size=base_size, family="Times New Roman"),
      # simple, black and white strips
      strip.background = element_rect(fill = "white", colour = "black", size = rel(2)),
      # NB: size is 1 but clipped, it looks like the 0.5 of the axes
      text=element_text(size=9, family="Times New Roman"),
      complete = TRUE
    )
}

# ------------------------------- Figure 1 - Expected Home Price Growth & Homeownership Transitions
#transition / exp home price
NL_HP_Move <- DHS %>%
  group_by(year) %>%
  summarise(HP = mean(exp_home_price_lag, na.rm=T), share_move = mean(transition)*100)

#Figure - note that lagged expectations are plotted
Figure_1=ggplot(data=NL_HP_Move) + 
  geom_bar(aes(x=year, y=HP, group=1, fill="Prior Expected\n1Y Home Price Growth"), stat = "identity",
           position = position_stack(reverse = TRUE),width=0.5, color = "black", size=0.2) +
  geom_line(aes(x=year, y=share_move, group=1, color="Share of Respondents\nwho Transition to Renting"), size=1.1) +
  theme_master_2() + 
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.4),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.7), #angeled x-axis text
  )  +
  ylab("Percent")+
  guides(color = guide_legend(order = 2, keywidth = unit(0.6, 'cm')), fill = guide_legend(order = 1)) + #For at f? legend med "Ansatte.." f?rst.
  scale_fill_manual(values=c("#2171b5"))+  scale_color_manual(values="#000000")+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1),
                     expand = expansion(mult = c(0, .0)), 
                     limits = c(-2, 2.5), 
                     breaks = seq(-2, 2.5, by=0.5)) +
  scale_x_continuous(limits=c(2004.5, 2022.5), breaks=seq(2004, 2022, by=1), 
                     expand=expansion(add=c(0.01, 0.01)))
Figure_1

# ------------------------------- Figure 2 – Expected Home Price Growth, Point Prediction & Mean of Bin Prediction
#1st stage binned scaterplot
#calculate bins
SCE_bin <- SCE %>%
  mutate(bin = ntile(exp_home_price_mean_w, 20)) %>%
  group_by(bin) %>%
  summarise(avg_x = mean(exp_home_price_mean_w), avg_y = mean(exp_home_price_point_w))

#plot
Figure_2 <-  ggplot(data = SCE_bin, aes(x = avg_x, y = avg_y)) + 
  geom_abline(slope = lm(SCE_bin$avg_y ~ SCE_bin$avg_x)$coefficients[2], 
              intercept = lm(SCE_bin$avg_y ~ SCE_bin$avg_x)$coefficients[1], 
              color = "#2171b5", size=1.1) +
  geom_point(color="#000000", shape=19) + 
  theme_master_2() + 
  theme(legend.position = "none")  +
  ylab("Expected Home Price Growth (Point Prediction)")+
  xlab("Expected Home Price Growth (Mean of Bin Prediction)")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     expand = expansion(mult = c(0, .0)), 
                     limits = c(-5, 15), 
                     breaks = seq(-5, 15, by=5)) +
  scale_x_continuous(limits=c(-6, 15), breaks=seq(-5, 15, by=5), 
                     expand=expansion(add=c(0.01, 0.01)))
Figure_2

# ------------------------------- Figure 3 – Actual & Expected Home Price Growth
# ------------------------------- Panel A - Actual and Expected Home Price Growth (DHS)
#set wd
setwd(dataDescriptive)
#Data from CBS https://opendata.cbs.nl/#/CBS/en/dataset/83906ENG/table 
HP_NL <- read.csv("Existing_own_homes__purchase_prices__price_indices_2015_100__12032023_204925.csv", header = F, sep = ";")

NL_HP_corrected <- HP_NL %>%
  slice(6:32) %>%
  rename(year = names(.)[1],
         pct_change = names(.)[3]) %>%
  mutate(year = as.numeric(year),
         pct_change = as.numeric(pct_change)) %>%
  select(year, pct_change) %>%
  slice(10:27)

DHS_HP <- DHS %>%
  filter(year>2003) %>%
  drop_na(exp_home_price_w) %>%
  group_by(year) %>%
  summarise(exp_HP = mean(exp_home_price_w),
            p75 = quantile(exp_home_price_w, 0.75),
            p25 = quantile(exp_home_price_w, 0.25)) %>%
  #lag hp growth to show expectation about current year
  mutate(year = year+1) 

NL_HP_expectations <- DHS_HP %>%
  inner_join(NL_HP_corrected, by ="year")

Figure_3_A <- ggplot(NL_HP_expectations, aes(x = year)) +
  geom_line(aes(y=exp_HP, group=1,color="Expected home price growth"),size=1.1) +
  geom_line(aes(y=p25, group=1,color="25th percentile of expectation"),size=1.1, linetype = "dashed") +
  geom_line(aes(y=p75, group=1,color="75th percentile of expectation"),size=1.1, linetype = "dashed") +
  geom_line(aes(y=pct_change, group=1,color="Actual home price growth"),size=1.1) +
  theme_master_2() +
  guides(color = guide_legend(nrow = 2, title.position = "top",
                              override.aes = list(linetype = c("solid", "dashed", "solid", "dashed")))) +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.4),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.7), #angeled x-axis text
  )  +
  ylab("Percent") + 
  scale_color_manual(values = c("#000000",  "#9ecae1","#2171b5", "#08306b"),
                     breaks = c("Actual home price growth", "25th percentile of expectation",
                                "Expected home price growth",  "75th percentile of expectation"))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     expand = expansion(mult = c(0, .0000001)), 
                     limits = c(-8, 16), 
                     breaks = seq(-8,16,by=4)) +
  scale_x_continuous(breaks=seq(2005,2022,by=1))
Figure_3_A

# ------------------------------- Figure 3 – Actual & Expected Home Price Growth
# ------------------------------- Panel B - Actual and Expected Home Price Growth (SCE)
#Load data downloaded from https://www.zillow.com/research/data/
HP_growth <- read.csv("Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv", header = TRUE, sep = ",")

#trim data to necesarry rows and months only
HP_growth_trim <- HP_growth %>%
  filter(RegionType == "country") %>%
  #select only start of febuary/end of january
  select(contains(".01.")) %>%
  #select only 2014 and onwards
  select(14:24)

#transpose
HP_growth_long <- as.data.frame(t(HP_growth_trim))
rownames(HP_growth_long) <- NULL

#find percentage change from last year
HP_growth_fixed <- HP_growth_long %>%
  mutate(year = row_number()+2012,
         V2 = lag(V1),
         pct_change = (V1/V2-1)*100,) %>%
  select(year, pct_change) %>%
  filter(year > 2014,
         year < 2023)

#Find average expectatoin in SCE
SCE_avg <- SCE %>%
  group_by(date) %>%
  summarize(expected_hp_growth = mean(exp_home_price_point_w),
            p75 = quantile(exp_home_price_point_w, 0.75),
            p25 = quantile(exp_home_price_point_w, 0.25)) %>%
  #year = previous year (as forecasting about next year)
  mutate(year = row_number()+2014) %>%
  select(-date) 

US_HP_expectations <- SCE_avg %>%
  inner_join(HP_growth_fixed, by ="year") 

Figure_3_B <- ggplot(US_HP_expectations, aes(x = year)) +
  geom_line(aes(y=expected_hp_growth, group=1,color="Expected home price growth"),size=1.1) +
  geom_line(aes(y=p75, group=1,color="75th percentile of expectation"),size=1.1, linetype = "dashed") +
  geom_line(aes(y=p25, group=1,color="25th percentile of expectation"),size=1.1, linetype = "dashed") +
  geom_line(aes(y=pct_change, group=1,color="Actual home price growth"),size=1.1) +
  theme_master_2() +
  guides(color = guide_legend(nrow = 2, title.position = "top",
                              override.aes = list(linetype = c("solid", "dashed", "solid", "dashed")))) +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.4),
        legend.title = element_blank(),
        legend.margin = margin(l = -15)
  )  +
  ylab("Percent") + 
  scale_color_manual(values = c("#000000",  "#9ecae1","#2171b5", "#08306b"),
                     breaks = c("Actual home price growth", "25th percentile of expectation",
                                "Expected home price growth",  "75th percentile of expectation"))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     expand = expansion(mult = c(0, .0000001)), 
                     limits = c(0, 18), 
                     breaks = seq(0,18,by=3)) +
  scale_x_continuous(breaks=seq(2015,2022,by=1))
Figure_3_B

# ------------------------------- Figure 4 – Distribution of Expected Home Price Growth
# ------------------------------- Panel A - Distribution of Expected Home Price Growth (DHS)
# Nuber of rows to calculate fraction for y-axis
DHS2019 <- DHS %>%
  drop_na(exp_home_price_w, exp_1y_mortgage_w) %>%
  filter(year == 2019)
DHS_n <- nrow(DHS2019)



#Crate the plot
Figure_4_A <- ggplot() +
  geom_histogram(data = DHS2019, aes(x = exp_home_price_w, y = (..count../DHS_n)*100, fill = "DHS2019"), color = "black", size = 0.2, bins = 40) +
  labs(x = "Expected Home Price Growth (Percent)", y = "Relative Freqeuncy (Percent)", fill = "Dataset") +
  scale_fill_manual(values = c("#2171b5"), labels = c("DHS 2019")) +
  scale_y_continuous(labels = scales::number_format(scale = 1), expand = expansion(mult = c(0, .0)), 
                     limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
  theme_master_2() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.4),
        legend.title = element_blank(), legend.box = "horizontal") +
  scale_x_continuous(limits = c(-15, 15), breaks = c(-15, -12, -10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10, 12, 15),
                     labels = c("<=-15", -12, -10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10, 12, ">=15"))
Figure_4_A

# ------------------------------- Figure 4 – Distribution of Expected Home Price Growth
# ------------------------------- Panel B - Distribution of Expected Home Price Growth (SCE)

SCE2019 <- SCE %>%
  filter(date == 201902) %>%
  drop_na(exp_home_price_point_w, exp_1y_mortgage_rate_w)
SCE_n <- nrow(SCE2019)

Figure_4_B <- ggplot() +
  geom_histogram(data = SCE2019, aes(x = exp_home_price_point_w, y = (..count../SCE_n)*100, fill = "SCE2019"), color = "black", size = 0.2, bins = 40) +
  labs(x = "Expected Home Price Growth (Percent)", y = "Relative Freqeuncy (Percent)", fill = "Dataset") +
  scale_fill_manual(values = c("#2171b5"), labels = c("SCE 2019")) +
  scale_y_continuous(labels = scales::number_format(scale = 1), expand = expansion(mult = c(0, .0)), 
                     limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
  theme_master_2() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.4),
        legend.title = element_blank(), legend.box = "horizontal") +
  scale_x_continuous(limits = c(-15, 15), breaks = c(-15, -12, -10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10, 12, 15),
                     labels = c("<=-15", -12, -10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10, 12, ">=15"))
Figure_4_B

####################################################################
###############################
# Appendix Figures
###############################
####################################################################

# ------------------------------- Figure A.1 – Actual & Expected Mortgage Rate Growth
# ------------------------------- Panel A - Actual & Expected Mortgage Rate Growth (DHS)
#Source https://www.statista.com/statistics/596336/interest-rate-for-new-mortgages-in-the-netherlands/
#Scraped manually
NL_MR_corrected <- data.frame(year = c(seq(from = 2003, to = 2022)),
                              avg_MR = c(5.19, 4.89, 4.17, 4.7, 5.3, 5.59, 5.3,
                                         4.86, 5.08, 5.02, 4.74, 3.82,
                                         3.21, 2.87, 2.96, 2.8, 2.28, 1.99, 1.73, 2.6)) %>%
  mutate(MR_growth = avg_MR - lag(avg_MR)) %>%
  filter(year>2004)

DHS_avg_mr <- DHS %>%
  filter(year>2003) %>%
  drop_na(exp_1y_mortgage_w) %>%
  group_by(year) %>%
  summarise(expected_mr_growth_post = mean(exp_1y_mortgage_w),
            p75 = quantile(exp_1y_mortgage_w, 0.75),
            p25 = quantile(exp_1y_mortgage_w, 0.25)) %>%
  #expectation in t is realized in t+1
  mutate(year = year+1)

NL_MR_expectations <- DHS_avg_mr %>%
  inner_join(NL_MR_corrected, by ="year")

Figure_A1_A <- ggplot(NL_MR_expectations, aes(x = year)) +
  geom_line(aes(y = expected_mr_growth_post, color = "Expected mortgage rate growth"), size=1.1) +
  geom_line(aes(y = MR_growth, color = "Actual mortgage rate growth"), size=1.1) +
  geom_line(aes(y=p25, group=1,color="25th percentile of expectation"),size=1.1, linetype = "dashed") +
  geom_line(aes(y=p75, group=1,color="75th percentile of expectation"),size=1.1, linetype = "dashed") +
  theme_master_2() +
  guides(color = guide_legend(nrow = 2, title.position = "top",
                              override.aes = list(linetype = c("solid", "dashed", "solid", "dashed")))) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.4),
        legend.margin = margin(l = -10),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.7), #angeled x-axis text
  )  +
  ylab("Percentage Points") + 
  scale_color_manual(values = c("#000000",  "#9ecae1","#2171b5", "#08306b"),
                     breaks = c("Actual mortgage rate growth", "25th percentile of expectation",
                                "Expected mortgage rate growth",  "75th percentile of expectation"))+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1),
                     expand = expansion(mult = c(0, .0000001)), 
                     limits = c(-1.5, 1), 
                     breaks = seq(-1.5,1,by=0.5)) +
  scale_x_continuous(breaks=seq(2005,2022,by=1),)
Figure_A1_A


# ------------------------------- Figure A.1 – Actual & Expected Mortgage Rate Growth
# ------------------------------- Panel B - Actual & Expected Mortgage Rate Growth (SCE)
#from https://fred.stlouisfed.org/series/MORTGAGE30US 
MR_avg <- read.csv("MORTGAGE30US.csv", header = TRUE, sep = ",")

#trim mortgage rate data
MR_avg_trim <- MR_avg %>%
  mutate(date = as.Date(DATE)) %>%
  filter(date >= as.Date("2012-02-01"), month(date)==2) %>%
  mutate(year = year(ymd(date)),
         MR_avg = as.numeric(MORTGAGE30US),
         MR_growth = MR_avg-lag(MR_avg)) %>%
  filter(year>2013) %>%
  select(year, MR_avg, MR_growth)


#find mortgage rate beliefs from SCE
SCE_avg_mr <- SCE %>%
  mutate(exp_mr_growth = exp_1y_mortgage_rate_w-percieved_mortgage_rate_w) %>%
  group_by(date) %>%
  summarise(expected_mr_growth = mean(exp_mr_growth),
            p75_growth = quantile(exp_mr_growth, 0.75),
            p25_growth = quantile(exp_mr_growth, 0.25),
            expected_mr_lead = mean(exp_1y_mortgage_rate_w),
            p75_lead = quantile(exp_1y_mortgage_rate_w, 0.75),
            p25_lead = quantile(exp_1y_mortgage_rate_w, 0.25),
            perceived_mr = mean(percieved_mortgage_rate_w)) %>%
  #lag mr growth to show expectatoin about current year
  add_row(date = 202202) %>%
  mutate(expected_mr_post = lag(expected_mr_lead),
         p75 = lag(p75_lead),
         p25 = lag(p25_lead),
         "year" = row_number()+2013) %>%
  select(-expected_mr_lead, -p75_lead, -p25_lead, -date) 

#Join info
US_MR_expectations <- SCE_avg_mr %>%
  inner_join(MR_avg_trim, by ="year")

Figure_A1_B <- ggplot(US_MR_expectations, aes(x = year)) +
  geom_line(aes(y = expected_mr_growth, color = "Expected mortgage rate growth"), size=1.1) +
  geom_line(aes(y=p75_growth, group=1,color="75th percentile of expectation"),size=1.1, linetype = "dashed") +
  geom_line(aes(y=p25_growth, group=1,color="25th percentile of expectation"),size=1.1, linetype = "dashed") +
  geom_line(aes(y = MR_growth, color = "Actual mortgage rate growth"), size=1.1) +
  theme_master_2() +
  guides(color = guide_legend(nrow = 2, title.position = "top",
                              override.aes = list(linetype = c("solid", "dashed", "solid", "dashed")))) +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.4),
        legend.margin = margin(l = -10),
        legend.title = element_blank(),
  )  +
  ylab("Percentage Points") + 
  scale_color_manual(values = c("#000000",  "#6baed6","#2171b5", "#08306b"),
                     breaks = c("Actual mortgage rate growth", "25th percentile of expectation",
                                "Expected mortgage rate growth",  "75th percentile of expectation"))+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1),
                     expand = expansion(mult = c(0, .0000001)), 
                     limits = c(-1, 1.5), 
                     breaks = seq(-1,1.5,by=0.5)) +
  scale_x_continuous(limits = c(2014, 2021), 
                     breaks=seq(2014,2021,by=1))
Figure_A1_B

# ------------------------------- Figure A.2 – Distribution of Expected Mortgage Rate Growth (2019)
# ------------------------------- Panel A - Distribution of Expected Mortgage Rate Growth (DHS 2019)
# Nuber of rows to calculate fraction for y-axis
DHS2019 <- DHS %>%
  drop_na(exp_home_price_w, exp_1y_mortgage_w) %>%
  filter(year == 2019)
DHS_n <- nrow(DHS2019)

Figure_A2_A <- ggplot() +
  geom_histogram(data = DHS2019, aes(x = exp_1y_mortgage_w, y = (..count../DHS_n)*100, fill = "DHS2019"), color = "black", size = 0.2, bins = 40) +
  labs(x = "Expected Mortgage Price Growth (Percentage Points)", y = "Relative Frequency (Percent)", fill = "Dataset") +
  scale_fill_manual(values = c("#2171b5"), labels = c("DHS 2019")) +
  scale_y_continuous(labels = scales::number_format(scale = 1), expand = expansion(mult = c(0, .0)), 
                     limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
  theme_master_2() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.4),
        legend.title = element_blank(), legend.box = "horizontal") +
  scale_x_continuous(limits = c(-10, 10), breaks = c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10),
                     labels = c("<=-10", -8, -6, -4, -2, 0, 2, 4, 6, 8, ">=10")) 
Figure_A2_A

# ------------------------------- Figure A.2 – Distribution of Expected Mortgage Rate Growth (2019)
# ------------------------------- Panel A - Distribution of Expected Mortgage Rate Growth (SCE 2019)
# Nuber of rows to calculate fraction for y-axis

SCE2019 <- SCE %>%
  filter(date == 201902) %>%
  drop_na(exp_home_price_point_w, exp_1y_mortgage_rate_w) %>%
  mutate(exp_mr_growth = exp_1y_mortgage_rate_w-percieved_mortgage_rate_w)
SCE_n <- nrow(SCE2019)

Figure_A2_B <- ggplot() +
  geom_histogram(data = SCE2019, aes(x = exp_mr_growth, y = (..count../SCE_n)*100, fill = "SCE2019"), color = "black", size = 0.2, bins = 40) +
  labs(x = "Expected Mortgage Price Growth (Percentage Points)", y = "Relative Frequency (Percent)", fill = "Dataset") +
  scale_fill_manual(values = c("#2171b5"), labels = c("SCE 2019")) +
  scale_y_continuous(labels = scales::number_format(scale = 1), expand = expansion(mult = c(0, .0)), 
                     limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
  theme_master_2() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.4),
        legend.title = element_blank(), legend.box = "horizontal") +
  scale_x_continuous(limits = c(-10, 10), breaks = c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10),
                     labels = c("<=-10", -8, -6, -4, -2, 0, 2, 4, 6, 8, ">=10")) 
Figure_A2_B

#Export figures (As .emf files)      
setwd(outputPlots)
emf("Figure_1.emf", width = 4.725, height = 4.05)
Figure_1
dev.off()
emf("Figure_2.emf", width = 4.725, height = 4.05)
Figure_2
dev.off()
emf("Figure_3_A.emf", width = 4.725, height = 4.05)
Figure_3_A
dev.off()
emf("Figure_3_B.emf", width = 4.725, height = 4.05)
Figure_3_B
dev.off()
emf("Figure_4_A.emf", width = 4.725, height = 4.05)
Figure_4_A
dev.off()
emf("Figure_4_B.emf", width = 4.725, height = 4.05)
Figure_4_B
dev.off()

emf("Figure_A1_A.emf", width = 4.725, height = 4.05)
Figure_A1_A
dev.off()
emf("Figure_A1_B.emf", width = 4.725, height = 4.05)
Figure_A1_B
dev.off()
emf("Figure_A2_A.emf", width = 4.725, height = 4.05)
Figure_A2_A
dev.off()
emf("Figure_A2_B.emf", width = 4.725, height = 4.05)
Figure_A2_B
dev.off()


####################################################################
###############################
# Numbers Presented in Thesis Text
###############################
####################################################################

# ------------------------------- Section 2 ------------------------------

# Check for balance between outcome variable and life events
xtabs(~ transition + divorced, data=DHS_panel)
xtabs(~ transition + got_members, data=DHS_panel)
xtabs(~ transition + lost_members, data=DHS_panel)
xtabs(~ transition + got_job, data=DHS_panel)
xtabs(~ transition + lost_job, data=DHS_panel)
xtabs(~ transition + got_retired, data=DHS_panel)
# got_job has no values =1 when transition=1. As such, will be removed from main analysis.

#Make function that calculates mode and mode share for each column of df
calc_mode <- function(x) {
  mode_val <- na.omit(x)[which.max(tabulate(match(na.omit(x), unique(na.omit(x)))))]
  mode_prop <- sum(x == mode_val, na.rm = TRUE) / sum(!is.na(x))
  return(c(mode_val = mode_val, mode_prop = mode_prop))
}

# Apply function to expectations
df_modes <- DHS_panel %>%
  summarise_all(calc_mode) %>%
  select(exp_home_price_lag, exp_1y_mortgage_lag)

# Print results
print(df_modes)

# ------------------------------- Section 3 ------------------------------

#How big a fraction of transitioners also experience a divorce??
share <- DHS_panel %>%
  filter(transition == 1) %>%
  summarise(share = sum(divorced == 1) / nrow(.))
share
#share is ~13 %

#How big a fraction of transitioners also experience a divorce??
share_2 <- DHS_panel %>%
  filter(divorced == 1) %>%
  summarise(share = sum(lost_members == 1) / nrow(.))
share_2
#share is ~40 %

#Fraction wihtout kids
5682/8051
#70 %

#Fraction who report as confident
3019/8051
#38 %

#Fraction who have a college degree
3970/8051
#49% (Slightly off from Table 2 due to Table 2 showing current education while Table 5 uses prior variables)

# ------------------------------- Section 5 ------------------------------
#Average change in expectations year-to-year
final_avg <- DHS %>% 
  filter(year>2003) %>%
  select(nohhold, year, exp_home_price_w) %>%
  group_by(nohhold) %>% 
  arrange(year) %>% 
  mutate(diff_exp_home_price_w = exp_home_price_w - lag(exp_home_price_w)) %>% 
  filter(!is.na(diff_exp_home_price_w)) %>% 
  group_by(nohhold) %>% 
  summarise(avg_diff_exp_home_price_w = mean(abs(diff_exp_home_price_w))) %>% 
  summarise(avg_of_avgs = mean(avg_diff_exp_home_price_w))

#Mean error between expectation and actual
NL_error <- NL_HP_expectations %>%
  mutate(error = abs(exp_HP - pct_change)) %>%
  summarise(mean_er = mean(error, na.rm=T))

