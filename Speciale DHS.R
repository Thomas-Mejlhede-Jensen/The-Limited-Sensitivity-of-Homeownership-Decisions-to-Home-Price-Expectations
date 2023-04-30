
#Clear all in enviorment
rm(list = ls())

#REMEMBER TO DROP ALL BEFORE 2004 OR NOT, DEPENDING ON IF YOURE MAKING EXPECTATIONS BASED ANALYSIS

library("haven") #For importing .dta files
library("tidyverse")
library("writexl")
library("openxlsx")
#set directories
data <- "C:/Users/thom0/Desktop/Thomas Stuff/Polit KU/Speciale/Programmering/Data DHS"
output <- "C:/Users/thom0/Desktop/Thomas Stuff/Polit KU/Speciale/Programmering/Stata Input"

#Load DHS data, by module, and create year variable
#Done individually rather than with a function, due to changes in variable and file names
setwd(data)

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
#Add WO1 fra 2001 og itlabge, siden den mangler i HHI
#moduels not yet fixed since 2003
#The questions about economic expectations, wod44p, wod44q, wod52a,and wod52b
#are missing from 2003 and earlier, making it impossible to conduct expectations based regresions

HSE1994 <- read_dta("hse1994en_2.0.dta") %>%
  mutate(year = 1994) %>%
  select(nohhold, nomem, year, wo5, wo1, wo34, wo41, wo53)
HSE1995 <- read_dta("hse1995en_2.0.dta") %>%
  mutate(year = 1995) %>%
  select(nohhold, nomem, year, wo5, wo1, wo34, wo41, wo53, hy71)
HSE1996 <- read_dta("hse1996en_2.0.dta") %>%
  mutate(year = 1996) %>%
  select(nohhold, nomem, year, wo5, wo1, wo34, wo41, wo53, hy71)
HSE1997 <- read_dta("hse1997en_2.0.dta") %>%
  mutate(year = 1997) %>%
  select(nohhold, nomem, year, wo5, wo1, wo34, wo41, wo53, hy71)
HSE1998 <- read_dta("hse1998en_2.0.dta") %>%
  mutate(year = 1998) %>%
  select(nohhold, nomem, year, wo5, wo1, wo34, wo41, wo53, hy71)
HSE1999 <- read_dta("hse1999en_2.0.dta") %>%
  mutate(year = 1999) %>%
  select(nohhold, nomem, year, wo5, wo1, wo34, wo41, wo53, hy71)
HSE2000 <- read_dta("hse2000en_2.0.dta") %>%
  mutate(year = 2000) %>%
  select(nohhold, nomem, year, wo5, wo1, wo34, wo41, wo53, hy71)
HSE2001 <- read_dta("hse2001en_2.0.dta") %>%
  mutate(year = 2001)%>%
  select(nohhold, nomem, year, wo5, wo1, wo34, wo41, wo53, hy71)
HSE2002 <- read_dta("hse2002en_2.0.dta") %>% 
  mutate(year = 2002) %>%
  select(nohhold, nomem, year, wo5, wo1, wo34, wo41, wo53, hy71)
HSE2003 <- read_dta("hse2003en_3.0.dta") %>%  
  mutate(year = 2003) %>%
  select(nohhold, nomem, year, wo5, wo1, wo34, wo41, wo53, hy71)
HSE2004 <- read_dta("hse2004en_2.0.dta") %>%  
  mutate(year = 2004) %>%
  select(nohhold, nomem, year, wo5, wo1, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2005 <- read_dta("hse2005en_2.0.dta") %>%  
  mutate(year = 2005) %>%
  select(nohhold, nomem, year, wo5, wo1, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2006 <- read_dta("hse2006en_2.0.dta") %>%  
  mutate(year = 2006) %>%
  select(nohhold, nomem, year, wo5, wo1, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, HY71, wo48) %>%
  #HY71 with capital letters in 2006
  rename(hy71 = HY71)
HSE2007 <- read_dta("hse2007en_2.0.dta") %>%  
  mutate(year = 2007) %>%
  select(nohhold, nomem, year, wo5, wo1, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2008 <- read_dta("hse2008en_3.0.dta") %>%  
  mutate(year = 2008) %>%
  select(nohhold, nomem, year, wo5, wo1, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2009 <- read_dta("hse2009en_2.1.dta") %>% 
  mutate(year = 2009) %>%
  select(nohhold, nomem, year, wo5, wo1, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2010 <- read_dta("hse2010en_2.0.dta") %>%  
  mutate(year = 2010) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2011 <- read_dta("hse2011en_2.0.dta") %>%  
  mutate(year = 2011) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2012 <- read_dta("hse2012en_2.0.dta") %>%  
  mutate(year = 2012) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2013 <- read_dta("hse2013en_2.0.dta") %>%  
  mutate(year = 2013) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2014 <- read_dta("hse2014en_2.0.dta") %>%  
  mutate(year = 2014) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2015 <- read_dta("hse2015en_1.0.dta") %>%  
  mutate(year = 2015) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2016 <- read_dta("hse2016en_1.0.dta") %>%   
  mutate(year = 2016) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2017 <- read_dta("hse2017en_1.0.dta") %>%   
  mutate(year = 2017) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2018 <- read_dta("hse2018en_1.0.dta") %>%  
  mutate(year = 2018) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2019 <- read_dta("hse2019en_1.0.dta") %>% 
  mutate(year = 2019) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2020 <- read_dta("hse2020en_2.0.dta") %>%   
  mutate(year = 2020) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2021 <- read_dta("hse2021en_1.0.dta") %>%  
  mutate(year = 2021) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)
HSE2022 <- read_dta("hse2022en_1.0.dta") %>%  
  mutate(year = 2022) %>%
  select(nohhold, nomem, year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53, hy71, wo48)

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
   PSY2016, PSY2017, PSY2018, PSY2019, PSY2020, PSY2021, PSY2022)


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

colSums(is.na(DHS_merge))

#REMEMBER TO DROP ALL BEFORE 2004 OR NOT, DEPENDING ON IF YOU'RE MAKING EXPECTATIONS BASED ANALYSIS
#filter på 2003 eller ej? Mine nuværende tanker:
#Det er ikke nødvendigt?


#Before finding first year, drop observations that are missing information for variables which need to be lagged
#these are: employment, household members, married, and income information
#These would not be included in regression anyway, but need to be dropped now to find first year with full info
#Before doing this, have to change "-9" to NA in relevant questions:
DHS_drop <- DHS_merge %>%
  #filter(year>2003) %>%
  select(-inkhh) %>%
  # mutate(inkhh = ifelse(inkhh==-9,NA,inkhh)) %>%
  mutate(bezighei = ifelse(bezighei==-9,NA,bezighei)) %>%
  rename(inkhh = btot) %>%
  drop_na(inkhh, bezighei, aantalhh, burgst)


#create indicator counting first participation year (needed for later variables)
DHS_first <- DHS_drop %>%
  arrange(nohhold, year) %>%
  filter(duplicated(nohhold) == FALSE) %>%
  mutate(first_year = year) %>%
  select(nohhold, first_year)

#Last year
DHS_last <- DHS_drop %>%
  arrange(nohhold, desc(year)) %>%
  filter(duplicated(nohhold) == FALSE) %>%
  mutate(last_year = year) %>%
  select(nohhold, last_year)
  
#define lists of employed and unemployed
employed <- c(1,2,3)
unemployed <- c(4,5,9,10,11)

#create new variables
#always set (-9) to NA, as it corressponds to "I don't know"
DHS_enrich <- DHS_drop %>%
  #Education changes not only name, but also answears form 2001-2002
  #ÆNDRE 9 TIL NUL EVENTUELT HVIS DEN ER IKKE MED INDIKATORER men istedet bare linær, fordi 9 er lavere end de andre
  #men prøv med indikatorer først
  mutate(education = ifelse(year>2001,oplmet,
                            ifelse(oplmet2== 10,1,
                                   ifelse(oplmet2== 1,2,
                                          ifelse(oplmet2==2|oplmet2==3,3,
                                                 ifelse(oplmet2==4,4,
                                                        ifelse(oplmet2==5|oplmet2==6|oplmet2==11,5,
                                                               ifelse(oplmet2==7|oplmet2==8,6,
                                                                      ifelse(oplmet2==9,7,
                                                                ifelse(oplmet2==12,9,NA)))))))))) %>%
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
  mutate(exp_1y_mortgage = exp_2y_mortgage/2) %>%
  #create sale_prob
  mutate(wo53 = ifelse(wo53==-9,NA,wo53)) %>%
  mutate(move_indicator = ifelse(wo53<6,1,0)) %>%
  mutate(buy_indicator = ifelse(wo53==2|wo53==4,1,0)) %>%
  mutate(rent_indicator = ifelse(wo53==3|wo53==5,1,0)) %>%
  #create owner
  mutate(owner = ifelse(wo1==3,1,ifelse(wo1==-9,NA,0))) %>%
  #create Members
  #merge first year onto dataset
  arrange(nohhold, year) %>%
  left_join(DHS_first, by = "nohhold") %>%
  mutate(current_members = aantalhh,
         #If first period, assume no change (MIGHT CHANGE LATER, NOT OBVIOUS)
         lagged_members = ifelse(year==first_year,NA,lag(aantalhh)),
         got_members = ifelse(current_members>lagged_members,1,0),
         lost_members = ifelse(current_members<lagged_members,1,0),
         members_change_indicator = ifelse(got_members==1|lost_members==1,1,0)) %>%
  #lagged plans
  mutate(rent_indicator_lag = ifelse(year==first_year,NA,lag(rent_indicator)),
         move_indicator_lag = ifelse(year==first_year,NA,lag(move_indicator))) %>%
  #create married, and divorced
  mutate(married = ifelse(burgst<3,1,0),
         lagged_married = ifelse(year==first_year,NA,lag(married)),
         divorced = ifelse(is.na(lagged_married)|is.na(married),NA,
           ifelse(lagged_married==1 & married==0,1,0))) %>%
  #years residence
  mutate(years_residence = year-wo5) %>%
  #employment
  #drop observations missing first year job employment information
  mutate(job = bezighei,
         #If first period, assume no change (MIGHT CHANGE LATER, NOT OBVIOUS)
         lagged_job = ifelse(year==first_year,NA,lag(job)),
         got_job = ifelse(year==first_year,NA,
                          ifelse(job %in% employed & lagged_job %in% unemployed,1,0)),
         lost_job = ifelse(year==first_year,NA,
                           ifelse(job %in% unemployed & lagged_job %in% employed,1,0)),
         job_change_indicator = ifelse(got_job==1|lost_job==1,1,0)) %>%
  #extra life events temp
  mutate(got_retired = ifelse(job==8 & lagged_job!= 8,1,0),
         lost_student = ifelse(job==6 & lagged_job!=6,1,0)) %>%
  #current job
  mutate(employed = ifelse(job %in% employed,1,0)) %>%
  #income & income change
  mutate(inkhh = ifelse(inkhh==-9,NA,inkhh),
  #If first period, assume no change (MIGHT CHANGE LATER, NOT OBVIOUS)
  lagged_income = ifelse(year==first_year,NA,lag(inkhh))) %>%
  #random indicators
  mutate(retired = ifelse(bezighei==8,1,0),
         student = ifelse(bezighei==6,1,0)) %>%
  #transition from ownership to renting/vice versa
  mutate(owner_lag = ifelse(year==first_year,NA,lag(owner)),
         got_owner = ifelse(owner>owner_lag,1,0),
         lost_owner = ifelse(owner<owner_lag,1,0),
         change_owner = ifelse(lost_owner==1|got_owner==1,1,0)) %>%
    #random variables
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
  #faimly friction
  mutate(large_hh = ifelse(current_members>3,1,0),
         larger_hh = ifelse(current_members>4,1,0)) %>%
  #Children
  mutate(twopluskids = ifelse(aantalki>1,1,0)) %>%
  mutate(kids_indicator = ifelse(aantalki>0,1,0)) %>%
  #income_cat
  mutate(income_cat = ifelse(inkhh<10000,1,ifelse(inkhh<20000,2,ifelse(inkhh<30000,3,ifelse(inkhh<40000,4,
                                                                                            ifelse(inkhh<50000,5,ifelse(inkhh<60000,6,ifelse(inkhh<75000,7,
                                                                                                                                             ifelse(inkhh<100000,8,ifelse(inkhh<150000,9,
                                                                                                                                                                          ifelse(inkhh<200000,10,11))))))))))) %>%
  #income_change
  mutate(lagged_income = ifelse(year==first_year,NA,lag(income_cat)),
         income_change = income_cat-lagged_income) %>%
  #age_cat
  mutate(under_40 = ifelse(age<40,1,0),
         between_40_60 = ifelse(age<61 & age>39,1,0),
         over_60 = ifelse(age>60,1,0)) %>%
  #any life event
  mutate(life_event = ifelse(divorced==1|got_members==1|lost_members==1|
                               got_job==1|lost_job==1|got_retired==1,1,0)) %>%
  #deflate move prob 
  mutate(move_indicator = move_indicator*100,
         buy_indicator = buy_indicator*100,
         rent_indicator = rent_indicator*100) %>%
  #log income
  mutate(income = ifelse(inkhh==0,NA,log(inkhh)))

#For actual house moving analysis
#Create moved indicator
#NB THOMAS - kan prøve at ændre til at bare at de skal være forskellige,
#Eller at den skal være lig med i år eller sidste år.
#Kan evt fjerne den med == 6
DHS_enrich_time <- DHS_enrich %>%
  arrange(nohhold, year) %>%
  mutate(income_lag = ifelse(year==first_year,NA,lag(income))) %>%
  mutate(lagged_wo5 = ifelse(year==first_year,NA,lag(wo5)),
         moved_indicator = ifelse(wo5>lagged_wo5|wo53==6,1,0),
         moved_indicator = moved_indicator*100) %>% #if have lived in residence newer than last year, or yet to move in
         #lagged expectations
  mutate(exp_home_price_lag_no_w = ifelse(year==first_year,NA,lag(exp_home_price)),
         exp_1y_mortgage_lag_no_w = ifelse(year==first_year,NA,lag(exp_1y_mortgage))) %>%
  #lagged events
  mutate(divorced_lag = ifelse(year==first_year,NA,lag(divorced)),
         got_job_lag= ifelse(year==first_year,NA,lag(got_job)), 
         lost_job_lag= ifelse(year==first_year,NA,lag(lost_job)), 
         got_members_lag= ifelse(year==first_year,NA,lag(got_members)), 
         lost_members_lag= ifelse(year==first_year,NA,lag(lost_members)), 
         job_change_lag= ifelse(year==first_year,NA,lag(job_change_indicator)), 
         members_change_lag= ifelse(year==first_year,NA,lag(members_change_indicator))) %>%
  #lagged controls
  mutate(married_lag = ifelse(year==first_year,NA,lag(married)),
         retired_lag = ifelse(year==first_year,NA,lag(retired)),
         student_lag = ifelse(year==first_year,NA,lag(student)),
         education_lag = ifelse(year==first_year,NA,lag(education)),
         regio_lag = ifelse(year==first_year,NA,lag(regio)),
         income_cat_lag = ifelse(year==first_year,NA,lag(income_cat))) %>%
  #lagged frictions
  mutate(confident = ifelse(kunde>2,1,0),
         confident_lag =ifelse(year==first_year,NA,lag(confident)),
         large_hh_lag =ifelse(year==first_year,NA,lag(large_hh)),
         larger_hh_lag =ifelse(year==first_year,NA,lag(larger_hh)),
         kids_indicator_lag = ifelse(year==first_year,NA,lag(kids_indicator)),
         twopluskids_lag = ifelse(year==first_year,NA,lag(twopluskids))) %>%
  #smart
  mutate(smart = ifelse(education_lag==7|education_lag==6,1,0)) %>%
  #lead life events
  left_join(DHS_last, by = "nohhold") %>%
  mutate(divorced_lead = ifelse(year==last_year,NA,lead(divorced)),
         got_retired_lead= ifelse(year==last_year,NA,lead(got_retired)), 
         got_job_lead= ifelse(year==last_year,NA,lead(got_job)), 
         lost_job_lead= ifelse(year==last_year,NA,lead(lost_job)), 
         got_members_lead= ifelse(year==last_year,NA,lead(got_members)), 
         lost_members_lead= ifelse(year==last_year,NA,lead(lost_members)), 
         job_change_lead= ifelse(year==last_year,NA,lead(job_change_indicator)), 
         members_change_lead= ifelse(year==last_year,NA,lead(members_change_indicator))) %>% 
  #til johannes - slet snart 
  mutate(owner_lag = ifelse(year==first_year,NA,lag(owner))) %>%
  #lag move prob 
  mutate(move_indicator_lag =  ifelse(year==first_year,NA,lag(move_indicator))) %>%
  #Simon variabel 
  mutate(transition = ifelse(got_owner==1,1,ifelse(lost_owner==1,-1,ifelse(is.na(got_owner)|is.na(lost_owner),NA,0))),
         job_s = ifelse(got_job==1,1,ifelse(lost_job==1,-1,ifelse(is.na(got_job)|is.na(lost_job),NA,0))),
         members_s = ifelse(got_members==1,1,ifelse(lost_members==1,-1,ifelse(is.na(got_members)|is.na(lost_members),NA,0))),
         job_s_lag = ifelse(year==first_year,NA,lag(job_s)),
         members_s_lag = ifelse(year==first_year,NA,lag(members_s)),
         job_s_lead = ifelse(year==first_year,NA,lead(job_s)),
         members_s_lead = ifelse(year==first_year,NA,lead(members_s)),
         simon_move = ifelse(move_indicator==100,-1,move_indicator)) %>%
  #SOM DET SIDSTE - DROP NA's der alligevel droppes i OLS til desc analyse og til beregne tal i brødtekst
  drop_na(lost_owner, age, male, married_lag, retired_lag, student_lag, education_lag, regio_lag, income_lag,
          divorced, got_members, lost_members, got_retired, got_job, lost_members) %>%
  # keep only t-1 owner
  filter(owner_lag==1) %>%
  #Keep 1 member from each household, prioritizing newer observations
  group_by(nohhold) %>%
  arrange(desc(year)) %>%
  filter(nomem == first(nomem)) %>%
  #calculate tenure for remaning member
  mutate(tenure = n()) %>%
  ungroup()

#WINSORIZE
DHS_enrich_w <- DHS_enrich_time %>%
  mutate(exp_home_price_w = ifelse(exp_home_price<(-15),-15, ifelse(exp_home_price>15,15,exp_home_price)),
         exp_1y_mortgage_w = ifelse(exp_1y_mortgage<(-10),-10, ifelse(exp_1y_mortgage>10,10,exp_1y_mortgage)),
         exp_home_price_lag = ifelse(exp_home_price_lag_no_w<(-15),-15, ifelse(exp_home_price_lag_no_w>15,15,exp_home_price_lag_no_w)),
         exp_1y_mortgage_lag = ifelse(exp_1y_mortgage_lag_no_w<(-10),-10, ifelse(exp_1y_mortgage_lag_no_w>10,10,exp_1y_mortgage_lag_no_w))) %>%
         #Alternative cutoffs for replication
  mutate(q1 = quantile(exp_home_price, 0.01, na.rm = T),
         q99 = quantile(exp_home_price, 0.99,  na.rm = T),
         exp_home_price_w_alt = ifelse(exp_home_price > q99, q99, ifelse(exp_home_price < q1, q1, exp_home_price)),
         q1_lag = quantile(exp_home_price_lag_no_w, 0.01, na.rm = T),
         q99_lag = quantile(exp_home_price_lag_no_w, 0.99,  na.rm = T),
         exp_home_price_lag_alt = ifelse(exp_home_price_lag_no_w > q99_lag, q99_lag, ifelse(exp_home_price_lag_no_w < q1_lag, q1_lag, exp_home_price_lag_no_w)),
         exp_home_price_lag_trim = ifelse(exp_home_price_lag_no_w > q99_lag, NA, ifelse(exp_home_price_lag_no_w < q1_lag, NA, exp_home_price_lag_no_w)),
         q1_mr = quantile(exp_1y_mortgage, 0.01,  na.rm = T),
         q99_mr = quantile(exp_1y_mortgage, 0.99,  na.rm = T),
         exp_1y_mortgage_w_alt = ifelse(exp_1y_mortgage > q99_mr, q99_mr, ifelse(exp_1y_mortgage < q1_mr, q1_mr, exp_1y_mortgage)),
         q1_mr_lag = quantile(exp_1y_mortgage_lag_no_w, 0.01,  na.rm = T),
         q99_mr_lag = quantile(exp_1y_mortgage_lag_no_w, 0.99,  na.rm = T),
         exp_1y_mortgage_lag_alt = ifelse(exp_1y_mortgage_lag_no_w > q99_mr_lag, q99_mr_lag, ifelse(exp_1y_mortgage_lag_no_w < q1_mr_lag, q1_mr_lag, exp_1y_mortgage_lag_no_w)),
         exp_1y_mortgage_lag_trim = ifelse(exp_1y_mortgage_lag_no_w > q99_mr_lag, NA, ifelse(exp_1y_mortgage_lag_no_w < q1_mr_lag, NA, exp_1y_mortgage_lag_no_w))) %>%
  select(-q1, -q1_mr, -q99, -q99_mr, -q1_lag, -q99_lag, -q1_mr_lag, -q99_mr_lag)
#99th percentiles are currently:
#mr = -2.5/+1, hp = -5/+5
#found by dropping select and investigating data

#Panel with corona
DHS_corona <- DHS_enrich_w

#Panel WITHOUT corona
DHS_move_full <- DHS_enrich_w %>%
  filter(year<2020)

#Output both
setwd(output)
write_xlsx(DHS_move_full, "DHS_move_full.xlsx")
write_xlsx(DHS_corona, "DHS_move_corona.xlsx")

#Make function that calculates mode and mode share for each column of df
calc_mode <- function(x) {
  mode_val <- na.omit(x)[which.max(tabulate(match(na.omit(x), unique(na.omit(x)))))]
  mode_prop <- sum(x == mode_val, na.rm = TRUE) / sum(!is.na(x))
  return(c(mode_val = mode_val, mode_prop = mode_prop))
}

# Apply function to expectations
df_modes <- DHS_move_full %>%
  summarise_all(calc_mode) %>%
  select(exp_home_price_lag, exp_1y_mortgage_lag)

# Print results
print(df_modes)

#Calculate mode expectations
mode_table <- DHS_move_full %>%
  select(exp_home_price_lag, exp_1y_mortgage_lag) %>%
  drop_na(exp_home_price_lag, exp_1y_mortgage_lag) %>%
  summarise_all(funs(value = .[which.max(tabulate(match(., unique(.))))]))
mode_table

mode <- names(sort(table(x), decreasing = TRUE))[1]

# Print the mode
mode

# Count the frequency of the mode for each column in mode_df
mode_freq <- mode_table %>%
  gather() %>%
  group_by(key, value) %>%
  summarise(freq = n()) %>%
  filter(value != "NA")

# Print the frequency of the mode for each column
mode_freq 


# KUNDE = confident (3-4)


#Finish conventional DHS
DHS <- DHS_enrich_time
%>%
  select(-nomem, -wo1, -wod44p, -wod44q, -wod52a, -wod52b, -wod205, -wod206,
         -bezighei, -gebjaar, -aantalhh, -woning, -burgst, -exp_home_price_1, -exp_home_price_2) %>%
  drop_na(move_indicator, exp_home_price, exp_2y_mortgage)


#Count number of NAs
colSums(is.na(DHS))




#How big a fraction of movers also experience change in household members?
tabel_1 <- DHS_move_full %>%
  group_by(lost_owner, divorced) %>%
  summarise(n = n())
#Calculate share
(18)/(18+123)
#share is ~13%

#correlation between lossing members and getting divorced
tabel_2 <- DHS_move_full %>%
  group_by(divorced, lost_members) %>%
  summarise(n = n())
#Calculate share
(84)/(84+124)
#share is ~40%

#Fraction who report to be confident
tabel_3 <- DHS_move_full %>%
  drop_na(exp_home_price_lag) %>%
  group_by(confident) %>%
  summarise(n = n())
3238/(3238+5388)
#share is ~37%



#How big a fraction of movers also lose employment?
tabel_3 <- DHS_enrich_time %>%
  filter(owner_lag == 1) %>%
  group_by(lost_owner, lost_job) %>%
  summarise(n = n())
#Calculate share
(5)/(5+162)
#Calculate how many unemployed
(5+147)/(5+147+162+16038)
#share is ~3%

#How big a fraction of movers also gain employment?
tabel_4 <- DHS_enrich_time %>%
  filter(owner_lag == 1) %>%
  group_by(lost_owner, got_job) %>%
  summarise(n = n())
#Calculate share
(5)/(5+162)

#How big a fraction of movers also change employment?
tabel_5 <- DHS_enrich_time %>%
  filter(owner_lag == 1) %>%
  group_by(lost_owner, job_change_indicator) %>%
  summarise(n = n())
#Calculate share
(5)/(5+162)


#How big a fraction of movers also change household members?
tabel_6 <- DHS_enrich_time %>%
  filter(owner_lag == 1) %>%
  group_by(lost_owner, got_retired) %>%
  summarise(n = n())
#Calculate share
(40)/(40+127)

#How many subjects consider themself confident?
tabel_7 <- DHS_enrich_time %>%
  filter(owner_lag == 1) %>%
  group_by(kunde) %>%
  summarise(n = n())
#Calculate share
(762+4486)/(762+4486+8269+1752)
(3578/(9462))
#share is ~3%



#how many owners in t plan to rent in t+1
tabel_3 <- DHS_enrich_time %>%
  drop_na(owner, rent_indicator) %>%
  group_by(owner, rent_indicator) %>%
  summarise(n = n())

tabel_3 <- DHS_enrich_time %>%
  group_by(wo53) %>%
  summarise(n = n())



tabel <- DHS_move %>%
  group_by(moved_indicator) %>%
  summarise(mean = mean(move_indicator_lag), n = n())


tabel_2 <- DHS_enrich_time %>%
  group_by(got_retired) %>%
  summarise(n = n())


############################################
#Logistic Regresoin
###########################################

logistic <- glm(lost_owner ~ di)


logistic <- glm(lost_owner ~ exp_home_price_lag+ exp_1y_mortgage_lag+ age+ male+ married_lag+ retired_lag+ year+ student_lag+
                  education_lag+ regio_lag+ income_lag+ divorced+ got_members+ lost_members+ got_retired+ got_job+ lost_job,
                  data=DHS_move_full, family="binomial")
summary(logistic)

############################################
#Random forest BEGIN
###########################################
library(ggplot2)
library(cowplot)
library(randomForest)

DHS_forest <- DHS_move_full %>%
  select(lost_owner, age, male, married_lag, 
         retired_lag, year, student_lag, education_lag, regio_lag, income_lag, divorced,
         got_s, lost_s, got_retired, got_job, lost_job) %>%
  #select(exp_home_price_lag, exp_1y_mortgage_lag,)
  drop_na()

str(DHS_forest)
DHS_forest$lost_owner <- as.factor(DHS_forest$lost_owner)
colSums(is.na(DHS_forest))
set.seed(42)

#Run random forest
forest <- randomForest(lost_owner ~., data=DHS_forest, proximity=TRUE)
#Print results
forest
############################################
#Random forest END
###########################################

quantile(DHS_enrich$income_cat, probs = c(0.25, 0.33, 0.4, 0.50, 0.75))

#check number of s from same household given year
tabel <- DHS_move %>%
  filter(year<2020) %>%
  drop_na(got, move_indicator_lag) %>%
  group_by(moved_indicator) %>%
  summarise(mean = mean(move_indicator_lag), n = n())


#chek overlap mellem divorced og moved
tabel <- DHS_enrich_time %>%
  group_by(lost_owner) %>%
  summarise(n = n())



#check number of members from same household given year
tabel <- DHS %>%
  filter(year<2020) %>%
  group_by(kunde) %>%
  summarise(n = n())






#Leg med spørsgmål wo560

HSE2012 <- read_dta("hse2012en_2.0.dta") %>%  
  mutate(year = 2012) %>%
  select(nohhold, nomem,  wo560, wo561, wo562, wo563, wo564, wo565, wo566, wo567, wo568, wo569,year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53)
HSE2013 <- read_dta("hse2013en_2.0.dta") %>%  
  mutate(year = 2013) %>%
  select(nohhold, nomem,  wo560, wo561, wo562, wo563, wo564, wo565, wo566, wo567, wo568, wo569,year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53)
HSE2014 <- read_dta("hse2014en_2.0.dta") %>%  
  mutate(year = 2014) %>%
  select(nohhold, nomem,  wo560, wo561, wo562, wo563, wo564, wo565, wo566, wo567, wo568, wo569,year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53)
HSE2015 <- read_dta("hse2015en_1.0.dta") %>%  
  mutate(year = 2015) %>%
  select(nohhold, nomem,  wo560, wo561, wo562, wo563, wo564, wo565, wo566, wo567, wo568, wo569,year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53)
HSE2016 <- read_dta("hse2016en_1.0.dta") %>%   
  mutate(year = 2016) %>%
  select(nohhold, nomem,  wo560, wo561, wo562, wo563, wo564, wo565, wo566, wo567, wo568, wo569,year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53)
HSE2017 <- read_dta("hse2017en_1.0.dta") %>%   
  mutate(year = 2017) %>%
  select(nohhold, nomem,  wo560, wo561, wo562, wo563, wo564, wo565, wo566, wo567, wo568, wo569,year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53)
HSE2018 <- read_dta("hse2018en_1.0.dta") %>%  
  mutate(year = 2018) %>%
  select(nohhold, nomem,  wo560, wo561, wo562, wo563, wo564, wo565, wo566, wo567, wo568, wo569,year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53)
HSE2019 <- read_dta("hse2019en_1.0.dta") %>% 
  mutate(year = 2019) %>%
  select(nohhold, nomem,  wo560, wo561, wo562, wo563, wo564, wo565, wo566, wo567, wo568, wo569,year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53)
HSE2020 <- read_dta("hse2020en_2.0.dta") %>%   
  mutate(year = 2020) %>%
  select(nohhold, nomem,  wo560, wo561, wo562, wo563, wo564, wo565, wo566, wo567, wo568, wo569,year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53)
HSE2021 <- read_dta("hse2021en_1.0.dta") %>%  
  mutate(year = 2021) %>%
  select(nohhold, nomem,  wo560, wo561, wo562, wo563, wo564, wo565, wo566, wo567, wo568, wo569,year, wo5, wo1, wod205, wod206, wod44p, wod44q, wo34, wo41, wod52a, wod52b, wo53)

HSE <- HSE2012 %>%
  bind_rows(HSE2013, HSE2014,HSE2015, HSE2016, HSE2017, HSE2018, HSE2019, HSE2020, HSE2021)

HSE_fixed <- HSE %>%
  mutate(life_move = ifelse(wo561==1|wo562==1|wo563==1|wo568==1,1,0),
         economic_move = ifelse(wo560==1|wo566==1|wo567==1,1,0))

tabel <- HSE_fixed %>%
  group_by(wo568) %>%
  summarise(n = n())

tabel <- HSE_fixed %>%
  summarise(0 = sum(wo560, na.rm=T),1 = sum(wo561), 2 = sum(wo562),3 = sum(wo563),4 = sum(wo56),5 = sum(wo565),6 = sum(wo566),7 = sum(wo567),8 = sum(wo568),9 = sum(wo569))




tbl_df(iris)








  
