#==============================================================================
#   Data Analysis Tutorial: Descriptive statistics
#==============================================================================

# original Bill Sundstrom 9/1/2014
# Latest version: Michael Kevane 12/29/2020

# Description: Create tables of descriptive statistics for 
# CA school district data and WDI data etc.

#==============================================================================
# Settings, packages, and options
#==============================================================================

# You should generally run all of the commands in Section 1 
# at the start of every R session.

  # Clear the working space
  rm(list = ls())
  
  # Set working directory (edit for YOUR econ 42 folder)
  # setwd("/Users/wsundstrom/econ_42/data")
  # or 
  # setwd("C:/Users/wsundstrom/econ_42/data")
  # or
   setwd("G:/My Drive/Classes/ECON 41-42 shared/files42/data")

  # Load the packages 
  library(tidyverse)
  library(readstata13)
  library(sandwich)
  library(stargazer)
  library(WDI)

  # turn off scientific notation except for big numbers
  options(scipen = 9)

#==============================================================================
# Data section
#==============================================================================

# Read California school data from a website (https://wps.pearsoned.com/aw_stock_ie_3/178/45691/11696965.cw/index.html)
  url="https://wps.pearsoned.com/wps/media/objects/11422/11696965/datasets3e/datasets/caschool.dta"
  caschool <- read.dta13(url)
  
  # new variable for "small" average class sizes: student teacher ratio < 20
  # Note "address" for a variable is the data frame name, then the dollar sign $, then the variable name
  # Note this variable is a "factor" variable, not a numeric variable
  caschool$smallclass <- caschool$str<20

# Read data from a World Bank Development Indicators database on the Internet
  # Make a list of variables
  wdilist <- c("NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2005 intl $)
               "SP.POP.TOTL", # Population, total
               "SP.POP.TOTL.FE.ZS", # Population, female (% of total)
               "SP.URB.TOTL.IN.ZS", # Urban population (% of total)
               "SP.POP.BRTH.MF", # Sex ratio at birth   # (females per 1000 males)
               "SP.DYN.LE00.IN", # Life expect at birth, total  # (years)
               "SP.DYN.LE00.FE.IN", # Life expect, female (years)
               "SP.DYN.LE00.MA.IN", # Life expect, male (years),
               "SP.DYN.IMRT.IN", # Infant mortality rate
               "SP.DYN.TFRT.IN" ) # Fertility rate,(births per woman) 

# Extract 2015 values for desired variables from WDI.
# This may take a few minutes, depending on connection speed
# Note WDI is a command in the package WDI
  wdim <- WDI(country="all", indicator = wdilist, 
             extra = TRUE, start = 1990, end = 1990)

# Rename the variables
  wdim <- wdim %>% rename(GDPpcUSDreal=NY.GDP.PCAP.PP.KD,
                         population= SP.POP.TOTL,
                         femaleperc=SP.POP.TOTL.FE.ZS,
                         urbanperc=SP.URB.TOTL.IN.ZS,
                         sexratiobirth=SP.POP.BRTH.MF,
                         lifeexp=SP.DYN.LE00.IN,
                         lifexpfem=SP.DYN.LE00.FE.IN,
                         lifeexpmale=SP.DYN.LE00.MA.IN,
                         infmort=SP.DYN.IMRT.IN,
                         fertility=SP.DYN.TFRT.IN)

# Take out the entries that are aggregates 
# (eg East Asia) and not countries
  wdim <- subset(wdim, !( region=="Aggregates")) 

# Read CPS earnings data used in SW Table 3.1 from SW web site
  earn <- read.dta13("https://wps.pearsoned.com/wps/media/objects/11422/11696965/datasets3e/datasets/cps_ch3.dta")

# Create new variable that adjusts 1992 values to 2008$ using CPI
  earn$realahe <- ifelse(earn$year==1992, earn$ahe*215.2/140.3, earn$ahe)
  table(earn$year)
  
#==============================================================================
# Analysis section
#==============================================================================

### Descriptive statistics for CA schools data

  # Standard descriptive statistics for all numerical variables in the data
  stargazer(caschool, type="text", median=TRUE,
            digits=2, title="CA school data set")
  
  # Descriptive statistics for selected variables
  stargazer(caschool[c("str","testscr","el_pct")], type="text", 
            digits=2, title="CA school data set")
  
  # Descriptive statistics for a subset of observations 
  stargazer(subset(caschool, smallclass==1), type="text", digits=2, 
            title="Schools with student-teacher ratio less than 20")
  
  stargazer(subset(caschool, smallclass==0), type="text", digits=2, 
            title="Schools with student-teacher ratio >= 20")
  
  # frequency tables by county, various permutations
  table(caschool$county)
  table(caschool$county, caschool$smallclass)
  table(caschool$county, caschool$smallclass, useNA="ifany")
  
  # Table of means by a factor or logical variable (a crosstab) using dplyr piping
  caschool %>% group_by(smallclass) %>% summarize(ts_mean=mean(testscr,na.rm=TRUE))
  caschool %>% group_by(smallclass) %>% summarize(ts_max=max(testscr,na.rm=TRUE))
  caschool %>% group_by(smallclass) %>% summarize(ts_min=min(testscr,na.rm=TRUE))

  # T-test for difference in means
  t.test(testscr~smallclass, data=caschool , 
         FUN=c(mean), na.rm=TRUE)

### Descriptive statistics for WDI data
  
  stargazer(wdim, type="text", median=TRUE,
            digits=2, title="WDI data set")
  
  stargazer(subset(wdim, wdim$region=="Middle East & North Africa"), 
            type="text", digits=2, 
            title="WDI data for Middle East")
  
  # Table by region of % female
  options(digits=5) # how many decimal places
  wdim %>% group_by(region) %>% 
    summarize(femperc_mean=mean(femaleperc,na.rm=TRUE))

### t-tests with the cps92_08.csv dataset

  # descriptive statistics
  stargazer(earn, type="text", median=TRUE,
            digits=2, title="Earnings Data")
  
  # two-sample tests of various null hypotheses (Ho)
  
  # Ho: mean real earnings were equal in 1992 and 2008
  earn = subset(earn, year==2008 | year==1992)
  t.test(earn$realahe ~ earn$year)  
  
  # Ho: mean real earnings were equal for men and women in 2008
  # create data frame for just 2008 observations
  earn08 = subset(earn, year==2008)
  # run the test
  t.test(earn08$realahe ~ earn08$a_sex)  
  # Note variable a_sex takes on two values, 1 and 2. Can you create a variable 'female'?
