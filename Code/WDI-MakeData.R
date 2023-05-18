#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro things                                    ####
#
# GSERM - St. Gallen (2023)
#
# Analyzing Panel Data
# Prof. Christopher Zorn
#
# Code to grab & create data from the World Bank's
# _World Development Indicators_. These are
# the "running example" data that we use in the
# course.
#
# NOTE: This code can take a hot second to
# run, depending on how fast the World Bank's
# API is operating on any given day...
#
# Packages: This code checks to see if the packages
# needed are installed. If not, it installs them;
# if so, it prints a little smiley face. :)
# Either way, it loads all the required packages
# when it's done.

P<-c("RCurl","readr","data.table","countrycode","WDI")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# You probably want to run the first 35 lines or so of 
# this file 2-3 times, to make sure everything gets loaded.
#
# Also, be sure to set a working directory in here
# someplace, a la:
#
# setwd("~/AllMyStuff")
#
# or whatever.
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Grab the data...                              ####
#
# Get the relevant data / indicators (add variables
# as you wish...):

wdi<-WDI(country="all",
   indicator=c("LandArea"="AG.LND.TOTL.K2", # Land area (sq. km)
  "ArablePercent"="AG.LND.ARBL.ZS", # Arable Land (% of total land area)
  "Population"="SP.POP.TOTL", # Popluation (in, like, people)
  "PopGrowth"="SP.POP.GROW", # Population Growth (%)
  "RuralPopulation"="SP.RUR.TOTL.ZS", # Rural Population (% of total)
  "UrbanPopulation"="SP.URB.TOTL.IN.ZS", # Urban Population (% of total)
  "BirthRatePer1K"="SP.DYN.CBRT.IN", # Birth Rate (births per 1K people)
  "FertilityRate"="SP.DYN.TFRT.IN", # Fertility Rate (births per woman)
  "PrimarySchoolAge"="SE.PRM.AGES", # Primary school starting age (years)
  "LifeExpectancy"="SP.DYN.LE00.IN", # Life Expectancy at birth (years)
  "AgeDepRatioOld"="SP.POP.DPND.OL", # Age Dependency Ratio (old), % working age population
  "CO2Emissions"="EN.ATM.CO2E.PC", # CO2 Emissions (metric tons per capita)
  "GDP"="NY.GDP.MKTP.KD", # GDP, constant 2010 $US
  "GDPPerCapita"="NY.GDP.PCAP.KD", # GDP per capita (constant 2010 $US)
  "GDPPerCapGrowth"="NY.GDP.PCAP.KD.ZG", # GDP Per Capita Growth (%)
  "Inflation"="FP.CPI.TOTL.ZG", # Inflation (CPI, annual %)
  "TotalTrade"="NE.TRD.GNFS.ZS", # Total trade, % of GDP
  "Exports"="NE.EXP.GNFS.ZS", # Exports, % of GDP
  "Imports"="NE.IMP.GNFS.ZS", # Imports, % of GDP
  "FDIIn"="BX.KLT.DINV.WD.GD.ZS", # FDI in, % of GDP
  "AgriEmployment"="SL.AGR.EMPL.ZS", # Percent of total employment in agriculture
  "NetAidReceived"="DT.ODA.ALLD.KD", # Net official dev. aid received (constant 2018 $US)
  "MobileCellSubscriptions"="IT.CEL.SETS.P2", # Mobile / cellular subscriptions per 100 people
  "NaturalResourceRents"="NY.GDP.TOTL.RT.ZS", # Total natural resource rents (% of GDP)
  "MilitaryExpenditures"="MS.MIL.XPND.GD.ZS", # Military expenditures, % of GDP
  "GovtExpenditures"="NE.CON.GOVT.ZS", # Government Expenditures, % of GDP
  "PublicEdExpend"="SE.XPD.EDUC.ZS", # Public expenditure on education (% of GDP)
  "PublicHealthExpend"="SH.XPD.HLTH.ZS", # Public expenditure on health (% of GDP)
  "HIVDeaths"="SH.DYN.AIDS.DH", # Deaths due to HIV/AIDS (UNAIDS estimate)
  "WomenBusLawIndex"="SG.LAW.INDX", # Women Business & the Law Index Score
  "PaidParentalLeave"="SH.PAR.LEVE.AL")) # Paid Parental Leave (0=no,1=yes)

# Remove aggregates (e.g., "World," "Arab World," etc.):

wdi$ISO3<-countrycode(wdi$iso2c,origin="iso2c",destination="iso3c")
wdi<-wdi[is.na(wdi$ISO3)==FALSE,]

# rename Year:

wdi$Year<-wdi$year
wdi$year<-NULL

# Delete ISO2:

wdi$iso2c<-NULL

# Create a "region" variable

wdi$Region<-countrycode(wdi$ISO3,origin="iso3c",destination="region")

# Put ISO3 + Year + Region at the front of the data:

nc<-ncol(wdi)
sb<-seq(nc-2,nc)
se<-seq(1,(nc-3))
wdi<-wdi[,c(sb,se)]
rm(nc,sb,se)

# Output the file:

write.csv(wdi,"WDI.csv",row.names=FALSE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Additional things...                          ####
#
# Now grab some different variables for the 
# day on causal inference...

wdi2<-WDI(country="all",
         indicator=c("RuralPopulation"="SP.RUR.TOTL.ZS", # Rural Population (% of total)
                     "UrbanPopulation"="SP.URB.TOTL.IN.ZS", # Urban Population (% of total)
                     "BirthRatePer1K"="SP.DYN.CBRT.IN", # Birth Rate (births per 1K people)
                     "FertilityRate"="SP.DYN.TFRT.IN", # Fertility Rate (births per woman)
                     "PrimarySchoolAge"="SE.PRM.AGES", # Primary school starting age (years)
                     "LifeExpectancy"="SP.DYN.LE00.IN", # Life Expectancy at birth (years)
                     "AgeDepRatioOld"="SP.POP.DPND.OL", # Age Dependency Ratio (old), % working age population
                     "ChildMortality"="SH.DYN.MORT", # Under-5 Child Mortality, deaths per 1K 
                     "GDP"="NY.GDP.MKTP.KD", # GDP, constant 2010 $US
                     "GDPPerCapita"="NY.GDP.PCAP.KD", # GDP per capita (constant 2010 $US)
                     "GDPPerCapGrowth"="NY.GDP.PCAP.KD.ZG", # GDP Per Capita Growth (%)
                     "TotalTrade"="NE.TRD.GNFS.ZS", # Total trade, % of GDP
                     "FDIIn"="BX.KLT.DINV.WD.GD.ZS", # FDI in, % of GDP
                     "NetAidReceived"="DT.ODA.ALLD.KD", # Net official dev. aid received (constant 2018 $US)
                     "MobileCellSubscriptions"="IT.CEL.SETS.P2", # Mobile / cellular subscriptions per 100 people
                     "NaturalResourceRents"="NY.GDP.TOTL.RT.ZS", # Total natural resource rents (% of GDP)
                     "GovtExpenditures"="NE.CON.GOVT.ZS", # Government Expenditures, % of GDP
                     "PublicHealthExpend"="SH.XPD.HLTH.ZS", # Public expenditure on health (% of GDP)
                     "WomenInLegislature"="SG.GEN.PARL.ZS", # Pct. Women in National Parliament 
                     "PaidParentalLeave"="SH.PAR.LEVE.AL")) # Paid Parental Leave (0=no,1=yes)

# Remove aggregates (e.g., "World," "Arab World," etc.):

wdi2$ISO3<-countrycode(wdi2$iso2c,origin="iso2c",destination="iso3c")
wdi2<-wdi2[is.na(wdi2$ISO3)==FALSE,]

# rename Year:

wdi2$Year<-wdi2$year
wdi2$year<-NULL

# Delete ISO2:

wdi2$iso2c<-NULL

# Create a "region" variable

wdi2$Region<-countrycode(wdi2$ISO3,origin="iso3c",destination="region")

# Put ISO3 + Year + Region at the front of the data:

nc<-ncol(wdi2)
sb<-seq(nc-2,nc)
se<-seq(1,(nc-3))
wdi2<-wdi2[,c(sb,se)]
rm(nc,sb,se)

# Output the file:

write.csv(wdi2,"WDI2.csv",row.names=FALSE)

# /fin