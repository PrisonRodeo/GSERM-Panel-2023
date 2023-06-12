#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                      ####
#
# GSERM - St. Gallen (2023)
#
# Analyzing Panel Data
# Prof. Christopher Zorn
#
# Day Two: "Unit Effects" models.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This code takes a list of packages ("P") and (a) checks for whether
# the package is installed or not, (b) installs it if it is not, and 
# then (c) loads each of them:


P<-c("RCurl","readr","haven","colorspace","foreign","psych",
     "lme4","plm","gtools","boot","plyr","dplyr","texreg","statmod",
     "plm","tibble","pscl","naniar","ExPanDaR","stargazer","prais",
     "nlme","tseries","pcse","panelView","performance")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run it a few times to make it work...
#
# R Options:

options(scipen = 8) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# setwd("~/Dropbox (Personal)/GSERM/Panel-2023/Notes and Slides")
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# FE plot:

i<-1:4
t<-20
NT<-t*max(i)
set.seed(7222009)
df<-data.frame(i=rep(i,t),
               t=rep(1:t,max(i)),
               X=runif(NT))
df$Y=1+2*i+4*df$X+runif(NT)
df<-df[order(df$i,df$t),] # sort

pdf("FEIntuition.pdf",7,6)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=i+14,col=i,
              xlim=c(0,1),ylim=c(3,14)))
abline(a=3.5,b=4,lwd=2,lty=1,col=1)
abline(a=5.5,b=4,lwd=2,lty=2,col=2)
abline(a=7.5,b=4,lwd=2,lty=3,col=3)
abline(a=9.5,b=4,lwd=2,lty=4,col=4)
legend("bottomright",bty="n",col=1:4,lty=1:4,
       lwd=2,legend=c("i=1","i=2","i=3","i=4"))
dev.off()

# World Development Indicators (WDI) data ####
#
# The WDI data we'll be using for this class can
# be accessed by running the "WDI-MakeData.R"
# script found on the Github repository. You can 
# modify that script to add additional variables
# if you choose to. The data are also available
# in ready-to-use format in the "Data" folder
# on the Github repo.
#
# Get the data:

wdi<-read_csv("https://github.com/PrisonRodeo/GSERM-Panel-2023/raw/main/Data/WDI.csv")

# Add a "Cold War" variable:

wdi$ColdWar <- with(wdi,ifelse(Year<1990,1,0))

# Summarize:

describe(wdi,fast=TRUE,ranges=FALSE,check=TRUE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Visualizing WDI data...                           ####

pdf("PanelWBLIViz.pdf",7,5)
panelview(WomenBusLawIndex~1,data=wdi,theme.bw=TRUE,
          outcome.type="continuous",type="outcome",
          by.timing=TRUE,index=c("ISO3","Year"),
          main=" ",ylab="Women's Business Law Index",
          legendOff=TRUE)
dev.off()

pdf("PanelPLeaveViz.pdf",7,5)
panelview(WomenBusLawIndex~PaidParentalLeave,data=wdi,theme.bw=TRUE,
          by.timing=FALSE,index=c("ISO3","Year"),
          color=c("orange","darkgreen"),
          legend.labs=c("No Paid Leave","Paid Leave"),
          main=" ",ylab="Country Code",axis.lab.gap=c(5,5),
          background="white")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Variation: Total, within, and between               ####
#
# Create "panel series"...

WDI<-pdata.frame(wdi)
WBLI<-WDI$WomenBusLawIndex
class(WBLI)

describe(WBLI,na.rm=TRUE) # all variation

pdf("WBLIAll.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(WDI$WomenBusLawIndex,na.rm=TRUE),
     main="",xlab="WBL Index",lwd=2)
abline(v=mean(WDI$WomenBusLawIndex,na.rm=TRUE),
       lwd=1,lty=2)
dev.off()

# "Between" variation:

describe(plm::between(WBLI,effect="individual",na.rm=TRUE)) # "between" variation

WBLIMeans<-plm::between(WBLI,effect="individual",na.rm=TRUE)

WBLIMeans<-ddply(WDI,.(ISO3),summarise,
                 WBLIMean=mean(WomenBusLawIndex,na.rm=TRUE))

pdf("WBLIBetween.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(WBLIMeans$WBLIMean,na.rm=TRUE),
     main="",xlab="Mean WBLI",lwd=2)
abline(v=mean(WBLIMeans$WBLIMean,na.rm=TRUE),
       lwd=1,lty=2)
dev.off()

# "Within" variation:

describe(Within(WBLI,na.rm=TRUE)) # "within" variation

pdf("WBLIWithin.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(Within(WBLI,na.rm=TRUE),na.rm=TRUE),
     main="",xlab="WBLI: Within-Country Variation",
     lwd=2)
abline(v=0,lty=2)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Regression! One-Way Unit Effects models               ####

# Subset (just for descriptives):

vars<-c("WomenBusLawIndex","PopGrowth","UrbanPopulation","FertilityRate",
                "GDPPerCapita","NaturalResourceRents","ColdWar")
subset<-WDI[vars]
subset<-subset[complete.cases(subset),] # listwise deletion
subset$lnGDPPerCap<-log(subset$GDPPerCapita)
subset$GDPPerCapita<-NULL
describe(subset,fast=TRUE)

# Pooled OLS:

OLS<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                 log(GDPPerCapita)+NaturalResourceRents+ColdWar, 
        data=WDI,model="pooling")

summary(OLS)

# "Fixed" / within effects:

FE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
        log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
        effect="individual",model="within")

summary(FE)

# Make a table:

Table1 <- stargazer(OLS,FE,
                    title="Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="UFX11.tex")

# Time-period Fixed Effects:

FE.Time<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
         log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
         effect="time",model="within")

# summary(FET)

# A comparison table:

FE.Units <- FE

CompFETable <- stargazer(FE.Units,FE.Time,
                    title="FE Models of WBLI (Units vs. Time)",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="FEComp1.tex")

# Test for \alpha_i = 0:

pFtest(FE,OLS)
plmtest(FE,effect=c("individual"),type=c("bp"))
plmtest(FE,effect=c("individual"),type=c("kw"))

pFtest(FE.Time,OLS)
plmtest(FE.Time,effect=c("time"),type=c("bp"))
plmtest(FE.Time,effect=c("time"),type=c("kw"))

#=-=-=-=-=-=-=-=-=-=-=-=
# Interpretation...

with(WDI, sd(UrbanPopulation,na.rm=TRUE)) # all variation

WDI<-ddply(WDI, .(ISO3), mutate,
               UPMean = mean(UrbanPopulation,na.rm=TRUE))
WDI$UPWithin<-with(WDI, UrbanPopulation-UPMean)

with(WDI, sd(UPWithin,na.rm=TRUE)) # "within" variation

#=-=-=-=-=-=-=-=-=-=-=-=-=
# Between effects:

BE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
        effect="individual",model="between")

summary(BE)

Table2 <- stargazer(OLS,FE,BE,
                    title="Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="UFX21.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=
# Random effects:

RE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
        effect="individual",model="random")

summary(RE)

Table3 <- stargazer(OLS,FE,BE,RE,
                    title="Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="UFX31.tex")

# Hausman test:

phtest(FE, RE)  # ugh...

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# A bit of HLMs...                                  ####

AltRE<-lmer(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
            log(GDPPerCapita)+NaturalResourceRents+ColdWar+(1|ISO3),
            data=WDI)

summary(AltRE)

# Are they the same?

TableHLM <- stargazer(RE,AltRE,
                    title="RE and HLM Models of WBLI",
                    column.separate=c(1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="HLMTable1.tex")

# ... yes. More or less.
#
# More HLM fun...

HLM1<-lmer(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
            log(GDPPerCapita)+NaturalResourceRents+ColdWar+(ColdWar|ISO3),
            data=WDI,control=lmerControl(optimizer="bobyqa"))

summary(HLM1)


# Testing:

anova(AltRE,HLM1)
VarCorr(HLM1)

# Get some of those sweeeet random slopes:

Bs<-data.frame(coef(HLM1)[1])

head(Bs)
mean(Bs$ISO3..Intercept.)
mean(Bs$ISO3.ColdWar)

# Plots:

pdf("WBLI-RandomIntercepts.pdf",6,5)
par(mar=c(4,4,2,2))
with(Bs, plot(density(ISO3..Intercept.),lwd=3,
              main="",xlab="Intercept Values"))
abline(v=mean(Bs$ISO3..Intercept.),lty=2)
dev.off()

pdf("WBLI-CWRandomSlopes.pdf",6,5)
par(mar=c(4,4,2,2))
with(Bs, plot(density(ISO3.ColdWar),lwd=3,
              main="",xlab="Cold War Slopes"))
abline(v=mean(Bs$ISO3.ColdWar),lty=2)
dev.off()

REcorr<-with(Bs,cor(ISO3..Intercept.,ISO3.ColdWar))

pdf("WBLI-HLMScatter.pdf",6,5)
par(mar=c(4,4,2,2))
with(Bs, plot(ISO3..Intercept.,ISO3.ColdWar,
              pch=19,main="",xlab="Intercept Values",
              ylab="Cold War Slopes"))
text(-90,-25,cex=1.2,
     labels=paste0("r = ",round(REcorr,3)))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Separating "within" and "between" effects           ####
# WBLI and Natural Resources:

WDI<-ddply(WDI,.(ISO3),mutate,
             NRR.Between=mean(NaturalResourceRents,na.rm=TRUE))
WDI$NRR.Within<- (WDI$NaturalResourceRents - WDI$NRR.Between) 

WEBE.OLS<-lm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
           log(GDPPerCapita)+NRR.Within+NRR.Between+ColdWar,
           data=WDI)

summary(WEBE.OLS)

# Nice table:

Table4 <- stargazer(WEBE.OLS,
                    title="BE + WE Model of WBLI",
                    column.separate=c(1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Within-Country Nat. Resource Rents",
                                       "Between-Country Nat. Resource Rents",
                                       "Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="WEBE1.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Two-way effects                                     ####

TwoWayFE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
              log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
              effect="twoway",model="within")

summary(TwoWayFE)


# Testing...
#
# Two-way effects:

pFtest(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
       log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
       effect="twoway",model="within")
plmtest(TwoWayFE,c("twoways"),type=("kw"))

# One-way effects in the two-way model:

plmtest(TwoWayFE,c("individual"),type=("kw"))
plmtest(TwoWayFE,c("time"),type=("kw"))

# Equivalence to -lm-:

TwoWayFE.BF<-lm(WomenBusLawIndex~PopGrowth+UrbanPopulation+
                FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
                factor(ISO3)+factor(Year),data=WDI)

summary(TwoWayFE.BF)

# Two-way random effects:

TwoWayRE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
              log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
              effect="twoway",model="random")

summary(TwoWayRE)

# Here's a nicer table:

Table5 <- stargazer(OLS,FE,BE,RE,TwoWayFE,TwoWayRE,
                    title="Models of WBLI",
                    column.separate=c(1,1,1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    column.sep.width="1pt",
                    omit.stat=c("f"),out="UFX51.tex")

# /fin