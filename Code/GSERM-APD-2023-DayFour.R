# Introduction  ##########################################
#
# GSERM - St. Gallen (2023)
#
# Analyzing Panel Data
# Prof. Christopher Zorn
#
# Day Four: Causal Inference.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","haven","psych","sandwich","countrycode","wbstats",
     "lme4","plm","gtools","boot","plyr","dplyr","texreg","statmod",
     "plm","tibble","pscl","naniar","ExPanDaR","stargazer","prais",
     "nlme","tseries","pcse","panelView","performance","pgmm","dynpanel",
     "OrthoPanels","dotwhisker","peacesciencer","corrplot","rgenoud",
     "MatchIt","Matching","did","optmatch","Synth","dotwhisker",
     "texreg","cobolt")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# If loading -cobolt- throws an error, use:
#
# install.packages("devtools")
# devtools::install_github("ngreifer/cobalt")
# library(cobalt)

options(scipen = 99) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# setwd yo' damn self, if you care to...
#
# setwd("~/This is a path, my guy")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# World Development Indicators data (again)... ####

# Pull the "alternative" WDI data...

WDI<-read_csv("https://github.com/PrisonRodeo/GSERM-Panel-2023/raw/main/Data/WDI2.csv")

# Add a "Cold War" variable:

WDI$ColdWar <- with(WDI,ifelse(Year<1990,1,0))

# Keep a numeric year variable (for -panelAR-):

WDI$YearNumeric<-WDI$Year

# summary(WDI)
#
# Make the data a panel dataframe:

WDI<-pdata.frame(WDI,index=c("ISO3","Year"))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Descriptive statistics on the WDI data      ####

describe(WDI,fast=TRUE,ranges=FALSE,check=TRUE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Some Regressions    ###########
#
# First, a simple preliminary investigation...
#
# Create logged variable:

WDI$lnCM <- log(WDI$ChildMortality)

# T-test (not reproduced):

t.test(lnCM~PaidParentalLeave,data=WDI)

# Bivariate regression:

BIV<-lm(lnCM~PaidParentalLeave,data=WDI)

# OLS:

OLS<-lm(lnCM~PaidParentalLeave+log(GDPPerCapita)+
             log(NetAidReceived)+GovtExpenditures,data=WDI)

# Fixed Effects... One-way:

FE.1way<-plm(lnCM~PaidParentalLeave+log(GDPPerCapita)+
            log(NetAidReceived)+GovtExpenditures,data=WDI,
            effect="individual",model="within")

# Two-way:

FE.2way<-plm(lnCM~PaidParentalLeave+log(GDPPerCapita)+
         log(NetAidReceived)+GovtExpenditures,data=WDI,
         effect="twoway",model="within")


FE.LDV<-plm(lnCM~PaidParentalLeave+log(GDPPerCapita)+
            log(NetAidReceived)+GovtExpenditures+
            lag(ChildMortality),data=WDI,
            effect="individual",model="within")

# A nice table:

MortTable1 <- stargazer(BIV,OLS,FE.1way,FE.2way,FE.LDV,
                    title="Models of log(Child Mortality)",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,p=c(0.05),
                    dep.var.caption="",omit.stat=c("f","ser"),
                    covariate.labels=c("Paid Parental Leave","ln(GDP Per Capita)",
                                       "ln(Net Aid Received)",
                                       "Government Expenditures",
                                       "Lagged Child Mortality"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="ChildMortTable1.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# INSTRUMENTAL VARIABLES!                           ####
#
# We'll instrument Paid Parental Leave with the
# Women in Legislature variable:

with(WDI,t.test(WomenInLegislature~PaidParentalLeave))

pdf("IVBoxplot.pdf",7,5)
par(mar=c(4,4,2,2))
boxplot(WomenInLegislature~PaidParentalLeave,data=WDI,
        xlab="Paid Parental Leave",
        ylab="Percent of Legislative Seats Held By Women")
dev.off()

# IV regressions (one-way FE and RE):

FE.IV<-plm(lnCM~PaidParentalLeave+log(GDPPerCapita)+
              log(NetAidReceived)+GovtExpenditures |
              .-PaidParentalLeave+WomenInLegislature,
              data=WDI,effect="individual",model="within")


RE.IV<-plm(lnCM~PaidParentalLeave+log(GDPPerCapita)+
             log(NetAidReceived)+GovtExpenditures |
             .-PaidParentalLeave+WomenInLegislature,
             data=WDI,effect="individual",model="random")

# A nice table:

IVTable<-stargazer(OLS,FE.1way,FE.IV,RE.IV,
                   title="IV Models of log(Child Mortality)",
                   column.separate=c(1,1),align=TRUE,
                   dep.var.labels.include=FALSE,p=c(0.05),
                   dep.var.caption="",omit.stat=c("f","ser"),
                   covariate.labels=c("Paid Parental Leave","ln(GDP Per Capita)",
                                      "ln(Net Aid Received)",
                                      "Government Expenditures"),
                   header=FALSE,model.names=FALSE,
                   model.numbers=FALSE,multicolumn=FALSE,
                   object.names=TRUE,notes.label="",
                   out="ChildMortTable2.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Matching!                                         ####
#
# Subset and variables and listwise delete missing data, 
# to simplify things...

vars<-c("ISO3","Year","Region","country","UrbanPopulation",
        "FertilityRate","PrimarySchoolAge","ChildMortality",
        "GDPPerCapita","NetAidReceived","NaturalResourceRents",
        "GovtExpenditures","PaidParentalLeave","ColdWar",
        "lnCM")
wdi<-WDI[vars]
wdi<-na.omit(wdi)

# Create discrete-valued variables (i.e., coarsen) for
# matching on continuous predictors:

wdi$GDP.Decile<-as.factor(ntile(wdi$GDPPerCapita,10))
wdi$Aid.Decile<-as.factor(ntile(wdi$NetAidReceived,10))
wdi$GSpend.Decile<-as.factor(ntile(wdi$GovtExpenditures,10))

# Pre-match balance statistics...

BeforeBal<-bal.tab(PaidParentalLeave~GDP.Decile+
                Aid.Decile+GSpend.Decile,data=wdi,
                stats=c("mean.diffs","ks.statistics"))

# Plot balance:

pdf("PreMatchBalance.pdf",5,6)
plot(BeforeBal)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Exact Matching:

M.exact <- matchit(PaidParentalLeave~GDP.Decile+Aid.Decile+
                  GSpend.Decile,data=wdi,method="exact")
summary(M.exact)

# Plot balance...

ExactBal<-bal.tab(M.exact,un=TRUE)

pdf("ExactMatchBal.pdf",5,6)
plot(ExactBal)
dev.off()

# Create matched data:

wdi.exact <- match.data(M.exact,group="all")
dim(wdi.exact)

# Model for propensity scores:

PS.fit<-glm(PaidParentalLeave~GDP.Decile+Aid.Decile+
              GSpend.Decile,data=wdi,
              family=binomial(link="logit"))

# Generate scores & check common support:

PS.df<-data.frame(PS = predict(PS.fit,type="response"),
                  PaidParentalLeave=PS.fit$model$PaidParentalLeave)

pdf("PS-Mort-Support.pdf",7,5)
par(mar=c(4,4,2,2))
with(PS.df[PS.df$PaidParentalLeave==0,],
     plot(density(PS),main="Propensity Score Balance",
          lwd=2,xlab="Propensity Score",xlim=c(0,1)))
with(PS.df[PS.df$PaidParentalLeave==1,],
     lines(density(PS),lwd=2,lty=2,col="red"))
legend("topright",bty="n",col=c("black","red"),
       lwd=2,lty=c(1,2),legend=c("No Paid Parental Leave",
                                 "Paid ParentalLeave"))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Propensity score matching:

M.prop <- matchit(PaidParentalLeave~GDP.Decile+Aid.Decile+
                  GSpend.Decile,data=wdi,method="nearest",
                  ratio=3)
summary(M.prop)

# Balance check:

PSBal<-bal.tab(M.prop,un=TRUE)

pdf("PSMatchBal.pdf",5,6)
plot(PSBal,drop.distance=TRUE)
dev.off()

# Matched data:

wdi.ps <- match.data(M.prop,group="all")
dim(wdi.ps)


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Optimal matching:

M.opt <- matchit(PaidParentalLeave~GDP.Decile+Aid.Decile+
                    GSpend.Decile,data=wdi,method="optimal",
                  ratio=3)
summary(M.opt)

# Matched data:

wdi.opt <- match.data(M.opt,group="all")
dim(wdi.opt)

# Balance check:

OptBal<-bal.tab(M.opt,un=TRUE)

pdf("OptMatchBal.pdf",5,6)
plot(OptBal,drop.distance=TRUE)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Regressions (before and) after matching... ####

PreMatch.FE<-plm(lnCM~PaidParentalLeave+log(GDPPerCapita)+
             log(NetAidReceived)+GovtExpenditures,data=wdi,
             effect="individual",model="within")

Exact.FE<-plm(lnCM~PaidParentalLeave+log(GDPPerCapita)+
              log(NetAidReceived)+GovtExpenditures,data=wdi.exact,
              effect="individual",model="within")

PS.FE<-plm(lnCM~PaidParentalLeave+log(GDPPerCapita)+
                 log(NetAidReceived)+GovtExpenditures,data=wdi.ps,
                 effect="individual",model="within")

Optimal.FE<-plm(lnCM~PaidParentalLeave+log(GDPPerCapita)+
                log(NetAidReceived)+GovtExpenditures,data=wdi.opt,
                effect="individual",model="within")

# A table...

MatchMortTable <- stargazer(PreMatch.FE,Exact.FE,PS.FE,Optimal.FE,
                    title="Models of log(Child Mortality)",
                    column.separate=c(1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Paid Parental Leave","ln(GDP Per Capita)",
                                       "ln(Net Aid Received)",
                                       "Government Expenditures"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    column.sep.width="1pt",
                    omit.stat=c("f","ser"),out="MatchMortRegs.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Simple DiD...   ####
#
# Pull out *only* those countries that, at some
# point during the observed periods, instituted
# a paid parental leave policy:

PPLs<-WDI
PPLs<-PPLs %>% group_by(ISO3) %>%
  filter(any(PaidParentalLeave==1))

# Plot the trends by value of PaidParentalLeave: 

pdf("PPL-Plot.pdf",7,5)
par(mar=c(4,4,2,2))
with(PPLs[PPLs$PaidParentalLeave==0,],
               plot(YearNumeric,log(ChildMortality),
               pch=20,col="darkorange",xlab="Year",
               ylim=c(0.7,5.3)))
with(PPLs[PPLs$PaidParentalLeave==1,],
               points(YearNumeric,log(ChildMortality),
               pch=17,col="forestgreen"))
legend("topright",bty="n",pch=c(20,17),
       col=c("darkorange","forestgreen"),
       legend=c("No Paid Parental Leave",
                "Paid Parental Leave"))
dev.off()

# Create a better trend variable:

PPLs$Time<-PPLs$YearNumeric-1950

# REGRESSION TIME 

DID.OLS1<-lm(ChildMortality~PaidParentalLeave+Time+
              PaidParentalLeave*Time,data=PPLs)

DID.OLS2<-lm(ChildMortality~PaidParentalLeave+Time+
             PaidParentalLeave*Time+log(GDPPerCapita)+
             log(NetAidReceived)+GovtExpenditures,
             data=PPLs)

# FE models...

# PPLs<-pdata.frame(PPLs,index=c("ISO3","Year")) # make panel data

DID.1way.1<-plm(ChildMortality~PaidParentalLeave+Time+
               PaidParentalLeave*Time,data=PPLs,
              effect="individual",model="within")

DID.1way.2<-plm(ChildMortality~PaidParentalLeave+Time+
               PaidParentalLeave*Time+log(GDPPerCapita)+
               log(NetAidReceived)+GovtExpenditures,
               data=PPLs,effect="individual",model="within")

DID.2way.1<-plm(ChildMortality~PaidParentalLeave+Time+
                  PaidParentalLeave*Time,data=PPLs,
                effect="twoway",model="within")

DID.2way.2<-plm(ChildMortality~PaidParentalLeave+Time+
                  PaidParentalLeave*Time+log(GDPPerCapita)+
                  log(NetAidReceived)+GovtExpenditures,
                data=PPLs,effect="twoway",model="within")

# TABLE TIME...

DiDMortTable<-stargazer(DID.OLS1,DID.OLS2,DID.1way.1,DID.1way.2,
                        DID.2way.1,DID.2way.2,
                        title="DiD Models of log(Child Mortality)",
                        column.separate=c(1,1,1),align=TRUE,
                        dep.var.labels.include=FALSE,
                        dep.var.caption="",
                        covariate.labels=c("Paid Parental Leave","Time (1950=0)",
                                           "Paid Parental Leave x Time",
                                           "ln(GDP Per Capita)",
                                           "ln(Net Aid Received)",
                                           "Government Expenditures"),
                        header=FALSE,model.names=FALSE,
                        model.numbers=FALSE,multicolumn=FALSE,
                        object.names=TRUE,notes.label="",
                        column.sep.width="-15pt",order=c(1,2,6,3,4,5),
                        omit.stat=c("f","ser"),out="DiDMortRegs.tex")

# Alternative:

texreg(list(DID.OLS1,DID.OLS2,DID.1way.1,DID.1way.2,
            DID.2way.1,DID.2way.2),file="DiDMortRegs.tex",
            caption="",label="")

# /fin