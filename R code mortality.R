
install.packages("tidyverse")
install.packages("survminer")
install.packages("survival")
install.packages("ggpubr")
install.packages("magrittr")

library(survival)
library(ggplot2)
library(survminer)

#### PLOTTING ####

setwd("C:/Users/Kristin/Desktop/Hovedfag") #Setting working directory
mortality.df <- read.table("mortality.october.csv",dec=",",sep=";",header=T) #Importing data

fit <- survfit(Surv(Day.of.death,Censoring)~Treatment, data=mortality.df) #Preparing data

p1 <- ggsurvplot(fit, data=mortality.df, ggtheme = theme_bw(base_size=20), xlab="Days")
p1

#I tried the following syntax without luck

# ggsurvplot(survfit(fit),ggtheme = theme_bw(),data=mortality.df, palette = c("#D81B60", "#1E88E5","#FFC107", "#004D40"), legend.labs = c("Control","Infection","Toxin", "Combined"),legend.title = "Group",censor=FALSE)

# survfit(Surv(Day.of.death,Censoring)~Treatment, data=mortality.df)
#ggsurvplot(fit, mortality.df, axes.offset = FALSE)

#### ANALYSIS ####

#Determining y variable distribution

fit1.surv <- survreg(Surv(Day.of.death,Censoring)~Treatment, dist="weibull", data=mortality.df)
fit2.surv <- survreg(Surv(Day.of.death,Censoring)~Treatment, dist="extreme", data=mortality.df)
fit3.surv <- survreg(Surv(Day.of.death,Censoring)~Treatment, dist="exponential", data=mortality.df)
fit4.surv <- survreg(Surv(Day.of.death,Censoring)~Treatment, dist="gaussian", data=mortality.df)
fit5.surv <- survreg(Surv(Day.of.death,Censoring)~Treatment, dist="logistic", data=mortality.df)
fit6.surv <- survreg(Surv(Day.of.death,Censoring)~Treatment, dist="loglogistic", data=mortality.df)
fit7.surv <- survreg(Surv(Day.of.death,Censoring)~Treatment, dist="lognormal", data=mortality.df)

anova(fit1.surv, fit2.surv, fit3.surv, fit4.surv,fit5.surv,fit6.surv, fit7.surv)

##ANOVA output

#    Terms Resid.  Df    -2*LL  Test Df    Deviance     Pr(>Chi)
#1 Treatment       307 839.3060      NA          NA           NA
#2 Treatment       307 846.6881    =  0  -7.3821366           NA
#3 Treatment       308 883.5411    = -1 -36.8529706 1.273819e-09
#4 Treatment       307 846.3824    =  1  37.1586925 1.088962e-09
#5 Treatment       307 845.9291    =  0   0.4532876           NA
#6 Treatment       307 843.9241    =  0   2.0049756           NA
#7 Treatment       307 861.6174    =  0 -17.6932502           NA

#fit1.surv has the lowest unexplained variability (-2LL=839.3060) -> "weibull" is the best y variable distribution

#Analysis

fit0.surv <- survreg(Surv(Day.of.death,Censoring)~+1, dist="weibull", data=mortality.df)
fit1.surv <- survreg(Surv(Day.of.death,Censoring)~Treatment, dist="weibull", data=mortality.df)

anova(fit0.surv, fit1.surv, test="Chi")

##Chi test output

#   Terms Resid.   Df    -2*LL   Test Df Deviance     Pr(>Chi)
#1        +1       310 880.3981      NA       NA           NA
#2 Treatment       307 839.3060    =  3 41.09217 6.251371e-09

summary(fit1.surv)

##Summary output

#  survreg(formula = Surv(Day.of.death, Censoring) ~ Treatment, data = mortality.df, dist = "weibull")
#                        Value Std. Error     z       p
#(Intercept)               4.456      0.354 12.60 < 2e-16
#Treatmentinfection       -0.520      0.347 -1.50  0.1345
#Treatmenttoxin           -0.903      0.361 -2.50  0.0123
#Treatmenttoxin.infection -1.120      0.349 -3.21  0.0013
#Log(scale)               -0.775      0.102 -7.59 3.3e-14

#Scale = 0.461

#### PREDICTIONS ####

predict(fit1.surv, list(Treatment="control"), type="response") 
#Predicted day of death for control group: 86.15938

predict(fit1.surv, list(Treatment="infection"), type="response") 
#Predicted day of death for infection group: 51.23018

predict(fit1.surv, list(Treatment="toxin"), type="response") 
##Predicted day of death for toxin group: 34.93353

predict(fit1.surv, list(Treatment="toxin.infection"), type="response") 
#Predicted day of death for combined infection and toxin group: #28.10189 


