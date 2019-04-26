

#clean up
rm(list=ls())

#set preferences
options(scipen=5,digits=6)

#set working directory (CHANGE THIS TO YOUR DIRECTORY)
setwd('Desktop/CS112 FP/')
#install packages
install.packages("combinat")
install.packages("tcltk")
install.packages("cem",repos="http://r.iq.harvard.edu", type="source")
install.packages("MatchIt")
install.packages("RItools")
install.packages("MNP")

#load libraries
library(foreign)
library(cem)
library(Matching)
library(MatchIt)
library(RItools)
library(MNP)
library(xtable)

#load data
preVote.0<-read.dta('debtCeilingPreOutcome.dta')

#create numeric party variable
preVote.0$dem<-as.numeric(preVote.0$party=="D")

#create variable for "no" vote
preVote.0$no<-abs(1-preVote.0$yes_ceiling)

#delete unobserved values
preVote<-subset(preVote.0, !is.na(yes_ceiling) & intermediate<6 & !is.na(nominate1))

#descriptive stats
summary(preVote)
table(preVote.0$intermediate)
100*table(preVote.0$intermediate)/sum(table(preVote.0$intermediate))
table(preVote$intermediate)
100*table(preVote$intermediate)/sum(table(preVote$intermediate))

#MATCH ALL OBSERVATIONS FOR WHETHER A SEAT WAS RETAINED
imbalance(group=preVote$no,data=preVote[c('nominate1','obama08','inc_vote10','log_cash')])
todrop<-c('name','abbr','state','olddist','newdist','stcd_old','intermediate','abs_nom','dem_vote10','inc_dem','unop10','cash_july11','party','dem','yes_ceiling')
full.match <- cem(treatment = "no", data = preVote, drop = todrop);full.match

#MATCH ONLY INCUMBENTS COMPETING IN THE GENERAL ELECTION
genElec<-subset(preVote, intermediate==1)
imbalance(group=genElec$no,data=genElec[c('nominate1','obama08','inc_vote10','log_cash')])
share.match <- cem(treatment = "no", data = genElec, drop = todrop);share.match

#multinomial probit of intermediate outcomes 
intermed.mnp<-mnp(intermediate~abs_nom+inc_vote10+obama08+log_cash+no,data=preVote); summary(intermed.mnp)
xtable(summary(intermed.mnp)$coef.table,digits=4)
xtable(summary(intermed.mnp)$cov.table,digits=4)

#Generate a complete list of matches for each case
names(full.match)

preVote.match<-subset(preVote,full.match$matched==TRUE)
full.mat<-cbind(preVote.match$name,preVote.match$abbr,preVote.match$olddist,preVote.match$no)
sideways<-cbind(full.mat[1:59,],full.mat[60:118,],rbind(full.mat[119:175,],0,0))
print(xtable(sideways),include.rownames=FALSE)

genElec.match<-subset(genElec,share.match$matched==TRUE)
elec.mat<-cbind(genElec.match$name,genElec.match$abbr,genElec.match$olddist,genElec.match$no)
sideways.2<-cbind(elec.mat[1:43,],elec.mat[44:86,],elec.mat[87:129,])
print(xtable(sideways.2),include.rownames=FALSE)

