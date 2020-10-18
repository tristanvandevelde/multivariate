# ---Assignment 1---
# - Question 4 - 
#setwd("C:\\Users\\altyn\\Desktop\\KU Leuven\\Multivariate Statistics\\Assignment")
#load("wvs.Rdata")
load("/Users/tristanvandevelde/Documents/Dev/multivariate/assignment1/data/wvs.Rdata")

library("dbplyr")
library("tidyverse")
library("MASS")
library("heplots")
library("lavaan")
sem1 <- '#measurement model
  justification =~J_claiming_benefits + J_stealing_property + J_cheating_taxes + J_accept_bribe + J_homosexuality + J_prostitution + J_abortion + J_divorce + J_sex_before_marriage + J_suicide+ J_beat_wife + J_parents_beating_children + J_violence
  religion =~ R_attend_religious_services + R_pray + R_importance_God
  
  #structural model
  justification ~~ religion
  
  #variances latent variables
  justification ~~ justification
  religion ~~ 1*religion'

#Netherlands
Neth <- filter(wvs[,16:33], country=="Netherlands")
#estimate model
fitsem1<- sem(sem1, Neth[,1:17])
summary(fitsem1, fit.measure=T)
standardizedSolution(fitsem1)

#Malasia
Malas <- filter(wvs[,16:33], country=="Malaysia")
#estimate model 
fitsem2<- sem(sem1, Malas[,1:17])
summary(fitsem2, fit.measures=T)
standardizedSolution(fitsem2)



