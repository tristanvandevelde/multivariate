# ---Assignment 1---
# - Question 4 - 
setwd("C:\\Users\\altyn\\Desktop\\KU Leuven\\Multivariate Statistics\\Assignment")
load("wvs.Rdata")
library("lavaan")
library("dplyr")

#database for Malaysia
Malaysia <- filter(wvs[,16:33], country=="Malaysia")
Malaysia <- Malaysia[,1:17]

#database for Netherlands
Neth <- filter(wvs[,16:33], country=="Netherlands")
Neth <- Neth[,1:17]


#building the model
model.sem <- '#measurement model
  sex =~ J_homosexuality + J_prostitution + J_abortion + J_divorce + 
  J_sex_before_marriage
  fraud =~ J_claiming_benefits + J_avoiding_fare + J_stealing_property + 
  J_cheating_taxes + J_accept_bribe
  violence =~ J_beat_wife + J_parents_beating_children + J_violence
  suicide =~J_suicide
  religion =~ NA*R_attend_religious_services + R_pray + R_importance_God
  
  #structural model
  religion ~~ sex + fraud + violence + suicide

  #variances of latent variables
  religion ~~ 1*religion
 '
#check the model fitting for the Netherlands
sem.Neth <- sem(model.sem, data = Neth)
summary(sem.Neth, standardized = TRUE)
fitmeasures(sem.Neth, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#checking for indices to improve
modificationindices(sem.Neth, sort=TRUE)


#check the model fitting for Malaysia
sem.Mal <- sem(model.sem, data = Malaysia)
summary(sem.Mal, standardized = TRUE)
fitmeasures(sem.Mal, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#checking for indices to improve
modificationindices(sem.Mal, sort=TRUE)



