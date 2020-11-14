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
Netherlands <- filter(wvs[,16:33], country=="Netherlands")
Netherlands <- Netherlands[,1:17]


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
  sex ~ religion
  fraud ~ religion 
  violence ~ religion
  suicide ~ religion

  #variances of latent variables
  religion ~~ 1*religion
 '
#check the model fitting for the Netherlands
sem.Neth <- sem(model.sem, data = Netherlands)
summary(sem.Neth, standardized = TRUE)
fitmeasures(sem.Neth, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#checking for indices to improve
modificationindices(sem.Neth, sort=TRUE)

#calculating the reliability
rel.Neth<-standardizedSolution(sem.Neth)
#sex factor
(sum(rel.Neth[1:5,4]^2))^2/((sum(rel.Neth[1:5,4]^2))^2+sum(1-rel.Neth[1:5,4]^2))
#fraud factor
(sum(rel.Neth[6:10,4]^2))^2/((sum(rel.Neth[6:10,4]^2))^2+sum(1-rel.Neth[6:10,4]^2))
#violence
(sum(rel.Neth[11:13,4]^2))^2/((sum(rel.Neth[11:13,4]^2))^2+sum(1-rel.Neth[11:13,4]^2))
#religion
(sum(rel.Neth[15:17,4]^2))^2/((sum(rel.Neth[15:17,4]^2))^2+sum(1-rel.Neth[15:17,4]^2))

#check the model fitting for Malaysia
sem.Mal <- sem(model.sem, data = Malaysia)
summary(sem.Mal, standardized = TRUE)
fitmeasures(sem.Mal, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#checking for indices to improve
modificationindices(sem.Mal, sort=TRUE)

#calculating the reliability
rel.Mal<-standardizedSolution(sem.Mal)
rel.Mal
#sex factor
(sum(rel.Mal[1:5,4]^2))^2/((sum(rel.Mal[1:5,4]^2))^2+sum(1-rel.Mal[1:5,4]^2))
#fraud factor
(sum(rel.Mal[6:10,4]^2))^2/((sum(rel.Mal[6:10,4]^2))^2+sum(1-rel.Mal[6:10,4]^2))
#violence
(sum(rel.Mal[11:13,4]^2))^2/((sum(rel.Mal[11:13,4]^2))^2+sum(1-rel.Mal[11:13,4]^2))
#religion
(sum(rel.Mal[15:17,4]^2))^2/((sum(rel.Mal[15:17,4]^2))^2+sum(1-rel.Mal[15:17,4]^2))

#we have unacceptable level of reliablilty on religion factor
#so we analyse the correlation matrix to make sure that the indicators do not correlate
cor.Mal <- cor(Malaysia[,15:17])
cor.Mal
