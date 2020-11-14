################################################################
################  Multivariate Statistics  #####################
################       Assignment 1        #####################
################################################################

#CFA

packages_used = c( "tidyverse", "data.table", "clusterSim","sqldf", "xtable", "lavaan","psych")


for(i in packages_used){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in packages_used){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) } 

memory.limit(size=8000000) 
options(scipen = 999)
rm(list=ls())

#setwd("")
#getwd()

load("./data/wvs.Rdata")
glimpse(wvs)

#16-29 Malaysia Netherlands 
#conduct confirmatory factor analysis on the covariance matrix of the justifiability items

data_cfa <- sqldf("SELECT * FROM wvs WHERE country == 'Malaysia' OR country =='Netherlands'")
data_cfa <- data_cfa[,c(16:29,33)]

#16 J_claiming_benefits         Claiming government benefits to which you are not entitled
#17 J_avoiding_fare             Avoiding a fare on public transport 
#18 J_stealing_property         Stealing property 
#19 J_cheating_taxes            Cheating on taxes if you have a chance 
#20 J_accept_bribe              Someone accepting a bribe in the course of their duties
#21 J_homosexuality             Homosexuality 
#22 J_prostitution               Prostitution 
#23 J_abortion                   Abortion 
#24 J_divorce                    Divorce
#25J_sex_before_marriage      Sex before marriage 
#26 J_suicide                    Suicide 
#27 J_beat_wife                  For a man to beat his wife 
#28J_parents_beating_children Parents beating children 
#29 J_violence                   Violence against other people

data_mala <- data_cfa[data_cfa$country == "Malaysia",]
data_neth <- data_cfa[data_cfa$country == "Netherlands",]

#covariance matrix and correlation matrix
covmat<-cov(data_cfa[,1:14])

#cor. matrix for comparison
cormat<-cor(data_cfa[,1:14])
print(covmat)

#Basic EFA
efa_obl3<-fa(cormat,covar=FALSE,3,rotate="oblimin",fm="ml",n.obs=2473)
print(efa_obl3)

round(efa_obl3$communalities,3)

print(efa_obl3$Structure,cutoff=0,digits=3)

round(efa_obl3$residual,3)

#Similarly to the main EFA, suicide is problematic


#Estimation of the CFA model, checking fit measures and looking for possible modifications

cfa4<-'fraudulent=~ NA*J_claiming_benefits + J_avoiding_fare + J_stealing_property + 
                   J_cheating_taxes + J_accept_bribe
       sexual =~ NA*J_homosexuality + J_prostitution + 
                 J_abortion + J_divorce + J_sex_before_marriage
       violence =~ NA*J_beat_wife + J_parents_beating_children + J_violence
       suicide =~ NA*J_suicide
       fraudulent ~~ 1*fraudulent
       suicide ~~ 1*suicide
       sexual ~~ 1*sexual
       violence  ~~ 1*violence
       suicide ~~ sexual
       suicide ~~ violence
       suicide ~~ fraudulent
       fraudulent ~~sexual 
       fraudulent ~~violence
       sexual~~violence'

fitcfa4<-cfa(cfa4,sample.cov=covmat,sample.nobs=2473)
summary(fitcfa4, fit.measures=TRUE)

cfa_m <- fitmeasures(fitcfa4,c("chisq","df","cfi","tli","rmsea","srmr"))
print(cfa_m)

mi <- modificationIndices(fitcfa4, sort = TRUE, maximum.number = 15 )
print(mi)
xtable(mi)


#Estimation of the CFA model, checking fit measures and looking for possible modifications


cfa4_1<-'fraudulent=~ NA*J_claiming_benefits + J_avoiding_fare + J_stealing_property + 
                   J_cheating_taxes + J_accept_bribe
       sexual =~ NA*J_homosexuality + J_prostitution + 
                 J_abortion + J_divorce + J_sex_before_marriage
       violence =~ NA*J_beat_wife + J_parents_beating_children + J_violence
       suicide =~ NA*J_suicide
       fraudulent ~~ 1*fraudulent
       suicide ~~ 1*suicide
       sexual ~~ 1*sexual
       violence  ~~ 1*violence
       suicide ~~ sexual
       suicide ~~ violence
       suicide ~~ fraudulent
       fraudulent ~~sexual 
       fraudulent ~~violence
       sexual~~violence
       J_homosexuality ~~ J_sex_before_marriage'



fitcfa4_1<-cfa(cfa4_1,sample.cov=covmat,sample.nobs=2473)

cfa_m_1 <- fitmeasures(fitcfa4_1,c("chisq","df","cfi","tli","rmsea","srmr"))
print(cfa_m_1)

modificationIndices(fitcfa4_1, sort = TRUE, maximum.number = 20 )




cfa4_2 <- 'fraudulent=~ NA*J_claiming_benefits + J_avoiding_fare + J_stealing_property + 
                   J_cheating_taxes + J_accept_bribe
       sexual =~ NA*J_homosexuality + J_prostitution + 
                 J_abortion + J_divorce + J_sex_before_marriage
       violence =~ NA*J_beat_wife + J_parents_beating_children + J_violence
       suicide =~ NA*J_suicide
       fraudulent ~~ 1*fraudulent
       suicide ~~ 1*suicide
       sexual ~~ 1*sexual
       violence  ~~ 1*violence
       suicide ~~ sexual
       suicide ~~ violence
       suicide ~~ fraudulent
       fraudulent ~~sexual 
       fraudulent ~~violence
       sexual~~violence
       J_homosexuality ~~ J_sex_before_marriage
       J_divorce ~~      J_sex_before_marriage'

fitcfa4_2<-cfa(cfa4_2,sample.cov=covmat,sample.nobs=2473)

cfa_m_2 <- fitmeasures(fitcfa4_2,c("chisq","df","cfi","tli","rmsea","srmr"))
print(cfa_m_2)

modificationIndices(fitcfa4_2, sort = TRUE, maximum.number = 20 )

#comparing these 3 models

combined_m <- rbind(cfa_m,cfa_m_1,cfa_m_2)
rownames(combined_m)<-c("Base CFA model","CFA model with 1 additional correlated error term", "CFA model with 2 additional correlated error term")
combined_m <- round(combined_m,3)
print(combined_m)
xtable(combined_m, digits = 3)


#Standarized solutions
std_sol <- standardizedSolution(fitcfa4)
print(std_sol)
xtable(std_sol)


#Reliabilities: acceptable above 0.7
reliab <- std_sol$est.std[1:12]^2
reliab <- as.data.frame(reliab)

row.names(reliab) <- c("J_claiming_benefits", "J_avoiding_fare", "J_stealing_property", "J_cheating_taxes", "J_accept_bribe" ,
                       "J_homosexuality", "J_prostitution", "J_abortion", "J_divorce", "J_sex_before_marriage", "J_beat_wife",
                       "J_parents_beating_children")
print(reliab)
xtable(reliab)


#Multigroup analysis


#Configural measurement invariance model
config_inv<-cfa(cfa4,data=data_cfa,group="country")

summary(config_inv,fit.measures=TRUE)
standardizedSolution(config_inv)


#Metric measurement invariance model
metric_inv<-cfa(cfa4,data=data_cfa,group="country",group.equal="loadings")
summary(metric_inv,fit.measures=TRUE)
standardizedSolution(metric_inv)

#Strong measurement invariance model
strong_inv<-cfa(cfa4,data=data_cfa,group="country",group.equal=c("loadings","intercepts"))
summary(strong_inv,fit.measures=TRUE)
standardizedSolution(strong_inv)

#L-R ratio test
xtable(anova(config_inv,metric_inv), digits = 3)
xtable(anova(config_inv,strong_inv), digits = 3)
anova(config_inv,metric_inv)



#Summarizing measures
fitconfig_inv<-fitmeasures(config_inv,c("chisq","df","cfi","tli","rmsea","srmr","bic","aic"))
fitmetric_inv<-fitmeasures(metric_inv,c("chisq","df","cfi","tli","rmsea","srmr","bic","aic"))
fitstrong_inv<-fitmeasures(strong_inv,c("chisq","df","cfi","tli","rmsea","srmr","bic","aic"))


combined_inv<-rbind(fitconfig_inv,fitmetric_inv,fitstrong_inv)
rownames(combined_inv)<-c("configural","metric","strong")

chidf<-combined_inv[,1]/fit1[,2]

combined_inv<-cbind(combined_inv,chidf)
print(combined_inv)

xtable(round(combined_inv,3), digits = 3)










