library(MASS)
library(ggplot2)
library(ggcorrplot)
library(xtable)
library(psych)
library(GPArotation)
library(maptools)
#load("data/wvs.Rdata")
load("/Users/tristanvandevelde/Documents/Dev/multivariate/assignment1/data/wvs.Rdata")

#### PREPROCESSING
##################

# standardize 14 justifiability items: J_x
justifiability_stand <- data.frame(wvs[c(16:29,33)])
justifiability_stand$J_claiming_benefits <- scale(justifiability_stand$J_claiming_benefits, center = TRUE, scale = TRUE)
justifiability_stand$J_avoiding_fare <- scale(justifiability_stand$J_avoiding_fare, center = TRUE, scale = TRUE)
justifiability_stand$J_stealing_property <- scale(justifiability_stand$J_stealing_property, center = TRUE, scale = TRUE)
justifiability_stand$J_cheating_taxes <- scale(justifiability_stand$J_cheating_taxes, center = TRUE, scale = TRUE)
justifiability_stand$J_accept_bribe <- scale(justifiability_stand$J_accept_bribe, center = TRUE, scale = TRUE)
justifiability_stand$J_homosexuality <- scale(justifiability_stand$J_homosexuality, center = TRUE, scale = TRUE)
justifiability_stand$J_prostitution <- scale(justifiability_stand$J_prostitution, center = TRUE, scale = TRUE)
justifiability_stand$J_abortion <- scale(justifiability_stand$J_abortion, center = TRUE, scale = TRUE)
justifiability_stand$J_divorce <- scale(justifiability_stand$J_divorce, center = TRUE, scale = TRUE)
justifiability_stand$J_sex_before_marriage <- scale(justifiability_stand$J_sex_before_marriage, center = TRUE, scale = TRUE)
justifiability_stand$J_suicide <- scale(justifiability_stand$J_suicide, center = TRUE, scale = TRUE)
justifiability_stand$J_beat_wife <- scale(justifiability_stand$J_beat_wife, center = TRUE, scale = TRUE)
justifiability_stand$J_parents_beating_children <- scale(justifiability_stand$J_parents_beating_children, center = TRUE, scale = TRUE)
justifiability_stand$J_violence <- scale(justifiability_stand$J_violence, center = TRUE, scale = TRUE)


#### EFA MODELLING
##################


### STEP 0: SOME EXPLORATION
cormat <- cor(justifiability_stand[1:14])
round(cormat,2)
ggcorrplot(cormat)

### STEP 1A: FIT PCA TO DETERMINE AMOUNT OF FACTORS
pca_justifiability <- prcomp(justifiability_stand[c(1:14)])
screeplot(pca_justifiability, type="lines")
abline(h=1, col="blue", lty=2)
abline(v=4, col="red", lty=2)
# both scree plot and kaisers criterium indicate te retain 3 facts
# Consistent with our intuition (3 factors: financial, sexual, violence)
### STEP 1B: COMPUTE PERCENTAGE OF NON-REDUNDANT RESIDUAL CORRELATIONS
# 2 factors
nonred_resid_corr <- function(f) {
  efa_model <- factanal(justifiability_stand[1:14], factors=f)
  resid <-cormat - (crossprod(t(efa_model$loadings))+diag(efa_model$uniquenesses))
  n<-sum(ifelse(abs(resid)>0.05,1,0))/2
  print(n/(14*13/2))
}
for (i in c(1:7))
{
  print(i)
  nonred_resid_corr(i)
}
# 3 factors


### STEP 2: FIT MODELS
### NO ROTATION
# no rotation 3
efa_just_3_none <- factanal(justifiability_stand[1:14], factors=3)
efa_just_3_none
# differs significantly from perfectly fitting model

# no rotation 4
efa_just_4_none <- factanal(justifiability_stand[1:14], factors=4)
efa_just_4_none
# differs significantly from perfectly fitting model

# no rotation 7
efa_just_7_none <- factanal(justifiability_stand[1:14], factors=7)
efa_just_7_none
# differs significantly from perfectly fitting model

# no rotation 9
efa_just_9_none <- factanal(justifiability_stand[1:14], factors=9)
efa_just_9_none
# differs significantly from perfectly fitting model

### ORTHOGONAL

# orthogonal 3
efa_just_3_orthogonal <- factanal(justifiability_stand[1:14], factors=3, rotation = "varimax")
efa_just_3_orthogonal
# differs significantly from  perfectly fitting model
#efa_just_3_orthogonal$loadings[1:14,]

# orthogonal 5
efa_just_5_orthogonal <- factanal(justifiability_stand[1:14], factors=5, rotation = "varimax")
efa_just_5_orthogonal

# orthogonal 7
efa_just_7_orthogonal <- factanal(justifiability_stand[1:14], factors=7, rotation = "varimax")
efa_just_7_orthogonal

# orthogonal 9
efa_just_9_orthogonal <- factanal(justifiability_stand[1:14], factors=9, rotation = "varimax")
efa_just_9_orthogonal

# oblique 3 (we expect this to be the best model)
efa_justifiability_3_oblique <- fa(justifiability_stand[1:14], 3, rotate="oblimin", fm="mle")
efa_justifiability_3_oblique
print(fa_just_3, cutoff=0)

# oblique 3 (we expect this to be the best model)
efa_justifiability_4_oblique <- fa(justifiability_stand[1:14], 4, rotate="oblimin", fm="mle")
efa_justifiability_4_oblique

# reproduce correlation matrix and check differences
cormat<-cor(justifiability_stand)
round(cormat-(crossprod(t(efa_justifiability_3_oblique$loadings))+diag(efa_justifiability_3_oblique$uniquenesses)),3)

# compute residual correlations
resid<-cormat- (crossprod(t(efa_justifiability_3_oblique$loadings))+diag(efa_justifiability_3_oblique$uniquenesses))
n<-sum(ifelse(abs(resid)>0.05,1,0))/2
print(n/(14*13/2))

# safe factor scores and visualize for each factor the distribution for the 34 countries
### STEP 3: PREDICTION & VISUALIZATION

# ORIGINAL VARIABLES
## Factor 1 and 2
par(pty="s")
plot(efa_justifiability_3_oblique$loadings[,1:2],xlim=c(-1,1),ylim=c(-1,1),cex=1.2)
pointLabel(efa_justifiability_3_oblique$loadings[,1:2], rownames(efa_justifiability_3_oblique$loadings[,1:2]), cex=1.2)
abline(h=0)
abline(v=0)
## Factor 2 and 3
par(pty="s")
plot(efa_justifiability_3_oblique$loadings[,c(1,3)],xlim=c(-1,1),ylim=c(-1,1),cex=1.2)
pointLabel(efa_justifiability_3_oblique$loadings[,c(1,3)], rownames(efa_justifiability_3_oblique$loadings[,c(1,3)]), cex=1.2)
abline(h=0)
abline(v=0)
## Factor 1 and 3
par(pty="s")
plot(efa_justifiability_3_oblique$loadings[,2:3],xlim=c(-1,1),ylim=c(-1,1),cex=1.2)
pointLabel(efa_justifiability_3_oblique$loadings[,2:3], rownames(efa_justifiability_3_oblique$loadings[,2:3]), cex=1.2)
abline(h=0)
abline(v=0)

# COUNTRIES