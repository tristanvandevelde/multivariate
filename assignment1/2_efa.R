library(MASS)
library(ggplot2)
library(ggcorrplot)
library(xtable)
library(psych)
library(GPArotation)
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



# use EFA

#### EFA MODELLING
##################


### STEP 0: SOME EXPLORATION
cormat <- cor(justifiability_stand[1:14])
round(cormat,2)
ggcorrplot(cormat)

### STEP 1: FIT PCA TO DETERMINE AMOUNT OF FACTORS
pca_justifiability <- prcomp(justifiability_stand[c(1:14)])
screeplot(pca_justifiability, type="lines")
abline(h=1, col="blue", lty=2)
abline(v=4, col="red", lty=2)
# both scree plot and kaisers criterium indicate te retain 3 facts
# Consistent with our intuition (3 factors: financial, sexual, violence)



### STEP 2: FIT EFA WITH 3 FACTORS
# orthogonal 3
efa_just_3_orthogonal <- factanal(wvs_stand[1:14], 3, rotation = "varimax")
efa_just_3_orthogonal$loadings[1:14,]

# oblique 3 (we expect this to be the best model)
efa_justifiability_3_oblique <- fa(justifiability_stand[1:14], 3, rotate="oblimin", fm="mle")
print(fa_just_3, cutoff=0)


# safe factor scores and visualize for each factor the distribution for the 34 countries

par(pty="s")
plot(favm_pain2$loadings,xlim=c(-1,1),ylim=c(-1,1),cex=1.2)
