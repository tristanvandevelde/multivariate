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
wvs_stand <- data.frame(wvs[c(16:29,33)])
wvs_stand$J_claiming_benefits <- scale(wvs_stand$J_claiming_benefits, center = TRUE, scale = TRUE)
wvs_stand$J_avoiding_fare <- scale(wvs_stand$J_avoiding_fare, center = TRUE, scale = TRUE)
wvs_stand$J_stealing_property <- scale(wvs_stand$J_stealing_property, center = TRUE, scale = TRUE)
wvs_stand$J_cheating_taxes <- scale(wvs_stand$J_cheating_taxes, center = TRUE, scale = TRUE)
wvs_stand$J_accept_bribe <- scale(wvs_stand$J_accept_bribe, center = TRUE, scale = TRUE)
wvs_stand$J_homosexuality <- scale(wvs_stand$J_homosexuality, center = TRUE, scale = TRUE)
wvs_stand$J_prostitution <- scale(wvs_stand$J_prostitution, center = TRUE, scale = TRUE)
wvs_stand$J_abortion <- scale(wvs_stand$J_abortion, center = TRUE, scale = TRUE)
wvs_stand$J_divorce <- scale(wvs_stand$J_divorce, center = TRUE, scale = TRUE)
wvs_stand$J_sex_before_marriage <- scale(wvs_stand$J_sex_before_marriage, center = TRUE, scale = TRUE)
wvs_stand$J_suicide <- scale(wvs_stand$J_suicide, center = TRUE, scale = TRUE)
wvs_stand$J_beat_wife <- scale(wvs_stand$J_beat_wife, center = TRUE, scale = TRUE)
wvs_stand$J_parents_beating_children <- scale(wvs_stand$J_parents_beating_children, center = TRUE, scale = TRUE)
wvs_stand$J_violence <- scale(wvs_stand$J_violence, center = TRUE, scale = TRUE)



# use EFA

#### EFA MODELLING
##################


### STEP 0: SOME EXPLORATION
cormat <- cor(wvs_stand[1:14])
round(cormat,2)
ggcorrplot(cormat)

### STEP 1: FIT PCA TO DETERMINE AMOUNT OF FACTORS
pca_just <- prcomp(wvs_stand[c(1:14)])
screeplot(pca_just, type="lines")
abline(h=1, col="blue", lty=2)
abline(v=4, col="red", lty=2)
# both scree plot and kaisers criterium indicate te retain 3 facts
# Consistent with our intuition (3 factors: financial, sexual, violence)



### STEP 2: FIT EFA WITH 3 FACTORS
# orthogonal 3
fa_just_3_orthogonal <- factanal(wvs_stand[1:14], 3, rotation = "varimax")
fa_just_3_orthogonal$loadings[1:14,]

# oblique 3 (we expect this to be the best model)
fa_just_3_oblique <- fa(wvs_stand[1:14], 3, rotate="oblimin", fm="mle")
print(fa_just_3, cutoff=0)


# safe factor scores and visualize for each factor the distribution for the 34 countries
