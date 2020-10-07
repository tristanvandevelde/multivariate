library(MASS)
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

### STEP 1: FIT PCA TO DETERMINE AMOUNT OF FACTORS

# print correlation matrix
cormat <- cor(wvs_stand[1:14])
round(cormat,2)
# intuitively: 3 factors (financial, sexual, violence)
#fa_just = <- factanal

# safe factor scores and visualize for each factor the distribution for the 34 countries
