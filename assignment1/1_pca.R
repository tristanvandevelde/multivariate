library(here)
library(MASS)
load("data/wvs.Rdata")

# standardize 10 variables that measure that values
# = all variables starting with V
wvs$V_creative_stand <- scale(wvs$V_creative, center = TRUE, scale = TRUE)
wvs$V_rich_stand <- scale(wvs$V_rich, center = TRUE, scale = TRUE)
wvs$V_secure_stand <- scale(wvs$V_secure, center = TRUE, scale = TRUE)
wvs$V_spoil_oneself_stand <- scale(wvs$V_spoil_oneself, center = TRUE, scale = TRUE)
wvs$V_do_good_stand <- scale(wvs$V_do_good, center = TRUE, scale = TRUE)
wvs$V_be_successful_stand <- scale(wvs$V_be_successful, center = TRUE, scale = TRUE)
wvs$V_exciting_life_stand <- scale(wvs$V_exciting_life, center = TRUE, scale = TRUE)
wvs$V_behave_properly_stand <- scale(wvs$V_behave_properly, center = TRUE, scale = TRUE)
wvs$V_protect_environment_stand <- scale(wvs$V_protect_environment, center = TRUE, scale = TRUE)
wvs$V_tradition_stand <- scale(wvs$V_tradition, center = TRUE, scale = TRUE)



#wvs_values <- wvs[1:10]
#wvs_values_standardized <- scale(wvs_values, center=TRUE, scale=TRUE)

# compute matrix of 34 countries x 10 variables (mean score on standardized var)
# apply PCA
# make biplot