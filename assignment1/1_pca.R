library(MASS)
#load("data/wvs.Rdata")
load("/Users/tristanvandevelde/Documents/Dev/multivariate/assignment1/data/wvs.Rdata")

#### PREPROCESSING
##################

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

# compute matrix of 34 countries x 10 variables (mean score on standardized var)
country_value_matrix <- aggregate(wvs[,33:43], list(wvs$country), mean)
# TODO: do something about the warning

#### PCA: PRE-ANALYSIS
######################

## apply PCA with #variables=#components
#
pca_1 <- prcomp(country_value_matrix[,3:12])
# eigenvalues
round(pca_1$sdev^2,3)
# component loadings
A <- pca_1$rotation%*%diag(pca_1$sdev)
A

## Determine number of components
screeplot(pca_1, type="lines")
# kaisers rule: how is this for standardized variables?
# scree plot: 2 because elbow at 3
# TODO: boostrapped screeplot


#### PCA: FINAL MODEL
#####################

## fit model

## Make biplot