library(MASS)
#load("data/wvs.Rdata")
load("/Users/tristanvandevelde/Documents/Dev/multivariate/assignment1/data/wvs.Rdata")

#### PREPROCESSING
##################

# standardize 10 variables that measure that values
# = all variables starting with V
values_stand <- data.frame(wvs[c(1:10,33)])
values_stand$V_creative <- scale(wvs$V_creative, center = TRUE, scale = TRUE)
values_stand$V_rich <- scale(wvs$V_rich, center = TRUE, scale = TRUE)
values_stand$V_secure <- scale(wvs$V_secure, center = TRUE, scale = TRUE)
values_stand$V_spoil_oneself <- scale(wvs$V_spoil_oneself, center = TRUE, scale = TRUE)
values_stand$V_do_good <- scale(wvs$V_do_good, center = TRUE, scale = TRUE)
values_stand$V_be_successful <- scale(wvs$V_be_successful, center = TRUE, scale = TRUE)
values_stand$V_exciting_life <- scale(wvs$V_exciting_life, center = TRUE, scale = TRUE)
values_stand$V_behave_properly <- scale(wvs$V_behave_properly, center = TRUE, scale = TRUE)
values_stand$V_protect_environment <- scale(wvs$V_protect_environment, center = TRUE, scale = TRUE)
values_stand$V_tradition <- scale(wvs$V_tradition, center = TRUE, scale = TRUE)

### in own dataset: hopefully to be removed
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


###################################
####### PCA ON INDIVIDUAL DATA ####
###################################

#### MODEL FITTING
pca_individual <- prcomp(values_stand[c(1:10)])
# eigenvalues
round(pca_individual$sdev^2,3)
# component loadings
A_individual <- pca_individual$rotation%*%diag(pca_individual$sdev)
A_individual

#### COMPONENTS DETERMINATION
# kaiser and screeplot
screeplot(pca_individual, type="lines")
abline(h=1, col="blue", lty=2)
abline(v=3, col="red", lty=3)
# bootstrapped
bootstrap_individual <- matrix(rep(0,34*10), ncol=10) #ok
for (i in 1:10) {
  samp <- sample(seq(1,34), size=34, replace=TRUE)
  bootstrap_individual[,i] <- values_stand[samp, i] # +2 because first 2 variables not needed
}
# maybe standardize this thing?
pca_bootstrapped_individual <- prcomp(bootstrap_individual)
plot(c(1:10),pca_individual$sdev[1:10]^2, type="b", xlab="component", ylab="eigenvalue")
lines(c(1:10),pca_bootstrapped_individual$sdev[1:10]^2, type="b", col="red")
legend(8,3, c("real data", "bootstrapped data"), bty="n", lty=c(1,1), col=c("black", "red"))
# include 95% CIs
# conclusion: 2

###################################
####### PCA ON COUNTRY DATA #######
###################################


#### PCA: MODEL FITTING
#######################

## apply PCA with #variables=#components
#
#pca_country <- prcomp(country_value_matrix[,3:12])
# eigenvalues
#round(pca_country$sdev^2,3)
# component loadings
#A_country <- pca_country$rotation%*%diag(pca_country$sdev)
#A_country

#### PCA: COMPONENTS CHOICE
###########################

#screeplot(pca_1, type="lines")
#abline(h=1, col="blue", lty=2)
#abline(v=3, col="red", lty=3)
# conclusion: 3 (Kaisers rule) or 2 (scree plot)
#bootstrap <- matrix(rep(0,34*10), ncol=10) #ok
#for (i in 1:10) {
#  samp <- sample(seq(1,34), size=34, replace=TRUE)
#  bootstrap[,i] <- country_value_matrix[samp, i+2] # +2 because first 2 variables not needed
#}
# maybe standardize this thing?
#pca_bootstrapped <- prcomp(bootstrap)
#plot(c(1:10),pca_1$sdev[1:10]^2, type="b", xlab="component", ylab="eigenvalue")
#lines(c(1:10),pca_bootstrapped$sdev[1:10]^2, type="b", col="red")
#legend(8,3, c("real data", "bootstrapped data"), bty="n", lty=c(1,1), col=c("black", "red"))
# include 95% CIs
# conclusion: 2

#round(diag(A[,1:2]%*%t(A[,1:2])),2)
# not looking good
#round(diag(A[,1:3]%*%t(A[,1:3])),2)
# not much change
#round(diag(A[,1:1]%*%t(A[,1:1])),2)
# why is this not working?
#round(diag(A[,1:10]%*%t(A[,1:10])),2)

### Suspection ###
# overall: PCA not suitable, not good model for this data. May be due to correlation among variables? not sure, maybe other explanation?
# however: 2 seems optimal number of components. imporvement over 1, not much improvement when going any higher than 2.

# possible explanations: features have non-linear relationship, no relationship
# or maybe because already working with consolodated data???? -> do not work with country grouped data???

#### PCA: PREDICTION & INTERPRETATION