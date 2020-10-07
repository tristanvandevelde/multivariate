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

#### PCA: MODEL FITTING
#######################

## apply PCA with #variables=#components
#
pca_1 <- prcomp(country_value_matrix[,3:12])
# eigenvalues
round(pca_1$sdev^2,3)
# component loadings
A <- pca_1$rotation%*%diag(pca_1$sdev)
A

#### PCA: COMPONENTS CHOICE
###########################

screeplot(pca_1, type="lines")
abline(h=1, col="blue", lty=2)
abline(v=3, col="red", lty=3)
# conclusion: 3 (Kaisers rule) or 2 (scree plot)
bootstrap <- matrix(rep(0,34*10), ncol=10) #ok
for (i in 1:10) {
  samp <- sample(seq(1,34), size=34, replace=TRUE)
  bootstrap[,i] <- country_value_matrix[samp, i+2] # +2 because first 2 variables not needed
}
# maybe standardize this thing?
pca_bootstrapped <- prcomp(bootstrap)
plot(c(1:10),pca_1$sdev[1:10]^2, type="b", xlab="component", ylab="eigenvalue")
lines(c(1:10),pca_bootstrapped$sdev[1:10]^2, type="b", col="red")
legend(8,3, c("real data", "bootstrapped data"), bty="n", lty=c(1,1), col=c("black", "red"))
# include 95% CIs
# conclusion: 2

round(diag(A[,1:2]%*%t(A[,1:2])),2)
# not looking good
round(diag(A[,1:3]%*%t(A[,1:3])),2)
# not much change
round(diag(A[,1:1]%*%t(A[,1:1])),2)
# why is this not working?
round(diag(A[,1:10]%*%t(A[,1:10])),2)

### Suspection ###
# overall: PCA not suitable, not good model for this data. May be due to correlation among variables? not sure, maybe other explanation?
# however: 2 seems optimal number of components. imporvement over 1, not much improvement when going any higher than 2.

# possible explanations: features have non-linear relationship, no relationship
# or maybe because already working with consolodated data???? -> do not work with country grouped data???

#### PCA: PREDICTION & INTERPRETATION