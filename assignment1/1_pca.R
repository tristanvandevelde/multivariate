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
# conclusion: 2
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

round(diag(A_individual[,1:2]%*%t(A_individual[,1:2])),2)
# actually still not extremely convincing
round(diag(A_individual[,1:3]%*%t(A_individual[,1:3])),2) # probably best model. Weird, because not concsistent with all other measures
# already a bunch better
round(diag(A_individual[,1:4]%*%t(A_individual[,1:4])),2)
# still getting better
round(diag(A_individual[,1:5]%*%t(A_individual[,1:5])),2)
# not a lot of improvement anymore

# not looking good
#round(diag(A[,1:3]%*%t(A[,1:3])),2)
# not much change
#round(diag(A[,1:1]%*%t(A[,1:1])),2)
# why is this not working?
#round(diag(A[,1:10]%*%t(A[,1:10])),2)

## AWESOME, IT'S FINALLY WORKING: 2 COMPONENTS IS AGREED UPON BY ALL METRICS.
