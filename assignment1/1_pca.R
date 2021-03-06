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
country_value_matrix <- aggregate(values_stand[,1:10], list(values_stand$country), mean)
rownames(country_value_matrix) <- country_value_matrix[,1]
country_value_df <- country_value_matrix[,2:11]
country_value_df <- scale(country_value_df, center = TRUE, scale = TRUE)


################################
####### PCA ON COUNTRY DATA ####
################################

#### MODEL FITTING
pca_country <- prcomp(country_value_df)
# eigenvalues
round(pca_country$sdev^2,2)
# proportion of explained variance
round(pca_country$sdev^2/10,2)
# summary
summary(pca_country)

#### COMPONENTS DETERMINATION
# kaiser and screeplot
screeplot(pca_country, type="lines")
abline(h=1, col="blue", lty=2)
abline(v=3, col="red", lty=3)
# conclusion: 2
# bootstrapped
bootstrap_country <- matrix(rep(0,34*10), ncol=10) #ok
for (i in 1:10) {
  samp <- sample(seq(1,34), size=34, replace=TRUE)
  bootstrap_country[,i] <- values_stand[samp, i] # +2 because first 2 variables not needed
}
bootstrap_country <- scale(bootstrap_country, center = TRUE, scale = TRUE)
# maybe standardize this thing?
pca_bootstrapped_country <- prcomp(bootstrap_country)
plot(c(1:10),pca_country$sdev[1:10]^2, type="b", xlab="component", ylab="eigenvalue", bty="n", xaxp  = c(1, 10, 9))
lines(c(1:10),pca_bootstrapped_country$sdev[1:10]^2, type="b", col="red")
legend(7,5, c("real data", "bootstrapped data"), bty="n", lty=c(1,1), col=c("black", "red"))
# OPTIONAL: Do multiple bootstraps (fe: 1000) and then compute the 2.5 and 92.5 percentile and fit the 95% confidence intervals.

#### ANALYSIS & INTERPRETATION
# eigenvalues: check explained variance

# component loadings
A_country <- pca_country$rotation%*%diag(pca_country$sdev)
A_country
# TODO: round values of loadings
round(diag(A_country[,1:1]%*%t(A_country[,1:1])),2)
round(diag(A_country[,1:2]%*%t(A_country[,1:2])),2)
round(diag(A_country[,1:3]%*%t(A_country[,1:3])),2)
round(diag(A_country[,1:4]%*%t(A_country[,1:4])),2)

biplot(pca_country,pc.biplot=TRUE,xlim=c(-3,3),ylim=c(-3,3))
