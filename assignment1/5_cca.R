# 5. Canonical correlation analysis
# Conduct a canonical correlation analysis to investigate the relations 
# between the following two sets of variables:
#   Set of X variables: 3 items about importance of religion (see Table 4)
# and 5 items about occurrence of crimes in the neighborhood (see Table 2)
#   Set of Y variables: 14 items about the justifiability of behaviors (see 
# Table 3)
# LW: According to Table 4 and 2, the X variables are 
# R_attend_religious_services, R_pray, R_importance_God, CR_robberies,
# CR_alcohol, CR_police_military, CR_racist_behavior, CR_drug_sale.
# LW: According to Table 3, the Y variables are 
# 16 J_claiming_benefits
# 17 J_avoiding_fare
# 18 J_stealing_property
# 19 J_cheating_taxes
# 20 J_accept_bribe
# 21 J_homosexuality
# 22 J_prostitution
# 23 J_abortion
# 24 J_divorce
# 25 J_sex_before_marriage
# 26 J_suicide
# 27 J_beat_wife
# 28 J_parents_beating_children
# 29 J_violence

# Conduct the analysis on standardized items using the data of the countries
# "Netherlands" and "Malaysia".

# Discuss the results of the canonical correlation analysis: How many 
# canonical correlations are significant? How can you interpret the canonical
# variates? How much variance in the Y variables can be explained by the X
# variables?

###############################################################################
# -----------------------------------------------------------------------------
###############################################################################

# Library list:
library(candisc)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(openxlsx)

# Set directory and load data
setwd("C:/Users/lasse/Dropbox/MSc in Statistics & Data Science - KU Leuven/1. Semester/Multivariate Statistics/Assignment 1 Multivariate Stat")
load("wvs(1).Rdata")

# Filter by country
wvs_nl = wvs[wvs$country == "Netherlands",]
wvs_my = wvs[wvs$country == "Malaysia",]

###############################################################################
# -----------------------------------------------------------------------------
###############################################################################

# Standardization
z_wvs_nl = wvs_nl
z_wvs_my = wvs_my
z_wvs_nl[,1:32] = scale(wvs_nl[,1:32], center = TRUE, scale = TRUE)
z_wvs_my[,1:32] = scale(wvs_my[,1:32], center = TRUE, scale = TRUE)

# Selecting the Y variables
Y_nl = as.matrix(z_wvs_nl[,16:29])
Y_my = as.matrix(z_wvs_my[,16:29])

# Selecting the X variables
X_nl = as.matrix(z_wvs_nl[,c(11,12,13,14,15,30,31,32)])
X_my = as.matrix(z_wvs_my[,c(11,12,13,14,15,30,31,32)])

# Performing the CCA
# install.packages("candisc")
# CCA Netherlands
cancor.out.nl = cancor(X_nl, Y_nl, set.names=c("Religion & Crimes (X)",
                                           "Justifiability (Y)"))

summary(cancor.out.nl)

# CCA Malaysia
cancor.out.my = cancor(X_my, Y_my, set.names=c("Religion & Crimes (X)",
                                               "Justifiability (Y)"))
summary(cancor.out.my)

#print canonical loadings
# canonical loadings, Netherlands
round(cancor.out.nl$structure$X.xscores[,1:3],3)
round(cancor.out.nl$structure$Y.yscores[,1:3],3)
# Canonical loadings, Malaysia
cancor.out.my$structure$X.xscores[,1:5]
cancor.out.my$structure$Y.yscores[,1:5]

#print redundancies
# NL
redu_nl<-redundancy(cancor.out.nl)
round(redu_nl$Xcan,3)
round(redu_nl$Ycan,3)
# MY
redu_my<-redundancy(cancor.out.my)
round(redu_my$Xcan,3)
round(redu_my$Ycan,3)

#computation redundancies from output
# NL
R2tu<-cancor.out.nl$cancor^2
VAFYbyt<-apply(cancor.out.nl$structure$Y.yscores^2,2,sum)/5
redund<-R2tu*VAFYbyt
round(cbind(R2tu,VAFYbyt,redund,total=cumsum(redund)),3)
# MY
R2tu_my<-cancor.out.my$cancor^2
VAFYbyt_my<-apply(cancor.out.my$structure$Y.yscores^2,2,sum)/5
redund_my<-R2tu_my*VAFYbyt
round(cbind(R2tu_my,VAFYbyt_my,redund_my,total=cumsum(redund_my)),3)

# Scatter plots of the canonical variates -------------------------------------
# Netherlands
u_nl = cancor.out.nl$scores$X
t_nl = cancor.out.nl$scores$Y
plot1_nl = ggplot() + geom_point(aes(u_nl[,1], t_nl[,1])) + theme_bw() +
  xlab(expression(u[1])) + ylab(expression(t[1]))
plot2_nl = ggplot() + geom_point(aes(u_nl[,2], t_nl[,2])) + theme_bw() +
  xlab(expression(u[2])) + ylab(expression(t[2]))
plot3_nl = ggplot() + geom_point(aes(u_nl[,3], t_nl[,3])) + theme_bw() +
  xlab(expression(u[3])) + ylab(expression(t[3]))

# Make the plots in a grid
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)}
print(plot1_nl, vp=vplayout(1,1))
print(plot2_nl, vp=vplayout(1,2))
print(plot3_nl, vp=vplayout(1,3))

# Malaysia
u_my = cancor.out.my$scores$X
t_my = cancor.out.my$scores$Y
plot1_my = ggplot() + geom_point(aes(u_my[,1], t_my[,1])) + theme_bw() +
  xlab(expression(u[1])) + ylab(expression(t[1]))
plot2_my = ggplot() + geom_point(aes(u_my[,2], t_my[,2])) + theme_bw() +
  xlab(expression(u[2])) + ylab(expression(t[2]))
plot3_my = ggplot() + geom_point(aes(u_my[,3], t_my[,3])) + theme_bw() +
  xlab(expression(u[3])) + ylab(expression(t[3]))
plot4_my = ggplot() + geom_point(aes(u_my[,4], t_my[,4])) + theme_bw() +
  xlab(expression(u[4])) + ylab(expression(t[4]))
plot5_my = ggplot() + geom_point(aes(u_my[,5], t_my[,5])) + theme_bw() +
  xlab(expression(u[5])) + ylab(expression(t[5]))

grid.newpage()
pushViewport(viewport(layout=grid.layout(2,3)))
vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)}
print(plot1_my, vp=vplayout(1,1))
print(plot2_my, vp=vplayout(1,2))
print(plot3_my, vp=vplayout(1,3))
print(plot4_my, vp=vplayout(2,1))
print(plot5_my, vp=vplayout(2,2))


# Exporting data
write.xlsx(round(cbind(R2tu,VAFYbyt,redund,total=cumsum(redund)),3), file = "Redundancies.xlsx")
write.xlsx(round(cbind(R2tu_my,VAFYbyt_my,redund_my,total=cumsum(redund_my)),3), file = "Redundancies_my.xlsx")
write.xlsx(round(cancor.out.nl$structure$X.xscores[,1:3],3), file = "NL_Loadings_X.xlsx")
write.xlsx(round(cancor.out.nl$structure$Y.yscores[,1:3],3), file = "NL_Loadings_Y.xlsx")
write.xlsx(round(cancor.out.my$structure$X.xscores[,1:5],3), file = "MY_Loadings_X.xlsx")
write.xlsx(round(cancor.out.my$structure$Y.yscores[,1:5],3), file = "MY_Loadings_Y.xlsx")
write.xlsx(summary(cancor.out.my), file = "Summary_my.xlsx")


###############################################################################
# assess validity of results using split-half approach ------------------------
###############################################################################
# Netherlands -----------------------------------------------------------------
# split data in two parts and standardize data
train_nl<-wvs_nl[seq(2,1260,by=2),]
valid_nl<-wvs_nl[seq(1,1260,by=2),]
z_train_nl = train_nl
z_valid_nl = valid_nl
z_train_nl[,1:32]<-scale(train_nl[,1:32],center=TRUE,scale=TRUE)
z_valid_nl[,1:32]<-scale(valid_nl[,1:32],center=TRUE,scale=TRUE)

# CCA Netherlands, calibration data -------------------------------------------
# Selecting the X and Y variables
X_train_nl = as.matrix(z_train_nl[,c(11,12,13,14,15,30,31,32)])
Y_train_nl = as.matrix(z_train_nl[,16:29])
# CCA
library(candisc)
cancor.out.train_nl = cancor(X_train_nl,Y_train_nl,
                             set.names=c("Religion & Crimes (X)",
                                         "Justifiability (Y)"))
summary(cancor.out.train_nl)
# cancor.out.train_nl$structure$X.xscores
# cancor.out.train_nl$structure$Y.yscores

# CCA Netherlands, validation data -------------------------------------------
# Selecting the X and Y variables
X_valid_nl = as.matrix(z_valid_nl[,c(11,12,13,14,15,30,31,32)])
Y_valid_nl = as.matrix(z_valid_nl[,16:29])
# CCA
cancor.out.valid_nl = cancor(X_valid_nl, Y_valid_nl,
                             set.names=c("Religion & Crimes (X)",
                                         "Justifiability (Y)"))
summary(cancor.out.valid_nl)
# cancor.out.valid_nl$structure$X.xscores
# cancor.out.valid_nl$structure$Y.yscores

# -----------------------------------------------------------------------------

# canonical variates calibration set
train.X1_nl <- cancor.out.train_nl$score$X
train.Y1_nl <- cancor.out.train_nl$score$Y

# compute canonical variates using data of calibration set and 
# coefficients estimated on validation set
train.X2_nl <- X_train_nl%*%cancor.out.valid_nl$coef$X
train.Y2_nl <- Y_train_nl%*%cancor.out.valid_nl$coef$Y


#R(T,T*) and R(U,U*)
R_1_nl_t = round(cor(train.Y1_nl,train.Y2_nl)[1:3,1:3],3)
write.xlsx(R_1_nl_t, file = "R_1_nl_t.xlsx")
R_1_nl_u = round(cor(train.X1_nl,train.X2_nl)[1:3,1:3],3)
write.xlsx(R_1_nl_u, file = "R_1_nl_u.xlsx")

#R(U*,T*) versus R(U,T)
R_2_nl_star = round(cor(train.X1_nl,train.Y1_nl)[1:3,1:3],3)
write.xlsx(R_2_nl_star, file = "R_2_nl_star.xlsx")
R_2_nl = round(cor(train.X2_nl,train.Y2_nl)[1:3,1:3],3)
write.xlsx(R_2_nl, file = "R_2_nl.xlsx")

#R(T*,T*) and R(U*,U*)
R_3_nl_t = round(cor(train.Y2_nl,train.Y2_nl)[1:3,1:3],3)
write.xlsx(R_3_nl_t, file = "R_3_nl_t.xlsx")
R_3_nl_u = round(cor(train.X2_nl,train.X2_nl)[1:3,1:3],3)
write.xlsx(R_3_nl_u, file = "R_3_nl_u.xlsx")

# -----------------------------------------------------------------------------
# Malaysia --------------------------------------------------------------------
# split data in two parts and standardize data
train_my<-wvs_my[seq(2,1213,by=2),]
valid_my<-wvs_my[seq(1,1213,by=2),]
z_train_my = train_my
z_valid_my = valid_my
z_train_my[,1:32]<-scale(train_my[,1:32],center=TRUE,scale=TRUE)
z_valid_my[,1:32]<-scale(valid_my[,1:32],center=TRUE,scale=TRUE)

# CCA Malaysia, calibration data ---------------------------------------------
# Selecting the X and Y variables
X_train_my = as.matrix(z_train_my[,c(11,12,13,14,15,30,31,32)])
Y_train_my = as.matrix(z_train_my[,16:29])
# CCA
cancor.out.train_my = cancor(X_train_my,Y_train_my,
                             set.names=c("Religion & Crimes (X)",
                                         "Justifiability (Y)"))
summary(cancor.out.train_my)
# cancor.out.train_my$structure$X.xscores
# cancor.out.train_my$structure$Y.yscores

# CCA Malaysia, validation data -----------------------------------------------
# Selecting the X and Y variables
X_valid_my = as.matrix(z_valid_my[,c(11,12,13,14,15,30,31,32)])
Y_valid_my = as.matrix(z_valid_my[,16:29])
# CCA
cancor.out.valid_my = cancor(X_valid_my, Y_valid_my,
                             set.names=c("Religion & Crimes (X)",
                                         "Justifiability (Y)"))
summary(cancor.out.valid_my)
# cancor.out.valid_nl$structure$X.xscores
# cancor.out.valid_nl$structure$Y.yscores

# -----------------------------------------------------------------------------
# canonical variates calibration set - Malaysia
train.X1_my <- cancor.out.train_my$score$X
train.Y1_my <- cancor.out.train_my$score$Y

# compute canonical variates using data of calibration set and 
# coefficients estimated on validation set - Malaysia
train.X2_my <- X_train_my%*%cancor.out.valid_my$coef$X
train.Y2_my <- Y_train_my%*%cancor.out.valid_my$coef$Y


#R(T,T*) and R(U,U*) - Malaysia
write.xlsx(round(cor(train.Y1_my,train.Y2_my)[1:5,1:5],3), file = "MY R_TTstar.xlsx")
write.xlsx(round(cor(train.X1_my,train.X2_my)[1:5,1:5],3), file = "MY R_UUstar.xlsx")

#R(U*,T*) versus R(U,T) - Malaysia
write.xlsx(round(cor(train.X1_my,train.Y1_my)[1:5,1:5],3), file = "MY R_UstarTstar.xlsx")
write.xlsx(round(cor(train.X2_my,train.Y2_my)[1:5,1:5],3), file = "MY R_UT.xlsx")

#R(T*,T*) and R(U*,U*) - Malaysia
write.xlsx(round(cor(train.Y2_my,train.Y2_my)[1:5,1:5],3), file = "MY R_TstarTstar.xlsx")
write.xlsx(round(cor(train.X2_my,train.X2_my)[1:5,1:5],3), file = "MY R_UstarUstar.xlsx")
