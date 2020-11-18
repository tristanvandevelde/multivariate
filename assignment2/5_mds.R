load("~/Documents/Dev/learning/multivariate/assignment2/data/confusion.Rdata")
load("~/Documents/Dev/learning/multivariate/assignment2/data/dissim.Rdata")

# confusion: percentage who thought that row = column
# dissim: dissimilarity matrix of this dataset
#           note: is symmetric

library(smacof)
library(stringr)
library(maptools)

### QUESTION 1
# fit mds with 2 dimensions
# multiple measurement levels

# ratio
m1 <- smacofSym(delta = dissim, ndim=2, type="ratio", init="torgerson")
# interval
m2 <- smacofSym(delta = dissim, ndim=2, type="interval", init="torgerson")
# ordinal
m3 <- smacofSym(delta = dissim, ndim=2, type="ordinal", init="torgerson")
## todo: spline


### QUESTION 2
# evaluate GOF with stress-1
round(c(m1$stress, m2$stress, m3$stress), 4)
par(mfrow=c(3,2))
plot(m1,plot.type="resplot",main="residual plot ordinal MDS") 
plot(m1,plot.type="Shepard",main="Shepard diagram ordinal MDS") 
plot(m2,plot.type="resplot",main="residual plot spline MDS") 
plot(m2,plot.type="Shepard",main="Shepard diagram spline MDS")
plot(m3,plot.type="resplot",main="residual plot spline MDS") 
plot(m3,plot.type="Shepard",main="Shepard diagram spline MDS")
#configuration ordinal MDS plot(m3,plot.type="conf")

# evaluatie GOF with stress norms (randomstress + permutation)
set.seed(1)
# ordinal
rstress <- randomstress(n=10, ndim=2, nrep=500, type="ordinal")
perm.sound <- permtest(m3, nrep=500)
par(mfrow=c(1,2),pty="s")
hist(rstress,main="stress random data") 
hist(perm.sound$stressvec,main="stress permuted data")

# evaluate stability with jackknife
jack.sound <- jackmds(m3)
par(mfrow=c(1,1),pty="s")
plot(jack.sound, xlim=c(-1.2,1.2), ylim=c(-1,1))

### QUESTION 3

## first: normal biplot

plot(m3$conf,pch=19,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2)) 
pointLabel(c(m3$conf[,1]),c(m3$conf[,2]),attributes(m3$delta)$Labels)

## second: add weirdo variables

new_vars <- dissim
new_vars[1:36] <- list(NULL)
for (i in 1:36) {
    new_vars$points[i] = str_count(rownames(new_vars)[i], "\\·")
}
new_vars$points[9] = 2
for (i in 1:36) {
    new_vars$dashes[i] = str_count(rownames(new_vars)[i], "-")
}
new_vars$total = new_vars$points + new_vars$dashes
new_vars$points_prop = new_vars$points / new_vars$total
new_vars$dashes_prop = new_vars$dashes / new_vars$total

# plotje
biSound <- biplotmds(m3, new_vars)
plot(biSound, main = "Biplot Vector Representation", vecscale = 0.8,
     xlim = c(-1.5, 1.5), vec.conf = list(col = "brown"), pch = 20, cex = 0.5)