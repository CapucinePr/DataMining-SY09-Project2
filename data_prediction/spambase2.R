setwd("C:/Users/plougue/Desktop/UVs/SY09/Projets/Projet 2")
data <- read.csv("donnees/spambase2.csv")
n <- dim(data)[1]
p <- dim(data)[2] - 2

X <- data[,2:(p+1)]
z <- data$Z

pr <- prcomp(X)
plot(pr$sdev, type='b')



# 
# g<-2
# n1 <- sum(z==1)
# n2 <- sum(z==2)
# C1 <- var_1 / (n1 - 1)
# C2 <- var_2 / (n2 - 1)
# C <- (var_1 + var_2) / (n - g)
# 
# M <- (n-g)*log(det(C)) - (n1-1)*log(det(C1))
# - (n2-1)*log(det(C2))
# h <- 1 - (2*p^2+3*p-1)/(6*(p+1)*(g-1)) * (1/(n1-1) + 1/(n2-1) - 1/(n-g))
# 
# 
# df <- p*(p+1)*(g-1)/2
# 
# 
# par(mfrow=c(1,1))
# plot(1:100/100, qchisq(1:100/100,df))
# abline(M*h,0)

var_1 <- var(X[z==1,])
var_2 <- var(X[z==2,])
X_1.pr <- princomp(X[z==1,])
X_2.pr <- princomp(X[z==2,])
cat("Log relative volume 1/2 : ", log(det(var_1)/det(var_2)),
    "\nDimension : ", p,
    "\nRelative average variance 1/2 : ", sum(diag(var_1))/sum(diag(var_2)),
    "\nRelative average precision 1/2 : ", sum(diag(solve(var_2)))/sum(diag(solve(var_1))),
    "\nRelative maximum variance 1/2 : ", max(X_1.pr$sdev)/max(X_2.pr$sdev))


