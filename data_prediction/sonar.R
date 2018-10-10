setwd("C:/Users/plougue/Desktop/UVs/SY09/Projets/Projet 2")
data <- read.csv("donnees/sonar.csv")
N <- dim(data)[1]
n <- N
p <- dim(data)[2] - 2
X <- data[,1:p+1]
z <- data[,p+2]


X.pca <- prcomp(X)
barplot(X.pca$sdev)

s <- cumsum(X.pca$sdev)/sum(X.pca$sdev)
s[20]
X.proj <- X.pca$x[,1:2]
plot(X.proj, col=z)

log((norm(as.matrix(X),type="F")-norm(diag(diag(as.matrix(X))),type="F"))/norm(as.matrix(X),type="F"))
(norm(as.matrix(X))-norm(diag(diag(as.matrix(X)))))/norm(as.matrix(X))
# 
# # library(plotly)
# # plot_ly(x = X.pca$x[,1], y=X.pca$x[,2], z=X.pca$x[,3], color=z)
# ###########################################
# ###########################################
# source("fonctions/draw_ellipse.R")
# 
# mu_1 <- colMeans(X.proj[z==1,])
# var_1 <- var(X.proj[z==1,])
# mu_2 <- colMeans(X.proj[z==2,])
# var_2 <- var(X.proj[z==2,])
# 
# draw_ellipse(var_1, mu_1, add=T, confidence=0.95)
# draw_ellipse(var_1, mu_1, add=T, confidence=0.8, col="blue")
# draw_ellipse(var_1, mu_1, add=T, confidence=0.6, col="orange")
# draw_ellipse(var_2, mu_2, add=T, confidence=0.95)
# draw_ellipse(var_2, mu_2, add=T, confidence=0.8, col="blue")
# draw_ellipse(var_2, mu_2, add=T, confidence=0.6, col="orange")
# 
# ##########################################
# ##########################################
# ##########################################
# require(MASS)
# library(e1071)
# PC1 <- X.pca$x[,1]
# PC2 <- X.pca$x[,2]
# data.plan <- data.frame(PC1,PC2,z)
# logit.fit <- glm(z ~ ., data = data.plan)
# w <- logit.fit$coefficients
# intercept <-  w[1]/w[3]
# slope <- w[2]/w[3]
# plot(X.proj,col=z,pch=22+predict(logit.fit,data=data))
# abline(a=1,b=slope,col="purple")

var_1 <- var(X[z==1,])
var_2 <- var(X[z==2,])
X_1.pr <- princomp(X[z==1,])
X_2.pr <- princomp(X[z==2,])
cat("Log relative volume 1/2 : ", log(det(var_1)/det(var_2)),
    "\nDimension : ", p,
    "\nRelative average variance 1/2 : ", sum(diag(var_1))/sum(diag(var_2)),
    "\nRelative average precision 1/2 : ", sum(diag(solve(var_2)))/sum(diag(solve(var_1))),
    "\nRelative maximum variance 1/2 : ", max(X_1.pr$sdev)/max(X_2.pr$sdev))


library(MASS)
library(e1071)
N_essais <- 50
err.cbn <- 0
err.lda <- 0
err.qda <- 0

min.cbn <- 999
min.lda <- 999
min.qda <- 999

max.cbn <- 0
max.lda <- 0
max.qda <- 0

X.pr <- princomp(X)
X.proj <- X.pr$scores[,1:p]
for(i in 1:N_essais)
{
  tst <- sample(n,n/5)
  X.tst <- X.proj[tst,]
  z.tst <- z[tst]
  X.app <- X.proj[-tst,]
  z.app <- z[-tst]
  fit.cbn <- naiveBayes(y = as.factor(z.app), x=X.app)
  # fit.cbn <- naiveBayes(as.factor(Z)~.-X, data=data[-tst,])
  pred.cbn <- predict(fit.cbn, newdata=X.tst)
  # pred.cbn <- predict(fit.cbn, newdata=data[tst,1:(p+1)])
  err.cbn <- err.cbn + mean(pred.cbn!=z.tst)/N_essais
  min.cbn <- min(min.cbn,mean(pred.cbn!=z.tst))
  max.cbn <- max(max.cbn,mean(pred.cbn!=z.tst))
  
  fit.lda <- lda(as.factor(Z)~.-X, data=data[-tst,])
  pred.lda <- predict(fit.lda, newdata=data[tst,])
  err.lda <- err.lda + mean(pred.lda$class != z.tst)/N_essais
  min.lda <- min(min.lda,mean(pred.lda$class!=z.tst))
  max.lda <- max(max.lda,mean(pred.lda$class!=z.tst))
  
  n.tst <- dim(data[-tst,])[1]
  fit.qda <- qda(grouping=as.factor(z.app), x=X.app)
  pred.qda <- predict(fit.qda, newdata=X.tst)
  err.qda <- err.qda + mean(pred.qda$class != z.tst)/N_essais
  min.qda <- min(min.qda,mean(pred.qda$class!=z.tst))
  max.qda <- max(max.qda,mean(pred.qda$class!=z.tst))
}
print(err.cbn)
print(min.cbn)
print(max.cbn)
print(err.lda)
print(min.lda)
print(max.lda)
print(err.qda)
print(min.qda)
print(max.qda)

