setwd("C:/Users/plougue/Desktop/UVs/SY09/Projets/Projet 2")
data <- read.csv("donnees/ionosphere.csv")
N <- dim(data)[1]
p <- dim(data)[2] - 2
X <- cbind(data[,2], data[,3:p+1]) 
z <- data[,p+2]


X.pca <- prcomp(X)
barplot(X.pca$sdev)

X.proj <- X.pca$x[,1:2]
plot(X.proj, col=z)

# library(plotly)
# plot_ly(x = X.pca$x[,1], y=X.pca$x[,2], z=X.pca$x[,3], color=z)
###########################################
###########################################
source("fonctions/draw_ellipse.R")

mu_1 <- colMeans(X.proj[z==1,])
var_1 <- var(X.proj[z==1,])
mu_2 <- colMeans(X.proj[z==2,])
var_2 <- var(X.proj[z==2,])

draw_ellipse(var_1, mu_1, add=T, confidence=0.6, col="purple")
draw_ellipse(var_1, mu_1, add=T, confidence=0.3, col="orange")
draw_ellipse(var_2, mu_2, add=T, confidence=0.6, col="purple")
draw_ellipse(var_2, mu_2, add=T, confidence=0.3, col="orange")

##########################################
##########################################
##########################################
require(MASS)
library(e1071)
PC1 <- X.pca$x[,1]
PC2 <- X.pca$x[,2]
data.plan <- data.frame(PC1,PC2,z)
logit.fit <- glm(z ~ ., data = data.plan)
w <- logit.fit$coefficients
intercept <-  w[1]/w[3]
slope <- w[2]/w[3]
plot(X.proj,col=z,pch=22+predict(logit.fit,data=data))
abline(a=1,b=slope,col="purple")
