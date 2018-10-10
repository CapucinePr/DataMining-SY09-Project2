setwd("C:/Users/plougue/Desktop/UVs/SY09/Projets/Projet 2")
data <- read.csv("donnees/breastcancer.csv")
N <- dim(data)[1]
p <- dim(data)[2] - 2
X <- data[,1:p+1]
z <- data[,p+2]


attach(mtcars)
par(mfrow=c(1,1))

X.pca <- prcomp(X)
explained_inertia <- X.pca$sdev/sum(X.pca$sdev)
explained_inertia.cumul <- cumsum(explained_inertia)
plot(explained_inertia.cumul, type='b',main='breastcancer P.C. inertia (cumulative)')
plot(explained_inertia, type='b',main='breastcancer P.C. inertia (non-cumulative)')

###########
### LDA ###
###########

