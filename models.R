library(tidyverse)
library(glmnet)

schools <- read_table("SchoolResults.txt")

n <- nrow(schools)
Y <- matrix(schools$Score, nrow=n, ncol=1)
schools$Income.squared <- schools$Income^2
X <- scale(schools[,2:ncol(schools)])

schools.lasso <- cv.glmnet(X, Y, alpha=1, nfolds = 10)

covariates <- names(coef(schools.lasso)[which(coef(schools.lasso) != 0),])
X.covariates <- X[,colnames(X) %in% covariates]

summary(lm(Y ~ X.covariates))





#Partial Out Income
Z <- X[,!(colnames(X) %in% c("Income", "Income.squared"))]
#Partial out Y
Y.tilde <- Z%*%(solve(t(Z)%*%Z)%*%t(Z)%*%Y)-Y
#Partial Income
Income <- matrix(X[,colnames(X) %in% c("Income")])
Income.squared <- matrix(X[,colnames(X) %in% c("Income.squared")])
Income.squared.tilde <- Z%*%(solve(t(Z)%*%Z)%*%t(Z)%*%Income.squared)-Income.squared
Income.tilde <- Z%*%(solve(t(Z)%*%Z)%*%t(Z)%*%Income)-Income
