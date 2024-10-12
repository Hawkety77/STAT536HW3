library(tidyverse)
library(glmnet)

get_covariates <- function(){
  
  schools <- read_table("SchoolResults.txt")
  
  n <- nrow(schools)
  Y <- matrix(schools$Score, nrow=n, ncol=1)
  schools$Income.squared <- schools$Income^2
  X <- scale(schools[,2:ncol(schools)])
  
  schools.lasso <- cv.glmnet(X, Y, alpha=1, nfolds = 10)
  
  covariates <- names(coef(schools.lasso)[which(coef(schools.lasso) != 0),])
  X.covariates <- X[,colnames(X) %in% covariates]
  
  return(list(
    covariates = covariates,
    X = X.covariates,
    Y = Y
  ))
}



#Partial Out Income
Z <- X[, !(colnames(X) %in% c("Income", "Income.squared"))]

# Partial out Y
Y.tilde <- Y - Z %*% (solve(t(Z) %*% Z) %*% t(Z) %*% Y)

# Partial out Income
Income <- matrix(X[, "Income"])
Income.squared <- matrix(X[, "Income.squared"])

# Partial out Income.squared
Income.squared.tilde <- Income.squared - Z %*% (solve(t(Z) %*% Z) %*% t(Z) %*% Income.squared)

# Partial out Income
Income.tilde <- Income - Z %*% (solve(t(Z) %*% Z) %*% t(Z) %*% Income)

lm(Y.tilde ~ Income.tilde + Income.squared.tilde)