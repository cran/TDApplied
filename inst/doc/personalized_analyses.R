## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval = F-----------------------------------------------------------------
#  library(xgboost)

## ----eval = F-----------------------------------------------------------------
#  # create 30 diagrams from circles, tori and spheres
#  # up to dimension 2
#  diags <- lapply(X = 1:30,FUN = function(X){
#  
#    if(X <= 10)
#    {
#      return(TDAstats::calculate_homology(TDA::circleUnif(n = 100),
#                                          dim = 2,threshold = 2))
#    }
#  
#    if(X > 10 & X <= 20)
#    {
#      return(TDAstats::calculate_homology(TDA::torusUnif(n = 100,a = 0.25,c = 0.75),
#                                          dim = 2,threshold = 2))
#    }
#  
#    if(X > 20)
#    {
#      return(TDAstats::calculate_homology(TDA::sphereUnif(n = 100,d = 2),
#                                          dim = 2,threshold = 2))
#    }
#  
#  })
#  
#  # subset into two features, dimension 1 and dimension 2
#  T1 <- lapply(X = diags,FUN = function(X){
#  
#    df <- X[which(X[,1] == 1),]
#    if(!is.matrix(df))
#    {
#      df <- as.data.frame(t(df))
#    }
#    return(as.data.frame(df))
#  
#  })
#  T2 <- lapply(X = diags,FUN = function(X){
#  
#    df <- X[which(X[,1] == 2),]
#    if(!is.matrix(df))
#    {
#      df <- as.data.frame(t(df))
#    }
#    return(as.data.frame(df))
#  
#  })
#  
#  # calculate max persistence of each diagram
#  max_pers_H1 <- unlist(lapply(X = T1,FUN = function(X){
#  
#    return(max(X[[3]] - X[[2]]))
#  
#  }))
#  max_pers_H2 <- unlist(lapply(X = T2,FUN = function(X){
#  
#    if(nrow(X) == 0)
#    {
#      return(0)
#    }
#    return(max(X[[3]] - X[[2]]))
#  
#  }))
#  
#  # create random binary vector
#  rand_bin <- sample(factor(c("yes","no")),size = 30,replace = T)
#  
#  # specify data labels, 0 for circle, 1 for torus, 2 for sphere
#  labs <- rep(c(0,1,2),each = 10)

## ----eval = F-----------------------------------------------------------------
#  # form non-topological feature matrix
#  NT <- cbind(max_pers_H1,max_pers_H2,rand_bin)
#  
#  # calculate the approximate Gram matrix for each topological feature
#  G1 <- gram_matrix(diagrams = T1,sigma = 0.01,dim = 1,rho = 0.0001)
#  G2 <- gram_matrix(diagrams = T2,sigma = 0.01,dim = 2,rho = 0.0001)
#  
#  # column bind G_i's into 30x60 feature matrix
#  G <- cbind(G1,G2)
#  
#  # column bind G and NT into 30x63 feature matrix
#  Fmat <- cbind(G,NT)
#  
#  # fit XGBoost model with maximum 50 boosting iterations
#  model <- xgboost(data = Fmat,label = rep(c(0,1,2),each = 10),nrounds = 50,verbose = 0,
#                   objective = "multi:softmax",num_class = 3)

## ----eval = F-----------------------------------------------------------------
#  new_diags <- list(TDAstats::calculate_homology(TDA::circleUnif(n = 100),
#                                          dim = 2,threshold = 2),
#                    TDAstats::calculate_homology(TDA::torusUnif(n = 100,a = 0.25,c = 0.75),
#                                          dim = 2,threshold = 2),
#                    TDAstats::calculate_homology(TDA::sphereUnif(n = 100,d = 2),
#                                          dim = 2,threshold = 2))
#  
#  # subset into two features, dimension 1 and dimension 2
#  T1_prime <- lapply(X = new_diags,FUN = function(X){
#  
#    df <- X[which(X[,1] == 1),]
#    if(!is.matrix(df))
#    {
#      df <- as.data.frame(t(df))
#    }
#    return(as.data.frame(df))
#  
#  })
#  T2_prime <- lapply(X = new_diags,FUN = function(X){
#  
#    df <- X[which(X[,1] == 2),]
#    if(!is.matrix(df))
#    {
#      df <- as.data.frame(t(df))
#    }
#    return(as.data.frame(df))
#  
#  })
#  
#  # calculate max persistence of each new diagram
#  max_pers_H1_prime <- unlist(lapply(X = T1_prime,FUN = function(X){
#  
#    return(max(X[,3] - X[,2]))
#  
#  }))
#  max_pers_H2_prime <- unlist(lapply(X = T2_prime,FUN = function(X){
#  
#    if(nrow(X) == 0)
#    {
#      return(0)
#    }
#    return(max(X[,3] - X[,2]))
#  
#  }))
#  
#  # create random binary vector
#  rand_bin_prime <- sample(factor(c("yes","no")),size = 3,replace = T)

## ----eval = F-----------------------------------------------------------------
#  # form non-topological feature matrix
#  NT_prime <- cbind(max_pers_H1_prime,max_pers_H2_prime,rand_bin_prime)
#  
#  # calculate the approximate cross Gram matrix for each topological feature
#  G1_prime <- gram_matrix(diagrams = T1_prime,other_diagrams = T1,sigma = 0.01,dim = 1,
#                          rho = 0.0001)
#  G2_prime <- gram_matrix(diagrams = T2_prime,other_diagrams = T2,sigma = 0.01,dim = 2,
#                          rho = 0.0001)
#  
#  # column bind G_i prime's into 3x60 feature matrix
#  G_prime <- cbind(G1_prime,G2_prime)
#  
#  # column bind G_prime and NT_prime into 3x63 feature matrix
#  Fmat_prime <- cbind(G_prime,NT_prime)
#  
#  # fix column names of Fmat_prime to be the same as Fmat
#  colnames(Fmat_prime) <- colnames(Fmat)
#  
#  # predict data labels
#  stats::predict(model,Fmat_prime)

## ----echo = F-----------------------------------------------------------------
c(0,1,2)

