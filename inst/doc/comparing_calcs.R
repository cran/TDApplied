## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

# get original graphic parameters to be able to
# revert back at the end of the vignette
original_mfrow <- par()$mfrow
original_xpd <- par()$xpd
original_mar <- par()$mar
original_scipen <- options()$scipen
if(exists(".Random.seed", .GlobalEnv) == F)
{
  runif(1)
}
oldseed <- get(".Random.seed", .GlobalEnv)
oldRNGkind <- RNGkind()

# set some new parameters for viewing and reproducibility
options(scipen = 999)
set.seed(123)

## ----echo = F,include = F-----------------------------------------------------
library(TDApplied)

## ----echo = F,fig.height = 3,fig.width = 7,fig.align = 'center'---------------
D1 = data.frame(dimension = c(0),birth = c(2),death = c(3))
D2 = data.frame(dimension = c(0),birth = c(2,0),death = c(3.3,0.5))
D3 = data.frame(dimension = c(0),birth = c(0),death = c(0.5))
par(mfrow = c(1,3))
plot_diagram(D1,title = "D1",max_radius = 4,legend = F)
plot_diagram(D2,title = "D2",max_radius = 4,legend = F)
plot_diagram(D3,title = "D3",max_radius = 4,legend = F)

## ----echo = F-----------------------------------------------------------------
par(mfrow = c(1,1))

## ----eval = F,include = F-----------------------------------------------------
#  D1_TDA <- as.matrix(D1)
#  colnames(D1_TDA) <- NULL
#  D2_TDA <- as.matrix(D2)
#  colnames(D2_TDA) <- NULL
#  D3_TDA <- as.matrix(D3)
#  colnames(D3_TDA) <- NULL
#  dis_bot <- rgudhi::BottleneckDistance$new()
#  dis_wass <- rgudhi::WassersteinDistance$new()
#  dis_fish <- rgudhi::PersistenceFisherDistance$new() # default sigma is 1
#  
#  bottleneck_comparison <- data.frame(pair = c("D1 and D2","D1 and D3","D2 and D3"),ground_truth = c(0.3,0.5,0.65),TDApplied = c(diagram_distance(D1,D2,p = Inf),diagram_distance(D1,D3,p = Inf),diagram_distance(D2,D3,p = Inf)),TDA = c(TDA::bottleneck(D1_TDA,D2_TDA,dimension = 0),TDA::bottleneck(D1_TDA,D3_TDA,dimension = 0),TDA::bottleneck(D2_TDA,D3_TDA,dimension = 0)),rgudhi = c(dis_bot$apply(D1[,2:3],D2[,2:3]),dis_bot$apply(D1[,2:3],D3[,2:3]),dis_bot$apply(D2[,2:3],D3[,2:3])))
#  
#  wasserstein_comparison <- data.frame(pair = c("D1 and D2","D1 and D3","D2 and D3"),ground_truth = c(0.3905125, 0.559017, 0.65),TDApplied = c(diagram_distance(D1,D2),diagram_distance(D1,D3),diagram_distance(D2,D3)),TDA = c(TDA::wasserstein(D1_TDA,D2_TDA,dimension = 0),TDA::wasserstein(D1_TDA,D3_TDA,dimension = 0),TDA::wasserstein(D2_TDA,D3_TDA,dimension = 0)),rgudhi = c(dis_wass$apply(D1[,2:3],D2[,2:3]),dis_wass$apply(D1[,2:3],D3[,2:3]),dis_wass$apply(D2[,2:3],D3[,2:3])))
#  
#  fisher_comparison <- data.frame(pair = c("D1 and D2","D1 and D3","D2 and D3"),ground_truth = c(0.02354624,0.08821907,0.1139891),TDApplied = c(diagram_distance(D1,D2,distance = "fisher",sigma = 1),diagram_distance(D1,D3,distance = "fisher",sigma = 1),diagram_distance(D2,D3,distance = "fisher",sigma = 1)),rgudhi = c(dis_fish$apply(D1[,2:3],D2[,2:3]),dis_fish$apply(D1[,2:3],D3[,2:3]),dis_fish$apply(D2[,2:3],D3[,2:3])))

## ----echo = F-----------------------------------------------------------------
bottleneck_comparison <- data.frame(pair = c("D1 and D2","D1 and D3","D2 and D3"),ground_truth = c(0.3,0.5,0.65),TDApplied = c(0.3,0.5,0.65),TDA = c(0.3,0.5,0.65),rgudhi = c(0.3,0.5,0.65))
wasserstein_comparison <- data.frame(pair = c("D1 and D2","D1 and D3","D2 and D3"),ground_truth = c(0.3905125 ,0.559017 ,0.65),TDApplied = c(0.3905125,0.559017,0.65),TDA = c(0.55,0.75,0.65),rgudhi = c(0.55,0.75,0.65))
fisher_comparison <- data.frame(pair = c("D1 and D2","D1 and D3","D2 and D3"),ground_truth = c(0.02354624 ,0.08821907 ,0.08741134),TDApplied = c(0.02354624 ,0.08821907 ,0.08741134),rgudhi = c(0.02354624 ,0.08821907 ,0.08741134))

## ----echo = F-----------------------------------------------------------------
bottleneck_comparison

## ----echo = F-----------------------------------------------------------------
wasserstein_comparison

## ----echo = F-----------------------------------------------------------------
fisher_comparison

## ----eval = F-----------------------------------------------------------------
#  gudhi_kern <- rgudhi::PersistenceFisherKernel$new(bandwidth = 0.5)
#  gudhi_kern$apply(D2[,2:3],D3[,2:3])

## ----eval = F-----------------------------------------------------------------
#  gudhi_kern <- rgudhi::PersistenceFisherKernel$new(bandwidth_fisher = 0.5)
#  gudhi_kern$apply(D2[,2:3],D3[,2:3])

## ----eval = F-----------------------------------------------------------------
#  d <- rgudhi::PersistenceFisherDistance$new() # sigma = 1
#  t <- 2 # or whatever desired parameter
#  exp(-t*d$apply(D2[,2:3],D3[,2:3]))

## ----eval = F-----------------------------------------------------------------
#  # create rgudhi distance object
#  # sigma = 0.01
#  gudhi_dist <- rgudhi::PersistenceFisherDistance$new(bandwidth = 0.01,n_jobs = 1L)
#  
#  # create list of diagrams, only birth and death values in dimension 1
#  g <- lapply(X = 1:10,FUN = function(X){
#  
#    df <- diagram_to_df(TDA::ripsDiag(X = TDA::circleUnif(n = 50),
#                                      maxdimension = 1,maxscale = 2))
#    return(df[which(df[,1] == 1),2:3])
#  
#  })
#  
#  # distance matrix function
#  # diagrams is a list of diagrams (only birth and death columns)
#  # gudhi_dist is the rgudhi distance object
#  gudhi_distance_matrix <- function(diagrams,gudhi_dist){
#  
#    # get number of rows of each diagram since rgudhi can't calculate
#    # distances with empty diagrams
#    rows <- unlist(lapply(diagrams,FUN = nrow))
#    inds <- which(rows > 0)
#  
#    # if inds is empty then return 0 matrix
#    if(length(inds) == 0)
#    {
#      return(matrix(data = 0,nrow = length(diagrams),ncol = length(diagrams)))
#    }
#  
#    # calculate distance matrix for non-zero-row diagrams
#    d_non_zero <- gudhi_dist$fit_transform(diagrams[inds])
#  
#    # fix diagonal which can sometimes have non-zero entries
#    diag(d_non_zero) <- rep(0,nrow(d_non_zero))
#  
#    # symmetrize (necessary due to numeric rounding issues)
#    d_non_zero[which(upper.tri(d_non_zero),arr.ind = T)[,c("col","row")]] <-
#      d_non_zero[upper.tri(d_non_zero)]
#  
#    # if all diagrams had at least one row, return
#    if(length(inds) == length(diagrams))
#    {
#      return(d_non_zero)
#    }
#  
#    # create empty distance matrix d
#    d <- matrix(data = 0,nrow = length(diagrams),ncol = length(diagrams))
#  
#    # update entries of d
#    e <- as.matrix(expand.grid(inds,inds))
#    e <- e[which(e[,1] < e[,2]),]
#    if(!is.matrix(e))
#    {
#      e <- t(as.matrix(e))
#    }
#    d[e] <- d_non_zero[which(upper.tri(d_non_zero),arr.ind = T)]
#    e <- e[,2:1]
#    if(!is.matrix(e))
#    {
#      e <- t(as.matrix(e))
#    }
#    d[e] <- d_non_zero[which(upper.tri(d_non_zero),arr.ind = T)]
#  
#    return(d)
#  
#  }
#  
#  # Gram matrix function
#  # diagrams is a list of diagrams (only birth and death columns)
#  # t is the t parameter like in diagram_kernel
#  # gudhi_dist is the rgudhi distance object
#  gudhi_gram_matrix <- function(diagrams,t,gudhi_dist){
#  
#    # calculate distance matrix
#    D <- gudhi_distance_matrix(diagrams = diagrams,gudhi_dist = gudhi_dist)
#    return(exp(-t*D))
#  
#  }
#  
#  # calculate the Gram matrix
#  G <- gudhi_gram_matrix(diagrams = g,t = 1,gudhi_dist = gudhi_dist)

## ----eval = F-----------------------------------------------------------------
#  # create data frame
#  D = data.frame(dimension = c(0,0),birth = c(0,0),death = c(1.089866,1.098640))
#  
#  # create rgudhi distance object
#  gudhi_dist <- rgudhi::PersistenceFisherDistance$new(bandwidth = 0.01,n_jobs = 1L)
#  
#  # compute distance
#  gudhi_dist$apply(D[,2:3],D[,2:3])

## ----eval = T,echo = F--------------------------------------------------------
0.00000001490116

## ----echo = F-----------------------------------------------------------------
D = data.frame(dimension = c(0,0),birth = c(0,0),death = c(1.089866,1.098640))

## -----------------------------------------------------------------------------
diagram_distance(D,D,distance = "fisher",sigma = 0.001)

## ----echo = F-----------------------------------------------------------------
# reset parameters
par(mfrow = original_mfrow,xpd = original_xpd,mar = original_mar)
options(scipen = original_scipen)
do.call("RNGkind",as.list(oldRNGkind))
assign(".Random.seed", oldseed, .GlobalEnv)

