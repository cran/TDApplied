#' Machine learning and inference for persistence diagrams
#'
#' This package aims to bridge topological data analysis (TDA) with data, statistical
#' and machine learning practitioners so that more analyses may benefit from the
#' power of TDA. The main tool of TDA is persistent homology, which computes a 
#' shape descriptor of a dataset, called a persistence diagram. There are five
#' goals of this package: (1) convert the output from the persistent homology
#' calculations in the two main R package for TDA into a data frame which can
#' easily be used in personalized analyses, (2) provide a fast method for computing
#' distances between persistence diagrams, (3) implement kernel machine learning and statistical
#' methods for analyzing groups of persistence diagrams, (4) supply a fast persistent homology 
#' calculation via python and reticulate, and (5) provide tools for interpreting persistence
#' diagrams.
#'
#' @docType package
#' @name TDApplied
#' @importFrom clue solve_LSAP
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom iterators iter
#' @importFrom kernlab as.kernelMatrix kkmeans kpca ksvm predict
#' @importFrom parallel clusterEvalQ clusterExport detectCores makeCluster stopCluster
#' @importFrom parallelly availableCores
#' @importFrom rdist cdist
#' @importFrom stats cmdscale complete.cases pgamma lm quantile
#' @importFrom utils combn
#' @importFrom iterators iter
#' @importFrom graphics abline legend
NULL
