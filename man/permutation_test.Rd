% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inference.R
\name{permutation_test}
\alias{permutation_test}
\title{Permutation test for finding group differences between persistence diagrams.}
\usage{
permutation_test(
  ...,
  iterations = 20,
  p = 2,
  q = 2,
  dims = c(0, 1),
  paired = FALSE,
  distance = "wasserstein",
  sigma = NULL,
  num_workers = parallelly::availableCores(omit = 1),
  verbose = FALSE
)
}
\arguments{
\item{...}{lists of persistence diagrams which are either the output of persistent homology calculations like \code{\link[TDA]{ripsDiag}}/\code{\link[TDAstats]{calculate_homology}}/\code{\link{PyH}}, or \code{\link{diagram_to_df}}. Each list must contain at least 2 diagrams.}

\item{iterations}{the number of iterations for permuting group labels, default 20.}

\item{p}{a positive number representing the wasserstein power parameter, a number at least 1 (and Inf if using the bottleneck distance) and default 2.}

\item{q}{a finite number at least 1 for exponentiation in the Turner loss function, default 2.}

\item{dims}{a non-negative integer vector of the homological dimensions in which the test is to be carried out, default c(0,1).}

\item{paired}{a boolean flag for if there is a second-order pairing between diagrams at the same index in different groups, default FALSE}

\item{distance}{a string which determines which type of distance calculation to carry out, either "wasserstein" (default) or "fisher".}

\item{sigma}{the positive bandwidth for the Fisher information metric, default NULL.}

\item{num_workers}{the number of cores used for parallel computation, default is one less than the number of cores on the machine.}

\item{verbose}{a boolean flag for if the time duration of the function call should be printed, default FALSE}
}
\value{
a list with the following elements:
\describe{

 \item{dimensions}{the input `dims` argument.}

 \item{permvals}{a numeric vector of length `iterations` with the permuted loss value for each iteration (permutation)}
 
 \item{test_statisics}{a numeric vector of the test statistic value in each dimension.}
 
 \item{p_values}{a numeric vector of the p-values in each dimension.}
 
 \item{run_time}{the run time of the function call, containing time units.}

}
}
\description{
A non-parametric ANOVA-like test for persistence diagrams 
(see \url{https://link.springer.com/article/10.1007/s41468-017-0008-7} for details). In each
desired dimension a test statistic (loss) is calculated, then the group labels are shuffled
for some number of iterations and the loss is recomputed each time thereby generating a null
distribution for the test statistic. This test generates a p-value in each desired dimension.
}
\details{
The test is carried out in parallel and optimized in order to not recompute already-calculated distances. As such, memory issues
may occur when the number of persistence diagrams is very large. 
Like in (\url{https://github.com/hassan-abdallah/Statistical_Inference_PH_fMRI/blob/main/Abdallah_et_al_Statistical_Inference_PH_fMRI.pdf})
an option is provided for pairing diagrams between groups to reduce variance (in order to boost statistical power), and
like it was suggested in the original paper functionality is provided for an arbitrary number of groups (not just 2).
A small p-value in a dimension suggests that the groups are different (separated) in that dimension.
If `distance` is "fisher" then `sigma` must not be NULL. TDAstats also has a `permutation_test` function
so care should be taken to use the desired function when using TDApplied with TDAstats.
}
\examples{

# create two groups of diagrams
D1 <- TDAstats::calculate_homology(TDA::circleUnif(n = 10,r = 1),
                                   dim = 0,threshold = 2)
D2 <- TDAstats::calculate_homology(TDA::circleUnif(n = 10,r = 1),
                                   dim = 0,threshold = 2)
g1 <- list(D1,D2)
g2 <- list(D1,D2)

# run test in dimension 0 with 1 iteration
perm_test <- permutation_test(g1,g2,iterations = 1,
                              num_workers = 2,
                              dims = c(0))
}
\references{
Robinson T, Turner K (2017). "Hypothesis testing for topological data analysis." \url{https://link.springer.com/article/10.1007/s41468-017-0008-7}.

Abdallah H et al. (2021). "Statistical Inference for Persistent Homology applied to fMRI." \url{https://github.com/hassan-abdallah/Statistical_Inference_PH_fMRI/blob/main/Abdallah_et_al_Statistical_Inference_PH_fMRI.pdf}.
}
\author{
Shael Brown - \email{shaelebrown@gmail.com}
}
