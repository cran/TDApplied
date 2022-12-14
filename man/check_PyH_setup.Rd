% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/python_functions.R
\name{check_PyH_setup}
\alias{check_PyH_setup}
\title{Make sure that python has been configured correctly for persistent homology calculations.}
\usage{
check_PyH_setup()
}
\description{
Ensures that the reticulate package has been installed, that python is available to be used
by reticulate functions, and that the python module "ripser" has been installed.
}
\details{
An error message will be thrown if any of the above conditions are not met.
}
\author{
Shael Brown - \email{shaelebrown@gmail.com}
}
