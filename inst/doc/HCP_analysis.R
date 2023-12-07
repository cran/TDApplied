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
par(mfrow = c(1,1))

## ----echo = F,out.width="45%", out.height="45%",fig.show="hold",fig.align = 'center',fig.cap="The VR graphs of (left) just the representative cycle time points, and (right) all time points, with both epsilon scales at the loop birth value."----
knitr::include_graphics(c("rips_cycle.png","rips_all.png"))

## ----echo = F,out.width="45%", out.height="45%",fig.show="hold",fig.align = 'center',fig.cap="VR graph of all time points, colored by (left) mean respiration and (right) time-since-last-block."----
knitr::include_graphics(c("rips_respiratory.png","rips_task.png"))

## ----echo = F,out.width="10%", out.height="10%",fig.show="hold",fig.align = 'center'----
knitr::include_graphics(c("color_bar.png"))

## ----echo = F,out.width="45%", out.height="45%",fig.show="hold",fig.align = 'center',fig.cap = "VR graph of the secondary loop, colored by time-since-last-block."----
knitr::include_graphics(c("rips_secondary.png"))

## ----echo = F,out.width="45%", out.height="45%",fig.show="hold",fig.align = 'center',fig.cap="Surface nodes whose activity was significantly correlated with (left) theta and (right) r."----
knitr::include_graphics(c("theta_nodes.png"))
knitr::include_graphics(c("r_nodes.png"))

## ----echo = F,out.width="45%", out.height="45%",fig.show="hold",fig.align = 'center',fig.cap="Boxplot of r values in shape and face blocks."----
knitr::include_graphics(c("r_boxplot.png"))

## ----echo = F,out.width="45%", out.height="45%",fig.show="hold",fig.align = 'center',fig.cap="A 2D scatterplot, whose x-axis is the 1D PCA embedding coordinates of the 100 subject's emotion persistence diagrams and whose y-axis is the 100 subject's mean response times in the shape blocks trials."----
knitr::include_graphics(c("topology_behavior.png"))

## ----echo = F-----------------------------------------------------------------
# reset parameters
par(mfrow = original_mfrow,xpd = original_xpd,mar = original_mar)
options(scipen = original_scipen)
do.call("RNGkind",as.list(oldRNGkind))
assign(".Random.seed", oldseed, .GlobalEnv)

