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

## ----echo=FALSE,message=FALSE-------------------------------------------------
library("TDApplied")

## ----echo = F,out.width="45%", out.height="45%",fig.show="hold",fig.align = 'center',fig.cap="The VR graphs of (left) just the representative cycle time points, and (right) all time points, with both epsilon scales at the loop birth value."----
g <- readRDS("rips_cycle.rds")
plot_vr_graph(g,eps = as.numeric(names(g$graphs)),vertex_labels = T,title = "Rips graph of representative cycle") # clear loop
g <- readRDS("rips_all.rds")
cols <- readRDS("cols.rds")
l <- plot_vr_graph(g,eps = as.numeric(names(g$graphs)),component_of = 20,vertex_labels = F,cols = cols,return_layout = T,title = "Rips graph of all data") # two clear loops! One big, one dense

## ----echo = F,out.width="45%", out.height="45%",fig.show="hold",fig.align = 'center',fig.cap="VR graph of all time points, colored by (left) mean respiration and (right) time-since-last-block."----
cols_respiratory <- readRDS("cols_respiratory.rds")
cols_time_since_last_block <- readRDS("cols_time_since_last_block.rds")
plot_vr_graph(g,eps = as.numeric(names(g$graphs)),cols = cols_respiratory,component_of = 20,vertex_labels = F,layout = l,title = "Respiratory")
plot_vr_graph(g,eps = as.numeric(names(g$graphs)),cols = cols_time_since_last_block,component_of = 20,vertex_labels = F,layout = l,title = "Time since last block")

## ----echo = F,out.width="45%", out.height="45%",fig.show="hold",fig.align = 'center',fig.cap = "VR graph of the secondary loop, colored by time-since-last-block."----
secondary_loop <- readRDS("rips_secondary.rds")
plot_vr_graph(secondary_loop,eps = as.numeric(names(secondary_loop$graphs)),component_of = 99,vertex_labels = F,cols = cols_time_since_last_block[as.numeric(secondary_loop$vertices)],title = "Rips graph of secondary loop")

## ----echo = F,out.width="45%", out.height="45%",fig.show="hold",fig.align = 'center',fig.cap="Surface nodes whose activity was significantly correlated with (left) theta and (right) r."----
knitr::include_graphics(c("theta_nodes.png"))
knitr::include_graphics(c("r_nodes.png"))

## ----echo = F,out.width="45%", out.height="45%",fig.show="hold",fig.align = 'center',fig.cap="Boxplot of r values in shape and face blocks."----
r_shape <- readRDS("r_shape.rds")
r_face <- readRDS("r_face.rds")
graphics::boxplot(r_shape,r_face,main = "r across conditions, p < 0.01",xlab = "Condition",ylab = "r",names = c("Shape","Face"))

## ----echo = F,out.width="45%", out.height="45%",fig.show="hold",fig.align = 'center',fig.cap="A 2D scatterplot, whose x-axis is the 1D PCA embedding coordinates of the 100 subject's emotion persistence diagrams and whose y-axis is the 100 subject's mean response times in the shape blocks trials."----
emb <- readRDS("emb.rds")
shape_rt <- readRDS("shape_rt.rds")
plot(emb,shape_rt,xlab = "Embedding dim 1",ylab = "Mean Shape Block Reaction Time (ms)",main = "Topology-Behavior Relationship")
l <- lm(formula = shape_rt ~ emb)
coefficients <- as.numeric(coef(l))
graphics::lines(x = c(min(emb),max(emb)),y = coefficients[[1]] + coefficients[[2]]*c(min(emb),max(emb)))
cor_val <- round(cor(emb,shape_rt),digits = 2)
graphics::text(x = 0,y = 1400,paste0("Correlation = ",cor_val))

## ----echo = F-----------------------------------------------------------------
# reset parameters
par(mfrow = original_mfrow,xpd = original_xpd,mar = original_mar)
options(scipen = original_scipen)
do.call("RNGkind",as.list(oldRNGkind))
assign(".Random.seed", oldseed, .GlobalEnv)

