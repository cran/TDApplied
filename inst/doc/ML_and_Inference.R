## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
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

## ----setup--------------------------------------------------------------------
library("TDApplied")

## ----echo = F,fig.height = 3,fig.width = 7,fig.align = 'center',eval = requireNamespace("TDAstats")----
par(mfrow = c(1,4))
circ <- TDAstats::circle2d[sample(1:100,50),]
plot(x = circ[,1],y = circ[,2],main = "Approximation 1:\nindividual data points",xlab = "",ylab = "",las = 1)
plot(x = circ[,1],y = circ[,2],main = "Approximation 2:\nnot a loop",xlab = "",ylab = "",las = 1)
for(i in 1:(nrow(circ)-1))
{
  for(j in (i+1):nrow(circ))
  {
    if(sqrt((circ[i,1]-circ[j,1])^2+(circ[i,2] - circ[j,2])^2) <= 0.2)
    {
      lines(c(circ[i,1],circ[j,1]),c(circ[i,2],circ[j,2]))
    }
  }
}
plot(x = circ[,1],y = circ[,2],main = "Approximation 3:\nloop",xlab = "",ylab = "",las = 1)
for(i in 1:(nrow(circ)-1))
{
  for(j in (i+1):nrow(circ))
  {
    if(sqrt((circ[i,1]-circ[j,1])^2+(circ[i,2] - circ[j,2])^2) <= 1)
    {
      lines(c(circ[i,1],circ[j,1]),c(circ[i,2],circ[j,2]))
    }
  }
}
plot(x = circ[,1],y = circ[,2],main = "Approximation 4:\nnot a loop",xlab = "",ylab = "",las = 1)
for(i in 1:(nrow(circ)-1))
{
  for(j in (i+1):nrow(circ))
  {
    if(sqrt((circ[i,1]-circ[j,1])^2+(circ[i,2] - circ[j,2])^2) <= 2)
    {
      lines(c(circ[i,1],circ[j,1]),c(circ[i,2],circ[j,2]))
    }
  }
}
par(mfrow = c(1,1))

## ----echo = F,eval = T--------------------------------------------------------
# create the data
circ <- data.frame(x = c(-0.815663170207959,-0.595099344842628,0.864889292249816,0.20793951765139,-0.954866650195679,-0.18706988463915,-0.798725432243614,0.830591026518797,-0.661799977235305,0.999878714877245,0.245504222689744,-0.431326750789015,0.666897829439335,0.30720650008886,0.661328840033743,-0.76039885584628,0.239379052558954,-0.351042298907497,-0.829639564028719,0.71216384463492,-0.940714425455924,-0.949907158729059,0.101402707416082,0.986585898475086,0.33772426530395,0.734256674456601,0.771307272532461,0.980540492964891,-0.750935667779769,0.63057972598311,0.75993549565125,-0.997652044265574,-0.199144715289604,-0.996513284779077,0.997484507122883,0.918374523107964,0.54816769740782,0.22028039764542,-0.965072372227865,-0.86565030108487,0.999975356226944,-0.0356376975101777,-0.998775901784022,0.879402540916111,0.389455305917558,-0.73827286958174,0.322030655401438,0.321397994754972,0.943823546436826,-0.565572997267532),y = c(0.578527089051413,-0.803652144754107,-0.501962660116878,0.978141685544025,-0.297034813353725,-0.982346608006102,0.601695673814639,0.55688288415649,-0.749680458683131,0.0155741945354593,0.969395521261319,-0.902195784768357,-0.745149169689602,-0.951642877503506,0.750096104069088,0.649456372690012,-0.970926191425475,-0.936359602064153,-0.558299376498163,0.702013289329204,-0.339199601619947,0.312532222011246,-0.994845460827303,0.163242962880818,0.941245090624598,-0.678871958484023,0.63646295362616,-0.196316941847027,0.660375365119076,-0.776124480466288,-0.649998494190016,0.0684864845989492,-0.979970092590699,0.0834342451204145,-0.0708848224221418,0.395712313816768,0.836368444836729,-0.975436592717935,-0.261983427648545,0.500649134855612,-0.00702046571072753,0.999364775503006,-0.0494641083566077,0.476078954618127,-0.921045365165398,0.674502164592185,-0.946729241642889,0.946944205836586,-0.330449864868202,0.824698238607201))

## ----echo = F,warning=F,fig.height = 4,fig.width = 4,fig.align = 'center'-----
plot(x = circ$x,y = circ$y,xlab = "x",ylab = "y",main = "circ")

## ----echo = T,eval = F--------------------------------------------------------
#  # import the ripser module
#  ripser <- import_ripser()
#  
#  # calculate the persistence diagram
#  diag <- PyH(X = circ,maxdim = 1,thresh = 2,ripser = ripser)
#  
#  # view last five rows of the diagram
#  diag[47:51,]

## ----echo = F,eval = T--------------------------------------------------------
diag <- data.frame(dimension = c(rep(0,50),1),birth = c(rep(0,50),0.5579783),death = c(0.0123064760118723,0.0144490944221616,0.0149910748004913,0.0156172784045339,0.0172923970967531,0.0189713705331087,0.0196240246295929,0.0225948672741652,0.0286996569484472,0.0365069359540939,0.038569450378418,0.0386403761804104,0.0444764532148838,0.0477333702147007,0.0612373314797878,0.0639129132032394,0.0699725300073624,0.0705153122544289,0.0721508488059044,0.0791449695825577,0.0858016163110733,0.0872511267662048,0.0882881134748459,0.0893174782395363,0.0925402045249939,0.0944025367498398,0.0944980531930923,0.099234826862812,0.117955945432186,0.120451688766479,0.126571387052536,0.139067515730858,0.142296731472015,0.148265853524208,0.158034011721611,0.181465998291969,0.188804805278778,0.19113427400589,0.20612421631813,0.21517525613308,0.228875741362572,0.233790531754494,0.235128790140152,0.242270082235336,0.244500055909157,0.245646774768829,0.254552245140076,0.281323730945587,0.288743227720261,2,1.73859250545502))
diag[47:51,]

## ----echo=FALSE,fig.height = 5,fig.width = 5,fig.align = 'center'-------------
theta <- stats::runif(n = 100,min = 0,max = 2*pi)
x <- cos(theta)
y <- sin(theta)
origin <- data.frame(x = 0,y = 0)
new_data <- rbind(data.frame(x = x,y = y), origin)
layout <- as.matrix(new_data)
rownames(layout) <- as.character(1:nrow(new_data))
vrs <- vr_graphs(new_data, eps = c(0.0001,1))
plot_vr_graph(vrs, eps = (0.0001), layout = layout, plot_isolated_vertices = TRUE,vertex_labels = FALSE)

## -----------------------------------------------------------------------------
enc_rad <- enclosing_radius(new_data, distance_mat = FALSE)
print(enc_rad)

## ----echo = FALSE,fig.height = 5,fig.width = 5,fig.align = 'center'-----------
plot_vr_graph(vrs, eps = enc_rad, layout = layout,vertex_labels = F)

## ----echo = T,eval = F--------------------------------------------------------
#  # convert TDA diagram into data frame
#  diag1 <- TDA::ripsDiag(circ,maxdimension = 1,maxscale = 2,library = "dionysus")
#  diag1_df <- diagram_to_df(diag1)
#  class(diag1_df)

## ----echo = F-----------------------------------------------------------------
c("data.frame")

## ----echo = T,eval = F--------------------------------------------------------
#  # convert TDAstats diagram into data frame
#  diag2 <- TDAstats::calculate_homology(circ,dim = 1,threshold = 2)
#  diag2_df <- diagram_to_df(diag1)
#  class(diag2_df)

## ----echo = F-----------------------------------------------------------------
c("data.frame")

## ----echo = F,fig.height = 3,fig.width = 7,fig.align = 'center'---------------
D1 = data.frame(dimension = c(0),birth = c(2),death = c(3))
D2 = data.frame(dimension = c(0),birth = c(2,0),death = c(3.3,0.5))
D3 = data.frame(dimension = c(0),birth = c(0),death = c(0.5))
par(mfrow = c(1,3))
plot_diagram(D1,title = "D1",max_radius = 4,legend = F)
plot_diagram(D2,title = "D2",max_radius = 4,legend = F)
plot_diagram(D3,title = "D3",max_radius = 4,legend = F)

## ----echo = F,fig.height = 3,fig.width = 5,fig.align = 'center'---------------
par(mfrow = c(1,2))
plot(x = c(D1$birth,D2$birth),y = c(D1$death,D2$death),main = "Best matching D1,D2",xlab = "",ylab = "",xlim = c(0,4),ylim = c(0,4))
abline(a = 0,b = 1)
lines(c(2,2),c(3,3.3))
lines(c(0,0.25),c(0.5,0.25))

plot(x = c(D1$birth,D3$birth),y = c(D1$death,D3$death),main = "Best matching D1,D3",xlab = "",ylab = "",xlim = c(0,4),ylim = c(0,4))
abline(a = 0,b = 1)
lines(c(2,2.5),c(3,2.5))
lines(c(0,0.25),c(0.5,0.25))
par(mfrow = c(1,1))

## ----echo = F,fig.height = 3,fig.width = 7,fig.align = 'center'---------------
par(mfrow = c(1,3))
x = seq(-4,4,0.01)
y = seq(-4,4,0.01)

D1_with_diag = rbind(D1,data.frame(dimension = c(0),birth = c(0.25),death = c(0.25)))
z1 = outer(x,y,FUN = function(x,y){
  
  # sigma = 1
  return((exp(-((x-D1_with_diag[1,2])^2+(y-D1_with_diag[1,3])^2)/(2*1^2)))/sqrt(2*pi*1^2) + 
           (exp(-((x-D1_with_diag[2,2])^2+(y-D1_with_diag[2,3])^2)/(2*1^2)))/sqrt(2*pi*1^2))
  
})
z1 = z1/sum(z1)
image(x = x,y = y,z1,main = "Distribution for D1",xlab = "",xlim = c(-4,4),ylim = c(-4,4),ylab = "")
abline(a = 0,b = 1)

D3_with_diag = rbind(D3,data.frame(dimension = c(0),birth = c(2.5),death = c(2.5)))
z3 = outer(x,y,FUN = function(x,y){
  
  # sigma = 1
  return((exp(-((x-D3_with_diag[1,2])^2+(y-D3_with_diag[1,3])^2)/(2*1^2)))/sqrt(2*pi*1^2) + 
           (exp(-((x-D3_with_diag[2,2])^2+(y-D3_with_diag[2,3])^2)/(2*1^2)))/sqrt(2*pi*1^2))
  
})
z3 = z3/sum(z3)
image(x = x,y = y,z3,main = "Distribution for D3",xlab = "",xlim = c(-4,4),ylim = c(-4,4),ylab = "")
abline(a = 0,b = 1)

image(x = x,y = y,z1-z3,main = "Difference of distributions",xlab = "",xlim = c(-4,4),ylim = c(-4,4),ylab = "")
abline(a = 0,b = 1)

par(mfrow = c(1,1))

## ----echo = T-----------------------------------------------------------------
# calculate 2-wasserstein distance between D1 and D2
diagram_distance(D1,D2,dim = 0,p = 2,distance = "wasserstein")

# calculate 2-wasserstein distance between D1 and D3
diagram_distance(D1,D3,dim = 0,p = 2,distance = "wasserstein")

# calculate bottleneck distance between D1 and D2
diagram_distance(D1,D2,dim = 0,p = Inf,distance = "wasserstein")

# calculate bottleneck distance between D1 and D3
diagram_distance(D1,D3,dim = 0,p = Inf,distance = "wasserstein")

## ----echo = T-----------------------------------------------------------------
# Fisher information metric calculation between D1 and D2 for sigma = 1
diagram_distance(D1,D2,dim = 0,distance = "fisher",sigma = 1)

# Fisher information metric calculation between D1 and D3 for sigma = 1
diagram_distance(D1,D3,dim = 0,distance = "fisher",sigma = 1)

## ----echo = T,eval = F--------------------------------------------------------
#  # Fisher information metric calculation between D1 and D2 for sigma = 1
#  diagram_distance(D1,D2,dim = 0,distance = "fisher",sigma = 1)

## ----echo = F-----------------------------------------------------------------
0.02354779

## ----echo = T,eval = F--------------------------------------------------------
#  # fast approximate Fisher information metric calculation between D1 and D3 for sigma = 1
#  diagram_distance(D1,D2,dim = 0,distance = "fisher",sigma = 1,rho = 0.001)

## ----echo = F-----------------------------------------------------------------
0.02354779

## ----echo = T-----------------------------------------------------------------
# calculate the kernel value between D1 and D2 with sigma = 2, t = 2
diagram_kernel(D1,D2,dim = 0,sigma = 2,t = 2)
# calculate the kernel value between D1 and D3 with sigma = 2, t = 2
diagram_kernel(D1,D3,dim = 0,sigma = 2,t = 2)

## ----echo = T,fig.height = 5,fig.width = 5,fig.align = 'center'---------------
plot_diagram(diag,title = "Circle diagram")

## -----------------------------------------------------------------------------
# determine significant features
circ_result <- universal_null(circ,
                                   thresh = enclosing_radius(circ))
circ_result$subsetted_diag

## ----echo = T,fig.height = 5,fig.width = 5,fig.align = 'center',eval = requireNamespace("TDA")----
# create circle with noise dataset and plot
circ_with_noise <- circ
x_noise <- stats::rnorm(n = 50,sd = 0.1)
y_noise <- stats::rnorm(n = 50,sd = 0.1)
circ_with_noise$x <- circ_with_noise$x + x_noise
circ_with_noise$y <- circ_with_noise$y + y_noise
plot(circ_with_noise)

# rerun the inference procedure
library(TDA)
noisy_circ_result <- universal_null(circ_with_noise, 
                                         FUN_diag = "ripsDiag",
                                         thresh = enclosing_radius(circ_with_noise),
                                         return_pvals = TRUE)
noisy_circ_result$subsetted_diag
noisy_circ_result$pvals

## ----eval = requireNamespace("TDA")-------------------------------------------
# inference without infinite cycle inference
res_non_inf_small_thresh <- universal_null(circ_with_noise,
                                           FUN_diag = "ripsDiag",
                                           thresh = 0.9)
res_non_inf_small_thresh$subsetted_diag

# inference with infinite cycle inference
res_inf_small_thresh <- universal_null(circ_with_noise,
                                       FUN_diag = "ripsDiag",
                                       thresh = 0.9,
                                       infinite_cycle_inference = TRUE)
res_inf_small_thresh$subsetted_diag

## ----echo = F,fig.height = 5,fig.width = 5,fig.align = 'center',eval = requireNamespace("TDAstats") & requireNamespace("TDA")----
par(mfrow = c(1,1))
pt <- as.numeric(diag[which(diag[,1L] == 1),])[2:3]
plot_diagram(D = diag,title = "Circ diagram with confidence interval",legend = T,max_radius = 2*0.3953059 + pt[[2]])
graphics::rect(xleft = pt[[1]]-0.3953059,xright = pt[[1]]+0.3953059,ybottom = pt[[2]]-0.3953059,ytop = pt[[2]]+0.3953059)
graphics::lines(x = c(pt[[1]],pt[[1]] + 0.3953059),y = c(pt[[2]],pt[[2]]))
graphics::lines(x = c(pt[[1]],pt[[1]]),y = c(pt[[2]],pt[[2]] - 0.3953059))
graphics::text(x = c(pt[[1]] + 0.3953059/2,0.4),y = c(1.83,1.55),c("t","t"))

## ----fig.height = 4,fig.width = 8,fig.align = 'center',eval = F---------------
#  # calculate the bootstrapped persistence thresholds using 2 cores
#  # and 30 iterations. We'll use the distance matrix of circ to
#  # make representative cycles more comprehensible
#  library("TDA")
#  thresh <- bootstrap_persistence_thresholds(X = as.matrix(dist(circ)),
#                                             FUN_diag = 'ripsDiag',
#                                             FUN_boot = 'ripsDiag',
#                                             distance_mat = T,
#                                             maxdim = 1,thresh = 2,num_workers = 2,
#                                             alpha = 0.05,num_samples = 30,
#                                             return_subsetted = T,return_pvals = T,
#                                             calculate_representatives = T)
#  diag <- thresh$diag
#  
#  # plot original diagram and thresholded diagram side-by-side, including
#  # p-values. These p-values are the smallest possible (1/31) when there
#  # are 30 bootstrap iterations
#  par(mfrow = c(1,2))
#  
#  plot_diagram(diag,title = "Circ diagram")
#  
#  plot_diagram(diag,title = "Circ diagram with thresholds",
#               thresholds = thresh$thresholds)
#  text(x = c(0.2,0.5),y = c(2,1.8),
#       paste("p = ",round(thresh$pvals,digits = 3)),
#       cex = 0.5)

## ----fig.height = 4,fig.width = 8,fig.align = 'center',eval = T,echo = F------
thresh <- readRDS("thresh.rds")
diag <- thresh$diag
par(mfrow = c(1,2))

plot_diagram(diag,title = "Circ diagram")

plot_diagram(diag,title = "Circ diagram with thresholds",
             thresholds = thresh$thresholds)
text(x = c(0.2,0.5),y = c(2,1.8),
     paste("p = ",round(thresh$pvals,digits = 3)),
     cex = 0.5)

## ----echo = F-----------------------------------------------------------------
par(mfrow = c(1,1))

## ----echo = T,eval = F--------------------------------------------------------
#  # ripser has already been imported, so calculate diagram with representatives
#  diag_rep <- PyH(circ,maxdim = 1,thresh = 2,ripser = ripser,calculate_representatives = T)
#  
#  # identify the loops in the diagram
#  diag_rep$diagram[which(diag_rep$diagram$dimension == 1),]

## ----echo = F-----------------------------------------------------------------
data.frame(dimension = c(1),birth = c(0.5579783),death = c(1.738593),row.names = c("50"))

## ----echo = T,eval = F--------------------------------------------------------
#  # show the representative for the loop, just the first five rows
#  diag_rep$representatives[[2]][[1]][1:5,]

## ----echo = F-----------------------------------------------------------------
representative = matrix(data = c(50,42,46,42,50,4,42,29,42,16,50,11,42,7,42,1,50,48,50,25,42,40,46,4,29,4,16,4,46,11,29,11,16,11,7,4,48,46,4,1,11,7,46,25,48,29,50,37,48,16,29,25,11,1,25,16,42,22,48,7,40,4,25,7,48,1,40,11,25,1,50,15,48,40,40,25,50,20,46,37,37,29,37,16,42,34,22,4,42,32,50,27,22,11,37,7,37,1,46,15,29,15,48,22,50,8,43,42,16,15,25,22,46,20,40,37,29,20,15,7,20,16,50,44,15,1,34,4,46,27,32,4,20,7,29,27,34,11,27,16,20,1,32,11,50,36,40,15,42,39,27,7,46,8,48,34,48,32,29,8,43,4,34,25,37,22,27,1,42,5,40,20,16,8,32,25,43,11,42,21,46,44,8,7,44,29,40,27,8,1,44,16,48,43,43,25,22,15,46,36,44,7,50,24,36,29,40,8,36,16,44,1,39,4,22,20,37,34,5,4,37,32,39,11,36,7),nrow = 113,ncol = 2,byrow = T)
representative[1:5,]

## ----echo = T,eval = F--------------------------------------------------------
#  unique(c(diag_rep$representatives[[2]][[1]][,1],diag_rep$representatives[[2]][[1]][,2]))

## ----echo = F-----------------------------------------------------------------
unique(c(representative[,1],representative[,2]))

## ----echo = T,eval = F--------------------------------------------------------
#  plot(x = circ$x,y = circ$y,xlab = "x",ylab = "y",main = "circ with representative")
#  for(i in 1:nrow(circ))
#  {
#    for(j in 1:nrow(circ))
#    {
#      pt1 <- circ[i,]
#      pt2 <- circ[j,]
#      if(sqrt((pt1[[1]] - pt2[[1]])^2 + (pt1[[2]] - pt2[[2]])^2) <= 1.7)
#      {
#        graphics::lines(x = c(pt1[[1]],pt2[[1]]),y = c(pt1[[2]],pt2[[2]]))
#      }
#    }
#  }
#  
#  for(i in 1:nrow(diag_rep$representatives[[2]][[1]]))
#  {
#    pt1 <- circ[diag_rep$representatives[[2]][[1]][i,1],]
#    pt2 <- circ[diag_rep$representatives[[2]][[1]][i,2],]
#    if(sqrt((pt1[[1]] - pt2[[1]])^2 + (pt1[[2]] - pt2[[2]])^2) <= 1.7)
#    {
#      graphics::lines(x = c(pt1[[1]],pt2[[1]]),y = c(pt1[[2]],pt2[[2]]),col = "red")
#    }
#  }

## ----echo = F,warning=F,fig.height = 4,fig.width = 4,fig.align = 'center'-----
plot(x = circ$x,y = circ$y,xlab = "x",ylab = "y",main = "circ with representative")
for(i in 1:nrow(circ))
{
  for(j in 1:nrow(circ))
  {
    pt1 <- circ[i,]
    pt2 <- circ[j,]
    if(sqrt((pt1[[1]] - pt2[[1]])^2 + (pt1[[2]] - pt2[[2]])^2) < 1.7)
    {
      graphics::lines(x = c(pt1[[1]],pt2[[1]]),y = c(pt1[[2]],pt2[[2]]))
    }
  }
}

for(i in 1:nrow(representative))
{
  pt1 <- circ[representative[i,1],]
  pt2 <- circ[representative[i,2],]
  if(sqrt((pt1[[1]] - pt2[[1]])^2 + (pt1[[2]] - pt2[[2]])^2) <= 1.7)
  {
    graphics::lines(x = c(pt1[[1]],pt2[[1]]),y = c(pt1[[2]],pt2[[2]]),col = "red")
  }
}

## ----echo = F,warning=F,fig.height = 4,fig.width = 4,fig.align = 'center'-----
plot(x = circ$x,y = circ$y,xlab = "x",ylab = "y",main = "circ with representative")
for(i in 1:nrow(circ))
{
  for(j in 1:nrow(circ))
  {
    pt1 <- circ[i,]
    pt2 <- circ[j,]
    if(sqrt((pt1[[1]] - pt2[[1]])^2 + (pt1[[2]] - pt2[[2]])^2) <= 0.6009)
    {
      graphics::lines(x = c(pt1[[1]],pt2[[1]]),y = c(pt1[[2]],pt2[[2]]))
    }
  }
}

for(i in 1:nrow(representative))
{
  pt1 <- circ[representative[i,1],]
  pt2 <- circ[representative[i,2],]
  if(sqrt((pt1[[1]] - pt2[[1]])^2 + (pt1[[2]] - pt2[[2]])^2) <= 0.6009)
  {
    graphics::lines(x = c(pt1[[1]],pt2[[1]]),y = c(pt1[[2]],pt2[[2]]),col = "red")
  }
}

## ----echo = T,eval = T--------------------------------------------------------
# get half of loop's birth radius
eps_1 <- diag[nrow(diag),2L]/2

# get mean of loop's birth and death radii
eps_2 <- (diag[nrow(diag),3L] + diag[nrow(diag),2L])/2

# compute two VR graphs
gs <- vr_graphs(X = circ,eps = c(eps_1,eps_2))

## ----eval = requireNamespace("igraph"),fig.height = 5,fig.width = 5,fig.align = 'center'----
# plot first graph
plot_vr_graph(gs,eps_1)

# plot second graph
layout <- plot_vr_graph(gs,eps_2,return_layout = TRUE)
layout <- apply(layout,MARGIN = 2,FUN = function(X){
  
  return(-1 + 2*(X - min(X))/(max(X) - min(X)))
  
})

## ----eval = requireNamespace('igraph'),echo = F,fig.width = 5,fig.align = 'center'----
# get the stimuli in the loop
stimuli_in_loop <- unique(as.numeric(thresh$subsetted_representatives[[2]]))

# create colors for the data points, light blue if not in the loop
# and red if in the loop
colors <- rep("lightblue",50) 
colors[stimuli_in_loop] <- "red"

# plot only component containing the loop stimuli with vertex colors
plot_vr_graph(gs,eps_2,cols = colors,component_of = stimuli_in_loop[[1]],layout = layout)

## ----eval = requireNamespace('igraph'),fig.width = 5,fig.align = 'center'-----
# plot only component containing the loop stimuli with vertex colors
plot_vr_graph(gs,eps_2,cols = colors,
              component_of = stimuli_in_loop[[1]],
              vertex_labels = FALSE,
              layout = layout)

# get indices of vertices in loop
# not necessary in this case but necessary when we have
# removed some vertices from the graph
vertex_inds <- match(stimuli_in_loop,as.numeric(rownames(layout)))

# add volcano image over loop nodes
# image could be anything like rasters read from
# png files! this is just an example..
utils::data("volcano")
volcano <- (volcano - min(volcano)) / diff(range(volcano))
for(i in vertex_inds)
{
  graphics::rasterImage(volcano,xleft = layout[i,1L] - 0.05,
                              xright = layout[i,1L] + 0.05,
                              ybottom = layout[i,2L] - 0.05,
                              ytop = layout[i,2L] + 0.05)
}

## ----echo = F,fig.height = 3,fig.width = 7,fig.align = 'center'---------------
par(mfrow = c(1,3))
plot_diagram(D1,title = "D1",max_radius = 4,legend = F)
plot_diagram(D2,title = "D2",max_radius = 4,legend = F)
plot_diagram(D3,title = "D3",max_radius = 4,legend = F)

## ----echo = F-----------------------------------------------------------------
par(mfrow = c(1,1))

## ----echo = F-----------------------------------------------------------------
generate_TDApplied_vignette_data <- function(num_D1,num_D2,num_D3){
  
  # num_D1 is the number of desired copies of D1, and likewise
  # for num_D2 and num_D3
  
  # create data
  D1 = data.frame(dimension = c(0),birth = c(2),death = c(3))
  D2 = data.frame(dimension = c(0),birth = c(2,0),death = c(3.3,0.5))
  D3 = data.frame(dimension = c(0),birth = c(0),death = c(0.5))
  
  # make noisy copies
  noisy_copies <- lapply(X = 1:(num_D1 + num_D2 + num_D3),FUN = function(X){
    
    # i stores the number of the data frame to make copies of:
    # i = 1 is for D1, i = 2 is for D2 and i = 3 is for D3
    i <- 1
    if(X > num_D1 & X <= num_D1 + num_D2)
    {
      i <- 2
    }
    if(X > num_D1 + num_D2)
    {
      i <- 3
    }
    # store correct data in noisy_copy
    noisy_copy <- get(paste0("D",i))
    
    # add Gaussian noise to birth and death values
    n <- nrow(noisy_copy)
    noisy_copy$dimension <- as.numeric(as.character(noisy_copy$dimension))
    noisy_copy$birth <- noisy_copy$birth + stats::rnorm(n = n,mean = 0,sd = 0.05)
    noisy_copy$death <- noisy_copy$death + stats::rnorm(n = n,mean = 0,sd = 0.05)
    
    # make any birth values which are less than 0 equal 0
    noisy_copy[which(noisy_copy$birth < 0),2] <- 0
    
    # make any birth values which are greater than their death values equal their death values
    noisy_copy[which(noisy_copy$birth > noisy_copy$death),2] <- 
      noisy_copy[which(noisy_copy$birth > noisy_copy$death),3]
    return(noisy_copy)
    
  })
  
  # return list containing num_D1 noisy copies of D1, then
  # num_D2 noisy copies of D2, and finally num_D3 noisy copies
  # of D3
  return(noisy_copies)
  
}

## ----echo = F,fig.height = 5,fig.width = 5,fig.align = 'center'---------------
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
noisy_copies_D1 = generate_TDApplied_vignette_data(2,0,0)
cols = factor(c("D1","D1\'","D2\'\'"),levels = c("D1","D1\'","D1\'\'"))
plot(x = c(D1$birth,noisy_copies_D1[[1]]$birth,noisy_copies_D1[[2]]$birth),y = c(D1$death,noisy_copies_D1[[1]]$death,noisy_copies_D1[[2]]$death),main = "D1 and noisy copies",xlab = "",ylab = "",xlim = c(0,4),ylim = c(0,4),col = c("black","red","blue"),bty = "L")
lines(x = c(-0.15,4.15),y = c(-0.15,4.15))
legend("topright", inset=c(-0.2,0), 
       legend=levels(cols), 
       pch=16, col=c("black","red","blue"))

## ----echo = T-----------------------------------------------------------------
# permutation test between three diagrams
g1 <- generate_TDApplied_vignette_data(3,0,0)
g2 <- generate_TDApplied_vignette_data(0,3,0)
g3 <- generate_TDApplied_vignette_data(0,0,3)
perm_test <- permutation_test(g1,g2,g3,
                              num_workers = 2,
                              dims = c(0))
perm_test$p_values

## ----echo = T-----------------------------------------------------------------
# create 10 noisy copies of D1 and D2
g1 <- generate_TDApplied_vignette_data(10,0,0)
g2 <- generate_TDApplied_vignette_data(0,10,0)

# do independence test with sigma = t = 1
indep_test <- independence_test(g1,g2,dims = c(0),num_workers = 2)
indep_test$p_values

## ----echo = F,fig.height = 3,fig.width = 6,fig.align = 'center'---------------
# plot
par(mfrow = c(1,2))
plot(x = circ[,1],y = circ[,2],main = "circ",xlab = "",ylab = "",las = 1)
theta <- stats::runif(n = 50,min = 0,max = 2*pi)
circ2 <- data.frame(x = cos(theta),y = sin(theta))
plot(x = circ2[,1],y = circ2[,2],main = "circ2",xlab = "",ylab = "",las = 1)

## -----------------------------------------------------------------------------
mod_comp <- permutation_model_inference(circ, circ2, iterations = 20,
                                        thresh = 2, num_samples = 3,
                                        num_workers = 2L)
mod_comp$p_values

## ----echo = F,fig.height = 3,fig.width = 6,fig.align = 'center'---------------
# plot
par(mfrow = c(1,2))
plot(x = circ[,1],y = circ[,2],main = "circ",xlab = "",ylab = "",las = 1)
circ_noise <- data.frame(x = circ[,1] + rnorm(50,sd = 0.01),y = circ[,2] + rnorm(50,sd = 0.01))
plot(x = circ_noise[,1],y = circ_noise[,2],main = "circ_noise",xlab = "",ylab = "",las = 1)

## -----------------------------------------------------------------------------
mod_comp_paired <- permutation_model_inference(circ, circ_noise, iterations = 20,
                                        thresh = 2, num_samples = 3,
                                        paired = TRUE, num_workers = 2L)
mod_comp_paired$p_values

## ----eval = F-----------------------------------------------------------------
#  # in this case we are creating 3 samples of the rows of circ
#  # (and equivalently the other two datasets)
#  boot_samples <- lapply(X = 1:3,
#                         FUN = function(X){return(unique(sample(1:nrow(circ),size = nrow(circ),replace = TRUE)))})
#  
#  # carry out three model comparisons between circ, circ2 and circ3
#  mod_comp_1_2 <- permutation_model_inference(circ, circ2, iterations = 20,
#                                          thresh = 2, num_samples = 3,
#                                          paired = TRUE, num_workers = 2L,
#                                          samp = boot_samples)
#  mod_comp_1_3 <- permutation_model_inference(circ, circ3, iterations = 20,
#                                          thresh = 2, num_samples = 3,
#                                          paired = TRUE, num_workers = 2L,
#                                          samp = boot_samples)
#  mod_comp_2_3 <- permutation_model_inference(circ2, circ3, iterations = 20,
#                                          thresh = 2, num_samples = 3,
#                                          paired = TRUE, num_workers = 2L,
#                                          samp = boot_samples)

## ----echo = T-----------------------------------------------------------------
# create noisy copies of D1, D2 and D3
g <- generate_TDApplied_vignette_data(3,3,3)
                              
# calculate kmeans clusters with centers = 3, and sigma = t = 0.1
clust <- diagram_kkmeans(diagrams = g,centers = 3,dim = 0,t = 0.1,sigma = 0.1,num_workers = 2)

# display cluster labels
clust$clustering@.Data

## ----echo = T-----------------------------------------------------------------
# create nine new diagrams
g_new <- generate_TDApplied_vignette_data(3,3,3)

# predict cluster labels
predict_diagram_kkmeans(new_diagrams = g_new,clustering = clust,num_workers = 2)

## ----echo = T,fig.height = 3,fig.width = 6,fig.align = 'center'---------------
# create 9 diagrams based on D1, D2 and D3
g <- generate_TDApplied_vignette_data(3,3,3)

# calculate their 2D MDS embedding in dimension 0 with the bottleneck distance
mds <- diagram_mds(diagrams = g,dim = 0,p = Inf,k = 2,num_workers = 2)

# plot
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(mds[,1],mds[,2],xlab = "Embedding coordinate 1",ylab = "Embedding coordinate 2",
     main = "MDS plot",col = as.factor(rep(c("D1","D2","D3"),each = 3)),bty = "L")
legend("topright", inset=c(-0.2,0), 
       legend=levels(as.factor(c("D1","D2","D3"))), 
       pch=16, col=unique(as.factor(c("D1","D2","D3"))))

## ----echo = T,fig.height = 3,fig.width = 6,fig.align = 'center'---------------
# create noisy copies of D1, D2 and D3
g <- generate_TDApplied_vignette_data(3,3,3)

# calculate their 2D PCA embedding with sigma = t = 2
pca <- diagram_kpca(diagrams = g,dim = 0,t = 2,sigma = 2,features = 2,num_workers = 2)

# plot
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(pca$pca@rotated[,1],pca$pca@rotated[,2],xlab = "Embedding coordinate 1",
     ylab = "Embedding coordinate 2",main = "PCA plot",
     col = as.factor(rep(c("D1","D2","D3"),each = 3)))
legend("topright",inset = c(-0.2,0), 
       legend=levels(as.factor(c("D1","D2","D3"))), pch=16, 
       col=unique(as.factor(c("D1","D2","D3"))))

## ----echo = T,fig.height = 3,fig.width = 6,fig.align = 'center'---------------
# create nine new diagrams
g_new <- generate_TDApplied_vignette_data(3,3,3)

# project new diagrams onto old model
new_pca <- predict_diagram_kpca(new_diagrams = g_new,embedding = pca,num_workers = 2)

# plot
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(new_pca[,1],new_pca[,2],xlab = "Embedding coordinate 1",
     ylab = "Embedding coordinate 2",main = "PCA prediction plot",
     col = as.factor(rep(c("D1","D2","D3"),each = 3)))
legend("topright",inset = c(-0.2,0), 
       legend=levels(as.factor(c("D1","D2","D3"))), pch=16, 
       col=unique(as.factor(c("D1","D2","D3"))))

## ----echo = T-----------------------------------------------------------------
# create thirty noisy copies of D1, D2 and D3
g <- generate_TDApplied_vignette_data(10,10,10)

# create response vector
y <- as.factor(rep(c("D1","D2","D3"),each = 10))

# fit model with cross validation
model_svm <- diagram_ksvm(diagrams = g,cv = 2,dim = c(0),
                          y = y,sigma = c(1,0.1),t = c(1,2),
                          num_workers = 2)

## ----echo = T-----------------------------------------------------------------
# create nine new diagrams
g_new <- generate_TDApplied_vignette_data(3,3,3)

# predict
predict_diagram_ksvm(new_diagrams = g_new,model = model_svm,num_workers = 2)

## ----echo = F-----------------------------------------------------------------
# reset parameters
par(mfrow = original_mfrow,xpd = original_xpd,mar = original_mar)
options(scipen = original_scipen)
do.call("RNGkind",as.list(oldRNGkind))
assign(".Random.seed", oldseed, .GlobalEnv)

