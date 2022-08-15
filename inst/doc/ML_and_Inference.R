## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# get original graphic parameters to be able to
# revert back at the end of the vignette
original_mfrow <- par()$mfrow
original_xpd <- par()$xpd
original_mar <- par()$mar

## ----echo = F,fig.height = 3,fig.width = 7,fig.align = 'center'---------------
circ <- TDA::circleUnif(n = 50,r = 1)
par(mfrow = c(1,4))
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

## ----setup--------------------------------------------------------------------
library("TDApplied")

## ----echo = F,fig.height = 3,fig.width = 7,fig.align = 'center'---------------
D1 = data.frame(dimension = c(0),birth = c(2),death = c(3))
D2 = data.frame(dimension = c(0),birth = c(2,0),death = c(3.3,0.5))
D3 = data.frame(dimension = c(0),birth = c(0),death = c(0.5))
par(mfrow = c(1,3))
plot(x = D1$birth,y = D1$death,main = "D1",xlab = "",ylab = "",xlim = c(0,4),ylim = c(0,4))
abline(a = 0,b = 1)
plot(x = D2$birth,y = D2$death,main = "D2",xlab = "",ylab = "",xlim = c(0,4),ylim = c(0,4))
abline(a = 0,b = 1)
plot(x = D3$birth,y = D3$death,main = "D3",xlab = "",ylab = "",xlim = c(0,4),ylim = c(0,4))
abline(a = 0,b = 1)
par(mfrow = c(1,1))

## ----echo = T-----------------------------------------------------------------
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
cols = factor(c("D1","Copy 1","Copy 2"),levels = c("D1","Copy 1","Copy 2"))
plot(x = c(D1$birth,noisy_copies_D1[[1]]$birth,noisy_copies_D1[[2]]$birth),y = c(D1$death,noisy_copies_D1[[1]]$death,noisy_copies_D1[[2]]$death),main = "D1 and noisy copies",xlab = "",ylab = "",xlim = c(0,4),ylim = c(0,4),col = c("black","red","blue"),bty = "L")
lines(x = c(-0.15,4.15),y = c(-0.15,4.15))
legend("topright", inset=c(-0.2,0), 
       legend=levels(cols), 
       pch=16, col=c("black","red","blue"))

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

## ----echo = T-----------------------------------------------------------------
# calculate 2-wasserstein distance between D1 and D2
diagram_distance(D1,D2,dim = 0,p = 2,distance = "wasserstein")

# calculate 2-wasserstein distance between D1 and D3
diagram_distance(D1,D3,dim = 0,p = 2,distance = "wasserstein")

# calculate bottleneck distance between D1 and D2
diagram_distance(D1,D2,dim = 0,p = Inf,distance = "wasserstein")

# calculate bottleneck distance between D1 and D3
diagram_distance(D1,D3,dim = 0,p = Inf,distance = "wasserstein")

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
# Fisher information metric calculation between D1 and D2 for sigma = 1
diagram_distance(D1,D2,dim = 0,distance = "fisher",sigma = 1)

# Fisher information metric calculation between D1 and D3 for sigma = 1
diagram_distance(D1,D3,dim = 0,distance = "fisher",sigma = 1)

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
# calculate the kernel value between D1 and D2 with sigma = 2, t = 2
diagram_kernel(D1,D2,dim = 0,sigma = 2,t = 2)
# calculate the kernel value between D1 and D3 with sigma = 2, t = 2
diagram_kernel(D1,D3,dim = 0,sigma = 2,t = 2)

## ----echo = T-----------------------------------------------------------------
# create noisy copies of D1, D2 and D3
g <- generate_TDApplied_vignette_data(3,3,3)
                              
# calculate kmeans clusters with centers = 3, and sigma = t = 2
clust <- diagram_kkmeans(diagrams = g,centers = 3,dim = 0,t = 2,sigma = 2,num_workers = 2)

# display cluster labels
clust$clustering@.Data

## ----echo = T-----------------------------------------------------------------
# create nine new diagrams
g_new <- generate_TDApplied_vignette_data(3,3,3)

# predict cluster labels
predict_diagram_kkmeans(new_diagrams = g_new,clustering = clust,num_workers = 2)

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

## ----echo = T-----------------------------------------------------------------
# create 10 noisy copies of D1 and D2
g1 <- generate_TDApplied_vignette_data(10,0,0)
g2 <- generate_TDApplied_vignette_data(0,10,0)

# do independence test with sigma = t = 1
indep_test <- independence_test(g1,g2,dims = c(0),num_workers = 2)
indep_test$p_values

## ----echo = T,warning=F,eval = F----------------------------------------------
#  # generate persistence diagrams from Tori and spheres with  100,200,...,1000 data points.
#  runtimes_shape <- data.frame(n_row = numeric(),package = character(),time_in_sec = numeric())
#  for(n_row in seq(100,1000,100)){
#  
#    for(iteration in 1:10)
#    {
#      # simulate pair of diagrams from the desired shapes
#      diagram_torus = ripsDiag(X = TDA::torusUnif(n = n_row,a = 1,c = 2),
#                               maxdimension = 2,maxscale = 2)
#      diagram_sphere = ripsDiag(X = TDA::sphereUnif(n = n_row,d = 2,r = 1),
#                                maxdimension = 2,maxscale = 1)
#  
#      # compute their wasserstein distances in all dimensions and benchmark
#      start_time_TDApplied = Sys.time()
#      diagram_distance(D1 = diagram_torus,D2 = diagram_sphere,dim = 0,
#                       p = 2,distance = "wasserstein")
#      diagram_distance(D1 = diagram_torus,D2 = diagram_sphere,dim = 1,
#                       p = 2,distance = "wasserstein")
#      diagram_distance(D1 = diagram_torus,D2 = diagram_sphere,dim = 2,
#                       p = 2,distance = "wasserstein")
#      end_time_TDApplied = Sys.time()
#      time_diff_TDApplied = as.numeric(end_time_TDApplied - start_time_TDApplied,units = "secs")
#  
#      start_time_TDA = Sys.time()
#      TDA::wasserstein(Diag1 = diagram_torus$diagram,Diag2 = diagram_sphere$diagram,
#                       dimension = 0,p = 2)
#      TDA::wasserstein(Diag1 = diagram_torus$diagram,Diag2 = diagram_sphere$diagram,
#                       dimension = 1,p = 2)
#      TDA::wasserstein(Diag1 = diagram_torus$diagram,Diag2 = diagram_sphere$diagram,
#                       dimension = 2,p = 2)
#      end_time_TDA = Sys.time()
#      time_diff_TDA = as.numeric(end_time_TDA - start_time_TDA,units = "secs")
#  
#      runtimes_shape = rbind(runtimes_shape,data.frame(n_row = n_row,
#                                                       package = "TDApplied",
#                                                       time_in_sec = time_diff_TDApplied))
#      runtimes_shape = rbind(runtimes_shape,data.frame(n_row = n_row,
#                                                       package = "TDA",
#                                                       time_in_sec = time_diff_TDA))
#  
#    }
#    print(paste0("Done ",n_row," rows"))
#  
#  }
#  
#  # compute means and sd's at each value of rows for both packages
#  summary_table = data.frame(n_row = numeric(),mean = numeric(),sd = numeric(),
#                             package = character())
#  for(n_row in seq(100,1000,100))
#  {
#    for(p in c("TDApplied","TDA"))
#    {
#      result = data.frame(n_row = n_row,
#                          mean = mean(runtimes_shape[which(runtimes_shape$n_row == n_row
#                                                           & runtimes_shape$package == p),
#                                                           3]),
#                          sd = sd(runtimes_shape[which(runtimes_shape$n_row == n_row
#                                                       & runtimes_shape$package == p),
#                                                       3]),
#                          package = p)
#      summary_table = rbind(summary_table,result)
#    }
#  }
#  
#  # plot table
#  plot(summary_table$n_row[summary_table$package=="TDA"],
#       summary_table$mean[summary_table$package=="TDA"],
#       type="b",
#       xlim=range(summary_table$n_row),
#       ylim=range(0,summary_table$mean+1.96*summary_table$sd/sqrt(10)),
#       xlab = "Points in shape",ylab = "Mean execution time (sec)")
#  lines(summary_table$n_row[summary_table$package=="TDApplied"],
#        summary_table$mean[summary_table$package=="TDApplied"],
#        col=2, type="b")
#  legend(x = 200,y = 2000,legend = c("TDApplied","TDA"),
#         col = c("red","black"),lty = c(1,1),cex = 0.8)
#  arrows(summary_table$n_row[summary_table$package == "TDApplied"],
#         summary_table$mean[summary_table$package == "TDApplied"]
#         -1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10),
#         summary_table$n_row[summary_table$package == "TDApplied"],
#         summary_table$mean[summary_table$package == "TDApplied"]
#         +1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10),
#         length=0.05, angle=90, code=3,col = "red")
#  arrows(summary_table$n_row[summary_table$package == "TDA"],
#         summary_table$mean[summary_table$package == "TDA"]
#         -1.96*summary_table$sd[summary_table$package == "TDA"]/sqrt(10),
#         summary_table$n_row[summary_table$package == "TDA"],
#         summary_table$mean[summary_table$package == "TDA"]
#         +1.96*summary_table$sd[summary_table$package == "TDA"]/sqrt(10),
#         length=0.05, angle=90, code=3,col = "black")

## ----echo = F,warning=F,fig.height = 4,fig.width = 4,fig.align = 'center'-----
summary_table = data.frame(n_row = rep(seq(100,1000,100),each = 2),mean = c(0.133,0.220,0.398,3.430,0.953,17.054,1.955,53.896,3.760,142.524,6.604,294.552,10.466,601.802,16.169,1130.289,23.458,2091.953,35.098,3518.517),sd = c(0.022,0.016,0.035,0.186,0.094,1.1889,0.130,4.501,0.211,5.924,0.772,11.244,0.623,9.064,0.989,43.385,1.989,88.432,2.747,172.684),package = rep(c("TDApplied","TDA"),10))

# plot table
plot(summary_table$n_row[summary_table$package=="TDA"], summary_table$mean[summary_table$package=="TDA"], type="b",
     xlim=range(summary_table$n_row), ylim=range(0,summary_table$mean+1.96*summary_table$sd/sqrt(10)),xlab = "Points in shape",ylab = "Mean execution time (sec)")
lines(summary_table$n_row[summary_table$package=="TDApplied"], summary_table$mean[summary_table$package=="TDApplied"], col=2, type="b")
legend(x = 200,y = 2000,legend = c("TDApplied","TDA"),col = c("red","black"),lty = c(1,1),cex = 0.8)
arrows(summary_table$n_row[summary_table$package == "TDApplied"], summary_table$mean[summary_table$package == "TDApplied"]-1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10), summary_table$n_row[summary_table$package == "TDApplied"], summary_table$mean[summary_table$package == "TDApplied"]+1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10), length=0.05, angle=90, code=3,col = "red")
arrows(summary_table$n_row[summary_table$package == "TDA"], summary_table$mean[summary_table$package == "TDA"]-1.96*summary_table$sd[summary_table$package == "TDA"]/sqrt(10), summary_table$n_row[summary_table$package == "TDA"], summary_table$mean[summary_table$package == "TDA"]+1.96*summary_table$sd[summary_table$package == "TDA"]/sqrt(10), length=0.05, angle=90, code=3,col = "black")

## ----echo = T-----------------------------------------------------------------
model <- stats::lm(data = data.frame(y = 
                                     summary_table$mean[summary_table$package == "TDA"]
                                     /summary_table$mean[summary_table$package == "TDApplied"],
                                     x = seq(100,1000,100)),
                   formula = y ~ x,)
summary(model)$coefficients
predict(model,newdata = data.frame(x = 1000))[[1]]

## ----echo = T,warning=F,eval = F----------------------------------------------
#  
#  # load reticulate package
#  library(reticulate)
#  persim <- reticulate::import("persim")
#  ripser <- reticulate::import("ripser")
#  
#  # generate persistence diagrams from Tori and spheres with  100,200,...,1000 data points.
#  runtimes_language <- data.frame(n_row = numeric(),package = character(),
#                                  time_in_sec = numeric())
#  for(n_row in seq(100,1000,100)){
#  
#    for(iteration in 1:10)
#    {
#      # simulate pair of diagrams from the desired shapes
#      torus = TDA::torusUnif(n = n_row,a = 1,c = 2)
#      sphere = TDA::sphereUnif(n = n_row,d = 2,r = 1)
#      diagram_torus = ripsDiag(X = torus,
#                               maxdimension = 2,maxscale = 2)
#      diagram_sphere = ripsDiag(X = sphere,
#                                maxdimension = 2,maxscale = 1)
#      diagram_torus_py = ripser$ripser(torus,maxdim = 2,thresh = 2)$dgms
#      diagram_torus_py[[1]][which(diagram_torus_py[[1]][,2] == Inf),2] = 2
#      diagram_torus_py[[2]][which(diagram_torus_py[[2]][,2] == Inf),2] = 2
#      diagram_torus_py[[3]][which(diagram_torus_py[[3]][,2] == Inf),2] = 2
#      diagram_sphere_py = ripser$ripser(sphere,maxdim = 2,thresh = 1)$dgms
#      diagram_sphere_py[[1]][which(diagram_sphere_py[[1]][,2] == Inf),2] = 2
#      diagram_sphere_py[[2]][which(diagram_sphere_py[[2]][,2] == Inf),2] = 2
#      diagram_sphere_py[[3]][which(diagram_sphere_py[[3]][,2] == Inf),2] = 2
#  
#      # compute their wasserstein distances in all dimensions and benchmark
#      start_time_TDApplied = Sys.time()
#      diagram_distance(D1 = diagram_torus,D2 = diagram_sphere,dim = 0,
#                       p = 2,distance = "wasserstein")
#      diagram_distance(D1 = diagram_torus,D2 = diagram_sphere,dim = 1,
#                       p = 2,distance = "wasserstein")
#      diagram_distance(D1 = diagram_torus,D2 = diagram_sphere,dim = 2,
#                       p = 2,distance = "wasserstein")
#      end_time_TDApplied = Sys.time()
#      time_diff_TDApplied = as.numeric(end_time_TDApplied - start_time_TDApplied,units = "secs")
#  
#      start_time_persim = Sys.time()
#      persim$wasserstein(diagram_torus_py[[1]],diagram_sphere_py[[1]])
#      persim$wasserstein(diagram_torus_py[[2]],diagram_sphere_py[[2]])
#      persim$wasserstein(diagram_torus_py[[3]],diagram_sphere_py[[3]])
#      end_time_persim = Sys.time()
#      time_diff_persim = as.numeric(end_time_persim - start_time_persim,units = "secs")
#  
#      runtimes_language = rbind(runtimes_language,data.frame(n_row = n_row,
#                                                       package = "TDApplied",
#                                                       time_in_sec = time_diff_TDApplied))
#      runtimes_language = rbind(runtimes_language,data.frame(n_row = n_row,
#                                                       package = "persim",
#                                                       time_in_sec = time_diff_persim))
#  
#    }
#    print(paste0("Done ",n_row," rows"))
#  
#  }
#  
#  # compute means and sd's at each value of rows for both packages
#  summary_table = data.frame(n_row = numeric(),mean = numeric(),sd = numeric(),
#                             package = character())
#  for(n_row in seq(100,1000,100))
#  {
#    for(p in c("TDApplied","persim"))
#    {
#      result = data.frame(n_row = n_row,
#                          mean = mean(runtimes_language[which(runtimes_language$n_row == n_row
#                                                           & runtimes_language$package == p),
#                                                     3]),
#                          sd = sd(runtimes_language[which(runtimes_language$n_row == n_row
#                                                       & runtimes_language$package == p),
#                                                 3]),
#                          package = p)
#      summary_table = rbind(summary_table,result)
#    }
#  }
#  
#  # plot table
#  plot(summary_table$n_row[summary_table$package=="TDApplied"],
#       summary_table$mean[summary_table$package=="persim"], type="b",
#       xlim=range(summary_table$n_row),
#       ylim=range(0,summary_table$mean+1.96*summary_table$sd/sqrt(10)),
#       xlab = "Points in shape",ylab = "Mean execution time (sec)")
#  lines(summary_table$n_row[summary_table$package=="TDApplied"],
#        summary_table$mean[summary_table$package=="TDApplied"],
#        col="red", type="b")
#  lines(summary_table$n_row[summary_table$package=="persim"],
#        summary_table$mean[summary_table$package=="persim"],
#        col="black", type="b")
#  legend(x = 200,y = 20,legend = c("TDApplied","persim"),
#         col = c("red","black"),lty = c(1,1),cex = 0.8)
#  arrows(summary_table$n_row[summary_table$package == "TDApplied"],
#         summary_table$mean[summary_table$package == "TDApplied"]
#         -1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10),
#         summary_table$n_row[summary_table$package == "TDApplied"],
#         summary_table$mean[summary_table$package == "TDApplied"]
#         +1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10),
#         length=0.05, angle=90, code=3,col = "red")
#  arrows(summary_table$n_row[summary_table$package == "persim"],
#         summary_table$mean[summary_table$package == "persim"]
#         -1.96*summary_table$sd[summary_table$package == "persim"]/sqrt(10),
#         summary_table$n_row[summary_table$package == "persim"],
#         summary_table$mean[summary_table$package == "persim"]
#         +1.96*summary_table$sd[summary_table$package == "persim"]/sqrt(10),
#         length=0.05, angle=90, code=3,col = "black")

## ----echo = F,warning=F,fig.height = 4,fig.width = 4,fig.align = 'center'-----
summary_table = data.frame(n_row = rep(seq(100,1000,100),each = 2),mean = c(0.1387,0.0068,0.3624,0.0271,0.8414,0.061,1.7132,0.148,3.6323,0.26,6.2132,0.4322,10.9508,0.6787,16.1213,1.027,23.2014,1.4571,31.6256,1.922),sd = c(0.043,0.0015,0.0166,0.0082,0.0553,0.0087,0.0549,0.019,0.1713,0.019,0.4691,0.0209,0.9173,0.0235,0.9388,0.0404,1.7964,0.0903,2.5457,0.0618),package = rep(c("TDApplied","persim"),10))

# plot table
plot(summary_table$n_row[summary_table$package=="TDApplied"], summary_table$mean[summary_table$package=="persim"], type="b",
     xlim=range(summary_table$n_row), ylim=range(0,summary_table$mean+1.96*summary_table$sd/sqrt(10)),xlab = "Points in shape",ylab = "Mean execution time (sec)")
lines(summary_table$n_row[summary_table$package=="TDApplied"], summary_table$mean[summary_table$package=="TDApplied"], col="red", type="b")
lines(summary_table$n_row[summary_table$package=="persim"], summary_table$mean[summary_table$package=="persim"], col="black", type="b")
legend(x = 200,y = 20,legend = c("TDApplied","persim"),col = c("red","black"),lty = c(1,1),cex = 0.8)
arrows(summary_table$n_row[summary_table$package == "TDApplied"], summary_table$mean[summary_table$package == "TDApplied"]-1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10), summary_table$n_row[summary_table$package == "TDApplied"], summary_table$mean[summary_table$package == "TDApplied"]+1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10), length=0.05, angle=90, code=3,col = "red")
arrows(summary_table$n_row[summary_table$package == "persim"], summary_table$mean[summary_table$package == "persim"]-1.96*summary_table$sd[summary_table$package == "persim"]/sqrt(10), summary_table$n_row[summary_table$package == "persim"], summary_table$mean[summary_table$package == "persim"]+1.96*summary_table$sd[summary_table$package == "persim"]/sqrt(10), length=0.05, angle=90, code=3,col = "black")

## ----echo = T-----------------------------------------------------------------
model <- stats::lm(data = data.frame(y = 
                                     summary_table$mean[summary_table$package == "TDApplied"]
                                     /summary_table$mean[summary_table$package == "persim"],
                                     x = seq(100,1000,100)),
                   formula = y ~ x,)
summary(model)$coefficients

## ----echo = F-----------------------------------------------------------------
# reset graphic parameters
par(mfrow = original_mfrow,xpd = original_xpd,mar = original_mar)

