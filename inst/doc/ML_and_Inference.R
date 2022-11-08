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
original_scipen <- options()$scipen
options(scipen = 999)

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

## ----echo = T,fig.height = 3,fig.width = 7,fig.align = 'center'---------------
D1 = data.frame(dimension = c(0),birth = c(2),death = c(3))
D2 = data.frame(dimension = c(0),birth = c(2,0),death = c(3.3,0.5))
D3 = data.frame(dimension = c(0),birth = c(0),death = c(0.5))
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

## ----echo = F,fig.height = 5,fig.width = 5,fig.align = 'center'---------------
par(mfrow = c(1,1))
D <- TDAstats::calculate_homology(mat = TDA::circleUnif(n = 50),dim = 1,threshold = 2)
pt <- as.numeric(D[which(D[,1L] == 1),])[2:3]
plot_diagram(D = D,title = "Circle diagram with confidence interval",legend = T,max_radius = 2*0.3953059 + pt[[2]])
graphics::rect(xleft = pt[[1]]-0.3953059,xright = pt[[1]]+0.3953059,ybottom = pt[[2]]-0.3953059,ytop = pt[[2]]+0.3953059,lty = "dashed")
graphics::lines(x = c(pt[[1]],pt[[1]] + 0.3953059),y = c(pt[[2]],pt[[2]]))
graphics::lines(x = c(pt[[1]],pt[[1]]),y = c(pt[[2]],pt[[2]] - 0.3953059))
graphics::text(x = c(pt[[1]] + 0.3953059/2,0.4),y = c(1.83,1.55),c("t","t"))

## ----fig.height = 4,fig.width = 8,fig.align = 'center'------------------------
# sample 50 points from the unit circle
circle <- TDA::circleUnif(n = 50)

# calculate the bootstrapped persistence thresholds using 2 cores
# and 20 iterations
thresh <- bootstrap_persistence_thresholds(X = circle,FUN = "ripsDiag",
                                 maxdim = 1,thresh = 2,num_workers = 2,
                                 num_samples = 30)
diag <- thresh$diag

# plot original diagram and thresholded diagram side-by-side:
par(mfrow = c(1,2))

plot_diagram(diag,title = "Circle diagram")

plot_diagram(diag,title = "Circle diagram with thresholds",
             thresholds = thresh$thresholds)

## ----echo = F-----------------------------------------------------------------
par(mfrow = c(1,1))

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

## ----echo = F,eval = T--------------------------------------------------------
# create the data
circ <- data.frame(x = c(0.37206401439763,0.187252903758128,-0.941946837003475,0.0442567282078249,0.184638555615293,-0.0837545101464481,-0.364816992194448,-0.722635069185668,-0.965713143554718,-0.832555262051846,0.416857546850322,0.262628948512087,0.335630458504239,-0.987359078856224,0.971143015697996,0.828642381920149,-0.993094328640157,0.864705993526812,-0.601822463909246,0.00855304668431679,-0.694732418249695,0.281097726412362,-0.536528403939666,0.999578844170687,0.500427719325381,0.972755007124087,-0.985828528101177,0.382116090228366,0.187341042513765,-0.999497942642912,0.447939273429357,-0.995551072494437,0.582222964962477,0.852912225230687,0.914794382538427,-0.977030870815178,0.999052449805392,0.986668984725482,0.379560512351362,-0.99022104692615,-0.9293797636278,0.028209644693279,0.125079985118729,0.126437768180937,0.504013549595047,0.933397935474915,0.574043084808964,0.747953695502559,0.841062122135969,0.228565768955052),y = c(-0.928207072366032,0.982311737705576,-0.335762053036297,0.999020190991323,-0.982806493558268,-0.996486418387189,0.93107924593248,-0.691229742403367,-0.259611487353053,0.553941996629414,0.908971828845059,-0.964896904028319,-0.941993734227694,-0.158499367191759,0.238497889007434,0.559778351569352,0.117318602202532,-0.502278353862486,-0.798629902980225,-0.999963422027234,-0.719268285852317,-0.959679148573001,-0.843882261791273,-0.0290195500721963,0.865778319046393,0.231835493648013,-0.167756112204196,0.924114329284308,0.982294932181702,0.0316837916384251,0.894063983906957,0.0942234687069815,-0.813029162496834,0.52205434204883,0.403919840656704,0.213097811988145,0.0435224371772166,-0.162740021447664,0.925166913299201,-0.13950726943238,-0.369124985552503,-0.999602028782595,-0.992146661196165,0.991974541395808,-0.863695746096159,-0.358842993593529,-0.818825095354929,-0.663750909139916,0.540938542450194,-0.973528473780806))

## ----echo = F,warning=F,fig.height = 4,fig.width = 4,fig.align = 'center'-----
plot(x = circ$x,y = circ$y,xlab = "x",ylab = "y",main = "circ")

## ----echo = T,eval = F--------------------------------------------------------
#  # import the ripser module
#  ripser <- import_ripser()
#  
#  # calculate the persistence diagram
#  diag <- PyH(X = circ,maxdim = 1,thresh = 1,ripser = ripser)

## ----echo = T,eval = F--------------------------------------------------------
#  # ripser has already been imported, so calculate diagram with representatives
#  diag <- PyH(circ,maxdim = 1,thresh = 2,ripser = ripser,calculate_representatives = T)
#  
#  # identify the loops in the diagram
#  diag$diagram[which(diag$diagram$dimension == 1),]

## ----echo = F-----------------------------------------------------------------
data.frame(dimension = c(1),birth = c(0.6008424),death = c(1.738894),row.names = c("50"))

## ----echo = T,eval = F--------------------------------------------------------
#  # show the representative for the loop, just the first five rows
#  diag$representatives[[2]][[1]][1:5,]

## ----echo = F-----------------------------------------------------------------
representative = matrix(data = c(9,6,35,6,9,3,16,6,31,6,43,9,29,6,9,1,28,9,39,6,13,6,26,6,38,9,27,9,35,3,10,9,30,9,8,6,43,35,16,3,24,9,31,3,6,2,35,1,35,28,40,6,43,16,29,3,43,31,16,1,28,16,31,1,31,28,43,29,29,1,29,28,38,35,35,27,39,3,13,3,26,3,35,10,35,30,43,39,38,16,27,16,43,13,38,31,43,26,31,27,8,3,35,24,16,10,39,1,39,28,31,10,30,16,13,1,28,13,26,1,28,26,38,29,29,27,31,30,3,2,43,8,15,9,7,6,29,10,24,16,48,9,40,3,20,6,31,24,30,29,33,9,8,1,28,8,43,2,29,24,43,40,39,38,2,1,28,2,39,27),nrow = 85,ncol = 2,byrow = T) + matrix(data = 1,nrow = 85,ncol = 2)
representative[1:5,]

## ----echo = T,eval = F--------------------------------------------------------
#  unique(c(diag$representatives[[2]][[1]][,1],diag$representatives[[2]][[1]][,2]))

## ----echo = F-----------------------------------------------------------------
c(9,35,16,31,43,29,28,39,13,26,38,27,10,30,8,24,6,40,3,15,7,48,20,33,2,1) + rep(1,26)

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
#  for(i in 1:nrow(diag$representatives[[2]][[1]]))
#  {
#    pt1 <- circ[diag$representatives[[2]][[1]][i,1],]
#    pt2 <- circ[diag$representatives[[2]][[1]][i,2],]
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

## ----echo = F,eval = T--------------------------------------------------------
summary_table_circle = data.frame(n_row = rep(seq(100,1000,100),each = 2),mean = c(0.01196892,0.01326358,0.01396093,0.05216095,0.03361042,0.12436731,0.06431270,0.22828629,0.10450339,0.35640080,0.17263622,0.54174340,0.26653392,0.76970870,0.38913472,1.06795118,0.56827831,1.57888129,0.77001960,2.02081304),sd = c(0.010148940,0.002938113,0.002576150,0.004909125,0.004535042,0.004654923,0.008832229,0.008654974,0.015860676,0.013255301,0.027655884,0.031779819,0.038884235,0.054684568,0.074091101,0.050891635,0.066695288,0.110982818,0.098805909,0.139048883),package = rep(c("TDApplied","TDAstats"),10))

summary_table_torus = data.frame(n_row = rep(seq(100,1000,100),each = 2),mean = c(0.0270284175872803,0.166255307197571,0.165945911407471,1.25921893119812,0.547216653823853,4.23308880329132,1.25613822937012,9.88947887420654,2.50379593372345,19.3656851053238,4.3738107919693,33.8885488033295,7.39101943969727,55.0618635654449,11.3016363143921,82.9730611801147,16.8212526082993,119.886588406563,22.7343117952347,166.021493530273),sd = c(0.00628026852286816,0.0207818797344753,0.0132287396951078,0.0617468263761529,0.0395387975829715,0.196572350595388,0.0412505644501839,0.351000536768941,0.142517673337179,0.621714418760195,0.139799366288941,0.715119581947307,0.330074208165256,3.34031068711377,0.216260497620014,1.14642158540614,0.328511302110799,2.05599729628367,0.703949594742105,3.81173452337388),package = rep(c("TDApplied","TDAstats"),10))

summary_table_sphere = data.frame(n_row = rep(seq(100,1000,100),each = 2),mean = c(0.0109694957733154,0.0594853162765503,0.0540436983108521,0.469167232513428,0.182804441452026,1.57912936210632,0.410391473770142,3.69215035438538,0.822147560119629,7.32947452068329,1.40918028354645,12.8482979059219,2.47986209392548,20.9446493387222,3.76364696025848,31.2942998409271,5.3408855676651,44.884921503067,7.59271986484528,62.6113245010376),sd = c(0.00193647497299544,0.00600547924173666,0.00563514382927968,0.0210564179408907,0.0129015865529426,0.0403660633577451,0.0218388344325696,0.077772517180065,0.0319754379649263,0.0762356686089166,0.0567664459109207,0.171412828638123,0.240398206520162,0.398396221629952,0.320522372278111,0.459994662433323,0.365647918767674,0.417413443923572,0.394211392866084,0.746903966284143),package = rep(c("TDApplied","TDAstats"),10))

## ----echo = F,warning=F,fig.height = 4,fig.width = 7,fig.align = 'center'-----
par(mfrow = c(1,3))
plot(summary_table_circle$n_row[summary_table_circle$package=="TDAstats"], 
     summary_table_circle$mean[summary_table_circle$package=="TDAstats"], 
     type="b",
     xlim=range(summary_table_circle$n_row),
     ylim=range(0,summary_table_circle$mean+1.96*summary_table_circle$sd/sqrt(10)),
     xlab = "Points in shape",ylab = "Mean execution time (sec)",
     main = "Circles")
lines(summary_table_circle$n_row[summary_table_circle$package=="TDApplied"],
      summary_table_circle$mean[summary_table_circle$package=="TDApplied"], 
      col=2, type="b")
legend(x = 200,y = 1.5,legend = c("TDApplied","TDAstats"),
       col = c("red","black"),lty = c(1,1),cex = 0.8)
arrows(summary_table_circle$n_row[summary_table_circle$package == "TDApplied"], 
       summary_table_circle$mean[summary_table_circle$package == "TDApplied"]
       -1.96*summary_table_circle$sd[summary_table_circle$package == "TDApplied"]/sqrt(10),
       summary_table_circle$n_row[summary_table_circle$package == "TDApplied"], 
       summary_table_circle$mean[summary_table_circle$package == "TDApplied"]
       +1.96*summary_table_circle$sd[summary_table_circle$package == "TDApplied"]/sqrt(10), 
       length=0.05, angle=90, code=3,col = "red")
arrows(summary_table_circle$n_row[summary_table_circle$package == "TDAstats"], 
       summary_table_circle$mean[summary_table_circle$package == "TDAstats"]
       -1.96*summary_table_circle$sd[summary_table_circle$package == "TDAstats"]/sqrt(10), 
       summary_table_circle$n_row[summary_table_circle$package == "TDAstats"], 
       summary_table_circle$mean[summary_table_circle$package == "TDAstats"]
       +1.96*summary_table_circle$sd[summary_table_circle$package == "TDAstats"]/sqrt(10), 
       length=0.05, angle=90, code=3,col = "black")

plot(summary_table_torus$n_row[summary_table_torus$package=="TDAstats"], 
     summary_table_torus$mean[summary_table_torus$package=="TDAstats"], 
     type="b",
     xlim=range(summary_table_torus$n_row),
     ylim=range(0,summary_table_torus$mean+1.96*summary_table_torus$sd/sqrt(10)),
     xlab = "Points in shape",ylab = "Mean execution time (sec)",
     main = "Tori")
lines(summary_table_torus$n_row[summary_table_torus$package=="TDApplied"],
      summary_table_torus$mean[summary_table_torus$package=="TDApplied"], 
      col=2, type="b")
legend(x = 200,y = 120,legend = c("TDApplied","TDAstats"),
       col = c("red","black"),lty = c(1,1),cex = 0.8)
arrows(summary_table_torus$n_row[summary_table_torus$package == "TDApplied"], 
       summary_table_torus$mean[summary_table_torus$package == "TDApplied"]
       -1.96*summary_table_torus$sd[summary_table_torus$package == "TDApplied"]/sqrt(10),
       summary_table_torus$n_row[summary_table_torus$package == "TDApplied"], 
       summary_table_torus$mean[summary_table_torus$package == "TDApplied"]
       +1.96*summary_table_torus$sd[summary_table_torus$package == "TDApplied"]/sqrt(10), 
       length=0.05, angle=90, code=3,col = "red")
arrows(summary_table_torus$n_row[summary_table_torus$package == "TDAstats"], 
       summary_table_torus$mean[summary_table_torus$package == "TDAstats"]
       -1.96*summary_table_torus$sd[summary_table_torus$package == "TDAstats"]/sqrt(10), 
       summary_table_torus$n_row[summary_table_torus$package == "TDAstats"], 
       summary_table_torus$mean[summary_table_torus$package == "TDAstats"]
       +1.96*summary_table_torus$sd[summary_table_torus$package == "TDAstats"]/sqrt(10), 
       length=0.05, angle=90, code=3,col = "black")

plot(summary_table_sphere$n_row[summary_table_sphere$package=="TDAstats"], 
     summary_table_sphere$mean[summary_table_sphere$package=="TDAstats"], 
     type="b",
     xlim=range(summary_table_sphere$n_row),
     ylim=range(0,summary_table_sphere$mean+1.96*summary_table_sphere$sd/sqrt(10)),
     xlab = "Points in shape",ylab = "Mean execution time (sec)",
     main = "Spheres")
lines(summary_table_sphere$n_row[summary_table_sphere$package=="TDApplied"],
      summary_table_sphere$mean[summary_table_sphere$package=="TDApplied"], 
      col=2, type="b")
legend(x = 200,y = 45,legend = c("TDApplied","TDAstats"),
       col = c("red","black"),lty = c(1,1),cex = 0.8)
arrows(summary_table_sphere$n_row[summary_table_sphere$package == "TDApplied"], 
       summary_table_sphere$mean[summary_table_sphere$package == "TDApplied"]
       -1.96*summary_table_sphere$sd[summary_table_sphere$package == "TDApplied"]/sqrt(10),
       summary_table_sphere$n_row[summary_table_sphere$package == "TDApplied"], 
       summary_table_sphere$mean[summary_table_sphere$package == "TDApplied"]
       +1.96*summary_table_sphere$sd[summary_table_sphere$package == "TDApplied"]/sqrt(10), 
       length=0.05, angle=90, code=3,col = "red")
arrows(summary_table_sphere$n_row[summary_table_sphere$package == "TDAstats"], 
       summary_table_sphere$mean[summary_table_sphere$package == "TDAstats"]
       -1.96*summary_table_sphere$sd[summary_table_sphere$package == "TDAstats"]/sqrt(10), 
       summary_table_sphere$n_row[summary_table_sphere$package == "TDAstats"], 
       summary_table_sphere$mean[summary_table_sphere$package == "TDAstats"]
       +1.96*summary_table_sphere$sd[summary_table_sphere$package == "TDAstats"]/sqrt(10), 
       length=0.05, angle=90, code=3,col = "black")

## ----echo = F,eval = F--------------------------------------------------------
#  model_circle <- stats::lm(data =
#                              data.frame(ratio =
#                                         summary_table_circle$mean[summary_table_circle$package
#                                                                     == "TDAstats"]/
#                                         summary_table_circle$mean[summary_table_circle$package
#                                                                     == "TDApplied"],
#                                         n_row = seq(100,1000,100)),
#  
#                     formula = ratio ~ n_row)
#  summary(model_circle)$coefficients
#  
#  model_torus <- stats::lm(data =
#                              data.frame(ratio =
#                                         summary_table_torus$mean[summary_table_torus$package
#                                                                     == "TDAstats"]/
#                                         summary_table_torus$mean[summary_table_torus$package
#                                                                     == "TDApplied"],
#                                         n_row = seq(100,1000,100)),
#  
#                     formula = ratio ~ n_row)
#  summary(model_torus)$coefficients
#  
#  model_sphere <- stats::lm(data =
#                              data.frame(ratio =
#                                         summary_table_sphere$mean[summary_table_sphere$package
#                                                                     == "TDAstats"]/
#                                         summary_table_sphere$mean[summary_table_sphere$package
#                                                                     == "TDApplied"],
#                                         n_row = seq(100,1000,100)),
#  
#                     formula = ratio ~ n_row)
#  summary(model_sphere)$coefficients

## ----echo = F,warning=F,fig.height = 4,fig.width = 4,fig.align = 'center'-----
summary_table = data.frame(n_row = rep(seq(100,1000,100),each = 2),mean = c(0.133,0.220,0.398,3.430,0.953,17.054,1.955,53.896,3.760,142.524,6.604,294.552,10.466,601.802,16.169,1130.289,23.458,2091.953,35.098,3518.517),sd = c(0.022,0.016,0.035,0.186,0.094,1.1889,0.130,4.501,0.211,5.924,0.772,11.244,0.623,9.064,0.989,43.385,1.989,88.432,2.747,172.684),package = rep(c("TDApplied","TDA"),10))

# plot table
plot(summary_table$n_row[summary_table$package=="TDA"], summary_table$mean[summary_table$package=="TDA"], type="b",
     xlim=range(summary_table$n_row), ylim=range(0,summary_table$mean+1.96*summary_table$sd/sqrt(10)),xlab = "Points in shape",ylab = "Mean execution time (sec)")
lines(summary_table$n_row[summary_table$package=="TDApplied"], summary_table$mean[summary_table$package=="TDApplied"], col=2, type="b")
legend(x = 200,y = 2000,legend = c("TDApplied","TDA"),col = c("red","black"),lty = c(1,1),cex = 0.8)
arrows(summary_table$n_row[summary_table$package == "TDApplied"], summary_table$mean[summary_table$package == "TDApplied"]-1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10), summary_table$n_row[summary_table$package == "TDApplied"], summary_table$mean[summary_table$package == "TDApplied"]+1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10), length=0.05, angle=90, code=3,col = "red")
arrows(summary_table$n_row[summary_table$package == "TDA"], summary_table$mean[summary_table$package == "TDA"]-1.96*summary_table$sd[summary_table$package == "TDA"]/sqrt(10), summary_table$n_row[summary_table$package == "TDA"], summary_table$mean[summary_table$package == "TDA"]+1.96*summary_table$sd[summary_table$package == "TDA"]/sqrt(10), length=0.05, angle=90, code=3,col = "black")

## ----echo = F,eval = F--------------------------------------------------------
#  model <- stats::lm(data = data.frame(ratio =
#                                       summary_table$mean[summary_table$package == "TDA"]/summary_table$mean[summary_table$package == "TDApplied"],
#                                       n_row = seq(100,1000,100)),
#                     formula = ratio ~ n_row)
#  summary(model)$coefficients
#  predict(model,newdata = data.frame(n_row = 1000))[[1]]

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

## ----echo = F,eval = F--------------------------------------------------------
#  model <- stats::lm(data = data.frame(ratio =
#                                       summary_table$mean[summary_table$package == "TDApplied"]
#                                       /summary_table$mean[summary_table$package == "persim"],
#                                       n_row = seq(100,1000,100)),
#                     formula = ratio ~ n_row)
#  summary(model)$coefficients

## ----echo = F-----------------------------------------------------------------
# reset parameters
par(mfrow = original_mfrow,xpd = original_xpd,mar = original_mar)
options(scipen = original_scipen)

