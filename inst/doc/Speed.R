## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo = F-----------------------------------------------------------------
summary_table_approx = data.frame(n_row = rep(seq(100,1000,100),each = 2),mean = c(4.48064579963684,0.0763566970825195,17.6674932003021,0.158217072486877,39.6661910772324,0.233239436149597,70.411101102829,0.312848711013794,110.796008181572,0.385414099693298,160.19043545723,0.471218752861023,217.045732426643,0.545907402038574,281.503244686127,0.619925212860107,354.64315507412,0.700030016899109,431.781597065926,0.760770702362061),sd = c(0.179846190861649,0.00403598155739429,0.233420419722357,0.00428043758708594,0.264231438627147,0.00795119513963388,0.888634398129357,0.00748616624765675,0.596708850996989,0.010836296230238,1.46664969337454,0.0129923342156653,1.75630863537414,0.010559974689952,1.85564970425653,0.010979028911925,1.91770673210479,0.0126778338886785,1.85627025308762,0.0119982175150673),method = rep(c("Exact","Approximation"),10))

## ----echo = F,warning=F,fig.height = 4,fig.width = 4,fig.align = 'center',eval = T----
plot(summary_table_approx$n_row[summary_table_approx$method=="Exact"],
     summary_table_approx$mean[summary_table_approx$method=="Exact"],
     type="b",
     xlim=range(summary_table_approx$n_row),
     ylim=range(0,summary_table_approx$mean+1.96*summary_table_approx$sd/sqrt(10)),
     xlab = "Points in shape",ylab = "Mean execution time (sec)",
     main = "Approximation Benchmarking")
lines(summary_table_approx$n_row[summary_table_approx$method=="Approximation"],
      summary_table_approx$mean[summary_table_approx$method=="Approximation"],
      col=2, type="b")
legend(x = 200,y = 350,legend = c("Approximation","Exact"),
       col = c("red","black"),lty = c(1,1),cex = 0.8)
arrows(summary_table_approx$n_row[summary_table_approx$method == "Approximation"],
       summary_table_approx$mean[summary_table_approx$method == "Approximation"]
       -1.96*summary_table_approx$sd[summary_table_approx$method == "Approximation"]/sqrt(10),
       summary_table_approx$n_row[summary_table_approx$method == "Approximation"],
       summary_table_approx$mean[summary_table_approx$method == "Approximation"]
       +1.96*summary_table_approx$sd[summary_table_approx$method == "Approximation"]/sqrt(10),
       length=0.05, angle=90, code=3,col = "red")
arrows(summary_table_approx$n_row[summary_table_approx$method == "Exact"],
       summary_table_approx$mean[summary_table_approx$method == "Exact"]
       -1.96*summary_table_approx$sd[summary_table_approx$method == "Exact"]/sqrt(10),
       summary_table_approx$n_row[summary_table_approx$method == "Exact"],
       summary_table_approx$mean[summary_table_approx$method == "Exact"]
       +1.96*summary_table_approx$sd[summary_table_approx$method == "Exact"]/sqrt(10),
       length=0.05, angle=90, code=3,col = "black")

## ----eval = F,echo = F--------------------------------------------------------
#  model_approx <- stats::lm(data =
#                              data.frame(ratio =
#                                         summary_table_approx$mean[summary_table_approx$method
#                                                                     == "Exact"]/
#                                         summary_table_approx$mean[summary_table_approx$method
#                                                                     == "Approximation"],
#                                         n_row = seq(100,1000,100)),
#  
#                     formula = ratio ~ n_row)
#  summary(model_approx)$coefficients

## ----eval = F-----------------------------------------------------------------
#  # create 20 diagrams from circles
#  g <- lapply(X = 1:10,FUN = function(X){
#  
#    return(TDAstats::calculate_homology(TDA::circleUnif(100),dim = 0,threshold = 2))
#  
#  })
#  
#  # calculate distance matrices
#  d_exact <- distance_matrix(g,distance = "fisher",sigma = 1)
#  d_approx <- parallel_approx_distance_mat(g,rho = 1e-6)

## ----echo = T,eval = F--------------------------------------------------------
#  generate_TDApplied_vignette_data <- function(num_D1,num_D2,num_D3){
#  
#    # num_D1 is the number of desired copies of D1, and likewise
#    # for num_D2 and num_D3
#  
#    # create data
#    D1 = data.frame(dimension = c(0),birth = c(2),death = c(3))
#    D2 = data.frame(dimension = c(0),birth = c(2,0),death = c(3.3,0.5))
#    D3 = data.frame(dimension = c(0),birth = c(0),death = c(0.5))
#  
#    # make noisy copies
#    noisy_copies <- lapply(X = 1:(num_D1 + num_D2 + num_D3),FUN = function(X){
#  
#      # i stores the number of the data frame to make copies of:
#      # i = 1 is for D1, i = 2 is for D2 and i = 3 is for D3
#      i <- 1
#      if(X > num_D1 & X <= num_D1 + num_D2)
#      {
#        i <- 2
#      }
#      if(X > num_D1 + num_D2)
#      {
#        i <- 3
#      }
#      # store correct data in noisy_copy
#      noisy_copy <- get(paste0("D",i))
#  
#      # add Gaussian noise to birth and death values
#      n <- nrow(noisy_copy)
#      noisy_copy$dimension <- as.numeric(as.character(noisy_copy$dimension))
#      noisy_copy$birth <- noisy_copy$birth + stats::rnorm(n = n,mean = 0,sd = 0.05)
#      noisy_copy$death <- noisy_copy$death + stats::rnorm(n = n,mean = 0,sd = 0.05)
#  
#      # make any birth values which are less than 0 equal 0
#      noisy_copy[which(noisy_copy$birth < 0),2] <- 0
#  
#      # make any birth values which are greater than their death values equal their death values
#      noisy_copy[which(noisy_copy$birth > noisy_copy$death),2] <-
#        noisy_copy[which(noisy_copy$birth > noisy_copy$death),3]
#      return(noisy_copy)
#  
#    })
#  
#    # return list containing num_D1 noisy copies of D1, then
#    # num_D2 noisy copies of D2, and finally num_D3 noisy copies
#    # of D3
#    return(noisy_copies)
#  
#  }
#  
#  # create noisy copies of D1, D2 and D3
#  g <- generate_TDApplied_vignette_data(3,3,3)
#  
#  # calculate MDS embedding
#  mds <- diagram_mds(diagrams = g,k = 2,dim = 0,sigma = 1.5,distance = "fisher")
#  
#  # calculate kmeans clusters with 3 centers
#  clust <- diagram_kkmeans(diagrams = g,centers = 3,dim = 0,t = 2,sigma = 1.5)
#  
#  # calculate kpca embedding
#  pca <- diagram_kpca(diagrams = g,dim = 0,t = 2,sigma = 1.5,features = 2)

## ----eval = F,echo = T--------------------------------------------------------
#  D <- distance_matrix(diagrams = g,dim = 0,sigma = 1.5,distance = "fisher")
#  K <- exp(-2*D)
#  class(K) <- "kernelMatrix"
#  
#  # calculate MDS embedding
#  mds <- diagram_mds(D = D,k = 2)
#  
#  # calculate kmeans clusters with 3 centers
#  clust <- diagram_kkmeans(diagrams = g,K = K,centers = 3,dim = 0,t = 2,sigma = 1.5)
#  
#  # calculate kpca embedding
#  pca <- diagram_kpca(diagrams = g,K = K,dim = 0,t = 2,sigma = 1.5,features = 2)

## ----echo = T,eval = F--------------------------------------------------------
#  # create noisy copies of D1, D2 and D3
#  g <- generate_TDApplied_vignette_data(100,100,100)

## ----echo = F,eval = T--------------------------------------------------------
summary_table_circle = data.frame(n_row = rep(seq(100,1000,100),each = 3),mean = c(0.00714747905731201,0.00719361305236816,0.00659072399139404,0.0120942831039429,0.0141546249389648,0.00679831504821777,0.0296238899230957,0.031975269317627,0.0120187282562256,0.0591738700866699,0.0736477851867676,0.013093113899231,0.0975550413131714,0.123826575279236,0.0270460844039917,0.174877691268921,0.190020537376404,0.0269159555435181,0.274690747261047,0.308429479598999,0.0326911926269531,0.375274395942688,0.429068613052368,0.0455755949020386,0.537416911125183,0.704735016822815,0.0602942943572998,0.714264035224915,0.934514427185059,0.0701823949813843),sd = c(0.00416409439817576,0.00103939428867545,0.00283514396310527,0.00330519413830816,0.0037098560689518,0.00487599995258418,0.00757202630969173,0.00655484172785339,0.00354816975542627,0.008634257921095,0.0111978902915261,0.00520196790797061,0.0163927418801066,0.0137217256776886,0.0189863689324652,0.0300851306771754,0.0251022408417127,0.00415049572015015,0.0476454379154694,0.0530950605958673,0.0027232003574667,0.0780886114819416,0.0625740498353545,0.00409270764445647,0.0798528960800227,0.0994870782527882,0.00734032492372556,0.125211131073649,0.150414931033262,0.0047393610211507),package = rep(c("TDApplied","TDAstats","rgudhi"),10))

summary_table_torus = data.frame(n_row = rep(seq(100,1000,100),each = 3),mean = c(0.0214290380477905,0.0361514568328857,0.0185693025588989,0.173486971855164,0.287985873222351,0.095064377784729,0.558014988899231,1.09399700164795,0.355920004844666,1.33284955024719,2.61763973236084,0.919509029388428,2.60778844356537,5.22925355434418,1.9862869977951,4.54079780578613,9.26885311603546,3.71534910202026,7.87146556377411,15.3612549304962,6.46634261608124,13.3764961957932,29.6161998748779,11.3355693340302,19.8860220432281,40.2391008138657,16.1777965307236,27.4777059316635,52.6725327730179,21.4953773736954),sd = c(0.00590719416332605,0.00435006457434934,0.00486331744259044,0.0163448033840174,0.0115563007835601,0.0154961115263816,0.0461848374322575,0.0640576197650689,0.0318588746495076,0.0926083026409299,0.111401912343372,0.0313178301226755,0.179409653051677,0.238560041022202,0.155636782475823,0.171232657410583,0.232484417148248,0.112126851006963,0.368501000186268,0.768291023396156,0.796058101972749,0.956629401963294,0.890320670107488,0.364811036989422,1.6083132055632,4.20347766266123,1.12898384866778,2.27807806543504,1.04152618265644,0.458315680861488),package = rep(c("TDApplied","TDAstats","rgudhi"),10))

summary_table_sphere = data.frame(n_row = rep(seq(100,1000,100),each = 3),mean = c(0.0109351873397827,0.0236736059188843,0.950725102424622,0.0561362266540527,0.186890292167664,0.0332694053649902,0.183296179771423,0.596820592880249,0.094449782371521,0.398182415962219,1.48544068336487,0.236253690719605,0.78551504611969,3.08689987659454,0.518527007102966,1.45327532291412,5.52202281951904,0.96248767375946,2.35780429840088,9.03737523555756,1.65125238895416,3.65333969593048,13.5423310518265,2.59944121837616,5.90856101512909,20.433601975441,3.91825902462006,8.94339470863342,29.2576765298843,5.58954420089722),sd = c(0.00381840566885969,0.00556572514553152,2.9610221027017,0.00493067843793022,0.0080879363741825,0.00391292209005512,0.0139272694879288,0.0101076637970659,0.00581921320251676,0.0228565950838192,0.0140904392678963,0.0111634038034156,0.0355001128716911,0.0512718584523393,0.0325010757187493,0.082096411565011,0.102444302459406,0.0203312515495755,0.187843608691578,0.233844190883059,0.0252998825243228,0.132835007447204,0.195363501741675,0.046906966025953,0.455113957164321,0.610849085688629,0.0928097915558342,0.626890440215277,0.763083525901216,0.181292076918064),package = rep(c("TDApplied","TDAstats","rgudhi"),10))

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
lines(summary_table_circle$n_row[summary_table_circle$package=="rgudhi"],
      summary_table_circle$mean[summary_table_circle$package=="rgudhi"], 
      col=4, type="b")
legend(x = 200,y = 0.8,legend = c("TDApplied","TDAstats","rgudhi"),
       col = c("red","black","blue"),lty = c(1,1),cex = 0.8)
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
arrows(summary_table_circle$n_row[summary_table_circle$package == "rgudhi"], 
       summary_table_circle$mean[summary_table_circle$package == "rgudhi"]
       -1.96*summary_table_circle$sd[summary_table_circle$package == "rgudhi"]/sqrt(10), 
       summary_table_circle$n_row[summary_table_circle$package == "rgudhi"], 
       summary_table_circle$mean[summary_table_circle$package == "rgudhi"]
       +1.96*summary_table_circle$sd[summary_table_circle$package == "rgudhi"]/sqrt(10), 
       length=0.05, angle=90, code=3,col = "blue")

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
lines(summary_table_torus$n_row[summary_table_torus$package=="rgudhi"],
      summary_table_torus$mean[summary_table_torus$package=="rgudhi"], 
      col=4, type="b")
legend(x = 200,y = 42,legend = c("TDApplied","TDAstats","rgudhi"),
       col = c("red","black","blue"),lty = c(1,1),cex = 0.8)
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
arrows(summary_table_torus$n_row[summary_table_torus$package == "rgudhi"], 
       summary_table_torus$mean[summary_table_torus$package == "rgudhi"]
       -1.96*summary_table_torus$sd[summary_table_torus$package == "rgudhi"]/sqrt(10), 
       summary_table_torus$n_row[summary_table_torus$package == "rgudhi"], 
       summary_table_torus$mean[summary_table_torus$package == "rgudhi"]
       +1.96*summary_table_torus$sd[summary_table_torus$package == "rgudhi"]/sqrt(10), 
       length=0.05, angle=90, code=3,col = "blue")

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
lines(summary_table_sphere$n_row[summary_table_sphere$package=="rgudhi"],
      summary_table_sphere$mean[summary_table_sphere$package=="rgudhi"], 
      col=4, type="b")
legend(x = 200,y = 23,legend = c("TDApplied","TDAstats","rgudhi"),
       col = c("red","black","blue"),lty = c(1,1),cex = 0.8)
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
arrows(summary_table_sphere$n_row[summary_table_sphere$package == "rgudhi"], 
       summary_table_sphere$mean[summary_table_sphere$package == "rgudhi"]
       -1.96*summary_table_sphere$sd[summary_table_sphere$package == "rgudhi"]/sqrt(10), 
       summary_table_sphere$n_row[summary_table_sphere$package == "rgudhi"], 
       summary_table_sphere$mean[summary_table_sphere$package == "rgudhi"]
       +1.96*summary_table_sphere$sd[summary_table_sphere$package == "rgudhi"]/sqrt(10), 
       length=0.05, angle=90, code=3,col = "blue")

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
     xlim=range(summary_table$n_row), ylim=range(0,summary_table$mean+1.96*summary_table$sd/sqrt(10)),xlab = "Points in shape",ylab = "Mean execution time (sec)",col = "darkgreen")
lines(summary_table$n_row[summary_table$package=="TDApplied"], summary_table$mean[summary_table$package=="TDApplied"], col=2, type="b")
legend(x = 200,y = 2000,legend = c("TDApplied","TDA"),col = c("red","darkgreen"),lty = c(1,1),cex = 0.8)
arrows(summary_table$n_row[summary_table$package == "TDApplied"], summary_table$mean[summary_table$package == "TDApplied"]-1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10), summary_table$n_row[summary_table$package == "TDApplied"], summary_table$mean[summary_table$package == "TDApplied"]+1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10), length=0.05, angle=90, code=3,col = "red")
arrows(summary_table$n_row[summary_table$package == "TDA"], summary_table$mean[summary_table$package == "TDA"]-1.96*summary_table$sd[summary_table$package == "TDA"]/sqrt(10), summary_table$n_row[summary_table$package == "TDA"], summary_table$mean[summary_table$package == "TDA"]+1.96*summary_table$sd[summary_table$package == "TDA"]/sqrt(10), length=0.05, angle=90, code=3,col = "darkgreen")

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
lines(summary_table$n_row[summary_table$package=="persim"], summary_table$mean[summary_table$package=="persim"], col="darkorange", type="b")
legend(x = 200,y = 20,legend = c("TDApplied","persim"),col = c("red","darkorange"),lty = c(1,1),cex = 0.8)
arrows(summary_table$n_row[summary_table$package == "TDApplied"], summary_table$mean[summary_table$package == "TDApplied"]-1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10), summary_table$n_row[summary_table$package == "TDApplied"], summary_table$mean[summary_table$package == "TDApplied"]+1.96*summary_table$sd[summary_table$package == "TDApplied"]/sqrt(10), length=0.05, angle=90, code=3,col = "red")
arrows(summary_table$n_row[summary_table$package == "persim"], summary_table$mean[summary_table$package == "persim"]-1.96*summary_table$sd[summary_table$package == "persim"]/sqrt(10), summary_table$n_row[summary_table$package == "persim"], summary_table$mean[summary_table$package == "persim"]+1.96*summary_table$sd[summary_table$package == "persim"]/sqrt(10), length=0.05, angle=90, code=3,col = "darkorange")

## ----echo = F,eval = F--------------------------------------------------------
#  model <- stats::lm(data = data.frame(ratio =
#                                       summary_table$mean[summary_table$package == "TDApplied"]
#                                       /summary_table$mean[summary_table$package == "persim"],
#                                       n_row = seq(100,1000,100)),
#                     formula = ratio ~ n_row)
#  summary(model)$coefficients

## ----echo = F,warning=F,fig.height = 4,fig.width = 4,fig.align = 'center'-----
summary_table_rgudhi = data.frame(n_row = rep(seq(100,1000,100),each = 2),mean = c(0.0817231178283691,0.00443823337554932,0.153634452819824,0.0169229507446289,0.224382472038269,0.0493201017379761,0.309246802330017,0.0796245098114014,0.386177682876587,0.117957305908203,0.464355993270874,0.169294333457947,0.550355219841003,0.228056526184082,0.610642981529236,0.287962102890015,0.675183773040771,0.349004602432251,0.780945706367493,0.465633773803711),sd = c(0.00902598692151941,0.00610716911690673,0.00811166474663635,0.00666017568130404,0.00826992404493182,0.0126037205619449,0.0114923068904925,0.0191562545838688,0.015757590639173,0.0113232964039647,0.0155500796109551,0.0283430734951902,0.0237791621809305,0.0234003435700701,0.039761635904003,0.0309534056318008,0.012595351107216,0.0213408986276033,0.045162458477071,0.0224582059363766),package = rep(c("TDApplied","rgudhi"),10))

# plot table
plot(summary_table_rgudhi$n_row[summary_table_rgudhi$package=="rgudhi"], 
     summary_table_rgudhi$mean[summary_table_rgudhi$package=="rgudhi"], 
     type="b",
     xlim=range(summary_table_rgudhi$n_row),
     ylim=range(0,summary_table_rgudhi$mean+1.96*summary_table_rgudhi$sd/sqrt(10)),
     xlab = "Points in shape",ylab = "Mean execution time (sec)")
lines(summary_table_rgudhi$n_row[summary_table_rgudhi$package=="TDApplied"],
      summary_table_rgudhi$mean[summary_table_rgudhi$package=="TDApplied"], 
      col=2, type="b")
legend(x = 200,y = 0.7,legend = c("TDApplied","rgudhi"),
       col = c("red","blue"),lty = c(1,1),cex = 0.8)
arrows(summary_table_rgudhi$n_row[summary_table_rgudhi$package == "TDApplied"], 
       summary_table_rgudhi$mean[summary_table_rgudhi$package == "TDApplied"]
       -1.96*summary_table_rgudhi$sd[summary_table_rgudhi$package == "TDApplied"]/sqrt(10),
       summary_table_rgudhi$n_row[summary_table_rgudhi$package == "TDApplied"], 
       summary_table_rgudhi$mean[summary_table_rgudhi$package == "TDApplied"]
       +1.96*summary_table_rgudhi$sd[summary_table_rgudhi$package == "TDApplied"]/sqrt(10), 
       length=0.05, angle=90, code=3,col = "red")
arrows(summary_table_rgudhi$n_row[summary_table_rgudhi$package == "rgudhi"], 
       summary_table_rgudhi$mean[summary_table_rgudhi$package == "rgudhi"]
       -1.96*summary_table_rgudhi$sd[summary_table_rgudhi$package == "rgudhi"]/sqrt(10), 
       summary_table_rgudhi$n_row[summary_table_rgudhi$package == "rgudhi"], 
       summary_table_rgudhi$mean[summary_table_rgudhi$package == "rgudhi"]
       +1.96*summary_table_rgudhi$sd[summary_table_rgudhi$package == "rgudhi"]/sqrt(10), 
       length=0.05, angle=90, code=3,col = "blue")

## ----echo = F,eval = F--------------------------------------------------------
#  model <- stats::lm(data = data.frame(ratio =
#                                       summary_table_rgudhi$mean[summary_table_rgudhi$package == "TDApplied"]/summary_table_rgudhi$mean[summary_table_rgudhi$package == "rgudhi"],
#                                       n_row = seq(100,1000,100)),
#                     formula = ratio ~ n_row)
#  summary(model)$coefficients

