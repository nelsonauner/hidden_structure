#Testing different methods of initializing cluster membership. 
require(textir);data(we8there);cl <- NUL
#load algorithm
source('Iterate.R')
X = we8thereCounts
y = we8thereRatings[,'Overall',drop=FALSE]
n.clusters = 5

##### random ##########
make_cl1 <- function(i) {sample(1:i,size=dim(we8thereRatings)[1],replace=TRUE)}
#### k -means  #######
make_cl2 <- function(i) {kmeans(X,i)$cluster}
##### k means on residuals#################
cl <- NULL
fits <- mnlm(cl, y,X, bins=5, gamma=1, nlambda=10)
##############################################
m <- rowSums(we8thereCounts)
resids <- X-m*predict(fits,y,type="response") #(these are not really good residuals?)
make_cl3 <- function(i) {kmeans(resids,i)$cluster}
# k mean

nloop = 15

#Now I will test each cluster initialization, for 15 iterations, 
sim.1 <- sim.2 <- sim.3 <- list()
for (i in c(5,10,15)) {
  sim.1 <- append(sim.1,list(iter_cluster(y,make_cl1(i),X,n.loop=nloop)))
  sim.2 <- append(sim.2,list(iter_cluster(y,make_cl2(i),X,n.loop=nloop)))
  sim.3 <- append(sim.3,list(iter_cluster(y,make_cl3(i),X,n.loop=nloop)))
}

