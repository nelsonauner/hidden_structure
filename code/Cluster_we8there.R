################################################################
# Testing different methods of initializing cluster membership.#
################################################################

#### k -means  #######
require(textir);data(we8there);cl <- NULL
#load algorithm
source('Iterate.R')
X = we8thereCounts
y = we8thereRatings[,'Overall',drop=FALSE]

##### random ##########
make_cl1 <- function(i) {sample(1:i,size=dim(we8thereRatings)[1],replace=TRUE)}
##### random ##########
make_cl1 <- function(i) {sample(1:i,size=dim(we8thereRatings)[1],replace=TRUE)}
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
sim.1 <- sim.2 <- sim.3<- list()
for (i in c(10,15)) {
  sim.1 <- append(sim.1,list(iter_cluster(y,make_cl1(i),X,n.loop=nloop)))
  sim.2 <- append(sim.2,list(iter_cluster(y,make_cl2(i),X,n.loop=nloop)))
  sim.3 <- append(sim.3,list(iter_cluster(y,make_cl3(i),X,n.loop=nloop)))
}

#######################################################
#       Show approx. log likelihood per iteration
######################################################
jpeg('rplot.jpg')
totres <- list(sim.1,sim.2,sim.3);names(totres) <- c("sim1", "sim2", "sim3")
par(mfrow= c(3,2))
cltitle <- c("10 clusters","15 clusters")
inititle <- c("Random", "k means on X", "kmeans on resids of X = Phi Y")
for (i in 1:3) {
  for (j in 1:2) {
    l <- totres[[i]][[j]]$likes
    plot(1:length(l),l,pch="+",main=paste(inititle[i],cltitle[j],sep="-"),xlab="iterations",ylab="approx. likelihood")
  }
}
dev.off()

############################################################
# Test Multinomial Deviance for 3 initializations #########
###########################################################
res <- array(rep(NA,6),dim=c(2,3))
for (i in 1:3) {
  for (j in 1:2) {
    #quick <- make the model matrix
    factorize <- as.factor(totres[[i]][[j]]$h.cluster[,dim(totres[[i]][[j]]$h.cluster)[2]])
    cl_m <- model.matrix(formula(~0+factorize))
    res[j,i] <- multi.devian(X,as.matrix(cbind(y,cl_m)),totres[[i]][[j]]$B)
  }
}
 

