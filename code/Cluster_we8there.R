################################################################
# Testing different methods of initializing cluster membership.#
################################################################

#### k -means  #######
require(textir);data(we8there);cl <- NULL
#load algorithm
source('Iterate.R')
X = we8thereCounts
covars = we8thereRatings[,'Overall',drop=FALSE]





nloop = 15

for(i in 1:n.sim) {
  num_cl = num_cl_vec[i]
  fsim.1 <- append(fsim.1,list(iter_cluster(covars,make_cl1(X=X,i=num_cl),X,nmax=nloop)))
  fsim.2 <- append(fsim.2,list(iter_cluster(covars,make_cl2(X=X,i=num_cl),X,nmax=nloop)))
  fsim.3 <- append(fsim.3,list(iter_cluster(covars,make_cl3(X=X,covars=covars,cl=cl,num_cl),X,nmax=nloop)))
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
 

