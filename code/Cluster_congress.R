################################################################
# Testing different methods of initializing cluster membership.#
################################################################

#### k -means  #######
require(textir);data(congress109);cl <- NULL
#load algorithm
#source('hidden_structure.R')
source('Iterate.R')
X = congress109Counts
covars <- data.frame(gop=congress109Ideology$party=="R")


#Initialization methods:
##### random ##########
make_cl1 <- function(i) {sample(1:i,size=dim(X)[1],replace=TRUE)}
##### k means on data ##########
make_cl2 <- function(i) {kmeans(X,i)$cluster}
##### k means on residuals#################
fits <- mnlm(cl, covars,X, bins=5, gamma=1, nlambda=10)
resids <- X-m*predict(fits,covars,type="response") #(these are not really good residuals?)
make_cl3 <- function(i) {kmeans(resids,i)$cluster}
# k mean



#test:
fsim.1 <- fsim.2 <- fsim.3<- list()
n_cl = 5
nloop = 30
  fsim.1 <- append(fsim.1,list(iter_cluster(covars,make_cl1(n_cl),X,n.loop=nloop)))
  fsim.2 <- append(fsim.2,list(iter_cluster(covars,make_cl2(n_cl),X,n.loop=nloop)))
  fsim.3 <- append(fsim.3,list(iter_cluster(covars,make_cl3(n_cl),X,n.loop=nloop)))


save.image()

#now, time to calculate multinomial deviance or something for all of them to compare

naive.dev <- multi.devian(X,as.matrix(covars),coef(fits))
ftotres<-list(fsim.1,fsim.2,fsim.3)
#prepare results matrix:
resm <- array(dim=c(3,1))

for (i in 1:3) {
rel_sim <- ftotres[[i]][[j]]
resm[i] <- multi.devian(X,as.matrix(cbind(covars,model.matrix(~0+as.factor(rel_sim$clusters)))),rel_sim$B)
}

save.image()

#Plot the log likelihood 

par(mfrow=c(3,4))
titles =c("Random Initialization", "K means Initialization", "Residual K Means")
nclust <- c("5","10")
likes <- array(dim=c(15,12))
for (i in 1:3) {
    plot(ftotres[[i]]$full_likes,type="l",ylab="relevant log likelihood",xlab="Iterations",main=paste(titles[i]," \n # clusters = ",nclust[(j>2)+1]))
}

#########Let's investigate the clustering results..? 
final_clusters <- cbind(cl2,ftotres[[1]][[1]]$h.clusters[,10],ftotres[[1]][[2]]$h.clusters[,15],
                        ftotres[[2]][[1]]$h.clusters[,10],ftotres[[2]][[2]]$h.clusters[,15])

table(final_clusters[final_clusters[,2]==1,3])

