################################################################
# Testing different methods of initializing cluster membership.#
################################################################

#### k -means  #######
require(textir);data(congress109);cl <- NULL
#load algorithm
#source('hidden_structure.R')
source('Iterate.R')
X = congress109Counts
covars <- data.frame(gop=congress109Ideology$party=="R",
cscore=congress109Ideology$cs1)
covars$cscore <- covars$cscore -
tapply(covars$cscore,covars$gop,mean)[covars$gop+1]
rownames(covars) <- rownames(congress109Ideology)


#Initialization methods:
##### random ##########
make_cl1 <- function(i) {sample(1:i,size=dim(X)[1],replace=TRUE)}
##### k means on data ##########
make_cl2 <- function(i) {kmeans(X,i)$cluster}
##### k means on residuals#################
fits <- mnlm(cl, covars,X, bins=5, gamma=1, nlambda=10)
m <- rowSums(X)
resids <- X-m*predict(fits,covars,type="response") #(these are not really good residuals?)
make_cl3 <- function(i) {kmeans(resids,i)$cluster}
# k mean



#test:
fsim.1 <- fsim.2 <- fsim.3<- list()
#n.sim=10
num_cl = 5
nloop = 25
for(i in 1:n.sim) {
  fsim.1 <- append(fsim.1,list(iter_cluster(covars,make_cl1(num_cl),X,n.loop=nloop)))
  fsim.2 <- append(fsim.2,list(iter_cluster(covars,make_cl2(num_cl),X,n.loop=nloop)))
  fsim.3 <- append(fsim.3,list(iter_cluster(covars,make_cl3(num_cl),X,n.loop=nloop)))
}

cong_res <- list(fsim.1,fsim.2,fsim.3)
save(cong_res,file="cong_res.RData")

#now, time to calculate multinomial deviance or something for all of them to compare

naive.dev <- multi.devian(X,as.matrix(covars),coef(fits))
ftotres<-list(fsim.1,fsim.2,fsim.3)
#prepare results matrix:
resm <- as.data.frame(c(rep(1,10),rep(2,10),rep(3,10))

resm$deviance = 0

for (i in 1:3) {
for(j in 1:n.sim) {
print(i)
print(j)
rel_sim <- totres[[i]][[1]]
resm$deviance[(i-1)*10+j] <- multi.devian(X,as.matrix(cbind(covars,model.matrix(~0+as.factor(rel_sim$clusters)))),rel_sim$B)
resm$elapsed[(i-1)*10+j]<- rel_sim$time[3]
}
}

rel_sim<-sim.1[[1]]

save.image()

#Plot the log likelihood 

par(mfrow=c(3,4))
titles =c("Random Initialization", "K means Initialization", "Residual K Means")
nclust <- c("5","10")
likes <- array(dim=c(15,12))

plot(ftotres[[3]][[1]]$likes$cluster_likes,type="l",ylab="relevant log likelihood",xlab="Iterations",main="Residual K means Initialization")
for (i in 2:n.sim) {
    lines(ftotres[[3]][[i]]$likes$cluster_likes,type="l",col=i)
}


plot(resm$init,resm$deviance,ylim=c(-1350000,-1000000),main="Multinomial Deviance",xlab="Initialization Type \n (1=rand, 2=kmeans, 3= resid kmeans)")
abline(naive.dev,0)
	
#########Let's investigate the clustering results..? 
final_clusters <- cbind(cl2,ftotres[[1]][[1]]$h.clusters[,10],ftotres[[1]][[2]]$h.clusters[,15],
                        ftotres[[2]][[1]]$h.clusters[,10],ftotres[[2]][[2]]$h.clusters[,15])

table(final_clusters[final_clusters[,2]==1,3])

collect_results<-as.data.frame(rep(1:30,30))
collect_results$init = c(rep(1,300),rep(2,300),rep(3,300))
collect_results$likes = 0
for(i in 1:3) { for (j in 1:10) {
collect_results$likes[((i-1)*300+(j-1)*30+1):((i-1)*300+j*30)] = ftotres[[i]][[j]]$likes$cluster_likes
}}
library(ggplot2)
p<-ggplot(collect_results,aes(x=Iteration,y=likes,colour=init))
p <- p + layer(geom = "point") +ggtitle("Likelihood convergence of clusters")+scale_fill_discrete(name="Cluster Initialization")
p  #render plot
