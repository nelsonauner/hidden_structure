################################################################
# Testing different methods of initializing cluster membership.#
################################################################

#### k -means  #######
require(textir);data(congress109);cl <- NULL
#load algorithm
source('hidden_structure.R')
X = congress109Counts
covars <- data.frame(gop=congress109Ideology$party=="R",
cscore=congress109Ideology$cs1)
covars$cscore <- covars$cscore -
tapply(covars$cscore,covars$gop,mean)[covars$gop+1]
rownames(covars) <- rownames(congress109Ideology)

##### random ##########
make_cl1 <- function(i) {sample(1:i,size=dim(X)[1],replace=TRUE)}
##### random ##########
make_cl1 <- function(i) {sample(1:i,size=dim(X)[1],replace=TRUE)}
make_cl2 <- function(i) {kmeans(X,i)$cluster}
##### k means on residuals#################
fits <- mnlm(cl, covars,X, bins=5, gamma=1, nlambda=10)
##############################################


m <- rowSums(we8thereCounts)
resids <- X-m*predict(fits,y,type="response") #(these are not really good residuals?)
make_cl3 <- function(i) {kmeans(resids,i)$cluster}
# k mean

nloop = 15

#test:

#Now I will test each cluster initialization, for 15 iterations,
sim.1 <- sim.2 <- sim.3<- list()
for (i in c(5,10)) {
for (nloop in c(10,15)) {    
  sim.1 <- append(sim.1,list(iter_cluster(covars,make_cl1(i),X,n.loop=nloop)))
  sim.2 <- append(sim.2,list(iter_cluster(covars,make_cl2(i),X,n.loop=nloop)))
  sim.3 <- append(sim.3,list(iter_cluster(covars,make_cl3(i),X,n.loop=nloop)))
}}

save.image()

#now, time to calculate multinomial deviance or something for all of them to compare

naive.dev <- multi.devian(X,as.matrix(covars),coef(fits))

#prepare results matrix:
resm <- array(dim=c(3,4))

for (i in 1:3) {
for (j in 1:4) {
rel_sim <- totres[[i]][[j]]
resm[i,j] <- multi.devian(X,as.matrix(cbind(covars,model.matrix(~0+as.factor(rel_sim$clusters)))),rel_sim$B)
}}

#Plot the log likelihood 

par(mfrow=c(3,4))
titles =c("Random Initialization", "K means Initialization", "Residual K Means")
nclust <- c("5","10")
likes <- array(dim=c(15,12))
for (i in 1:3) {
  for (j in 1:4) {
    plot(totres[[1]][[j]]$likes,type="l",ylab="relevant log likelihood",xlab="Iterations",main=paste(titles[i]," \n # clusters = ",nclust[(j>2)+1]))
}}

