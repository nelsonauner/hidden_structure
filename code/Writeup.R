setwd("C:/Users/nauner/tech/hidden_structure/code")
load("cong_res.RData")

##A brief explanation of how the data was obtained: 
##This graph kind of sucks: 
#First graph: illustrate convergence of clusters

c_c <- as.data.frame(cbind(x=rep(1:14,each=10),
					y=rep(1:10,14),
					cluster=c(cong_res[[1]][[1]]$h.clusters[1:10,])))
					
plot(c_c$x,c_c$y,col=c_c$cluster,pch=15)

##Better idea? make a chart of how many are left to be switched




#multinomial deviance



## Find high word ratings	



##Compare how fast things settle: 
table(cong_res[[1]][[1]]$h.clusters[,1])  #vs
table(cong_res[[1]][[1]]$h.clusters[,14]) #no longer evenly distributed

 head(cong_res[[1]][[1]]$B@Dimnames[2][order(cong_res[[1]][[1]]$B@Dimnames[1])])
 
 beta <- as.matrix(cong_res[[1]][[1]]$B)
 
#find number of non-zero coeffecients
length(beta[4,][beta[4,]!=0])  #the first cluster
#find highest rated coeffecients for each cluster and puht into a table!

head(beta[6,][order(abs(beta[6,]),decreasing=TRUE)])
berta
high_loadings <- function(beta,cl_range,digits=2) {
if(length(cl_range)==1) { 
cl_range <- cl_index #startpoint:(dim(beta)[1])
highloadings <- data.frame(do.call(cbind,lapply(cl_index,
					FUN=function(x){
						res<-head(
						beta[x,][order(abs(beta[x,]),decreasing=TRUE)])
						return(cbind(names(res),round(res,digits=2)))
						}
						)),row.names=NULL)
names(highloadings) = rep(c("term","loading"),length(cl_range))
return(highloadings)
}

y<-high_loadings(beta,c(6,8))
z

