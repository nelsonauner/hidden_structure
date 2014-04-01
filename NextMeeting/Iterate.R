## Iterating Cluster Membership and Parameter Values ##
# borrowing heavily from documentation: http://cran.r-project.org/web/packages/textir/textir.pdf
library(textir);data(we8there);cl <- NULL
#Initialize Cluster Membership randomly
cl_num<- as.factor(floor(runif(n=dim(we8thereRatings)[1],min=0,max=4)))
m <- rowSums(we8thereCounts)
# Notes: mnlm does not handle factors!
#Turn our factor (membership = 1,2 or 3) into vector (membership = [0 1 0]), etc
cl_matrix <- model.matrix(formula(~0+cl_num))
#Add clustermembership to the 'attributes' matrix so we can regress on it
Y<-Y_orig <- cbind(we8thereRatings[,'Overall',drop=FALSE],cl_matrix)
#Iterating loop. Takes a while
n.loop = 10
likes <- rep(NA,n.loop) #store likelihood updates here! 
fits <- mnlm(cl,Y ,we8thereCounts, bins=5, gamma=1, nlambda=10); B <- coef(fits)  #
#Fit the model 
B <- B_orig <- coef(fits) #Pull the coeffecients

for (i in 1:n.loop) {  
  ll_left <- we8thereCounts%*%t(B[3:6,])   #this is still correct for the first part. 
  #this produces an N x p matrix used for calculating penalization
  hm<-sweep(matrix(Y[,1]) %*% t(matrix(B[2,])),MARGIN=2, B[1,],'+')  #Faster than sweep: t(t())? http://stackoverflow.com/questions/3643555/multiply-rows-of-matrix-by-vector
  #for each possible cluster, we want to add the cluster coeffecients to the intercept and y*beta term we already have
  #the results should be an N x (#clusters) matrix
  #so basically a repeated sweep!
  cust_sweep <- function(m,v) { sweep(m,MARGIN=2,v,'+')}
  ll_penal <- cust_sweep(matrix(Y[,1]) %*% B[2,] ,B[1,]) #add intercept and v_i term
  ll_penal_tot <- apply(B[3:6,],MARGIN=1,function(x) log(rowSums(exp(cust_sweep(ll_penal,x)))))  
  ll <- m*ll_penal_tot - ll_left  #We cannot expect to be positive as we took out some common terms. 
  # check out cust_sweep(ll_penal,B[4,]) for an example of what's going on here
  #Select new cluster membership if better. 
  likes[i] <- sum(apply(ll,MARGIN=1,FUN=min))
  n_cl <- as.factor(apply(ll,MARGIN=1,FUN=which.min)) #select cluster to minimize L 
  n_cl_matrix <- model.matrix(formula(~0+(n_cl))) #and convert to [0 0 1] form. 
  #update our Y:
  Y[,2:5] <- n_cl_matrix
  #And refit: 
  fits <- mnlm(cl,Y ,we8thereCounts, bins=5, gamma=1, nlambda=10); B <- coef(fits)  #
  B <- B_orig <- coef(fits) #Pull the coeffecients
}

#TODO: Check that the log likelihood is actually higher: 
qplot(1:10,likes,xlab="iteration",ylab="log likelihood (relative)",main="MNIR with Clustering")
qfinal <- predict(B, newdata=we8thereRatings,type="response")
sum(we8thereCounts[1,]*log(q[1,]))