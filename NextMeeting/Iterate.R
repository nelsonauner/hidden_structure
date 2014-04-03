######################################################################
###         Iterate for Cluster Membership                        ####
######################################################################

# borrowing heavily from documentation: http://cran.r-project.org/web/packages/textir/textir.pdf

iter_cluster <- function(y,clusters,X,n.loop,debug=FALSE) {
  ptm<-proc.time()
  require(textir)
  ## note, so far, y must be a vector, not matrix :(
  m <- rowSums(X)
  # Notes: mnlm does not handle factors!
  #Turn our factor (membership = 1,2 or 3) into vector (membership = [0 1 0]), etc
  cl_matrix <- model.matrix(formula(~0+as.factor(clusters)))
  #Add 
  Y<-Y_orig <- cbind(y,cl_matrix)
  likes <- rep(NA,n.loop) #store likelihood updates here! 
  clusters <- array(,dim=c(dim(X)[1],n.loop))
  fits <- mnlm(cl,Y ,X, bins=5, gamma=1, nlambda=10); B <- coef(fits)
  res = list(likes,clusters,B,NULL);names(res) <- c("likes","clusters","B","time")
  for (i in 1:n.loop) {
    ll_left <- X%*%t(B[3:6,])   #Only take he relevant part
    #The above formula verified to correctly multiply! We want to maximize ll_left!
    #Below produces an N x p matrix used for calculating penalization
    #We want, for each document: m_i * log(sum(exp(alpha+beta*v+gamma*u)))
    #hm<-sweep(matrix(Y[,1]) %*% t(matrix(B[2,])),MARGIN=2, B[1,],'+')  #Faster than sweep: t(t())? http://stackoverflow.com/questions/3643555/multiply-rows-of-matrix-by-vector
    #Above verified to work 
    #for each possible cluster, we want to add the cluster coeffecients to the intercept and y*beta term we already have
    #the results should be an N x (#clusters) matrix
    #so basically a repeated sweep!
    cust_sweep <- function(m,v) { sweep(m,MARGIN=2,v,'+')}
    ll_penal <- cust_sweep(matrix(Y[,1]) %*% B[2,] ,B[1,]) #add intercept and v_i term
    #The apply function is flipping our matrix, etf. 
    #ll_penal_tot <- vapply(B[3:6,],MARGIN=1,FUN.VALUE=1,FUN=function(x) cust_sweep(ll_penal,x))
    #let's try plyr version:
    ll_penal_tot <- t(aaply(B[3:6,],.margins=1,.fun=function(x) log(rowSums(exp(cust_sweep(ll_penal,x))))))
    ll <- m*ll_penal_tot - ll_left  #We cannot expect to be positive as we took out some common terms. 
    # check out cust_sweep(ll_penal,B[4,]) for an example of what's going on here
    #Store the new (relative) log likelihood here:
    likes[i] <- sum(apply(ll,MARGIN=1,FUN=min))
    #Select new cluster membership if better. 
    clusters[,i] <- n_cl <- as.factor(apply(ll,MARGIN=1,FUN=which.min)) #select cluster to minimize L 
    n_cl_matrix <- model.matrix(formula(~0+(n_cl))) #and convert to [0 0 1] form. 
    #update our Y:
    Y[,2:5] <- n_cl_matrix
    #And refit: 
    fits <- mnlm(cl,Y ,we8thereCounts, bins=5, gamma=1, nlambda=10); B <- coef(fits)  #
    B <- coef(fits) #Pull the coeffecients
  }
  res$likes = likes
  res$clusters = clusters
  res$B = B #the final loadings matrix
  res$time <- proc.time()-ptm
  return(res)
}
