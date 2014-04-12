######################################################################
###         Iterate for Cluster Membership                        ####
######################################################################

# borrowing heavily from documentation: http://cran.r-project.org/web/packages/textir/textir.pdf
# It would  be nice to implement some error checking

iter_cluster <- function(y,clusters,X,n.loop,debug=FALSE,cl=NULL) {
  require('plyr') 
  ptm<-proc.time()
  require(textir)
  ## note, so far, y must be a vector, not matrix :(
  m <- rowSums(X)
  # Notes: mnlm does not handle factors!
  #Turn our factor (membership = 1,2 or 3) into vector (membership = [0 1 0]), etc
  cl_matrix <- model.matrix(formula(~0+as.factor(clusters)))
  #Add 
  ncl <- dim(cl_matrix)[2]
  Y<-Y_orig <- as.matrix(cbind(y,cl_matrix))
  d.Y <- dim(Y)[2]
  n.meta <- dim(matrix(y))[2]
  likes <- rep(NA,n.loop) #store likelihood updates here! 
  h.clusters <- array(,dim=c(dim(X)[1],n.loop))
  fits <- mnlm(cl,Y ,X, bins=5, gamma=1, nlambda=10); 
  B <- coef(fits)
  res = list(likes,clusters,B,NULL);names(res) <- c("likes","clusters","B","time")
  cust_sweep <- function(m,v) { sweep(m,MARGIN=2,v,'+')}  #this adds vector v to every row in matrix m
  for (i in 1:n.loop) {
    ll_left <- X%*%t(B[(2+n.meta):d.Y,])   #This ignores the alpha and meta data terms of the coeffecient matrix 
    print("ll_left OK")
    #The above formula verified to correctly multiply! We want to maximize ll_left!
    ##################################################################################
    #  This operation could be simplified by collapsing over metadata levels:        #
    ##################################################################################
    ll_penal <- cust_sweep(matrix(Y[,1:n.meta]) %*% B[2:(1+n.meta),] ,B[1,]) #add intercept and v_i*theta term
    #The apply function is flipping our matrix, etf. 
    #ll_penal_tot <- vapply(B[3:6,],MARGIN=1,FUN.VALUE=1,FUN=function(x) cust_sweep(ll_penal,x))
    #let's try plyr version:
    ll_penal_tot <- t(aaply(B[(2+n.meta):d.Y,],.margins=1,.fun=function(x) log(rowSums(exp(cust_sweep(ll_penal,x))))))
    print("ll_penal_tot OK")
    ll <- m*ll_penal_tot - ll_left  #We cannot expect to be positive as we took out some common terms. 
    # check out cust_sweep(ll_penal,B[4,]) for an example of what's going on here
    #Store the new (relative) log likelihood here:
    likes[i] <- sum(apply(ll,MARGIN=1,FUN=min))
    #Select new cluster membership if better. 
    h.clusters[,i] <- n_cl <- as.factor(apply(ll,MARGIN=1,FUN=which.min)) #select cluster to minimize L 
    n_cl_matrix <- model.matrix(formula(~0+(n_cl))) #and convert to [0 0 1] form. 
    #update our Y:
    Y[,(2+n.meta):d.Y] <- n_cl_matrix
    #And refit: 
    fits <- mnlm(cl,Y ,we8thereCounts, bins=5, gamma=1, nlambda=10); B <- coef(fits)  #
    B <- coef(fits) #Pull the coeffecients
  }
  res$likes = likes
  res$h.clusters = h.clusters
  res$B = B #the final loadings matrix
  res$time <- proc.time()-ptm
  return(res)
}

predict.qij<-function(Y,B){ #we need to make a function to make the predicted q_ij
  ##WTF are you serious where is d.Y????
  d.Y = dim(Y)[2]
  ## Important! Y needs to be in model matrix form! ## [alpha beta [1 0 0 ] ]
  q.num <- exp(cust_sweep(Y[,1:d.Y] %*% B[2:(1+d.Y),] ,B[1,])) #add intercept, v_i*theta, and cluster term
  q.denom <- rowSums(q.num) #since we add up for all terms j = 1 to p!
  return(q.num/q.denom)
}

multi.devian <- function(X,Y,B) {
  q.ij <- predict.qij(Y,B)
  return(sum(rowSums(X*q.ij)))
}
