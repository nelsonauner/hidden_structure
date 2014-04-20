######################################################################
###         Iterate for Cluster Membership                        ####
######################################################################

# borrowing heavily from documentation: http://cran.r-project.org/web/packages/textir/textir.pdf
# It would  be nice to implement some error checking

iter_cluster <- function(y,clusters,X,n.loop,debug=FALSE,cl=NULL,collapse=FALSE) {
  require('plyr');require(textir)
  m <- rowSums(X)
  # mnlm does not handle factors! Turn our factor (membership = 1,2 or 3) into vector (membership = [0 1 0]), etc
  cl_matrix <- model.matrix(formula(~0+as.factor(clusters)))
  ncl <- dim(cl_matrix)[2]  #Keep track of how many clusters we're using. 
  Y<-Y_orig <- as.matrix(cbind(y,cl_matrix))
  d.Y <- dim(Y)[2]
  n.meta <- dim(as.matrix(y))[2]
  likes <- rep(NA,n.loop) #store likelihood updates here! 
  h.clusters <- array(,dim=c(dim(X)[1],n.loop)) #we'll keep track of cluster assignments over time here
  fits <- mnlm(cl,Y ,X, bins=5, gamma=1, nlambda=10); 
  B <- coef(fits)
  #Initialize a result to return. 
  res = list(likes,clusters,B,NULL);names(res) <- c("likes","clusters","B","time")
  cust_sweep <- function(m,v) { sweep(m,MARGIN=2,v,'+')}  #this adds vector v to every row in matrix m
  normalize <- function(Xhat,vector) log(rowSums(exp(cust_sweep(Xhat,x))))
  for (i in 1:n.loop) {
    ll_left <- X%*%t(B[(2+n.meta):d.Y,])   #This ignores the alpha and meta data terms of the coeffecient matrix 
    #This is definitely necessary (above)
    print("ll_left OK")
    #The above formula verified to correctly multiply! We want to maximize ll_left
    #  This operation could be simplified by collapsing over metadata levels:        #
    Xhat <- cust_sweep(as.matrix(Y[,1:n.meta]) %*% B[2:(1+n.meta),] ,B[1,]) #add intercept and v_i*theta term
    #The apply function is flipping our matrix, etf. 
    #ll_penal_tot <- vapply(B[3:6,],MARGIN=1,FUN.VALUE=1,FUN=function(x) cust_sweep(ll_penal,x))
    #let's try plyr version:
    Gamma_cl <-B[(2+n.meta):d.Y,] #the cluster coeffecients
    
    left_term<-function(x,Xhat) { log(rowSums(exp( t(t(Xhat)+x) ))) }  
    ll_right = m*apply(X=Gamma_cl,MARGIN=1,FUN=function(x,matrix){log(rowSums(exp( t(t(matrix)+x) )))},matrix=Xhat)
    #system.time(apply(X=Gamma_cl,MARGIN=1,FUN=function(x,matrix){log(rowSums(exp( t(t(matrix)+x) )))},matrix=Xhat))
    #system.time(parRapply(cl,x = Gamma_cl,FUN=function(x,matrix){log(rowSums(exp( t(t(matrix)+x) )))},matrix=Xhat))
    print("ll_penal_right OK")
    #Compute total log likelihood
    ll <- ll_right - ll_left  #We cannot expect to be positive as we took out some common terms.   
    cluster_likes[i] <- sum(apply(ll,MARGIN=1,FUN=min)+)
    full_likes[i] = cluster_likes[i] - rowSums(Xhat*X) #Add in the likelihood from the -x'(alpha+phi*vi)
    #Select new cluster membership if better. 
    h.clusters[,i] <- n_cl <- as.factor(apply(ll,MARGIN=1,FUN=which.min)) #select cluster to minimize L 
    n_cl_matrix <- model.matrix(formula(~0+(n_cl))) #and convert to [0 0 1] form. 
    #update our Y:
    Y[,(2+n.meta):d.Y] <- n_cl_matrix
    #And refit: 
    fits <- mnlm(cl,Y ,X, bins=5, gamma=1, nlambda=10); B <- coef(fits)  #
    B <- coef(fits) #Pull the coeffecients
  }
  res$likes = likes
  res$h.clusters = h.clusters
  res$B = B #the final loadings matrix
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
