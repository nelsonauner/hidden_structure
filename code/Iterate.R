######################################################################
###         Iterate for Cluster Membership                        ####
######################################################################

# borrowing heavily from documentation: http://cran.r-project.org/web/packages/textir/textir.pdf
# It would  be nice to implement some error checking

iter_cluster <- function(y,clusters,X,n.loop,debug=FALSE,cl=NULL,collapse=FALSE) {
  require('plyr');require(textir);ptm<-proc.time()
  m <- rowSums(X)
  # mnlm does not handle factors! Turn our factor (membership = 1,2 or 3) into vector (membership = [0 1 0]), etc
  cl_matrix <- model.matrix(formula(~0+as.factor(clusters)))
  ncl <- dim(cl_matrix)[2]  #Keep track of how many clusters we're using. 
  Y<-Y_orig <- as.matrix(cbind(y,cl_matrix))
  d.Y <- dim(Y)[2]
  n.meta <- dim(as.matrix(y))[2]
  cluster_likes<-full_likes <- rep(NA,n.loop) #store likelihood updates here! 
  h.clusters <- array(,dim=c(dim(X)[1],n.loop)) #we'll keep track of cluster assignments over time here
  fits <- mnlm(cl,Y ,X, bins=5, gamma=1, nlambda=10); 
  B <- coef(fits)

  res = list(NULL,clusters,NULL,NULL);names(res) <- c("likes","clusters","B","time")
  cust_sweep <- function(m,v) { t(t(m)+v) }  #this adds vector v to every row in matrix m
  #normalize <- function(Xhat,vector) log(rowSums(exp(cust_sweep(Xhat,x))))
  for (i in 1:n.loop) {
	Gamma_cl <- B[(2+n.meta):d.Y+1,] #include intercept term...
    ll_left <- X%*%t(Gamma_cl)   #This ignores the alpha and meta data terms of the coeffecient matrix 
    #This is definitely necessary (above)
    if(debug) print("ll_left OK")
    #The above formula verified to correctly multiply! We want to maximize ll_left
    #  This operation could be simplified by collapsing over metadata levels:        #
    Xhat <- cust_sweep(as.matrix(Y[,1:n.meta]) %*% B[2:(1+n.meta),] ,B[1,]) #add intercept and v_i*theta term
    #The apply function is flipping our matrix, etf. 
    #ll_penal_tot <- vapply(B[3:6,],MARGIN=1,FUN.VALUE=1,FUN=function(x) cust_sweep(ll_penal,x))
    #let's try plyr version:
    #A quick function to 
	#Now, for each cluster coeffecient (row of Gamma_cl, add it to Xhat, then take exp,sum, and log
    ll_right = apply(X=Gamma_cl,MARGIN=1,FUN=function(x,matrix){log(rowSums(exp( t(t(matrix)+x) )))},matrix=Xhat)
    #system.time(apply(X=Gamma_cl,MARGIN=1,FUN=function(x,matrix){log(rowSums(exp( t(t(matrix)+x) )))},matrix=Xhat))
    #system.time(parRapply(cl,x = Gamma_cl,FUN=function(x,matrix){log(rowSums(exp( t(t(matrix)+x) )))},matrix=Xhat))
    if(debug) print("ll_right OK")
    #Compute total log likelihood
    ll <- m*ll_right - ll_left  #We cannot expect to be positive as we took out some common terms.   
    cluster_likes[i] <- sum(apply(ll,MARGIN=1,FUN=min))
    full_likes[i] = cluster_likes[i] - sum(rowSums(Xhat*X)) #Add in the likelihood from the -x'(alpha+phi*vi)
    #Select new cluster membership if better. 
    h.clusters[,i] <- n_cl <- factor(apply(ll,MARGIN=1,FUN=which.min),levels=1:(ncl-1)) #select cluster to minimize L 
    n_cl_matrix <- model.matrix(formula(~0+(n_cl))) #and convert to [0 0 1] form. 
    #update our Y:
    Y[,(2+n.meta):d.Y] <- n_cl_matrix
    #And refit: 
    fits <- mnlm(cl,Y ,X, bins=5, gamma=1, nlambda=10); B <- coef(fits)  #
    B <- coef(fits) #Pull the coeffecients
  }
  res$time <- proc.time()-ptm
  res$likes <- as.data.frame(cbind(cluster_likes,full_likes))
  res$h.clusters = h.clusters
  res$B = B #the final loadings matrix
  return(res)
}


predict.qij<-function(Y,B){ #we need to make a function to make the predicted q_ij
#verified to produce same results as  
 d.Y = dim(Y)[2]
  ## Important! Y needs to be in model matrix form! ## [alpha beta [1 0 0 ] ]
  q.num <- exp(cust_sweep(as.matrix(Y)[,1:d.Y] %*% B[2:(1+d.Y),] ,B[1,])) #add intercept, v_i*theta, and cluster term
  q.denom <- rowSums(q.num) #since we add up for all terms j = 1 to p!
  return(q.num/q.denom)
}

multi.devian <- function(X,Y,B) {
  q.ij <- predict(B, newdata=Y,type="response")
  return(sum(rowSums(X*log(q.ij))))
}
