######################################################################
###         Iterate for Cluster Membership                        ####
######################################################################

# borrowing heavily from documentation: http://cran.r-project.org/web/packages/textir/textir.pdf
# It would  be nice to implement some error checking

iter_cluster <- function(
  y,
  clusters,
  X,
  nmax,
  debug=FALSE,
  cl=NULL,
  collapse=FALSE) 
{
  require('plyr')
  require(textir)
  ptm<-proc.time()
  CONVERGENCE <- FALSE #initialize CONVERGENCE dummy variable
  m <- rowSums(X)
  # Turn our factor (membership = 1,2 or 3) 
  # into vector (membership = [0 1 0]), etc
  cl_matrix <- model.matrix(formula(~0+as.factor(clusters)))
  ncl <- dim(cl_matrix)[2]  #Keep track of how many clusters we're using. 
  Y<- Y_orig <- Matrix(cbind(as.matrix(y),cl_matrix),sparse=TRUE)
  d.Y <- dim(Y)[2]
  n.meta <- dim(as.matrix(y))[2]
  #store likelihood updates here! 
  cluster_likes<-full_likes <- rep(NA,nmax) 
  #we'll keep track of cluster assignments over time here
  h.clusters <- array(,dim=c(dim(X)[1],nmax)) 
  fits <- mnlm(cl,Y ,X, bins=5, gamma=1, nlambda=10); 
  B <- coef(fits)

  res = list(NULL,NULL,NULL,NULL)
  names(res) <- c("likes","covars","B","time")
  #we'll keep track of cluster assignments over time here
  #normalize <- function(Xhat,vector) log(rowSums(exp(cust_sweep(Xhat,x))))
	for (i in 1:nmax) {
	if (debug) print(i)	
	Gamma_cl <- B[(1+n.meta):d.Y+1,] #include intercept term...
    #This ignores the alpha and meta data terms of the coeffecient matrix 
    ll_left <- X%*%t(Gamma_cl)   
    if(debug) print("ll_left OK")
    # The above formula verified to correctly multiply! 
    # We want to maximize ll_left + ll_right
    # This operation could be simplified by collapsing over metadata levels.
    VPHI <- Y[,1:n.meta,drop=FALSE] %*% B[2:(1+n.meta),,drop=FALSE]  
    tXhat <- t(VPHI)+B[1,]
	  # Now, for each cluster coeffecient 
    # (row of Gamma_cl, add it to Xhat, then take exp,sum, and log
    xhg <- function(j){ q
      expetaj <- exp(tXhat +  Gamma_cl[j,])
      log(colSums(expetaj))
    } 
    ll_right = do.call(cbind,lapply(1:ncl, xhg))
    if(debug) print("ll_right OK")
    #Compute total log likelihood
    ll <- m*ll_right - ll_left   
    h.clusters[,i] <-  n_cl <- factor(apply(ll,MARGIN=1,FUN=which.min),levels=1:ncl) #select cluster to minimize L   
	#if we have converged (same cluster labels as last iteration, then quit)
	 if( i > 1) {if ( all(n_cl ==h.clusters[,i-1])) {CONVERGENCE <- TRUE; break}}
    cluster_likes[i] <- sum(apply(ll,MARGIN=1,FUN=min)) ## use n_cl
    full_likes[i] = cluster_likes[i] - sum(rowSums(t(tXhat)*X)) #Add in the likelihood from the -x'(alpha+phi*vi)
    #Select new cluster membership if better. 
    n_cl_matrix <- sparse.model.matrix(formula(~0+(n_cl))) # and convert to [0 0 1] form. 
    #update our Y:
    Y[,(1+n.meta):d.Y] <- n_cl_matrix
    #And refit: 
    fits <- mnlm(cl,Y ,X, bins=5, gamma=1, nlambda=10);
    B <- coef(fits) #Pull the coeffecients
  }
  #Only return relevant clusters--pre-allocated NA vectors will be garbage disposed of.
  res$time <- proc.time()-ptm
  res$likes <- as.data.frame(cbind(cluster_likes[1:(i-1)],full_likes[1:(i-1)]))
  res$covars <- Matrix(cbind(as.matrix(y),h.clusters[,(i-1)]),sparse=TRUE)
  res$h.clusters <-  h.clusters[,1:(i-1)]
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


##This works fine but we have to be careful that the dimensions are matching up.
multi.devian <- function(X,Y,B) {
  q.ij <- predict(B,Y,type="response")
  return(sum(rowSums(X*log(q.ij))))
}

#Initialization methods: 

#random initialization
make_cl1 <- function(X,i) {sample(1:i,size=dim(X)[1],replace=TRUE)} 

# cluster on word vector
make_cl2 <- function(X,i) {kmeans(X,i)$cluster} 

# cluster on residuals
make_cl3 <- function(X,covars,cl=NULL,i) {
	fits <- mnlm(cl, covars,X, bins=5, gamma=1, nlambda=10) 
	m <- rowSums(X) 
	resids <- X-m*predict(fits,covars,type="response") #(these are not really good residuals?) 
	return(kmeans(resids,i)$cluster)
} 

md <- function(X,covars,iter_obj) {
	len = length(iter_obj$likes$V1)
	return(multi.devian(X,
				 Y=cbind(covars,model.matrix(~0+factor(iter_obj$h.clusters[,len]))),
				 iter_obj$B))
				 }