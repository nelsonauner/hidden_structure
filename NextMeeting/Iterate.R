## Iterating Cluster Membership and Parameter Values ##
# borrowing heavily from documentation: http://cran.r-project.org/web/packages/textir/textir.pdf
library(textir);data(we8there);cl <- NULL

#Initialize Cluster Membership randomly
cl_num<- as.factor(floor(runif(n=dim(we8thereRatings)[1],min=0,max=4)))

# Notes: mnlm does not handle factors!
#Turn our factor (membership = 1,2 or 3) into vector (membership = [0 1 0]), etc
cl_matrix <- model.matrix(formula(~0+cl_num))
#Add clustermembership to the 'attributes' matrix so we can regress on it
Y<-Y_orig <- cbind(we8thereRatings[,'Overall',drop=FALSE],cl_matrix)

#Iterating loop. Takes a while
n.loop = 10
for (i in 1:n.loop) {
fits <- mnlm(cl,Y ,we8thereCounts, bins=5, gamma=1, nlambda=10); B <- coef(fits)  #
#Fit the model 
B <- coef(fits) #Pull the coeffecients

#We want to make our "prediction" but are only interested in the effect of cluster membership
#There's no need to multiply out the entire B matrix since the model is linear:
#However, the predict method will not work if we subset the B matrix, so we do it manually
#q <- predict(B[,1:2], Y[,1],type="response")    
#Manual fits: devide into both parts. First, intercept and rating attribute:
#qp <- cbind(1,Y[,1])%*%B[1:2,] #Add 1 for intercept
#These is the part of the prediction from the cluster effect:
#qq <- as.matrix(Y[,2:5]) %*% B[3:6,] #these look mis-aligned because of the intercept term
#THIS IS WHAT WE WANT:
qz <- we8thereCounts%*%t(B[3:6,])
#Select new cluster membership if better. 
n_cl <- as.factor(apply(qz,MARGIN=1,FUN=which.max)) #biased towards 1. Oh well.
n_cl_matrix <- model.matrix(formula(~0+(n_cl))) #and convert to [0 0 1] form. 
#update our Y:
Y[,2:5] <- n_cl_matrixi
}

