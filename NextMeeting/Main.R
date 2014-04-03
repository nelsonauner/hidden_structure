######################################################################
###         Establish Baseline: "Vanilla" MNIR:                   ####
######################################################################


require(textir);data(we8there);cl <- NULL
fits <- mnlm(cl, we8thereRatings[,'Overall',drop=FALSE],we8thereCounts, bins=5, gamma=1, nlambda=10)
B.naive <- coef(fits)
z <- srproj(B,we8thereCounts)
## fit a fwd model to the factors
summary(fwd <- lm(we8thereRatings$Overall ~ z))
## truncate the fwd predictions to our known range
fwd$fitted[fwd$fitted<1] <- 1
fwd$fitted[fwd$fitted>5] <- 5
## plot the fitted rating by true rating
par(mfrow=c(1,1))
plot(fwd$fitted ~ factor(we8thereRatings$Overall),
     varwidth=TRUE, col="lightslategrey")

sum(round(fwd$fitted) != we8thereRatings$Overall)/length(we8thereRatings$Overall)
mean(abs(round(fwd$fitted)-we8thereRatings$Overall))
#[1] 0.5591956

######################################################################
###         Initialize  Cluster Membership                        ####
######################################################################
#load algorithm
source('Iterate.R')

##############################################################
#Initialize Cluster Membership randomly and assign variables:#
##############################################################
X = we8thereCounts
y = we8thereRatings['Overall']
clusters <- sample(1:4,size=dim(we8thereRatings)[1],replace=TRUE)
n.loop = 15
#################################
#         Run Algorithm         #
#################################
res<-iter_cluster(y,clusters,X,n.loop)

#####################
#forward regression:#
#####################
z2 <- srproj(res$B,we8thereCounts)

summary(fwd2 <- lm(we8thereRatings$Overall ~ z2))
## truncate the fwd predictions to our known range
fwd2$fitted[fwd2$fitted<1] <- 1
fwd2$fitted[fwd2$fitted>5] <- 5
## plot the fitted rating by true rating
par(mfrow=c(1,1))
plot(fwd$fitted ~ factor(we8thereRatings$Overall),
     varwidth=TRUE, col="lightslategrey")

fwd2$fitted <- round(fwd2$fitted)
sum(round(fwd2$fitted) != we8thereRatings$Overall)/length(we8thereRatings$Overall)

fwd2$fitted[fwd2$fitted!=rfwd$fitted]

#visual inspection:
cbind(fwd2$fitted,fwd$fitted,we8thereRatings['Overall'])

round(fwd2$fitted != fwd$fittedres)

#20 simulations:
n.sim = 20
res.sim=list()
names(res.sim) <- paste("sim",1:20,sep="")
for (i in 1:n.sim){
  res.sim = append(res.sim,list(iter_cluster(y,(sample(1:4,size=dim(we8thereRatings)[1],replace=TRUE)),X,n.loop)))
}


res.sim_cl=list()
for (i in 4:n.sim){
  res.sim_cl = append(res.sim_cl,list(iter_cluster(y,sample(1:i,size=dim(we8thereRatings)[1],replace=TRUE),X,n.loop)))
}


#we now have a bunch of simulations....

#Issues: Are clustered predictions any better?  Let's see? 
#Using this general method:
#sum(we8thereCounts[1,]*log(q[1,]))

cl_any_good <- list()
m <- rowSums(we8thereCounts)

pred.cl <- function(y,cl,B,m) {
 res <- exp(B[1,] + as.matrix(y) %*% B[2,] +   model.matrix(formula(~0+factor(cl))) %*% B[3:6,])
 return(res/rowSums(res))
}

for (i in 1:n.sim) {
fitted.sim1 <- m*pred.cl(we8thereRatings$Overall, res.sim[[i]]$clusters[,15],res.sim[[i]]$B)
pred.lik <- abs(we8thereCounts - fitted.sim1)
cl_any_good <- append(cl_any_good,sum(rowSums(pred.lik)))
}

pred.naive <- exp(B.naive[1,] + matrix(we8thereRatings$Overall) %*% B.naive[2,])
pred.naive <- pred.naive/rowSums(pred.naive)
fitted.naive <- m*pred.naive
lik.naive <- abs(we8thereCounts-fitted.naive
