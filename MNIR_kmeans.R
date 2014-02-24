#####  Generate sample data ######

#Simulate 150 documents from a mix of two topics
t1prob = c(.2,.1,.4,.05,.25)
t2prob = c(.1,.2,.05,.4,.25)

#Create data frame, label topic mix proportion
C<-as.data.frame(rbind(
		cbind(t(rmultinom(n=50, size=50, t1prob)),1,0),
		cbind(t(rmultinom(n=50, size=50, t2prob)),0,1),
		cbind(t(rmultinom(n=50, size=50, (t2prob+t2prob)/2)),.5,.5)
		))

#### Run a distrom without k means - ########
cl <- makeCluster(2,type=ifelse(.Platform$OS.type=="unix","FORK","PSOCK"))
print(cl)

#fit in parallel 
fits<-dmr(cl,C[,1:5],as.factor(C$V6-C$V7),verb=1) #create a difference variable, otherwise colinear!
stopCluster(cl) #remember to stop your cluster! 
B <- coef(fits) 
log(B@lambda) #This shows the log lambda chosen--we can also see it is a line on the plot method


#### - add in k-means ####

km <- kmeans(C[,1:5],2)
C[,8:9] <- model.matrix(~ as.factor(km$cluster)-1)
cl3 <- makeCluster(3,type=ifelse(.Platform$OS.type=="unix","FORK","PSOCK"))
fits3<-dmr(cl3,C[,c(1:5,8:9)],as.factor(C$V6-C$V7),verb=1)
stopCluster(cl3) #remember to stop your cluster?


##################### up to here ########
par(mfrow=c(1,1))
#now we're going to look at our predictions!
P <- predict(B,fgl[,1:9],type="response") #This should predict probability of that certain characteristic
boxplot(P[cbind(1:214,fgl$type)]~fgl$type,ylab="fitted prob of true class")
#OK, here's what this trickery does:
#P n x p matrix of the predicted probability of observation i = 1,..,n that it is of type j = 1,..,p
#P[cbind(1:214,fgl$type)] returns an n x 1 vector of the probability that i = 1,..,n is l = 1,..,n, where l is what the glass ACTUALLY IS
#so it will be the range of probabilities for the true class. We want it to be high...
#Or at least higher than 1/6 ~ naive guess. 