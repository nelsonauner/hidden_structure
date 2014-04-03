source('Iterate.R')




fits <- mnlm(cl, we8thereRatings, we8thereCounts,nlambda=10)
B <- coef(fits)
q <- predict(B, newdata=we8thereRatings[1:5,'Overall',drop=FALSE],type="response")
sum(we8thereCounts[1,]*log(q[1,]))

######################################################################
###         Establish Baseline: "Vanilla" MNIR:                   ####
######################################################################


library(textir);data(we8there);cl <- NULL
fits <- mnlm(cl, we8thereRatings[,'Overall',drop=FALSE],we8thereCounts, bins=5, gamma=1, nlambda=10)
B <- coef(fits);z <- srproj(B,we8thereCounts)
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


#Initialize Cluster Membership randomly
#################################
X = we8thereCounts
y = we8thereRatings['Overall']
clusters <- sample(1:4,size=dim(we8thereRatings)[1],replace=TRUE)
n.loop = 15
#################################
res<-iter_cluster(y,clusters,X,n.loop)

#What's the error ofa 
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

cbind(fwd2$fitted,fwd$fitted,we8thereRatings['Overall'])

round(fwd2$fitted != fwd$fittedres)

#2: 