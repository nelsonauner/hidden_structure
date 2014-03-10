## textir & k-means ##

# borrowing heavily from documentation: http://cran.r-project.org/web/packages/textir/textir.pdf


library(textir)
#load data & run MN
data(we8there)
cl <- NULL
fits <- mnlm(cl, we8thereRatings[,'Overall',drop=FALSE],we8thereCounts, bins=5, gamma=1, nlambda=10)
B <- coef(fits)
mean(B[2,]==0) # B[2,] is the coef estimate
# do MNIR projection onto factors
z <- srproj(B,we8thereCounts)
## fit a fwd model to the factors
summary(fwd <- lm(we8thereRatings$Overall ~ z))

#Why do we care about m? 
#truncate the fwd predictions to our known range
fwd$fitted[fwd$fitted<1] <- 1
fwd$fitted[fwd$fitted>5] <- 5
#plot the fitted rating by true rating
par(mfrow=c(1,1))
plot(fwd$fitted ~ factor(we8thereRatings$Overall),varwidth=TRUE, col="lightslategrey")


#### k-means ##

#quickly scale data
normalize<- function(x) {
return(	
	apply(
		t(apply(t(x),2,FUN=function(x) {x/sum(x)})),
		2,FUN=function(x) {x/sd(x)})
	)}

#how to select # of clusters? I know to be five...
(km5 <- kmeans(normalize(we8thereCounts),5))

# takes ~7 seconds.

#What's actually going on? 

tail(km5$centers)

mean(we8thereRatings[,'Overall'][km5$cluster==