# a 2-dimensional example
x <- rbind(matrix(rnorm(10, mean=4,sd = 0.3), ncol = 2),
           matrix(rnorm(10, mean = 10, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
(cl <- kmeans(x, 2))
plot(x, col = cl$cluster,main="k means on a 2-normal mixture")
points(cl$centers, col = 1:2, pch = 8, cex = 2)


normalize<- function(x) {
return(	
	apply(
		t(apply(t(x),2,FUN=function(x) {x/sum(x)})),
		2,FUN=function(x) {x/sd(x)})
	)}
	
(cl2 <- kmeans(normalize(x), 2))
plot(normalize(x), col = cl2$cluster,main="k means on normalized data")
points(cl2$centers, col = 1:2, pch = 8, cex = 2)

