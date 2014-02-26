I'm sti

Sorry for the delay. I fit a MN regression for the we8there data (as per the documentation). 
I also rescaled the count data as you directed and ran k means:
```R
normalize<- function(x) {
return(	
	apply(
		t(apply(t(x),2,FUN=function(x) {x/sum(x)})),
		2,FUN=function(x) {x/sd(x)})
	)}
#how to set number of clusters?
km <- kmeans(normalize(we8thereCounts),5)
# takes ~7 seconds.
table(km)
```
How many data points in each cluster (1-5)? 

   1    |2|    3 |   4 |   5 
-------|---|-------|------| -------
  46  |  6| 5981 | 124 |   9 


Most of the data is assigned to one cluster (cluster 3).
This could be a mistake on my end, or a consequence of fitting k-means on very very sparse matrices. 

What shall I do next?