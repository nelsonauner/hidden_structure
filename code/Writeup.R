##REQUIRED FOR THIS FILE: 
##Results of the algorithm run on congress data, stored as a list of lists in "cong_res", as per Cluster_congress.R

setwd("C:/Users/nauner/tech/hidden_structure/code")
load("cong_res.RData")
source('multiplot.R')
as.numeric.factor <- function(x) {(as.numeric(levels(x))[x]}
##A brief explanation of how the data was obtained: 
##This graph kind of sucks: 
#First graph: illustrate convergence of clusters

c_c <- as.data.frame(cbind(x=rep(1:14,each=10),
					y=rep(1:10,14),
					cluster=c(cong_res[[1]][[1]]$h.clusters[1:10,])))
					
plot(c_c$x,c_c$y,col=c_c$cluster,pch=15)

##Better idea? make a chart of how many are left to be switched




#multinomial deviance



## Find high word ratings	



##Compare how fast things settle: 
table(cong_res[[1]][[1]]$h.clusters[,1])  #vs
table(cong_res[[1]][[1]]$h.clusters[,14]) #no longer evenly distributed

 head(cong_res[[1]][[1]]$B@Dimnames[2][order(cong_res[[1]][[1]]$B@Dimnames[1])])

congress_res_2.10 <-  cong_res[[2]][[2]]  #the results for kmeans initialization, 15 clusters
congress_res_2.15 <- cong_res[[2]][[3]]  #the results for kmeans initialization, 15 clusters
 
#find number of non-zero coeffecients
length(beta[4,][beta[4,]!=0])  #the first cluster
#find highest rated coeffecients for each cluster and puht into a table!


head(beta[6,][order(abs(beta[6,]),decreasing=TRUE)])

high_loadings <- function(beta,cl_range,terms=10,digits=2) {
beta <- as.matrix(beta) #could be something else :)
highloadings <- data.frame(do.call(cbind,lapply(cl_range,
					FUN=function(x){
						res<-head(
						(beta[x,][order(beta[x,],decreasing=TRUE)]),n=terms)
						return(cbind(names(res),round(res,digits=2)))
						}
						)),row.names=NULL)
names(highloadings) = rep(c("term","loading"),length(cl_range))
return(highloadings)
}

(y<-high_loadings(beta,c(6,8)))

high_loadings(cong_res[[2]][[3]]$B,2,terms=15)

##Sweep out statistics from our 15 simulations: 
sink("congress_topic.txt")
cat("Referenced by initialization method, then by # of clusters")
hl_list = list()
num_cl_vec = c(5,10,15,20,25)
for (method in 1:3) {
for (cls in 1:5) {
cat("Initialization Method: ");cat(method);cat(" # of Topics: ");cat(num_cl_vec[cls]);cat("\n")
print(high_loadings(
										cong_res[[method]][[cls]]$B,
										(4:(3+num_cl_vec[cls]))))

	}
	}
sink()



##How does this measure up against just fitting a topic model: 

require(maptpx)
just_tpx <- topics(counts = congress109Counts,K=12)
summary(just_tpx)
#looks good


#Now, step two. I want to look up a words and their loadings in a pure topic model vs in my mixed semantic topic model. 
#word weights in gop

#prepare covars
covars <- data.frame(gop=congress109Ideology$party=="R",
cscore=congress109Ideology$cs1)
covars$cscore <- covars$cscore -
tapply(covars$cscore,covars$gop,mean)[covars$gop+1]
rownames(covars) <- rownames(congress109Ideology)


cong_mnlm_beta <- coef(cong_fits <- fits <- mnlm(cl=NULL,covars,counts=congress109Counts,bins=5, gamma=1, nlambda=10));

xtable(cbind(high_loadings(cong_mnlm_beta,2,terms=10),high_loadings(congress_res_2.15$B,2,terms=10)))

#Republican Term: 
cong_mnlm_beta[,'nation.oil.food']
congress_res_2.10$B[,'nation.oil.food']

#Democrat Term: 
cong_mnlm_beta[,'death.penalty.system']
congress_res_2.10$B[,'death.penalty.system']


 

##who are these groups? function idea: reveal the members of a cluster....cong_
show_members = function(fitted_model,clusternumber){
	criteria <- tail(t(fitted_model$h.clusters),n=1)==clusternumber
	return(fitted_model$covars[,'gop'][criteria])
	}


xtable(as.data.frame(show_members(congress_res_2.10,2))) #for nation.oil.food
xtable(as.data.frame(show_members(congress_res_2.10,4))) #for death.penalty


##we see that the nation.oil.food category includes cluster number 14
#The loadings on cluster #14 are 
xtable(high_loadings(congress_res_2.10$B,2+3,terms=10))

###Finally, we recreate the graphs requested 
##From examining topics, we'll use cong_res_1.10:
congress_res_1.10 <- cong_res[[1]][[2]]

### May 7th: The comparison of different results
# Initialization Method #3, 20 topic, 14th topic: 
congress_res_3.20 <- cong_res[[3]][[4]]
xtable(high_loadings(congress_res_3.20$B,14+3,terms=15))

##Find who the members are:
#xtable(data.frame(show_members(congress_res_3.20,14)))


#Now we want to campare this to the distortion vector on coeffecients. 
matchcoefs <- function(fitted_res,vectorofterms) { fitted_res$B[2,][names(vectorofterms)]}

comparison <- data.frame(high_loadings(congress_res_3.20$B,14+3,terms=15))
comparison$dem_loading <- as.numeric.factor(comparison$loading)
comparison$GOP <- head(sort(congress_res_3.20$B[2,]+congress_res_3.20$B[14+3,],decreasing=TRUE),n=15)

comparison$GOP_term <- names(head(sort(congress_res_3.20$B[2,]+congress_res_3.20$B[14+3,],decreasing=TRUE),n=15))

comparison$GOP_Beta<-congress_res_3.20$B[2,][comparison$GOP_term]

matchcoefs(congress_res_3.20,comparison$term)
comparison$yplot <- seq(from=24,to=5)
comparison$order_gop <- with(comparison,order(GOP))
comparison$order_dem <- with(comparison,order(loading),increasing=TRUE)

#first plot: dems
p_dem <- ggplot(comparison,aes(x=0,y=order_dem)) + geom_text(label=comparison$term,size=3) + ylab("term") + xlab(" (a) Democrat") +theme_bw() + scale_x_continuous(limits=c(-5,5))   #+ theme(axis.text.x = element_blank(),axis.text.y = element_blank()) 
					
p_gop <- ggplot(comparison,aes(x=GOP_Beta,y=order_gop)) + geom_text(label=comparison$GOP_term,size=3)  + ylab("term") + xlab(" (b) Republican ")+theme_bw() + scale_x_continuous(limits=c(-8,12)) #+ theme(axis.text.x = element_blank(),axis.text.y = element_blank()) 

multiplot(p_dem,p_gop,cols=2)

 
### NICE

#Now, plot general performance. Ooofff.
#we have cong_res, so let's import we8there_res
load('we8there_res.RData')
we8there_res <- w8there_res; rm(w8there_res);save.image("we8there_res",file="we8there_res.RData")


#Anyways, first we'll look at performance of congressional data

produce_summary <- function(results_file,FUN,X) {
	clusters <- seq(from=5,to=25,by=5)
	resm <- data.frame(rep(1:3,each=5))
	resm$n_clusters <- rep(clusters,3)
	resm$y <- NA
	for (init in 1:3) {
		for (n.cl in 1:length(clusters)) {
		resm$y[(init-1)*5+n.cl] <- FUN(X,results_file[[init]][[n.cl]])
		}
	}
	return(resm)
	}


produce_summary(cong_res,length)	
##Ok it works. 

#Now, first things first, plot multinomial deviance:
res_to_mdev <- function(X,rel_sim) {
	l = dim(rel_sim$covars)[2]
	covars <- as.matrix(cbind(as.matrix(rel_sim$covars[,1:(l-1)]),model.matrix(~0+as.factor(rel_sim$covars[,l]))))
	return(multi.devian(X,Y=covars,rel_sim$B))
	}


we8there_res_1.10 <- we8there_res[[1]][[1]]

save(resm,file="mdev_we8there.RData")
save(resm2,file="mdev_congress.RData")

r
#####
resm <- as.data.frame(c(rep(1,10),rep(2,10),rep(3,10)))

resm$deviance = 0

resm <- res_to_mdev(we8thereCounts, we8there_res_1.10)
resm <- produce_summary(w8there_res,res_to_mdev,we8thereCounts)
resm2 <- produce_summary(cong_res,res_to_mdev,congress109Counts)

load('mdev_congress.RData')
load('mdev_we8there.RData')

resm$dev <- -2*resm$y
resm2$dev <- -2*resm2$y
names(resm)[1] <- "init"
names(resm2)[1] <- "init"
### We want to create plots of performance for these two data sets.


#first plot: dems
p_cong <- ggplot(resm,aes(x=n_clusters,y=dev,label=init,size=4)) +  geom_text() +
#scale_shape_discrete(name="Initialization Method",breaks=c("1","2","3"),labels=c("Random","K Means on Text", "K Means on Residuals"))  +
theme_bw() + scale_colour_grey() + xlab("Number of Clusters") + ylab("Multinomial Deviance") + ggtitle("Congress Data: Multinomial Deviance") + theme(legend.position="none")
p_cong


#first plot: dems
p_eat <- ggplot(resm2,aes(x=n_clusters,y=dev,label=init,size=4)) +geom_text() +
#scale_shape_discrete(name="Initialization Method",breaks=c("1","2","3"),labels=c("Random","K Means on Text", "K Means on Residuals"))  + 
theme_bw() + scale_colour_grey() + xlab("Number of Clusters") + ylab("Multinomial Deviance") + ggtitle("We8there Data: Multinomial Deviance") + theme(legend.position="none")
p_eat



g_legend<-function(a.gplot){
tmp <- ggplot_gtable(ggplot_build(a.gplot))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]
return(legend)}

legend <- g_legend(p_cong)
lwidth <- sum(legend$width)

p3 <- grid.arrange(arrangeGrob(p_cong + theme(legend.position="none"),
                         p_eat + theme(legend.position="none"),
						 legend,
                         nrow=1))
multiplot(p_cong,p_eat,cols=2)


#### Now, we want to look at 