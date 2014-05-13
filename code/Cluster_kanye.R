require(SnowballC,tm)
library(textir)
kanye = read.csv("C:/Users/nauner/SkyDrive/4year/Spring/Spanish/KANYE_LYRICS.csv")
kCorp <- Corpus(VectorSource(as.character(kanye[,1])))
pyong <- kanye[,2]
kCorp <- tm_map(kCorp, tolower)
# remove punctuation
kCorp <- tm_map(kCorp, removePunctuation)
# remove numbers
kCorp <- tm_map(kCorp, removeNumbers)
# remove stopwords
# keep "r" by removing it from stopwords
#kCorp <- tm_map(kCorp, removeWords, stopwords('english'))
kCorp <- tm_map(kCorp, stemDocument)
ctrl = list( wordLengths = c(4, 15),
			 bounds = list(global = c(2,Inf))
			 )

##now, we do our thing
dtm <- DocumentTermMatrix(kCorp,control=ctrl)
library(wordcloud)
m <- as.matrix(dtm)
emp <- rowSums(m)
# calculate the frequency of words
m <- m[emp!=0,]
pyong <- pyong[emp!=0]
spm <- Matrix(m,sparse=TRUE)

v <- sort(colSums(m), decreasing=TRUE)
myNames <- dimnames(m)[2]
d <- data.frame(word=names(v), freq=v)
wordcloud(d$word, d$freq, min.freq=3)


setwd("C:/Users/nauner/tech/hidden_structure/code")
source('Iterate.R')

#spm is our sparse x:doc y: term matrix. That's the way it's supposed to be. 
#pyong is our covars matrix

dimnames(spm) = dimnames(m)

naive_kanye <- mnlm(NULL,as.matrix(pyong) ,spm, b,ins=5, gamma=1, nlambda=10); 
kanye_cl <- make_cl2(X=spm,i=5)
kanye_clusters <- iter_cluster(as.matrix(log(pyong+1)),kanye_cl,X=spm,nmax=15)



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


dimnames(kanye_clusters$B)[2][[1]] = dimnames(m)[2]$Terms

high<-cbind(high_loadings(kanye_clusters$B,1,terms=50),
high_loadings(kanye_clusters$B,2,terms=50),
high_loadings(kanye_clusters$B,3,terms=50),
high_loadings(kanye_clusters$B,4,terms=50))

show_members = function(fitted_model,clusternumber){
	criteria <- (1:218)[tail(t(fitted_model$h.clusters),n=1)==clusternumber]
	#return(fitted_model$covars[,'gop'][criteria])
	return(criteria)
	}

	
show_members(kanye_clusters,1)
library(xtable)
xtable(high_loadings(kanye_clusters$B,2,terms=14))