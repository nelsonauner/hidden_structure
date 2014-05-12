require(SnowballC)

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
kCorp <- tm_map(kCorp, removeWords, stopwords('english'))
kCorp <- tm_map(kCorp, stemDocument)

ctrl = list( wordLengths = c(4, 15),
			 bounds = list(local = c(2,Inf))
			 )

			 
dtm <- DocumentTermMatrix(kCorp, control = ctrl)

spm <- Matrix(dtm,sparse=TRUE,ncol=dim(dtm[1]))

##now, we do our thing

 library(wordcloud)
m <- as.matrix(dtm)
# calculate the frequency of words
v <- sort(colSums(m), decreasing=TRUE)
myNames <- dimnames(m)[2]
d <- data.frame(word=names(v), freq=v)
wordcloud(d$word, d$freq, min.freq=3)


setwd("C:/Users/nauner/tech/hidden_structure/code")
source('Iterate.R')

#spm is our sparse x:doc y: term matrix. That's the way it's supposed to be. 
#pyong is our covars matrix

dimnames(spm) = dimnames(m)
kanye_clusters <- iter_cluster(pyong,make_cl2(X=spm,i=5),X=spm,nmax=10)


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


dimnames(kanye_clusters$B)[2] = dimnames(m)[2]
high_loadings(kanye_clusters$B,2)