Thursday Feb 27th Textir & K means - Nelson Auner
========================================================


```r
## textir & k-means ## borrowing heavily from documentation:
## http://cran.r-project.org/web/packages/textir/textir.pdf
library(textir)
data(we8there)
cl <- NULL
fits <- mnlm(cl, we8thereRatings[, "Overall", drop = FALSE], we8thereCounts, 
    bins = 5, gamma = 1, nlambda = 10)
B <- coef(fits)
# do MNIR projection onto factors
z <- srproj(B, we8thereCounts)
## fit a fwd model to the factors
fwd <- lm(we8thereRatings$Overall ~ z)
head(z)
```

```
   Overall  m
1   0.4801  8
2  -0.2714  2
5   0.8178  2
11  0.3721 11
12  0.3728  8
13  0.5097  8
```


Understanding m: total number of tracked utterances. Linear effect?


```r
#### k-means ##

# quickly scale data
normalize <- function(x) {
    return(apply(t(apply(t(x), 2, FUN = function(x) {
        x/sum(x)
    })), 2, FUN = function(x) {
        x/sd(x)
    }))
}
# how to select # of clusters? I know to be five...
ptm <- proc.time()
km5 <- kmeans(normalize(we8thereCounts), 5)
proc.time() - ptm
```

```
   user  system elapsed 
  33.63    0.72   38.02 
```


Some more investigative work on how the centers of the means are, since we can't see them:


```r
hdkmtable <- function(kmobj, y) {
    n_y <- length(yu <- as.integer(levels(factor(y))))
    n_km <- length(kmobj$size)
    res <- data.frame(matrix(rep(NA, n_y * (2 * n_y)), nrow = n_y))
    names(res) <- c("mean.y", "sd.y", paste("cos", 2:n_km), paste("dist", 2:n_km))
    for (i in 1:n_km) {
        temp.y <- y[kmobj$cluster == i]
        res[i, 1:2] <- c(mean(temp.y), sd(temp.y))
        if (i == n_km) {
            (break)()
        }
        for (j in (i + 1):n_km) {
            res[i, (1 + j)] <- acos(cor(kmobj$centers[i, ], kmobj$centers[j, 
                ]))
            res[i, (j + n_km)] <- sqrt(sum((kmobj$centers[i, ] - kmobj$centers[j, 
                ])^2))
        }
    }
    res$Freq = table(kmobj$cluster)
    return(res)
}

(table5 <- hdkmtable(km5, we8thereRatings$Overall))
```

```
  mean.y   sd.y cos 2 cos 3 cos 4 cos 5 dist 2 dist 3 dist 4 dist 5 Freq
1  4.186 1.1814 1.604 1.574 1.595 1.693  16.82  16.19  22.18 13.399   59
2  3.268 1.6678    NA 1.588 1.604 1.737     NA  13.74  20.42 10.216   97
3  3.641 0.8777    NA    NA 1.542 1.897     NA     NA  19.55  9.792  231
4  3.562 1.4354    NA    NA    NA 1.710     NA     NA     NA 17.692   32
5  3.968 1.3574    NA    NA    NA    NA     NA     NA     NA     NA 5747
```



Nelson's Notes:
- Next Steps? 
- Balancing this vs. pygamlr. 
