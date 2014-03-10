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
  26.50    1.25   28.56 
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
  mean.y   sd.y cos 2 cos 3 cos 4 cos 5 dist 2  dist 3  dist 4 dist 5 Freq
1  4.174 1.1986 1.626 1.912 1.961 1.713  116.7   9.418   5.313  11.64 5203
2  2.000     NA    NA 1.549 1.561 1.582     NA 116.741 116.667 117.31    1
3  3.634 0.9304    NA    NA 1.541 1.597     NA      NA   9.839  14.59  257
4  2.232 1.3819    NA    NA    NA 1.615     NA      NA      NA  12.50  633
5  3.528 1.5920    NA    NA    NA    NA     NA      NA      NA     NA   72
```



Nelson's Notes:
- Next Steps? 
- Balancing this vs. pygamlr. 
