Results of summaryRprof on the Iteration algorithm

```r
summaryRprof("clusterinit.out")
```

```
Warning: incomplete final line found on 'clusterinit.out'
```

```
$by.self
                        self.time self.pct total.time total.pct
"exp"                      240.12    46.86     240.18     46.87
".C"                        68.70    13.41      68.70     13.41
"aperm.default"             42.80     8.35      42.80      8.35
"array"                     17.38     3.39      17.38      3.39
"<Anonymous>"               10.28     2.01     471.90     92.10
"sweep"                      9.10     1.78      69.68     13.60
"loadMethod"                 8.52     1.66      12.90      2.52
"match"                      7.40     1.44      19.46      3.80
"subCsp_cols"                6.16     1.20      40.02      7.81
".identC"                    5.34     1.04       5.34      1.04
"ls"                         4.82     0.94      11.38      2.22
"assign"                     4.38     0.85       4.38      0.85
"standardGeneric"            4.22     0.82     349.94     68.29
"ifelse"                     3.84     0.75       4.34      0.85
"possibleExtends"            3.52     0.69       7.92      1.55
".getClassFromCache"         3.46     0.68       3.46      0.68
"stopifnot"                  2.76     0.54       5.68      1.11
"asMethod"                   2.70     0.53      14.16      2.76
"exists"                     2.38     0.46       2.46      0.48
"get"                        2.30     0.45       2.36      0.46
"doTryCatch"                 2.28     0.44       2.74      0.53
"head"                       2.16     0.42      10.00      1.95
"FUN"                        2.14     0.42     150.70     29.41
"is"                         2.12     0.41      17.32      3.38
"as"                         1.76     0.34      41.38      8.08
"tryCatch"                   1.72     0.34       6.20      1.21
"paste"                      1.72     0.34       1.74      0.34
".getGeneric"                1.62     0.32       5.98      1.17
"match.call"                 1.62     0.32       2.76      0.54
".requirePackage"            1.40     0.27      16.58      3.24
"getClassDef"                1.34     0.26       3.62      0.71
"mapply"                     1.32     0.26      38.04      7.42
".cacheGenericTable"         1.30     0.25       2.00      0.39
"intI"                       1.28     0.25       9.58      1.87
".classEnv"                  1.24     0.24      19.04      3.72
"unlist"                     1.22     0.24       2.72      0.53
"slot<-"                     1.16     0.23       4.16      0.81
"["                          1.06     0.21      47.22      9.22
"%*%"                        1.04     0.20       1.04      0.20
"isN0"                       0.98     0.19       0.98      0.19
"$"                          0.94     0.18       0.94      0.18
"sys.call"                   0.92     0.18       1.16      0.23
"packageSlot"                0.90     0.18       0.90      0.18
"Matrix"                     0.86     0.17      22.70      4.43
"tryCatchList"               0.86     0.17       4.02      0.78
"%in%"                       0.84     0.16      15.04      2.94
"head.default"               0.80     0.16       2.76      0.54
"checkSlotAssignment"        0.76     0.15       3.00      0.59
"[["                         0.76     0.15       0.82      0.16
"lapply"                     0.72     0.14     150.22     29.32
"factor"                     0.72     0.14       1.02      0.20
"getGeneric"                 0.70     0.14       6.68      1.30
"as.name"                    0.68     0.13       0.68      0.13
"callGeneric"                0.64     0.12      33.98      6.63
".findMethodInTable"         0.64     0.12      11.06      2.16
"getNamespace"               0.60     0.12       1.18      0.23
"is.null.DN"                 0.60     0.12       0.70      0.14
"paste0"                     0.60     0.12       0.64      0.12
"loadedNamespaces"           0.58     0.11      11.96      2.33
"drop"                       0.54     0.11      25.62      5.00
"getClass"                   0.54     0.11       1.72      0.34
"environment"                0.54     0.11       0.54      0.11
"dim"                        0.52     0.10       2.40      0.47
"parent.frame"               0.52     0.10       0.52      0.10
"getOption"                  0.50     0.10       0.80      0.16
"el"                         0.50     0.10       0.50      0.10
"eval"                       0.48     0.09     512.38    100.00
".quickCoerceSelect"         0.46     0.09      11.52      2.25
"apply"                      0.46     0.09       2.46      0.48
"t"                          0.44     0.09     308.82     60.27
"tryCatchOne"                0.42     0.08       3.16      0.62
"match.arg"                  0.42     0.08       1.64      0.32
".getMethodsTable"           0.42     0.08       1.46      0.28
"elNamed"                    0.42     0.08       1.40      0.27
".class1"                    0.40     0.08       0.42      0.08
"@<-"                        0.38     0.07       4.32      0.84
"sys.parent"                 0.38     0.07       0.38      0.07
"dmr"                        0.36     0.07     148.18     28.92
"dimnames"                   0.36     0.07       1.68      0.33
"try"                        0.34     0.07       6.54      1.28
".local"                     0.34     0.07       4.72      0.92
"isSymmetric"                0.34     0.07       1.44      0.28
"c"                          0.34     0.07       0.34      0.07
"mnlm"                       0.30     0.06     148.48     28.98
"options"                    0.30     0.06       0.30      0.06
"do.call"                    0.28     0.05     128.78     25.13
"colnames"                   0.24     0.05       2.16      0.42
"/"                          0.24     0.05       0.38      0.07
".deparseOpts"               0.24     0.05       0.32      0.06
"is.data.frame"              0.24     0.05       0.24      0.05
".cacheGeneric"              0.22     0.04       2.22      0.43
"isTriangular"               0.22     0.04       0.74      0.14
"t.default"                  0.22     0.04       0.22      0.04
"Matrix.msg"                 0.20     0.04       1.00      0.20
".sigLabel"                  0.18     0.04       1.88      0.37
"=="                         0.18     0.04       0.18      0.04
"isSymmetric.matrix"         0.18     0.04       0.18      0.04
"list"                       0.18     0.04       0.18      0.04
"pmatch"                     0.18     0.04       0.18      0.04
"unique.default"             0.16     0.03       0.18      0.04
"double"                     0.16     0.03       0.16      0.03
"inherits"                   0.16     0.03       0.16      0.03
"deparse"                    0.14     0.03       0.70      0.14
"names<-"                    0.14     0.03       0.18      0.04
"*"                          0.14     0.03       0.14      0.03
"min"                        0.14     0.03       0.14      0.03
"+"                          0.12     0.02       0.30      0.06
"formals"                    0.12     0.02       0.20      0.04
"all"                        0.12     0.02       0.12      0.02
"is.primitive"               0.12     0.02       0.12      0.02
"-"                          0.10     0.02       1.56      0.30
"is.factor"                  0.10     0.02       0.52      0.10
"as.environment"             0.10     0.02       0.10      0.02
"character"                  0.10     0.02       0.10      0.02
"length"                     0.10     0.02       0.10      0.02
"as.vector"                  0.08     0.02       1.46      0.28
">"                          0.08     0.02       0.08      0.02
"order"                      0.08     0.02       0.08      0.02
"initialize"                 0.06     0.01       0.34      0.07
"sparseMatrix"               0.06     0.01       0.12      0.02
"sys.function"               0.06     0.01       0.08      0.02
"colnames<-"                 0.06     0.01       0.06      0.01
"mean.default"               0.06     0.01       0.06      0.01
"rownames<-"                 0.06     0.01       0.06      0.01
".Call"                      0.04     0.01     307.16     59.95
"unique"                     0.04     0.01       1.80      0.35
"tapply"                     0.04     0.01       0.64      0.12
"as.character"               0.04     0.01       0.08      0.02
"sys.frame"                  0.04     0.01       0.08      0.02
"mode"                       0.04     0.01       0.06      0.01
"<="                         0.04     0.01       0.04      0.01
"!"                          0.04     0.01       0.04      0.01
"("                          0.04     0.01       0.04      0.01
"as.character.factor"        0.04     0.01       0.04      0.01
"class<-"                    0.04     0.01       0.04      0.01
"dimnames<-"                 0.04     0.01       0.04      0.01
"is.list"                    0.04     0.01       0.04      0.01
"llply"                      0.02     0.00     307.16     59.95
"AIC.default"                0.02     0.00       2.08      0.41
"nrow"                       0.02     0.00       0.68      0.13
"nnzero"                     0.02     0.00       0.52      0.10
"sparseDefault"              0.02     0.00       0.50      0.10
"split.default"              0.02     0.00       0.24      0.05
"[.factor"                   0.02     0.00       0.18      0.04
"as.data.frame"              0.02     0.00       0.12      0.02
"data.frame"                 0.02     0.00       0.08      0.02
"as.data.frame.integer"      0.02     0.00       0.04      0.01
"sort.list"                  0.02     0.00       0.04      0.01
"!="                         0.02     0.00       0.02      0.00
"&"                          0.02     0.00       0.02      0.00
"anyDuplicated.default"      0.02     0.00       0.02      0.00
"as.double"                  0.02     0.00       0.02      0.00
"as.integer"                 0.02     0.00       0.02      0.00
"is.na"                      0.02     0.00       0.02      0.00
"is.null"                    0.02     0.00       0.02      0.00
"lazyLoadDBfetch"            0.02     0.00       0.02      0.00
"load"                       0.02     0.00       0.02      0.00
".set_row_names"             0.02     0.00       0.02      0.00
"slot"                       0.02     0.00       0.02      0.00
"validityMethod"             0.02     0.00       0.02      0.00

$by.total
                        total.time total.pct self.time self.pct
"eval"                      512.38    100.00      0.48     0.09
"source"                    512.38    100.00      0.00     0.00
"withVisible"               512.38    100.00      0.00     0.00
"append"                    500.04     97.59      0.00     0.00
"iter_cluster"              500.04     97.59      0.00     0.00
"<Anonymous>"               471.90     92.10     10.28     2.01
"standardGeneric"           349.94     68.29      4.22     0.82
"t"                         308.82     60.27      0.44     0.09
"aaply"                     307.36     59.98      0.00     0.00
"laply"                     307.34     59.98      0.00     0.00
".Call"                     307.16     59.95      0.04     0.01
"llply"                     307.16     59.95      0.02     0.00
"loop_apply"                307.12     59.94      0.00     0.00
"rowSums"                   307.10     59.93      0.00     0.00
".fun"                      307.06     59.93      0.00     0.00
"exp"                       240.18     46.87    240.12    46.86
"FUN"                       150.70     29.41      2.14     0.42
"lapply"                    150.22     29.32      0.72     0.14
"mnlm"                      148.48     28.98      0.30     0.06
"dmr"                       148.18     28.92      0.36     0.07
"do.call"                   128.78     25.13      0.28     0.05
"sweep"                      69.68     13.60      9.10     1.78
"cust_sweep"                 69.68     13.60      0.00     0.00
".C"                         68.70     13.41     68.70    13.41
"aperm"                      59.70     11.65      0.00     0.00
"["                          47.22      9.22      1.06     0.21
"coef"                       42.88      8.37      0.00     0.00
"coef.dmr"                   42.88      8.37      0.00     0.00
"aperm.default"              42.80      8.35     42.80     8.35
"as"                         41.38      8.08      1.76     0.34
"subCsp_cols"                40.02      7.81      6.16     1.20
"mapply"                     38.04      7.42      1.32     0.26
"callGeneric"                33.98      6.63      0.64     0.12
"drop"                       25.62      5.00      0.54     0.11
"Matrix"                     22.70      4.43      0.86     0.17
"sapply"                     20.60      4.02      0.00     0.00
"match"                      19.46      3.80      7.40     1.44
".classEnv"                  19.04      3.72      1.24     0.24
"array"                      17.38      3.39     17.38     3.39
"is"                         17.32      3.38      2.12     0.41
".requirePackage"            16.58      3.24      1.40     0.27
"%in%"                       15.04      2.94      0.84     0.16
"asMethod"                   14.16      2.76      2.70     0.53
"loadMethod"                 12.90      2.52      8.52     1.66
"loadedNamespaces"           11.96      2.33      0.58     0.11
".quickCoerceSelect"         11.52      2.25      0.46     0.09
"ls"                         11.38      2.22      4.82     0.94
".findMethodInTable"         11.06      2.16      0.64     0.12
"head"                       10.00      1.95      2.16     0.42
"intI"                        9.58      1.87      1.28     0.25
"possibleExtends"             7.92      1.55      3.52     0.69
"getGeneric"                  6.68      1.30      0.70     0.14
"try"                         6.54      1.28      0.34     0.07
"tryCatch"                    6.20      1.21      1.72     0.34
"predict"                     6.04      1.18      0.00     0.00
"predict.dmr"                 6.04      1.18      0.00     0.00
".getGeneric"                 5.98      1.17      1.62     0.32
"stopifnot"                   5.68      1.11      2.76     0.54
".identC"                     5.34      1.04      5.34     1.04
".local"                      4.72      0.92      0.34     0.07
"assign"                      4.38      0.85      4.38     0.85
"ifelse"                      4.34      0.85      3.84     0.75
"@<-"                         4.32      0.84      0.38     0.07
"slot<-"                      4.16      0.81      1.16     0.23
"tryCatchList"                4.02      0.78      0.86     0.17
"getClassDef"                 3.62      0.71      1.34     0.26
".getClassFromCache"          3.46      0.68      3.46     0.68
"tryCatchOne"                 3.16      0.62      0.42     0.08
"checkSlotAssignment"         3.00      0.59      0.76     0.15
"match.call"                  2.76      0.54      1.62     0.32
"head.default"                2.76      0.54      0.80     0.16
"doTryCatch"                  2.74      0.53      2.28     0.44
"unlist"                      2.72      0.53      1.22     0.24
"simplify2array"              2.60      0.51      0.00     0.00
"exists"                      2.46      0.48      2.38     0.46
"apply"                       2.46      0.48      0.46     0.09
"dim"                         2.40      0.47      0.52     0.10
"get"                         2.36      0.46      2.30     0.45
".cacheGeneric"               2.22      0.43      0.22     0.04
"colnames"                    2.16      0.42      0.24     0.05
"AIC.default"                 2.08      0.41      0.02     0.00
"AIC"                         2.08      0.41      0.00     0.00
"ll"                          2.06      0.40      0.00     0.00
"logLik.dmr"                  2.06      0.40      0.00     0.00
".cacheGenericTable"          2.00      0.39      1.30     0.25
".sigLabel"                   1.88      0.37      0.18     0.04
"collapse"                    1.86      0.36      0.00     0.00
"unique"                      1.80      0.35      0.04     0.01
"paste"                       1.74      0.34      1.72     0.34
"getClass"                    1.72      0.34      0.54     0.11
"dimnames"                    1.68      0.33      0.36     0.07
"match.arg"                   1.64      0.32      0.42     0.08
"-"                           1.56      0.30      0.10     0.02
".getMethodsTable"            1.46      0.28      0.42     0.08
"as.vector"                   1.46      0.28      0.08     0.02
"isSymmetric"                 1.44      0.28      0.34     0.07
"elNamed"                     1.40      0.27      0.42     0.08
"getNamespace"                1.18      0.23      0.60     0.12
"sys.call"                    1.16      0.23      0.92     0.18
"%*%"                         1.04      0.20      1.04     0.20
"factor"                      1.02      0.20      0.72     0.14
"Matrix.msg"                  1.00      0.20      0.20     0.04
"isN0"                        0.98      0.19      0.98     0.19
"$"                           0.94      0.18      0.94     0.18
"packageSlot"                 0.90      0.18      0.90     0.18
"[["                          0.82      0.16      0.76     0.15
"getOption"                   0.80      0.16      0.50     0.10
"isTriangular"                0.74      0.14      0.22     0.04
"is.null.DN"                  0.70      0.14      0.60     0.12
"deparse"                     0.70      0.14      0.14     0.03
"as.name"                     0.68      0.13      0.68     0.13
"nrow"                        0.68      0.13      0.02     0.00
"paste0"                      0.64      0.12      0.60     0.12
"tapply"                      0.64      0.12      0.04     0.01
"ncol"                        0.62      0.12      0.00     0.00
"environment"                 0.54      0.11      0.54     0.11
"parent.frame"                0.52      0.10      0.52     0.10
"is.factor"                   0.52      0.10      0.10     0.02
"nnzero"                      0.52      0.10      0.02     0.00
"as.factor"                   0.52      0.10      0.00     0.00
"el"                          0.50      0.10      0.50     0.10
"sparseDefault"               0.50      0.10      0.02     0.00
"nz.NA"                       0.50      0.10      0.00     0.00
".class1"                     0.42      0.08      0.40     0.08
"sys.parent"                  0.38      0.07      0.38     0.07
"/"                           0.38      0.07      0.24     0.05
"c"                           0.34      0.07      0.34     0.07
"initialize"                  0.34      0.07      0.06     0.01
"new"                         0.34      0.07      0.00     0.00
".deparseOpts"                0.32      0.06      0.24     0.05
"options"                     0.30      0.06      0.30     0.06
"+"                           0.30      0.06      0.12     0.02
"as.matrix"                   0.26      0.05      0.00     0.00
"interaction"                 0.26      0.05      0.00     0.00
"is.data.frame"               0.24      0.05      0.24     0.05
"split.default"               0.24      0.05      0.02     0.00
"split"                       0.24      0.05      0.00     0.00
"t.default"                   0.22      0.04      0.22     0.04
"formals"                     0.20      0.04      0.12     0.02
"=="                          0.18      0.04      0.18     0.04
"isSymmetric.matrix"          0.18      0.04      0.18     0.04
"list"                        0.18      0.04      0.18     0.04
"pmatch"                      0.18      0.04      0.18     0.04
"unique.default"              0.18      0.04      0.16     0.03
"names<-"                     0.18      0.04      0.14     0.03
"[.factor"                    0.18      0.04      0.02     0.00
"list_to_array"               0.18      0.04      0.00     0.00
"vapply"                      0.18      0.04      0.00     0.00
"double"                      0.16      0.03      0.16     0.03
"inherits"                    0.16      0.03      0.16     0.03
"*"                           0.14      0.03      0.14     0.03
"min"                         0.14      0.03      0.14     0.03
"tcrossprod"                  0.14      0.03      0.00     0.00
"all"                         0.12      0.02      0.12     0.02
"is.primitive"                0.12      0.02      0.12     0.02
"sparseMatrix"                0.12      0.02      0.06     0.01
"as.data.frame"               0.12      0.02      0.02     0.00
"as.environment"              0.10      0.02      0.10     0.02
"character"                   0.10      0.02      0.10     0.02
"length"                      0.10      0.02      0.10     0.02
">"                           0.08      0.02      0.08     0.02
"order"                       0.08      0.02      0.08     0.02
"sys.function"                0.08      0.02      0.06     0.01
"as.character"                0.08      0.02      0.04     0.01
"sys.frame"                   0.08      0.02      0.04     0.01
"data.frame"                  0.08      0.02      0.02     0.00
"summary"                     0.08      0.02      0.00     0.00
"colnames<-"                  0.06      0.01      0.06     0.01
"mean.default"                0.06      0.01      0.06     0.01
"rownames<-"                  0.06      0.01      0.06     0.01
"mode"                        0.06      0.01      0.04     0.01
"as.data.frame.matrix"        0.06      0.01      0.00     0.00
"[.data.frame"                0.06      0.01      0.00     0.00
"[[.indexed_array"            0.06      0.01      0.00     0.00
"model.frame"                 0.06      0.01      0.00     0.00
"model.frame.default"         0.06      0.01      0.00     0.00
"model.matrix"                0.06      0.01      0.00     0.00
"model.matrix.default"        0.06      0.01      0.00     0.00
"na.omit"                     0.06      0.01      0.00     0.00
"na.omit.data.frame"          0.06      0.01      0.00     0.00
"subCsp_rows"                 0.06      0.01      0.00     0.00
"<="                          0.04      0.01      0.04     0.01
"!"                           0.04      0.01      0.04     0.01
"("                           0.04      0.01      0.04     0.01
"as.character.factor"         0.04      0.01      0.04     0.01
"class<-"                     0.04      0.01      0.04     0.01
"dimnames<-"                  0.04      0.01      0.04     0.01
"is.list"                     0.04      0.01      0.04     0.01
"as.data.frame.integer"       0.04      0.01      0.02     0.00
"sort.list"                   0.04      0.01      0.02     0.00
"as<-"                        0.04      0.01      0.00     0.00
"expand.grid"                 0.04      0.01      0.00     0.00
"reduce_dim"                  0.04      0.01      0.00     0.00
"validObject"                 0.04      0.01      0.00     0.00
"!="                          0.02      0.00      0.02     0.00
"&"                           0.02      0.00      0.02     0.00
"anyDuplicated.default"       0.02      0.00      0.02     0.00
"as.double"                   0.02      0.00      0.02     0.00
"as.integer"                  0.02      0.00      0.02     0.00
"is.na"                       0.02      0.00      0.02     0.00
"is.null"                     0.02      0.00      0.02     0.00
"lazyLoadDBfetch"             0.02      0.00      0.02     0.00
"load"                        0.02      0.00      0.02     0.00
".set_row_names"              0.02      0.00      0.02     0.00
"slot"                        0.02      0.00      0.02     0.00
"validityMethod"              0.02      0.00      0.02     0.00
"anyDuplicated"               0.02      0.00      0.00     0.00
"anyStrings"                  0.02      0.00      0.00     0.00
"as.matrix.Matrix"            0.02      0.00      0.00     0.00
"attach"                      0.02      0.00      0.00     0.00
"attachNamespace"             0.02      0.00      0.00     0.00
"cbind"                       0.02      0.00      0.00     0.00
"checkConflicts"              0.02      0.00      0.00     0.00
"data"                        0.02      0.00      0.00     0.00
"eval                         0.02      0.00      0.00     0.00
"force"                       0.02      0.00      0.00     0.00
"library"                     0.02      0.00      0.00     0.00
"objects"                     0.02      0.00      0.00     0.00
"require"                     0.02      0.00      0.00     0.00
"sort"                        0.02      0.00      0.00     0.00
"splitter_a"                  0.02      0.00      0.00     0.00

$sample.interval
[1] 0.02

$sampling.time
[1] 512.4
```
