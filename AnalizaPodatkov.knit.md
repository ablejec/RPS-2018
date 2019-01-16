---
title: "Analiza podatkov"
author: "A Blejec"
date: "23 oktober 2018"
output: html_document
---



## Branje podatkov

Pot do datoteke


```r
fpath <- "http://bit.ly/16oBVpR"
```


```r
data <- read.table(file = fpath
                   , sep="\t"
                   , header=TRUE
                   )
names(data)
```

```
##  [1] "starost" "mesec"   "spol"    "masa"    "visina"  "roke"    "cevelj" 
##  [8] "lasje"   "oci"     "mati"    "oce"     "majica"
```

Izpis začetka tabele

```r
head(data)
```

```
##   starost mesec spol masa visina roke cevelj lasje oci mati oce majica
## 1      59     7    M   91    178  189     44     T   S  155 180      L
## 2      21     1    F   60    173  176     43     T   T  162 184      S
## 3      21     7    F   55    178  178     39     T   T  170 180      S
## 4      21     8    F   70    167  165     39     S   T  160 190      S
## 5      21     4    F   65    171  168     40     T   S  169 176      M
## 6      21     3    M   88    171  173     41     T   T  165 182     XL
```

```r
n <- dim(data)[1]
n
```

```
## [1] 43
```

```r
nrow(data)
```

```
## [1] 43
```

```r
ncol(data)
```

```
## [1] 12
```
Izpisovanje vrednosti

```r
cat(" Število enot:",n,"\n"
    , "Število spremenljivk:", ncol(data))
```

```
##  Število enot: 43 
##  Število spremenljivk: 12
```

V markdown lahko v besedilo vključim R vrednsti

Število enot je 43, 
število spremenljiv pa 12.
Dimenzija pa je 43, 12.

Izpišimo zadnjih 6 vrstic

```r
n <- nrow(data)
data[ (n-(6-1)):n    ,  ]
```

```
##    starost mesec spol masa visina roke cevelj lasje oci mati oce majica
## 38      22    10    M   73    173  180     42     T   S  168 175      M
## 39      21    10    F   58    170  171     48     S   S  177 180      M
## 40      22     7    F   56    158  156     37     T   T  165 179      M
## 41      21     4    F   55    157   NA     37     T   T  157 178      S
## 42      22    10    F   50    160  160     37     S   T  164 174      S
## 43      22     0    M   73    181  187     43     T   T  167 175      M
```

Naredimo funkcijo


```r
my.tail <- function( x, m = 6){
  n <- nrow(x)
  print(x[ (n-(m-1)):n , ])
}
# preizkus
my.tail(data)
```

```
##    starost mesec spol masa visina roke cevelj lasje oci mati oce majica
## 38      22    10    M   73    173  180     42     T   S  168 175      M
## 39      21    10    F   58    170  171     48     S   S  177 180      M
## 40      22     7    F   56    158  156     37     T   T  165 179      M
## 41      21     4    F   55    157   NA     37     T   T  157 178      S
## 42      22    10    F   50    160  160     37     S   T  164 174      S
## 43      22     0    M   73    181  187     43     T   T  167 175      M
```

```r
# 3 vrstice
my.tail(data, 3)
```

```
##    starost mesec spol masa visina roke cevelj lasje oci mati oce majica
## 41      21     4    F   55    157   NA     37     T   T  157 178      S
## 42      22    10    F   50    160  160     37     S   T  164 174      S
## 43      22     0    M   73    181  187     43     T   T  167 175      M
```

```r
my.tail(data, 1)
```

```
##    starost mesec spol masa visina roke cevelj lasje oci mati oce majica
## 43      22     0    M   73    181  187     43     T   T  167 175      M
```

```r
my.tail(data,0)  # ne dela !!!
```

```
##    starost mesec spol masa visina roke cevelj lasje  oci mati oce majica
## NA      NA    NA <NA>   NA     NA   NA     NA  <NA> <NA>   NA  NA   <NA>
## 43      22     0    M   73    181  187     43     T    T  167 175      M
```


Struktura podatkovne tabele (data frame)


```r
str(data)
```

```
## 'data.frame':	43 obs. of  12 variables:
##  $ starost: int  59 21 21 21 21 21 21 20 22 23 ...
##  $ mesec  : int  7 1 7 8 4 3 7 11 6 10 ...
##  $ spol   : Factor w/ 2 levels "F","M": 2 1 1 1 1 2 1 1 1 1 ...
##  $ masa   : int  91 60 55 70 65 88 52 53 62 59 ...
##  $ visina : int  178 173 178 167 171 171 162 161 168 169 ...
##  $ roke   : num  189 176 178 165 168 173 164 160 164 168 ...
##  $ cevelj : int  44 43 39 39 40 41 39 38 41 38 ...
##  $ lasje  : Factor w/ 2 levels "S","T": 2 2 2 1 2 2 2 2 2 1 ...
##  $ oci    : Factor w/ 2 levels "S","T": 1 2 2 2 1 2 2 2 1 1 ...
##  $ mati   : int  155 162 170 160 169 165 160 158 170 178 ...
##  $ oce    : int  180 184 180 190 176 182 170 180 185 180 ...
##  $ majica : Factor w/ 5 levels "L","M","S","XL",..: 1 3 3 3 2 4 3 3 2 2 ...
```
Povzetek

```r
summary(data[, 1:6])
```

```
##     starost          mesec        spol        masa           visina     
##  Min.   :20.00   Min.   : 0.000   F:33   Min.   :50.00   Min.   :156.0  
##  1st Qu.:21.00   1st Qu.: 5.000   M:10   1st Qu.:55.50   1st Qu.:164.0  
##  Median :21.00   Median : 7.000          Median :61.00   Median :170.0  
##  Mean   :22.07   Mean   : 6.814          Mean   :63.42   Mean   :169.9  
##  3rd Qu.:22.00   3rd Qu.: 9.500          3rd Qu.:70.00   3rd Qu.:173.5  
##  Max.   :59.00   Max.   :11.000          Max.   :91.00   Max.   :189.0  
##                                                                         
##       roke      
##  Min.   :154.0  
##  1st Qu.:163.2  
##  Median :167.8  
##  Mean   :169.3  
##  3rd Qu.:172.5  
##  Max.   :193.0  
##  NA's   :5
```

Filtriranje


```r
filter <- data$starost < 30
filter
```

```
##  [1] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
## [12]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
## [23]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
## [34]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
```

```r
which(!filter)
```

```
## [1] 1
```

```r
head(data[filter,])
```

```
##   starost mesec spol masa visina roke cevelj lasje oci mati oce majica
## 2      21     1    F   60    173  176     43     T   T  162 184      S
## 3      21     7    F   55    178  178     39     T   T  170 180      S
## 4      21     8    F   70    167  165     39     S   T  160 190      S
## 5      21     4    F   65    171  168     40     T   S  169 176      M
## 6      21     3    M   88    171  173     41     T   T  165 182     XL
## 7      21     7    F   52    162  164     39     T   T  160 170      S
```

```r
data <- data[filter, ]
```

Spreminjanje vrednosti v vektorju


```r
data$mesec
```

```
##  [1]  1  7  8  4  3  7 11  6 10  9  7  2  6  2 10  6  8 11  5  4  9 11  8
## [24]  5  3  5 11  7  2  7  8  5  7  8 11 11 10 10  7  4 10  0
```

```r
x <- data$mesec
x
```

```
##  [1]  1  7  8  4  3  7 11  6 10  9  7  2  6  2 10  6  8 11  5  4  9 11  8
## [24]  5  3  5 11  7  2  7  8  5  7  8 11 11 10 10  7  4 10  0
```

Na katerih mestih ima x vredost 3?


```r
filter <- x == 3
filter
```

```
##  [1] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
## [12] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [23] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [34] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
```

```r
head(filter)
```

```
## [1] FALSE FALSE FALSE FALSE  TRUE FALSE
```

```r
which(filter)
```

```
## [1]  5 25
```

```r
x[filter]
```

```
## [1] 3 3
```

```r
x[which(filter)]
```

```
## [1] 3 3
```

```r
data[filter,]
```

```
##    starost mesec spol masa visina roke cevelj lasje oci mati oce majica
## 6       21     3    M   88    171  173     41     T   T  165 182     XL
## 26      21     3    F   59    164  165     36     S   S  165 172      M
```
Popravek podatka 

```r
(filter <- x==0)
```

```
##  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [12] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [23] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [34] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
```

```r
x[filter] <- NA
x
```

```
##  [1]  1  7  8  4  3  7 11  6 10  9  7  2  6  2 10  6  8 11  5  4  9 11  8
## [24]  5  3  5 11  7  2  7  8  5  7  8 11 11 10 10  7  4 10 NA
```
Prazen prostor se pri branju spremeni v NA


```r
data$mati
```

```
##  [1] 162 170 160 169 165 160 158 170 178  NA 160 158 165 160  NA  NA 165
## [18] 168 164 172 168 180 174 160 165 162 168 157  NA  NA 168 157 165 174
## [35] 166 163 168 177 165 157 164 167
```

```r
is.na(data$mati)
```

```
##  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
## [12] FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
## [23] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE
## [34] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
```

Funkcije `as.*`


```r
is.numeric(x)
```

```
## [1] TRUE
```

```r
y <- as.factor(x)
sum(x,na.rm=TRUE)
```

```
## [1] 286
```

```r
as.character(x)
```

```
##  [1] "1"  "7"  "8"  "4"  "3"  "7"  "11" "6"  "10" "9"  "7"  "2"  "6"  "2" 
## [15] "10" "6"  "8"  "11" "5"  "4"  "9"  "11" "8"  "5"  "3"  "5"  "11" "7" 
## [29] "2"  "7"  "8"  "5"  "7"  "8"  "11" "11" "10" "10" "7"  "4"  "10" NA
```

Vrnemo popravljen vektor v data


```r
data$mesec <- y # da bo faktor
```


```r
summary(data[, 1:6])
```

```
##     starost          mesec    spol        masa           visina     
##  Min.   :20.00   7      : 7   F:33   Min.   :50.00   Min.   :156.0  
##  1st Qu.:21.00   11     : 6   M: 9   1st Qu.:55.25   1st Qu.:164.0  
##  Median :21.00   8      : 5          Median :60.50   Median :169.5  
##  Mean   :21.19   10     : 5          Mean   :62.76   Mean   :169.7  
##  3rd Qu.:22.00   5      : 4          3rd Qu.:69.00   3rd Qu.:173.0  
##  Max.   :23.00   (Other):14          Max.   :90.00   Max.   :189.0  
##                  NA's   : 1                                         
##       roke      
##  Min.   :154.0  
##  1st Qu.:163.0  
##  Median :167.5  
##  Mean   :168.8  
##  3rd Qu.:171.0  
##  Max.   :193.0  
##  NA's   :5
```

Popravljanje po elementih


```r
u <- x
u[u==7]<- 999
u
```

```
##  [1]   1 999   8   4   3 999  11   6  10   9 999   2   6   2  10   6   8
## [18]  11   5   4   9  11   8   5   3   5  11 999   2 999   8   5 999   8
## [35]  11  11  10  10 999   4  10  NA
```


```r
u <- x
n <- length(u)
for ( i in 1:n){
  if( !is.na(u[i])&&u[i]==7) u[i] <- 999
}
u
```

```
##  [1]   1 999   8   4   3 999  11   6  10   9 999   2   6   2  10   6   8
## [18]  11   5   4   9  11   8   5   3   5  11 999   2 999   8   5 999   8
## [35]  11  11  10  10 999   4  10  NA
```

## Uredimo dostop do spremenljivk v data.frame


```r
search()
```

```
## [1] ".GlobalEnv"        "package:stats"     "package:graphics" 
## [4] "package:grDevices" "package:utils"     "package:datasets" 
## [7] "package:methods"   "Autoloads"         "package:base"
```

Pripnimo data.frame `data` v ikalni seznam


```r
attach(data)
search()
```

```
##  [1] ".GlobalEnv"        "data"              "package:stats"    
##  [4] "package:graphics"  "package:grDevices" "package:utils"    
##  [7] "package:datasets"  "package:methods"   "Autoloads"        
## [10] "package:base"
```


```r
mesec
```

```
##  [1] 1    7    8    4    3    7    11   6    10   9    7    2    6    2   
## [15] 10   6    8    11   5    4    9    11   8    5    3    5    11   7   
## [29] 2    7    8    5    7    8    11   11   10   10   7    4    10   <NA>
## Levels: 1 2 3 4 5 6 7 8 9 10 11
```

Dodatek spremenljivke v data.frame innzdružitev dveh podatkovnih tabel.



```r
x <- data[1:5,1:6]
x
```

```
##   starost mesec spol masa visina roke
## 2      21     1    F   60    173  176
## 3      21     7    F   55    178  178
## 4      21     8    F   70    167  165
## 5      21     4    F   65    171  168
## 6      21     3    M   88    171  173
```

```r
data.frame(x, leto=2017)
```

```
##   starost mesec spol masa visina roke leto
## 2      21     1    F   60    173  176 2017
## 3      21     7    F   55    178  178 2017
## 4      21     8    F   70    167  165 2017
## 5      21     4    F   65    171  168 2017
## 6      21     3    M   88    171  173 2017
```

```r
y <- data[10:15,1:6]
xx <- data.frame(x,leto=2017)
xx
```

```
##   starost mesec spol masa visina roke leto
## 2      21     1    F   60    173  176 2017
## 3      21     7    F   55    178  178 2017
## 4      21     8    F   70    167  165 2017
## 5      21     4    F   65    171  168 2017
## 6      21     3    M   88    171  173 2017
```

```r
yy <- data.frame(y,leto=2016)
yy
```

```
##    starost mesec spol masa visina  roke leto
## 11      21     9    F   65    170 169.0 2016
## 12      21     7    F   55    164 167.5 2016
## 13      20     2    F   51    156 154.0 2016
## 14      23     6    F   56    170 170.0 2016
## 15      21     2    F   56    167 166.0 2016
## 16      21    10    M   75    185 193.0 2016
```

```r
rbind(xx,yy)
```

```
##    starost mesec spol masa visina  roke leto
## 2       21     1    F   60    173 176.0 2017
## 3       21     7    F   55    178 178.0 2017
## 4       21     8    F   70    167 165.0 2017
## 5       21     4    F   65    171 168.0 2017
## 6       21     3    M   88    171 173.0 2017
## 11      21     9    F   65    170 169.0 2016
## 12      21     7    F   55    164 167.5 2016
## 13      20     2    F   51    156 154.0 2016
## 14      23     6    F   56    170 170.0 2016
## 15      21     2    F   56    167 166.0 2016
## 16      21    10    M   75    185 193.0 2016
```

## Malo risanja

Tabela "sedenja"


```r
x <- c(3,6,3,1)
x
```

```
## [1] 3 6 3 1
```

```r
tbl <- matrix(x
              , nrow=2
              , ncol=2
              , dimnames=list(
                c("M","F"),
                c("1.","2.")
              )
              )
tbl
```

```
##   1. 2.
## M  3  3
## F  6  1
```

Narišimo


```r
plot(tbl)
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-24-1.png" width="672" />
To ni tisto, kar pričakujemo.


```r
barplot(tbl)
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-25-1.png" width="672" />

```r
barplot(tbl,beside = TRUE)
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-25-2.png" width="672" />


```r
mosaicplot(tbl)
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-26-1.png" width="672" />

```r
mosaicplot(t(tbl))
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-26-2.png" width="672" />

## Opisne spremenljivke


```r
names(data)
```

```
##  [1] "starost" "mesec"   "spol"    "masa"    "visina"  "roke"    "cevelj" 
##  [8] "lasje"   "oci"     "mati"    "oce"     "majica"
```
Spol. Pripraviti moramo tabelo frekvenc/POGSTOSTI.

```r
tbl <- table(spol)
barplot(tbl)
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-28-1.png" width="672" />


```r
tbl <- table(lasje,oci)
tbl
```

```
##      oci
## lasje  S  T
##     S 16  3
##     T  7 16
```

```r
plot(tbl,col=c("lightblue","brown"))
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-29-1.png" width="672" />


```r
barplot(tbl,xlab="Barva oči"
        , legend=c("Svetli lasje","Temni lasje")
        , col=c("gold","brown"))
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-30-1.png" width="672" />
Legendo premaknite navzgor.

## Testiranje odvisnosti


```r
chisq.test(tbl)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  tbl
## X-squared = 10.072, df = 1, p-value = 0.001505
```


```r
hi2 <- chisq.test(lasje,oci)
hi2
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  lasje and oci
## X-squared = 10.072, df = 1, p-value = 0.001505
```
Struktura rezultata


```r
str(hi2)
```

```
## List of 9
##  $ statistic: Named num 10.1
##   ..- attr(*, "names")= chr "X-squared"
##  $ parameter: Named int 1
##   ..- attr(*, "names")= chr "df"
##  $ p.value  : num 0.00151
##  $ method   : chr "Pearson's Chi-squared test with Yates' continuity correction"
##  $ data.name: chr "lasje and oci"
##  $ observed : 'table' int [1:2, 1:2] 16 7 3 16
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ lasje: chr [1:2] "S" "T"
##   .. ..$ oci  : chr [1:2] "S" "T"
##  $ expected : num [1:2, 1:2] 10.4 12.6 8.6 10.4
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ lasje: chr [1:2] "S" "T"
##   .. ..$ oci  : chr [1:2] "S" "T"
##  $ residuals: table [1:2, 1:2] 1.73 -1.58 -1.91 1.73
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ lasje: chr [1:2] "S" "T"
##   .. ..$ oci  : chr [1:2] "S" "T"
##  $ stdres   : table [1:2, 1:2] 3.49 -3.49 -3.49 3.49
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ lasje: chr [1:2] "S" "T"
##   .. ..$ oci  : chr [1:2] "S" "T"
##  - attr(*, "class")= chr "htest"
```
p vrednost


```r
hi2$p.value
```

```
## [1] 0.001505429
```

```r
cat("P vrednost=", hi2$p.value)
```

```
## P vrednost= 0.001505429
```

```r
hi2$p.value < 0.05
```

```
## [1] TRUE
```

```r
if(hi2$p.value < 0.05) cat("Spremenljivki ",
                          hi2$data.name, 
                          "sta povezani",
                          "(p =", round(hi2$p.value,3),
                          ")") else cat("nista povezani")
```

```
## Spremenljivki  lasje and oci sta povezani (p = 0.002 )
```

Uporabili smo $\chi^2$ test.

$$\sum_{i=1}^n \frac{(o_i-e_i)^2}{e_i} \sim \chi^2((c-1)\times(r-1) )$$

## Vizualizacija številskih spremenljivk

Povezava višine in mase


```r
plot(visina, masa)
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-35-1.png" width="672" />

```r
plot(visina, masa, col = spol, pch=16,cex=1.5)
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-35-2.png" width="672" />

Ali na visino in maso vpliva barva las?
ALi pa barva oci?


```r
plot(visina, masa, col = lasje, pch=16,cex=1.5,
     main="lasje")
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-36-1.png" width="672" />

```r
plot(visina, masa, col = oci, pch=16,cex=1.5)
title("Barvo določa: oci")
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-36-2.png" width="672" />

V dveh panelih


```r
par(mfrow=c(2,1),mar=c(4,4,2,1))
plot(visina, masa, col = lasje, pch=16,cex=1.5,
     main="lasje")
plot(visina, masa, col = oci, pch=16,cex=1.5)
title("Barvo določa: oci")
```

<img src="AnalizaPodatkov_files/figure-html/unnamed-chunk-37-1.png" width="672" />




