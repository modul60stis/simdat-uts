# Test Hipotesis <img src="https://img.shields.io/badge/r-%23276DC3.svg?&style=for-the-badge&logo=r&logoColor=white"/> 

Kolmogorov Test
===============

Misalnya kita ingin menguji data berdistribusi normal standard atau
tidak

-   *H*<sub>0</sub> : Data berdistribusi N(0,1)
-   *H*<sub>1</sub> : Data tidak berdistribusi N(0,1)

### Ketika Datanya N(0,1)


``` r
set.seed(7)
data <- rnorm(100, mean = 0, sd = 1)
ks.test(data, "pnorm", mean = 0, sd = 1)
```

    ## 
    ##  One-sample Kolmogorov-Smirnov test
    ## 
    ## data:  data
    ## D = 0.072791, p-value = 0.6644
    ## alternative hypothesis: two-sided

Terlihat bahwa `p-value` sebesar 0.6644, sehingga gagal tolak
*H*<sub>0</sub>.

### Ketika Datanya Bukan N(0,1)


``` r
set.seed(2)
data <- rnorm(100, mean = 2, sd = sqrt(7))
ks.test(data, "pnorm", mean = 0, sd = 1)
```

    ## 
    ##  One-sample Kolmogorov-Smirnov test
    ## 
    ## data:  data
    ## D = 0.45794, p-value < 2.2e-16
    ## alternative hypothesis: two-sided

Sintax diatas menguji data yang sebenarnya berdistribusi
$N(2, \\sqrt{7})$ akan tetapi diuji dengan *N*(0, 1) dan terlihat bahwa
`p-value` sangat kecil yang berarti tolak *H*<sub>0</sub>.

Hal yang perlu diperhatikan ketika menggunakan fungsi `ks.test` adalah
kita harus menspesifikasi kita ingin menguji data tersebut pada
distribusi apa dan parameternya apa.

``` r
ks.test(data, "pnorm", mean = 2, sd = sqrt(7))
```

    ## 
    ##  One-sample Kolmogorov-Smirnov test
    ## 
    ## data:  data
    ## D = 0.093839, p-value = 0.342
    ## alternative hypothesis: two-sided

Run Test
========

### Get Run


``` r
getRun <- function(x){
      n <- length(x)
      run <- rep("", n - 1)
      for(i in 1:(n-1)){
            run[i] <- ifelse(x[i] < x[i+1], "+", "-")
      }
      run      
}
```

### Get Banyak Run


``` r
getBanyakRun <- function(x){
      run <- getRun(x)
      tanda <- run[1]
      n <- 1
      for(i in 1:length(run)){
            if(tanda != run[i]){
                  n <- n + 1
                  tanda <- run[i]
            }
      }
      n
}
```

### Run Test Biasa


``` r
run.test <- function(x, alpha = 0.05){
      n <- length(x)
      
      mu <- (2*n - 1) / 3
      sigma <- sqrt((16*n - 29) / 90)
      a <- getBanyakRun(x)
      
      z <- (a - mu) / sigma
      p_value <-  2 * pnorm(abs(z), lower.tail = FALSE)
      
       keputusan <- if(p_value < alpha){
            "Tolak H0"
      } else {
            "Gagal Tolak H0"
      }
       
      list(zhitung = z,
           p_value = p_value,
           keputusan = keputusan)
}
```

``` r
x <- c(0.41, 0.68, 0.89, 0.94, 0.74, 0.91, 0.55, 0.62, 0.36, 0.27, 
       0.19, 0.72, 0.75, 0.08, 0.54, 0.02, 0.01, 0.36, 0.16, 0.28, 
       0.18, 0.01, 0.95, 0.69, 0.18, 0.47, 0.23, 0.32, 0.82, 0.53,
       0.31, 0.42, 0.73, 0.04, 0.83, 0.45, 0.13, 0.57, 0.63, 0.29)
run.test(x)
```

    ## $zhitung
    ## [1] -0.1279321
    ## 
    ## $p_value
    ## [1] 0.8982027
    ## 
    ## $keputusan
    ## [1] "Gagal Tolak H0"

### Run Test Above and Below Mean


``` r
run.test.mean <- function(x, alpha = 0.05){
      b <- getBanyakRun(x)
      n1 <- sum(x < mean(x))
      n2 <- sum(x > mean(x))
      n <- length(x)
      
      mu <- (2*n1*n2/n) + (1/2)
      sigma <- sqrt((2*n1*n2) * (2*n1*n2 - n) / (n^2 * (n-1)))
      
      z <- (b - mu) / sigma
      p_value <-  2 * pnorm(abs(z), lower.tail = FALSE)
      
       keputusan <- if(p_value < alpha){
            "Tolak H0"
      } else {
            "Gagal Tolak H0"
      }
       
      list(zhitung = z,
           p_value = p_value,
           keputusan = keputusan)
}
```

``` r
x <- c(0.41, 0.68, 0.89, 0.94, 0.74, 0.91, 0.55, 0.62, 0.36, 0.27, 
       0.19, 0.72, 0.75, 0.08, 0.54, 0.02, 0.01, 0.36, 0.16, 0.28, 
       0.18, 0.01, 0.95, 0.69, 0.18, 0.47, 0.23, 0.32, 0.82, 0.53,
       0.31, 0.42, 0.73, 0.04, 0.83, 0.45, 0.13, 0.57, 0.63, 0.29)
run.test.mean(x)
```

    ## $zhitung
    ## [1] 1.782581
    ## 
    ## $p_value
    ## [1] 0.07465456
    ## 
    ## $keputusan
    ## [1] "Gagal Tolak H0"

Auto Correlation Test
=====================

### Membuat Fungsi

``` r
auto.corr.test <- function(x, i, m, alpha = 0.05){
      N <- length(x)
      M <- floor(((N - i) / m) - 1)
      
      r1 <- c()
      r2 <- c()
      for(k in 0:M){
            r1[k] <- x[i + k*m]
            r2[k] <- x[(k + 1) * m]
      }
      
      rho <- (sum(r1*r2) / (M + 1)) - 0.25
      sigma <- sqrt(13*M + 7) / 12 * (M + 1)
      
      z <- rho / sigma
      
      p_value <-  2 * pnorm(abs(z), lower.tail = FALSE)
      
      keputusan <- if(p_value < alpha){
            "Tolak H0"
      } else {
            "Gagal Tolak H0"
      }
       
      list(zhitung = z,
           p_value = p_value,
           keputusan = keputusan)
}
```

### Mencoba Fungsi
``` r
x <- c(0.41, 0.68, 0.89, 0.94, 0.74, 0.91, 0.55, 0.62, 0.36, 0.27, 
       0.19, 0.72, 0.75, 0.08, 0.54, 0.02, 0.01, 0.36, 0.16, 0.28, 
       0.18, 0.01, 0.95, 0.69, 0.18, 0.47, 0.23, 0.32, 0.82, 0.53,
       0.31, 0.42, 0.73, 0.04, 0.83, 0.45, 0.13, 0.57, 0.63, 0.29)

auto.corr.test(x, 2, 5)
```

    ## $zhitung
    ## [1] -0.0195928
    ## 
    ## $p_value
    ## [1] 0.9843682
    ## 
    ## $keputusan
    ## [1] "Gagal Tolak H0"

**Catatan** : Fungsi diatas masih meragukan, entah bener apa ga

Gap Test
========

### Membuat Fungsi


#### Fungsi f(x) dari gap

``` r
fx <- function(data, x){
      range <- (max(data) - min(data)) + 1
      p <- (range - 1) / range
      p^x * (1 - p)
}
```

#### Membuat Tabel

Tabel yang dibuat mencontoh yang ada di ppt

``` r
table.gap <- function(x){
      panjang <- c()
      n <- length(x)
      
      # Mencari panjang gap
      for (i in 1:n){
            panjang[i] <- 0
            
            j <- i + 1
            while(j <= n){
                  if(x[i] == x[j]) break
                  
                  panjang[i] <- panjang[i] + 1
                  j <- j + 1
            }
            
            if(j > n)
                  panjang[i] <- -1
            
      }
      
      panjang <- panjang[panjang != -1]
      df <- data.frame(table(panjang))
      df$panjang <- as.numeric(as.character(df$panjang))
      
      df$sx <- cumsum(df$Freq) / sum(df$Freq)
      df$fx <- cumsum(sapply(df$panjang, function(p) fx(x, p)))
      df$selisih <- abs(df$sx - df$fx)
      df
}
```

### Mencoba Fungsi


``` r
x <- c(4, 1, 3, 5, 1, 7, 2, 8, 2, 0, 7, 9, 1, 3, 5, 2, 7, 9, 4, 1, 6, 3, 3, 9, 
       6, 3, 4, 8, 2, 3, 1, 9, 4, 4, 6, 8, 4, 1, 3, 8, 9, 5, 5, 7, 3, 9, 5, 9, 
       8, 5, 3, 2, 2, 3, 7, 4, 7, 0, 3, 6, 3, 5, 9, 9, 5, 5, 5, 0, 4, 6, 8, 0, 
       4, 7, 0, 3, 3, 0, 9, 5, 7, 9, 5, 1, 6, 6, 3, 8, 8, 8, 9, 2, 9, 1, 8, 5, 
       4, 4, 5, 0, 2, 3, 9, 7, 1, 2, 0, 3, 6, 3)

gap <- table.gap(x)
knitr::kable(gap)
```

|  panjang|  Freq|    sx|         fx|    selisih|
|--------:|-----:|-----:|----------:|----------:|
|        0|    12|  0.12|  0.1000000|  0.0200000|
|        1|     6|  0.18|  0.1900000|  0.0100000|
|        2|    11|  0.29|  0.2710000|  0.0190000|
|        3|     6|  0.35|  0.3439000|  0.0061000|
|        4|     5|  0.40|  0.4095100|  0.0095100|
|        5|     7|  0.47|  0.4685590|  0.0014410|
|        6|     5|  0.52|  0.5217031|  0.0017031|
|        7|     5|  0.57|  0.5695328|  0.0004672|
|        8|     5|  0.62|  0.6125795|  0.0074205|
|        9|     6|  0.68|  0.6513216|  0.0286784|
|       10|     5|  0.73|  0.6861894|  0.0438106|
|       11|     1|  0.74|  0.7175705|  0.0224295|
|       12|     4|  0.78|  0.7458134|  0.0341866|
|       14|     5|  0.83|  0.7686902|  0.0613098|
|       16|     2|  0.85|  0.7872204|  0.0627796|
|       17|     1|  0.86|  0.8038976|  0.0561024|
|       18|     1|  0.87|  0.8189071|  0.0510929|
|       19|     1|  0.88|  0.8324156|  0.0475844|
|       21|     2|  0.90|  0.8433575|  0.0566425|
|       22|     3|  0.93|  0.8532052|  0.0767948|
|       23|     1|  0.94|  0.8620681|  0.0779319|
|       24|     1|  0.95|  0.8700448|  0.0799552|
|       26|     2|  0.97|  0.8765058|  0.0934942|
|       38|     1|  0.98|  0.8783306|  0.1016694|
|       45|     1|  0.99|  0.8792034|  0.1107966|
|       47|     1|  1.00|  0.8799104|  0.1200896|

Tabel yang dihasilkan masih belum dikelompokkan seperti yang ada di ppt

#### Statistik Uji

``` r
d <- max(gap$selisih)
d
```

    ## [1] 0.1200896

Poker Test
==========

Untuk melakukan uji poker akan digunaan fungsi bawaan dari package
`randtoolbox`, terlalu sulit untuk dibuat fungsi manualnya

``` r
library(rngWELL)
library(randtoolbox)

data <- runif(20, 0, 1)
poker.test(data)
```

    ## 
    ##           Poker test
    ## 
    ## chisq stat = 0.69, df = 4, p-value = 0.95
    ## 
    ##       (sample size : 20)
    ## 
    ##  observed number  0 0 2 2 0 
    ##  expected number  0.0064 0.38 1.9 1.5 0.15
