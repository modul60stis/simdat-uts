# Linear Congruential Generator (LCG) <img src="https://img.shields.io/badge/r-%23276DC3.svg?&style=for-the-badge&logo=r&logoColor=white"/> 


### Algoritma


Algoritma dari LCG sebagai berikut

1.  Tentukan seed *X*<sub>0</sub>
2.  Untuk *i* = 1, 2, ..., *n* hitung
    *X*<sub>*i*</sub> = (*a**X*<sub>*i* − 1</sub> + *c*)*m**o**d* *m*

### Buat Fungsi


``` r
rand.lcg <- function(n, m, a, c, seed = 0) {
      y <- vector(length = n)
      m <- m
      a <- a
      c <- c
      d <- seed

      for (i in 1:n) {
            d <- (a * d + c) %% m
            y[i] <- d
      }
      y
}
```

### Mencoba Fungsi

``` r
rand.lcg(20, m = 7, a = 1, c = 5, seed = 2)
```

    ##  [1] 0 5 3 1 6 4 2 0 5 3 1 6 4 2 0 5 3 1 6 4


# Membangkitkan UNIF(0,1)


Untuk memperoleh UNIF(0,1) dapat menggunakan LCG terus membaginya dengan
m

### Buat Fungsi


``` r
rand.unif1 <- function(n, m, a, c, seed = 0){
      rand.lcg(n, m, a, c, seed) / m
}
```

### Coba Fungsi


``` r
rand.unif1(20, m = 97, a = 98, c = 5, seed = 2)
```

    ##  [1] 0.07216495 0.12371134 0.17525773 0.22680412 0.27835052 0.32989691
    ##  [7] 0.38144330 0.43298969 0.48453608 0.53608247 0.58762887 0.63917526
    ## [13] 0.69072165 0.74226804 0.79381443 0.84536082 0.89690722 0.94845361
    ## [19] 0.00000000 0.05154639


