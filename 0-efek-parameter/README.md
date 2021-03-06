# Efek Parameter pada Distribusi Peluang <img src="https://img.shields.io/badge/r-%23276DC3.svg?&style=for-the-badge&logo=r&logoColor=white"/> 

``` r
library(ggplot2)
library(gridExtra)
library(dplyr)
```

**Catatan : Kodingan ini untuk memenuhi tugas simdat, kemungkinan banyak
kesalahan. Mohon dimaklumi**

Normal Distribution
===================

### Density Function


#### Mean constant, sd berubah

``` r
normalDistribution <- function(mean = 0, sd = 1){
      judul <- paste("mean =", mean, "and sd =", sd)
      ggplot(data = data.frame(x = c(mean - 5, mean + 5)), aes(x)) +
            stat_function(fun = dnorm, n = 1000, args = list(mean = mean, sd = sd), 
                          size = 1) +
            labs(x = "",
                 y = "",
                 title = judul) + 
            scale_y_continuous(limits = c(0, 2)) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5))
}

a <- normalDistribution(mean = 0, sd = 0.25)
b <- normalDistribution(mean = 0, sd = 0.5)
c <- normalDistribution(mean = 0, sd = 0.75)
d <- normalDistribution(mean = 0, sd = 1)
e <- normalDistribution(mean = 0, sd = 1.5)
f <- normalDistribution(mean = 0, sd = 2)
g <- normalDistribution(mean = 0, sd = 2.5)
h <- normalDistribution(mean = 0, sd = 3)
grid.arrange(a, b, c, d, e, f, g, h, nrow = 2)
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-3-1.png)

#### Mean berubah, sd konstant

``` r
ggplot(data = data.frame(x = c(-10, 10)), aes(x)) +
            stat_function(fun = dnorm, n = 1000, args = list(mean = 0, sd = 1), 
                          size = 1, aes(color = "mean = 0")) +
            stat_function(fun = dnorm, n = 1000, args = list(mean = 1, sd = 1), 
                          size = 1, aes(color = "mean = 1")) +
            stat_function(fun = dnorm, n = 1000, args = list(mean = 2, sd = 1), 
                          size = 1, aes(color = "mean = 2")) +
            stat_function(fun = dnorm, n = 1000, args = list(mean = -5, sd = 1), 
                          size = 1, aes(color = "mean = -5")) +
            stat_function(fun = dnorm, n = 1000, args = list(mean = -1, sd = 1), 
                          size = 1, aes(color = "mean = -1")) +
            stat_function(fun = dnorm, n = 1000, args = list(mean = -2, sd = 1), 
                          size = 1, aes(color = "mean = -2")) +
            stat_function(fun = dnorm, n = 1000, args = list(mean = 5, sd = 1), 
                          size = 1, aes(color = "mean = 5")) +
            labs(x = "",
                 y = "",
                 title = "Mean berubah, sd konstant") + 
            scale_y_continuous(breaks = NULL) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-4-1.png)

### Random Data


``` r
normalDistribution2 <- function(n, seed, mean = 0, sd = 1, yaxis = 0.6){
      set.seed(seed)
      data <- data.frame(x = rnorm(n = n, mean = mean, sd = sd))
      ggplot() +
            geom_density(data = data, 
                         aes(x, color = paste("n =", n)), size = 1) +
            stat_function(fun = dnorm, n = 1000, args = list(mean = mean, sd = sd), 
                                size = 1, aes(color = "true distribution")) +
            scale_x_continuous(limits = c(mean - 5, mean + 5)) + 
            scale_y_continuous(limits = c(0, yaxis)) +
            labs(x = "", y = "") +
            theme_minimal() +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
}

p1 <- normalDistribution2(n = 10, seed = 270101, mean = 0, sd = 1)
p2 <- normalDistribution2(n = 100, seed = 270101, mean = 0, sd = 1)
p3 <- normalDistribution2(n = 1000, seed = 270101, mean = 0, sd = 1)
p4 <- normalDistribution2(n = 10000, seed = 270101, mean = 0, sd = 1)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = "Mean = 0, sd = 1, seed = 270101")
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
p1 <- normalDistribution2(n = 10, seed = 230700, mean = 0, sd = 1)
p2 <- normalDistribution2(n = 100, seed = 230700, mean = 0, sd = 1)
p3 <- normalDistribution2(n = 1000, seed = 230700, mean = 0, sd = 1)
p4 <- normalDistribution2(n = 10000, seed = 230700, mean = 0, sd = 1)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = "Mean = 0, sd = 1, seed = 230700")
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
p1 <- normalDistribution2(n = 10, seed = 2327, mean = 0, sd = 1)
p2 <- normalDistribution2(n = 100, seed = 2327, mean = 0, sd = 1)
p3 <- normalDistribution2(n = 1000, seed = 2327, mean = 0, sd = 1)
p4 <- normalDistribution2(n = 10000, seed = 2327, mean = 0, sd = 1)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = "Mean = 0, sd = 1, seed = 2327")
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
p1 <- normalDistribution2(n = 10, seed = 230700, mean = 1, sd = 0.5, yaxis = 1.1)
p2 <- normalDistribution2(n = 100, seed = 230700, mean = 1, sd = 0.5, yaxis = 1.1)
p3 <- normalDistribution2(n = 1000, seed = 230700, mean = 1, sd = 0.5, yaxis = 1.1)
p4 <- normalDistribution2(n = 10000, seed = 230700, mean = 1, sd = 0.5, yaxis = 1.1)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = "Mean = 1, sd = 0.5, seed = 230700")
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-8-1.png)

### Kesimpulan


Dari beberapa plot diatas dapat disimpulkan

1.  Nilai `sd` berpengaruh pada keruncingan kurva, semakin kecil
    nilainya maka akan semakin runcing dan begitu sebaliknya.

2.  Nilai `mean` berpengaruh pada letak pusat dari kurva. Perubahan
    nilai `mean` akan merubah letak kurva di sumbu x

3.  Dari beberapa percobaan dengan membangkitkan data random, terlihat
    bahwa untuk `n <= 100`, data yabg dibangkitkan masih agak berbeda
    dengan distribusi aslinya. Untuk `n` sekitar ribuan masih tergantung
    hasil random datanya. Sementara itu untuk `n` yang sangat besar,
    `n >= 10000`, hasil randomnya sangat mendekati dengan distribusi
    aslinya.

Chi Square Distribution
=======================

### Density Function


``` r
ggplot(data = data.frame(x = c(0, 50)), aes(x)) +
            stat_function(fun = dchisq, n = 1000, args = list(df = 5), 
                          size = 1, aes(color = "df = 5")) +
            stat_function(fun = dchisq, n = 1000, args = list(df = 8), 
                          size = 1, aes(color = "df = 8")) +
            stat_function(fun = dchisq, n = 1000, args = list(df = 11), 
                          size = 1, aes(color = "df = 11")) +
            stat_function(fun = dchisq, n = 1000, args = list(df = 15), 
                          size = 1, aes(color = "df = 15")) +
            stat_function(fun = dchisq, n = 1000, args = list(df = 20), 
                          size = 1, aes(color = "df = 20")) +
            stat_function(fun = dchisq, n = 1000, args = list(df = 30), 
                          size = 1, aes(color = "df = 30")) +
            labs(x = "",
                 y = "",
                 title = "Chi Square Distribution") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-9-1.png)

### Random Data


``` r
chiSquare <- function(n, seed, df, yaxis = 0.11, xaxis = 50){
      set.seed(seed)
      data <- data.frame(x = rchisq(n = n, df = df))
      ggplot() +
            geom_density(data = data, 
                         aes(x, color = paste("n =", n)), size = 1) +
            stat_function(fun = dchisq, n = 1000, args = list(df = df), 
                                size = 1, aes(color = "true distribution")) +
            scale_x_continuous(limits = c(0, xaxis)) + 
            scale_y_continuous(limits = c(0, yaxis)) +
            labs(x = "", y = "") +
            theme_minimal() +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
}

seed <- 270101
df <- 10
p1 <- chiSquare(n = 10, seed = seed, df = df)
p2 <- chiSquare(n = 100, seed = seed, df = df)
p3 <- chiSquare(n = 1000, seed = seed, df = df)
p4 <- chiSquare(n = 10000, seed = seed, df = df)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("df =", df, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
seed <- 230700
df <- 10
p1 <- chiSquare(n = 10, seed = seed, df = df)
p2 <- chiSquare(n = 100, seed = seed, df = df)
p3 <- chiSquare(n = 1000, seed = seed, df = df)
p4 <- chiSquare(n = 10000, seed = seed, df = df)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("df =", df, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
seed <- 2327
df <- 10
p1 <- chiSquare(n = 10, seed = seed, df = df)
p2 <- chiSquare(n = 100, seed = seed, df = df)
p3 <- chiSquare(n = 1000, seed = seed, df = df)
p4 <- chiSquare(n = 10000, seed = seed, df = df)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("df =", df, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
seed <- 2327
df <- 40
yaxis <- 0.05
xaxis <- 100
p1 <- chiSquare(n = 10, seed = seed, df = df, yaxis = yaxis, xaxis = xaxis)
p2 <- chiSquare(n = 100, seed = seed, df = df, yaxis = yaxis, xaxis = xaxis)
p3 <- chiSquare(n = 1000, seed = seed, df = df, yaxis = yaxis, xaxis = xaxis)
p4 <- chiSquare(n = 10000, seed = seed, df = df, yaxis = yaxis, xaxis = xaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("df =", df, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
seed <- 17
df <- 50
yaxis <- 0.05
xaxis <- 100
p1 <- chiSquare(n = 10, seed = seed, df = df, yaxis = yaxis, xaxis = xaxis)
p2 <- chiSquare(n = 100, seed = seed, df = df, yaxis = yaxis, xaxis = xaxis)
p3 <- chiSquare(n = 1000, seed = seed, df = df, yaxis = yaxis, xaxis = xaxis)
p4 <- chiSquare(n = 10000, seed = seed, df = df, yaxis = yaxis, xaxis = xaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("df =", df, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-14-1.png)

### Kesimpulan


Dari beberapa plot diatas diperoleh bahwa

1.  Nilai `degree of freedom` berpengaruh pada kurva, ketika nilainya
    kecil maka kurva akan menceng kanan, dan ketika semakin besar,
    `df > 30`, kurvanya akan mirip dengan distribusi.
2.  Berdasarkan random data, ketika `n` sangat kecil kurva dari hasil
    random sangat berbeda jauh dengan curva chi-square. Dari percaobaan
    diatas curva akan mengikuti yang sebenarnya ketika `n > 1000`.

Student t Distribution
======================

### Density FUnction


``` r
ggplot(data = data.frame(x = c(-10, 10)), aes(x)) +
            stat_function(fun = dt, n = 1000, args = list(df = 1), 
                          size = 1, aes(color = "df = 1")) +
            stat_function(fun = dt, n = 1000, args = list(df = 5), 
                          size = 1, aes(color = "df = 5")) +
            stat_function(fun = dt, n = 1000, args = list(df = 15), 
                          size = 1, aes(color = "df = 15")) +
            stat_function(fun = dt, n = 1000, args = list(df = 25), 
                          size = 1, aes(color = "df = 25")) +
            stat_function(fun = dt, n = 1000, args = list(df = 100), 
                          size = 1, aes(color = "df = 100")) +
            labs(x = "",
                 y = "",
                 title = "Student t Distribution") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-15-1.png)

### Random Data


``` r
tDistribution <- function(n, seed, df, yaxis = 0.4){
      set.seed(seed)
      data <- data.frame(x = rt(n = n, df = df))
      ggplot() +
            geom_density(data = data, 
                         aes(x, color = paste("n =", n)), size = 1) +
            stat_function(fun = dt, n = 1000, args = list(df = df), 
                                size = 1, aes(color = "true distribution")) +
            scale_x_continuous(limits = c(-10, 10)) + 
            scale_y_continuous(limits = c(0, yaxis)) +
            labs(x = "", y = "") +
            theme_minimal() +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
}

seed <- 270101
df <- 10
p1 <- tDistribution(n = 10, seed = seed, df = df)
p2 <- tDistribution(n = 100, seed = seed, df = df)
p3 <- tDistribution(n = 1000, seed = seed, df = df)
p4 <- tDistribution(n = 10000, seed = seed, df = df)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("df =", df, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
seed <- 230700
df <- 10
p1 <- tDistribution(n = 10, seed = seed, df = df)
p2 <- tDistribution(n = 100, seed = seed, df = df)
p3 <- tDistribution(n = 1000, seed = seed, df = df)
p4 <- tDistribution(n = 10000, seed = seed, df = df)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("df =", df, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
seed <- 2327
df <- 10
yaxis <- 0.45
p1 <- tDistribution(n = 10, seed = seed, df = df, yaxis = yaxis)
p2 <- tDistribution(n = 100, seed = seed, df = df, yaxis = yaxis)
p3 <- tDistribution(n = 1000, seed = seed, df = df, yaxis = yaxis)
p4 <- tDistribution(n = 10000, seed = seed, df = df, yaxis = yaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("df =", df, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
seed <- 2327
df <- 40
yaxis <- 0.5
p1 <- tDistribution(n = 10, seed = seed, df = df, yaxis = yaxis)
p2 <- tDistribution(n = 100, seed = seed, df = df, yaxis = yaxis)
p3 <- tDistribution(n = 1000, seed = seed, df = df, yaxis = yaxis)
p4 <- tDistribution(n = 10000, seed = seed, df = df, yaxis = yaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("df =", df, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
seed <- 17
df <- 50
yaxis <- 1

p1 <- tDistribution(n = 10, seed = seed, df = df, yaxis = yaxis)
p2 <- tDistribution(n = 100, seed = seed, df = df, yaxis = yaxis)
p3 <- tDistribution(n = 1000, seed = seed, df = df, yaxis = yaxis)
p4 <- tDistribution(n = 10000, seed = seed, df = df, yaxis = yaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("df =", df, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-20-1.png)

### Kesimpulan


Dari beberapa plot diatas diperoleh bahwa

1.  Distribusi *t* selalu berpusat di titik 0
2.  Distribusi *t* dipengaruhi oleh nilai `degree of freedom`. Nilai ini
    berpengaruh pada keruncingan kurva
3.  Ketika nilai `df` masih kecil, perubahannya akan terlihat jelas,
    akan tetapi semikin besar nilai `df` perubahannya akan tidak terlalu
    berpengaruh. Hal tersebut terlihat pada kurva dengan `df = 25`
    sangat berhimpitan dengan `df = 100` akan tetapi untuk `df = 1`
    sangat berbeda ketika `df = 5`
4.  Ketikan `df` semakin besar maka akan mendekati distribusi normal
    dengan `mean = 0` dan `sd = 1`
5.  Untuk data random, ketika `n > 1000`, data yang dibangkitkan semakin
    mirip dengan distribusi aslinya.

F Distribution
==============

### Density Function


#### df1 konstan dan df2 berubah

``` r
ggplot(data = data.frame(x = c(0, 4)), aes(x)) +
            stat_function(fun = stats::df, n = 1000, args = list(df1 = 100, df2 = 1), 
                          size = 1, aes(color = "df2 = 1")) +
            stat_function(fun = stats::df, n = 1000, args = list(df1 = 100, df2 = 5), 
                          size = 1, aes(color = "df2 = 5")) +
            stat_function(fun = stats::df, n = 1000, args = list(df1 = 100, df2 = 10),
                          size = 1, aes(color = "df2 = 10")) +
            stat_function(fun = stats::df, n = 1000, args = list(df1 = 100, df2 = 25),
                          size = 1, aes(color = "df2 = 25")) +
            stat_function(fun = stats::df, n = 1000, args = list(df1 = 100, df2 = 100),
                          size = 1, aes(color = "df2 = 100")) +
            labs(x = "",
                 y = "",
                 title = "df1 konstan dan df2 berubah") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-21-1.png)

#### df1 berubah dan df2 konstant

``` r
ggplot(data = data.frame(x = c(0, 4)), aes(x)) +
            stat_function(fun = stats::df, n = 1000, args = list(df1 = 3, df2 = 100), 
                          size = 1, aes(color = "df1 = 1")) +
            stat_function(fun = stats::df, n = 1000, args = list(df1 = 5, df2 = 100), 
                          size = 1, aes(color = "df1 = 5")) +
            stat_function(fun = stats::df, n = 1000, args = list(df1 = 10, df2 = 100),
                          size = 1, aes(color = "df1 = 10")) +
            stat_function(fun = stats::df, n = 1000, args = list(df1 = 25, df2 = 100),
                          size = 1, aes(color = "df1 = 25")) +
            stat_function(fun = stats::df, n = 1000, args = list(df1 = 100, df2 = 100),
                          size = 1, aes(color = "df1 = 100")) +
            labs(x = "",
                 y = "",
                 title = "df1 berubah dan df2 konstan") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-22-1.png)

### Data Random


``` r
fDistribution <- function(n, seed, df1, df2, yaxis = 0.8){
      set.seed(seed)
      data <- data.frame(x = rf(n = n, df1 = df1, df2 = df2))
      ggplot() +
            geom_density(data = data, 
                         aes(x, color = paste("n =", n)), size = 1) +
            stat_function(fun = stats::df, n = 1000, args = list(df1 = df1, df2 = df2), 
                                size = 1, aes(color = "true distribution")) +
            scale_x_continuous(limits = c(0, 4)) + 
            scale_y_continuous(limits = c(0, yaxis)) +
            labs(x = "", y = "") +
            theme_minimal() +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
}

seed <- 270101
df1 <- 10
df2 <- 10
p1 <- fDistribution(n = 10, seed = seed, df1 = df1, df2 = df2)
p2 <- fDistribution(n = 100, seed = seed, df1 = df1, df2 = df2)
p3 <- fDistribution(n = 1000, seed = seed, df1 = df1, df2 = df2)
p4 <- fDistribution(n = 10000, seed = seed, df1 = df1, df2 = df2)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = "df1 = 10, df2 = 10, seed = 270101")
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
seed <- 2723
df1 <- 15
df2 <- 100
yaxis <- 1.3
p1 <- fDistribution(n = 10, seed = seed, df1 = df1, df2 = df2, yaxis = yaxis)
p2 <- fDistribution(n = 100, seed = seed, df1 = df1, df2 = df2, yaxis = yaxis)
p3 <- fDistribution(n = 1000, seed = seed, df1 = df1, df2 = df2, yaxis = yaxis)
p4 <- fDistribution(n = 10000, seed = seed, df1 = df1, df2 = df2, yaxis = yaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = "df1 = 15, df = 100, seed = 2723")
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-24-1.png)

``` r
seed <- 27
df1 <- 40
df2 <- 5
yaxis <- 1.5
p1 <- fDistribution(n = 10, seed = seed, df1 = df1, df2 = df2, yaxis = yaxis)
p2 <- fDistribution(n = 100, seed = seed, df1 = df1, df2 = df2, yaxis = yaxis)
p3 <- fDistribution(n = 1000, seed = seed, df1 = df1, df2 = df2, yaxis = yaxis)
p4 <- fDistribution(n = 10000, seed = seed, df1 = df1, df2 = df2, yaxis = yaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = "df1 = 40, df = 5, seed = 27")
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-25-1.png)

### Kesimpulan


Dari beberapa plot diatas diperoleh beberapa kesimpulan

1.  Distribusi *F* selalu bernilai positif dan bergantung pada parameter
    `df1` dan `df2` 2 Parameter `df1` dan `df2` berpengaruh pada
    keruncingan dan kemencengan data. Ketika `df1` dan `df2` kecil maka
    kurva akan menceng kanan dan tidak runcing. Seiring bertambahnya
    `df1` dan `df2` kurva akan semakin runcing dan kemencengan akan
    semakin berkurang
2.  Data beberapa percaobaan terlihat bahwa untuk `n` yang sangat besar
    ???\>???10000 barulah kurvanya sangat mirip dengan distribusi aslinya

Gamma Distribution
==================

### Density Function


#### shape berubah, scale konstant

``` r
ggplot(data = data.frame(x = c(0, 40)), aes(x)) +
            stat_function(fun = stats::dgamma, n = 1000, args = list(shape = 5, scale = 1),
                          size = 1, aes(color = "shape = 5")) +
            stat_function(fun = stats::dgamma, n = 1000, args = list(shape = 10, scale = 1),
                          size = 1, aes(color = "shape = 10")) +
            stat_function(fun = stats::dgamma, n = 1000, args = list(shape = 15, scale = 1),
                          size = 1, aes(color = "shape = 15")) +
            stat_function(fun = stats::dgamma, n = 1000, args = list(shape = 20, scale = 1),
                          size = 1, aes(color = "shape = 20")) +
            stat_function(fun = stats::dgamma, n = 1000, args = list(shape = 25, scale = 1),
                          size = 1, aes(color = "shape = 25")) +
            labs(x = "",
                 y = "",
                 title = "shape berubah, scale konstant") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-26-1.png)

#### shape konstant, scale berubah

``` r
ggplot(data = data.frame(x = c(0, 40)), aes(x)) +
            stat_function(fun = stats::dgamma, n = 1000, args = list(shape = 10, scale = 0.5),
                          size = 1, aes(color = "scale = 0.5")) +
            stat_function(fun = stats::dgamma, n = 1000, args = list(shape = 10, scale = 0.8),
                          size = 1, aes(color = "scale = 0.8")) +
            stat_function(fun = stats::dgamma, n = 1000, args = list(shape = 10, scale = 1.1),
                          size = 1, aes(color = "scale = 1.1")) +
            stat_function(fun = stats::dgamma, n = 1000, args = list(shape = 10, scale = 1.5),
                          size = 1, aes(color = "scale = 1.5")) +
            stat_function(fun = stats::dgamma, n = 1000, args = list(shape = 10, scale = 2),
                          size = 1, aes(color = "scale = 2")) +
            labs(x = "",
                 y = "",
                 title = "shape berubah, scale konstant") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-27-1.png)

### Random Data


``` r
gammaDistribution <- function(n, seed, shape, scale, yaxis = 0.1, xaxis = 80){
      set.seed(seed)
      data <- data.frame(x = rgamma(n = n, shape = shape, scale = scale))
      ggplot() +
            geom_density(data = data, 
                         aes(x, color = paste("n =", n)), size = 1) +
            stat_function(fun = dgamma, n = 1000, args = list(shape = shape, scale = scale), 
                                size = 1, aes(color = "true distribution")) +
            scale_x_continuous(limits = c(0, xaxis)) + 
            scale_y_continuous(limits = c(0, yaxis)) +
            labs(x = "", y = "") +
            theme_minimal() +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
}

seed <- 270101
shape <- 5
scale <- 3
p1 <- gammaDistribution(n = 10, seed = seed, shape = shape, scale = scale)
p2 <- gammaDistribution(n = 100, seed = seed, shape = shape, scale = scale)
p3 <- gammaDistribution(n = 1000, seed = seed, shape = shape, scale = scale)
p4 <- gammaDistribution(n = 10000, seed = seed, shape = shape, scale = scale)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("shape =", shape, ", scale =", scale, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-28-1.png)

``` r
seed <- 230700
shape <- 5
scale <- 10
yaxis <- 0.025
xaxis <- 120
p1 <- gammaDistribution(n = 10, seed = seed, shape = shape, scale = scale, yaxis = yaxis, xaxis = xaxis)
p2 <- gammaDistribution(n = 100, seed = seed, shape = shape, scale = scale, yaxis = yaxis, xaxis = xaxis)
p3 <- gammaDistribution(n = 1000, seed = seed, shape = shape, scale = scale, yaxis = yaxis, xaxis = xaxis)
p4 <- gammaDistribution(n = 10000, seed = seed, shape = shape, scale = scale, yaxis = yaxis, xaxis = xaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("shape =", shape, ", scale =", scale, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-29-1.png)

``` r
seed <- 2372
shape <- 5
scale <- 0.2
yaxis <- 1
xaxis <- 10
p1 <- gammaDistribution(n = 10, seed = seed, shape = shape, scale = scale, yaxis = yaxis, xaxis = xaxis)
p2 <- gammaDistribution(n = 100, seed = seed, shape = shape, scale = scale, yaxis = yaxis, xaxis = xaxis)
p3 <- gammaDistribution(n = 1000, seed = seed, shape = shape, scale = scale, yaxis = yaxis, xaxis = xaxis)
p4 <- gammaDistribution(n = 10000, seed = seed, shape = shape, scale = scale, yaxis = yaxis, xaxis = xaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("shape =", shape, ", scale =", scale, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-30-1.png)

### Kesimpulan


Dari beberapa plot diatas diperoleh bahwa

1.  Terk=lihat bahwa nilai `shape` (*??*) dan `scale` (*??*) berpengaruh
    terhadap kurva, semakin kecil nilai keduanya maka akan semakin
    runcing kurvanya dan semakin besar nilainya maka kurvanya semakin
    leter.
2.  Kurva hasil bangkitan sangat bergantung pada data yang dibangkitkan
    tersebut. Untuk beberapa kasus meskipun `n` sudah sangat besar,
    kurva yang dibentuk masih agak jauh dengan yang sebenarnya. Tetapi
    secara umum ketika *n*???\>??????=???10000 kurva sudah cukup mengikuti
    distibusi aslinya.

Exponential Distribution
========================

### Density Function


``` r
ggplot(data = data.frame(x = c(0, 2)), aes(x)) +
            stat_function(fun = dexp, n = 1000, args = list(rate = 0.5), 
                          size = 1, aes(color = "rate = 0.5")) +
            stat_function(fun = dexp, n = 1000, args = list(rate = 1), 
                          size = 1, aes(color = "rate = 1")) +
            stat_function(fun = dexp, n = 1000, args = list(rate = 2), 
                          size = 1, aes(color = "rate = 2")) +
            stat_function(fun = dexp, n = 1000, args = list(rate = 4), 
                          size = 1, aes(color = "rate = 4")) +
            stat_function(fun = dexp, n = 1000, args = list(rate = 8), 
                          size = 1, aes(color = "rate = 8")) +
            labs(x = "",
                 y = "",
                 title = "Exponential Distribution") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-31-1.png)

### Random Data


``` r
expDistribution <- function(n, seed, rate, xaxis = 3, yaxis = NULL){
      if(is.null(yaxis))
            yaxis = rate
      set.seed(seed)
      data <- data.frame(x = rexp(n = n, rate = rate))
      ggplot() +
            geom_density(data = data, 
                         aes(x, color = paste("n =", n)), size = 1) +
            stat_function(fun = dexp, n = 1000, args = list(rate = rate), 
                                size = 1, aes(color = "true distribution")) +
            scale_x_continuous(limits = c(0, xaxis)) + 
            scale_y_continuous(limits = c(0, yaxis)) +
            labs(x = "", y = "") +
            theme_minimal() +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
}

seed <- 270101
rate <- 1
xaxis <-  4
p1 <- expDistribution(n = 10, seed = seed, rate = rate, xaxis = xaxis)
p2 <- expDistribution(n = 100, seed = seed, rate = rate, xaxis = xaxis)
p3 <- expDistribution(n = 1000, seed = seed, rate = rate, xaxis = xaxis)
p4 <- expDistribution(n = 10000, seed = seed, rate = rate, xaxis = xaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("rate =", rate, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-32-1.png)

``` r
seed <- 230700
rate <- 4
xaxis <- 1
p1 <- expDistribution(n = 10, seed = seed, rate = rate, xaxis = xaxis)
p2 <- expDistribution(n = 100, seed = seed, rate = rate, xaxis = xaxis)
p3 <- expDistribution(n = 1000, seed = seed, rate = rate, xaxis = xaxis)
p4 <- expDistribution(n = 10000, seed = seed, rate = rate, xaxis = xaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("rate =", rate, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-33-1.png)

``` r
seed <- 27
rate <- 0.5
xaxis <- 5
yaxis <- 0.5
p1 <- expDistribution(n = 10, seed = seed, rate = rate, xaxis = xaxis, yaxis = yaxis)
p2 <- expDistribution(n = 100, seed = seed, rate = rate, xaxis = xaxis, yaxis = yaxis)
p3 <- expDistribution(n = 1000, seed = seed, rate = rate, xaxis = xaxis, yaxis = yaxis)
p4 <- expDistribution(n = 10000, seed = seed, rate = rate, xaxis = xaxis, yaxis = yaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("rate =", rate, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-34-1.png)

``` r
seed <- 23
rate <- 0.1
xaxis <- 15
yaxis <- 0.12
p1 <- expDistribution(n = 10, seed = seed, rate = rate, xaxis = xaxis, yaxis = yaxis)
p2 <- expDistribution(n = 100, seed = seed, rate = rate, xaxis = xaxis, yaxis = yaxis)
p3 <- expDistribution(n = 1000, seed = seed, rate = rate, xaxis = xaxis, yaxis = yaxis)
p4 <- expDistribution(n = 10000, seed = seed, rate = rate, xaxis = xaxis, yaxis = yaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("rate =", rate, ", seed =", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-35-1.png)

### Kesimpulan


Dari beberapa plot diatas diperoleh bahwa

1.  Nilai `rate` berpengaruh pada titik awal pada sumbu y, dan semakin
    besar maka kurvanya akan semakin menyempit, begitu pun sebaliknya.
2.  Dari hasil random terlihat bahwa sangat sulit untuk membentuk kurva
    yang sama persis dengan distribusi aslinya, meskipun untuk nilai `n`
    yang sangat besar.

Beta Distribution
=================

### Density Function


#### shape1 berubah, shape2 konstant

``` r
ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
            stat_function(fun = stats::dbeta, n = 1000, args = list(shape1 = 5, shape2 = 5),
                          size = 1, aes(color = "shape1 = 5")) +
            stat_function(fun = stats::dbeta, n = 1000, args = list(shape1 = 10, shape2 = 5),
                          size = 1, aes(color = "shape1 = 10")) +
            stat_function(fun = stats::dbeta, n = 1000, args = list(shape1 = 15, shape2 = 5),
                          size = 1, aes(color = "shape1 = 15")) +
            stat_function(fun = stats::dbeta, n = 1000, args = list(shape1 = 20, shape2 = 5),
                          size = 1, aes(color = "shape1 = 20")) +
            labs(x = "",
                 y = "",
                 title = "shape1 berubah, shape2 konstant") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-36-1.png)

#### shape1 konstant, shape2 berubah

``` r
ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
            stat_function(fun = stats::dbeta, n = 1000, args = list(shape1 = 5, shape2 = 5),
                          size = 1, aes(color = "shape1 = 5")) +
            stat_function(fun = stats::dbeta, n = 1000, args = list(shape1 = 5, shape2 = 10),
                          size = 1, aes(color = "shape1 = 10")) +
            stat_function(fun = stats::dbeta, n = 1000, args = list(shape1 = 5, shape2 = 15),
                          size = 1, aes(color = "shape1 = 15")) +
            stat_function(fun = stats::dbeta, n = 1000, args = list(shape1 = 5, shape2 = 20),
                          size = 1, aes(color = "shape1 = 20")) +
            labs(x = "",
                 y = "",
                 title = "shape1 berubah, shape2 konstant") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-37-1.png)

### Random Data


``` r
betaDistribution <- function(n, seed, shape1, shape2, yaxis = 4.5){
      set.seed(seed)
      data <- data.frame(x = rbeta(n = n, shape1 = shape1, shape2 = shape2))
      ggplot() +
            geom_density(data = data, 
                         aes(x, color = paste("n =", n)), size = 1) +
            stat_function(fun = stats::dbeta, n = 1000, args = list(shape1 = shape1, shape2 = shape2), 
                                size = 1, aes(color = "true distribution")) +
            scale_x_continuous(limits = c(0, 1)) + 
            scale_y_continuous(limits = c(0, yaxis)) +
            labs(x = "", y = "") +
            theme_minimal() +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
}

seed <- 270101
shape1 <- 10
shape2 <- 10
p1 <- betaDistribution(n = 10, seed = seed, shape1 = shape1, shape2 = shape2)
p2 <- betaDistribution(n = 100, seed = seed, shape1 = shape1, shape2 = shape2)
p3 <- betaDistribution(n = 1000, seed = seed, shape1 = shape1, shape2 = shape2)
p4 <- betaDistribution(n = 10000, seed = seed, shape1 = shape1, shape2 = shape2)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("shape 1 =", shape1, ", shape2 =", shape2, ", seed", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-38-1.png)

``` r
seed <- 230700
shape1 <- 10
shape2 <- 50
yaxis <- 9
p1 <- betaDistribution(n = 10, seed = seed, shape1 = shape1, shape2 = shape2, yaxis = yaxis)
p2 <- betaDistribution(n = 100, seed = seed, shape1 = shape1, shape2 = shape2, yaxis = yaxis)
p3 <- betaDistribution(n = 1000, seed = seed, shape1 = shape1, shape2 = shape2, yaxis = yaxis)
p4 <- betaDistribution(n = 10000, seed = seed, shape1 = shape1, shape2 = shape2, yaxis = yaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("shape 1 =", shape1, ", shape2 =", shape2, ", seed", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-39-1.png)

``` r
seed <- 1
shape1 <- 50
shape2 <- 10
yaxis <- 9
p1 <- betaDistribution(n = 10, seed = seed, shape1 = shape1, shape2 = shape2, yaxis = yaxis)
p2 <- betaDistribution(n = 100, seed = seed, shape1 = shape1, shape2 = shape2, yaxis = yaxis)
p3 <- betaDistribution(n = 1000, seed = seed, shape1 = shape1, shape2 = shape2, yaxis = yaxis)
p4 <- betaDistribution(n = 10000, seed = seed, shape1 = shape1, shape2 = shape2, yaxis = yaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("shape 1 =", shape1, ", shape2 =", shape2, ", seed", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-40-1.png)

### Kesimpulan


Dari beberapa plot diatas diperoleh bahwa

1.  Distribusi `beta` sangat dipengaruhi oleh nilai parameter `shape1`
    dan `shape2`
2.  Ketika `shape1 = shape2` maka kurvanya akan menyerupai normal.
    Ketika `shape1 < shaape2` kurvanya menceng kanan. Ketika
    `shape1 > shape2` kurvanya menceng kiri.
3.  Untuk data bangkitan, akan stabil mengikuti kurva yang sebenarnya
    ketika n mencapai ribuan

Uniform Distribution
====================

### Density Function


``` r
ggplot(data = data.frame(x = c(0, 11)), aes(x)) +
            stat_function(fun = stats::dunif, n = 1000, args = list(min = 5, max = 10),
                          size = 1, aes(color = "min = 5, max = 10")) +
            stat_function(fun = stats::dunif, n = 1000, args = list(min = 6, max = 10),
                          size = 1, aes(color = "min = 6, max = 10")) +
            stat_function(fun = stats::dunif, n = 1000, args = list(min = 7, max = 10),
                          size = 1, aes(color = "min = 7, max = 10")) +
            stat_function(fun = stats::dunif, n = 1000, args = list(min = 1, max = 6),
                          size = 1, aes(color = "min = 1, max = 6")) +
            stat_function(fun = stats::dunif, n = 1000, args = list(min = 1, max = 5),
                          size = 1, aes(color = "min = 1, max = 5")) +
            stat_function(fun = stats::dunif, n = 1000, args = list(min = 1, max = 4),
                          size = 1, aes(color = "min = 1, max = 4")) +
            labs(x = "",
                 y = "",
                 title = "shape1 berubah, shape2 konstant") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-41-1.png)

### Random Data


``` r
unifDistribution <- function(n, seed, min, max, yaxis = 1){
      set.seed(seed)
      data <- data.frame(x = runif(n = n, min = min, max = max))
      ggplot() +
            geom_density(data = data, 
                         aes(x, color = paste("n =", n)), size = 1) +
            stat_function(fun = stats::dunif, n = 1000, args = list(min = min, max = max), 
                                size = 1, aes(color = "true distribution")) +
            scale_x_continuous(limits = c(min - 1, max + 1)) + 
            scale_y_continuous(limits = c(0, yaxis)) +
            labs(x = "", y = "") +
            theme_minimal() +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
}

seed <- 270101
min <- 3
max <- 5
p1 <- unifDistribution(n = 10, seed = seed, min = min, max = max)
p2 <- unifDistribution(n = 100, seed = seed, min = min, max = max)
p3 <- unifDistribution(n = 1000, seed = seed, min = min, max = max)
p4 <- unifDistribution(n = 10000, seed = seed, min = min, max = max)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("min =", min, ", max =", max, ", seed", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-42-1.png)

``` r
seed <- 230700
min <- 0
max <- 10
yaxis <- 0.2
p1 <- unifDistribution(n = 10, seed = seed, min = min, max = max, yaxis = yaxis)
p2 <- unifDistribution(n = 100, seed = seed, min = min, max = max, yaxis = yaxis)
p3 <- unifDistribution(n = 1000, seed = seed, min = min, max = max, yaxis = yaxis)
p4 <- unifDistribution(n = 10000, seed = seed, min = min, max = max, yaxis = yaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("min =", min, ", max =", max, ", seed", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-43-1.png)

``` r
seed <- 2327
min <- 9
max <- 10
yaxis <- 2
p1 <- unifDistribution(n = 10, seed = seed, min = min, max = max, yaxis = yaxis)
p2 <- unifDistribution(n = 100, seed = seed, min = min, max = max, yaxis = yaxis)
p3 <- unifDistribution(n = 1000, seed = seed, min = min, max = max, yaxis = yaxis)
p4 <- unifDistribution(n = 10000, seed = seed, min = min, max = max, yaxis = yaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("min =", min, ", max =", max, ", seed", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-44-1.png)

### Kesimpulan


Dari beberapa plot diatas diperoleh bahwa

1.  Bentuk kurvanya hanya garis lurus dari `min` hingga `max`.
2.  Letak kurva disumbu y sangat bergantung pada range nilai `min` dan
    `max`. Ketika rangenya kecil maka akan semakin tinggi, begitu pun
    sebaliknya.
3.  Untuk data random sangat sulit untuk membangkitkan yang sesuai
    dengan distribusinya, meskipun untuk nilai n yang sangat besar.

Logistic Distribution
=====================

### Density Function


#### location constant, scale berubah

``` r
ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = 0, scale = 0.3),
                          size = 1, aes(color = "scale = 0.3")) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = 0, scale = 0.5),
                          size = 1, aes(color = "scale = 0.5")) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = 0, scale = 0.8),
                          size = 1, aes(color = "scale = 0.8")) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = 0, scale = 1),
                          size = 1, aes(color = "scale = 1")) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = 0, scale = 1.5),
                          size = 1, aes(color = "scale = 1.5")) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = 0, scale = 2),
                          size = 1, aes(color = "scale = 2")) +
            labs(x = "",
                 y = "",
                 title = "location constant, scale berubah") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-45-1.png)

#### location berubah, scale constant

``` r
ggplot(data = data.frame(x = c(-7, 20)), aes(x)) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = -2, scale = 1),
                          size = 1, aes(color = "scale = 0.3")) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = 0, scale = 1),
                          size = 1, aes(color = "scale = 0.5")) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = 2, scale = 1),
                          size = 1, aes(color = "scale = 0.8")) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = 4, scale = 1),
                          size = 1, aes(color = "scale = 1")) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = 8, scale = 1),
                          size = 1, aes(color = "scale = 1.5")) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = 16, scale = 1),
                          size = 1, aes(color = "scale = 2")) +
            labs(x = "",
                 y = "",
                 title = "location constant, scale berubah") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-46-1.png)

### Random Data


``` r
logisDistribution <- function(n, seed, location, scale, yaxis = 1, xaxis = 4){
      set.seed(seed)
      data <- data.frame(x = rlogis(n = n, location = location, scale = scale))
      ggplot() +
            geom_density(data = data, 
                         aes(x, color = paste("n =", n)), size = 1) +
            stat_function(fun = stats::dlogis, n = 1000, args = list(location = location, scale = scale), 
                                size = 1, aes(color = "true distribution")) +
            scale_x_continuous(limits = c(location - xaxis, location + xaxis)) + 
            scale_y_continuous(limits = c(0, yaxis)) +
            labs(x = "", y = "") +
            theme_minimal() +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.box.margin = margin(-20, 0, 10, 0))
}

seed <- 270101
location <- 5
scale <- 1
yaxis <- 0.4
xaxis <- 4
p1 <- logisDistribution(n = 10, seed = seed, location = location, scale = scale, yaxis = yaxis, xaxis = xaxis)
p2 <- logisDistribution(n = 100, seed = seed, location = location, scale = scale, yaxis = yaxis, xaxis = xaxis)
p3 <- logisDistribution(n = 1000, seed = seed, location = location, scale = scale, yaxis = yaxis, xaxis = xaxis)
p4 <- logisDistribution(n = 10000, seed = seed, location = location, scale = scale, yaxis = yaxis, xaxis = xaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("location =", location, ", scale =", scale, ", seed", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-47-1.png)

``` r
seed <- 2301
location <- 1.5
scale <- 7
yaxis <- 0.040
xaxis <- 50
p1 <- logisDistribution(n = 10, seed = seed, location = location, scale = scale, yaxis = yaxis, xaxis = xaxis)
p2 <- logisDistribution(n = 100, seed = seed, location = location, scale = scale, yaxis = yaxis, xaxis = xaxis)
p3 <- logisDistribution(n = 1000, seed = seed, location = location, scale = scale, yaxis = yaxis, xaxis = xaxis)
p4 <- logisDistribution(n = 10000, seed = seed, location = location, scale = scale, yaxis = yaxis, xaxis = xaxis)

grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("location =", location, ", scale =", scale, ", seed", seed))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-48-1.png)

### Kesimpulan


Dari beberapa plot diperoleh bahwa 1. Nilai `scale` berpengaruh pada
keruncingan kurva dan `location` berpengaruh pada pusat dari kurva. Jika
diliat plotnya cukup mirip dengan normal distribution 2. Untuk data
bangkitan ketika `n` mencapai hasil bangkitannya sudah cukup mengikuti
distribusi aslinya.

Binomial Distribution
=====================

### Density Function


#### Size konstant, p berubah

``` r
size <- 100
data <- data.frame(x1 = 0:size, y1 = dbinom(0:size, size = size, p = 0.3),
                   x2 = 0:size, y2 = dbinom(0:size, size = size, p = 0.4),
                   x3 = 0:size, y3 = dbinom(0:size, size = size, p = 0.5),
                   x4 = 0:size, y4 = dbinom(0:size, size = size, p = 0.6),
                   x5 = 0:size, y5 = dbinom(0:size, size = size, p = 0.7),
                   x6 = 0:size, y6 = dbinom(0:size, size = size, p = 0.8))

ggplot(data = data) +
      geom_line(aes(x1, y1, color = paste("p =", 0.3)), size = 1) +
      geom_line(aes(x2, y2, color = paste("p =", 0.4)), size = 1) +
      geom_line(aes(x3, y3, color = paste("p =", 0.5)), size = 1) +
      geom_line(aes(x4, y4, color = paste("p =", 0.6)), size = 1) +
      geom_line(aes(x5, y5, color = paste("p =", 0.7)), size = 1) +
      geom_line(aes(x6, y6, color = paste("p =", 0.8)), size = 1) +
      scale_x_continuous(limits = c(0, size)) + 
      labs(x = "", y = "", title = "size konstant, p berubah") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-49-1.png)

#### Size berubah, p konstant

``` r
ggplot() +
      geom_line(data = data.frame(x = 0:20, y = dbinom(0:20, size = 20, p = 0.5)),
                aes(x, y, color = paste("x =", 20)), size = 1) +
      geom_line(data = data.frame(x = 0:40, y = dbinom(0:40, size = 40, p = 0.5)),
                aes(x, y, color = paste("x =", 40)), size = 1) +
      geom_line(data = data.frame(x = 0:60, y = dbinom(0:60, size = 60, p = 0.5)),
                aes(x, y, color = paste("x =", 60)), size = 1) +
      scale_x_continuous(limits = c(0, 60)) + 
      labs(x = "", y = "", title = "size berubah, p konstant") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-50-1.png)

### Data Random


``` r
pCum <- function(x){
      data.frame(table(x)) %>%
            mutate(x = as.integer(as.character(x)),
                   p = Freq/sum(Freq)) %>%
            select(x, p)
}
```

``` r
set.seed(27)
ggplot() +
      geom_line(data = pCum(data.frame(x = rbinom(10000, 20, 0.5))), aes(x, p, color = "n =  10000"), size = 1) +
      geom_line(data = pCum(data.frame(x = rbinom(1000, 20, 0.5))), aes(x, p, color = "n =  1000"), size = 1) + 
      geom_line(data = pCum(data.frame(x = rbinom(100, 20, 0.5))), aes(x, p, color = "n =  100"), size = 1) +
      geom_line(data = pCum(data.frame(x = rbinom(10, 20, 0.5))), aes(x, p, color = "n =  10"), size = 1) +
      labs(x = "", y = "", title = "") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-52-1.png)

``` r
set.seed(27)
ggplot() +
      geom_line(data = pCum(data.frame(x = rbinom(1000, 20, 0.4))), aes(x, p, color = "p =  0.4"), size = 1) +
      geom_line(data = pCum(data.frame(x = rbinom(1000, 20, 0.5))), aes(x, p, color = "p =  0.5"), size = 1) + 
      geom_line(data = pCum(data.frame(x = rbinom(1000, 20, 0.6))), aes(x, p, color = "p =  0.6"), size = 1) +
      geom_line(data = pCum(data.frame(x = rbinom(1000, 20, 0.7))), aes(x, p, color = "p =  0.7"), size = 1) +
      labs(x = "", y = "", title = "") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-53-1.png)

### Kesimpulan


Dari beberapa plot diatas diperoleh bahwa

1.  Nilai `size` akan berpengaruh pada lebar dari kurva. Misalnya untuk
    `size = 60` maka kurvanya ada pada titik 1-60 dan akan memiliki
    puncak sesuai dengan nilai `p`
2.  Ketika `p > 0.5` maka data akan menceng ke kiri dan ketika `p < 0.5`
    maka data akan menceng kanan.
3.  Untuk `n < 100` bentuk kurvaya masih jauh dari bentuk distribusinya.

Poisson Distribution
====================

### Density Function


``` r
data <- data.frame(x1 = 0:25, y1 = dpois(0:25, 0.5),
                   x2 = 0:25, y2 = dpois(0:25, 1),
                   x3 = 0:25, y3 = dpois(0:25, 2),
                   x4 = 0:25, y4 = dpois(0:25, 4),
                   x5 = 0:25, y5 = dpois(0:25, 8),
                   x6 = 0:25, y6 = dpois(0:25, 16))

ggplot(data = data) +
      geom_line(aes(x1, y1, color = paste("lambda =", 0.5)), size = 1) +
      geom_line(aes(x2, y2, color = paste("lambda =", 1)), size = 1) +
      geom_line(aes(x3, y3, color = paste("lambda =", 2)), size = 1) +
      geom_line(aes(x4, y4, color = paste("lambda =", 4)), size = 1) +
      geom_line(aes(x5, y5, color = paste("lambda =", 8)), size = 1) +
      geom_line(aes(x6, y6, color = paste("lambda =", 16)), size = 1) +
      scale_x_continuous(limits = c(0, 25)) + 
      labs(x = "", y = "", title = "Poisson Distribution") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-54-1.png)

### Random Data


``` r
set.seed(27)
ggplot() +
      geom_line(data = pCum(data.frame(x = rpois(10, lambda = 16))), aes(x, p, color = "n =  10"), size = 1) +
      geom_line(data = pCum(data.frame(x = rpois(100, lambda = 16))), aes(x, p, color = "n =  100"), size = 1) + 
      geom_line(data = pCum(data.frame(x = rpois(1000, lambda = 16))), aes(x, p, color = "n =  1000"), size = 1) +
      geom_line(data = pCum(data.frame(x = rpois(10000, lambda = 16))), aes(x, p, color = "n =  10000"), size = 1) +
      geom_line(data = data.frame(x6 = 0:25, y6 = dpois(0:25, 16)),
             aes(x6, y6, color = "true distribution"), size = 1) +
      labs(x = "", y = "", title = "lambda 16") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-55-1.png)

``` r
set.seed(23)
ggplot() +
      geom_line(data = pCum(data.frame(x = rpois(10, lambda = 1))), aes(x, p, color = "n =  10"), size = 1) +
      geom_line(data = pCum(data.frame(x = rpois(100, lambda = 1))), aes(x, p, color = "n =  100"), size = 1) + 
      geom_line(data = pCum(data.frame(x = rpois(1000, lambda = 1))), aes(x, p, color = "n =  1000"), size = 1) +
      geom_line(data = pCum(data.frame(x = rpois(10000, lambda = 1))), aes(x, p, color = "n =  10000"), size = 1) +
      geom_line(data = data.frame(x6 = 0:5, y6 = dpois(0:5, 1)),
             aes(x6, y6, color = "true distribution"), size = 1) +
      labs(x = "", y = "", title = "lambda 1") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-56-1.png)

### Kesimpulan


Dari beberapa plot diatas diperoleh bahwa

1.  Terlihat bahwa nilai `lambda` berpengaruh pada bentuk kurva. Semakin
    kecil nilainya puncaknya akan mendekati titik 0 pada sumbu x.
    Sebaliknya semakin besar maka puncaknya semakin menjauh titik 0 pada
    sumbu y.
2.  Data bangkitan akan menyerupai disribusi sebenarnya ketika n
    mencapai puluhan ribu.

Geometrik Distribution
======================

### Density Function


``` r
data <- data.frame(x1 = 0:25, y1 = dgeom(0:25, 0.1),
                   x2 = 0:25, y2 = dgeom(0:25, 0.3),
                   x3 = 0:25, y3 = dgeom(0:25, 0.5),
                   x4 = 0:25, y4 = dgeom(0:25, 0.7),
                   x5 = 0:25, y5 = dgeom(0:25, 0.9))

ggplot(data = data) +
      geom_line(aes(x1, y1, color = paste("p =", 0.1)), size = 1) +
      geom_line(aes(x2, y2, color = paste("p =", 0.3)), size = 1) +
      geom_line(aes(x3, y3, color = paste("p =", 0.5)), size = 1) +
      geom_line(aes(x4, y4, color = paste("p =", 0.7)), size = 1) +
      geom_line(aes(x5, y5, color = paste("p =", 0.9)), size = 1) +
      scale_x_continuous(limits = c(0, 25)) + 
      labs(x = "", y = "", title = "Geometrik Distribution") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-57-1.png)

### Random Data


``` r
set.seed(27)
ggplot() +
      geom_line(data = pCum(data.frame(x = rgeom(10, 0.5))), aes(x, p, color = "n =  10"), size = 1) +
      geom_line(data = pCum(data.frame(x = rgeom(100, 0.5))), aes(x, p, color = "n =  100"), size = 1) + 
      geom_line(data = pCum(data.frame(x = rgeom(1000, 0.5))), aes(x, p, color = "n =  1000"), size = 1) +
      geom_line(data = pCum(data.frame(x = rgeom(10000, 0.5))), aes(x, p, color = "n =  10000"), size = 1) +
      geom_line(data = data.frame(x3 = 0:8, y3 = dgeom(0:8, 0.5)),
             aes(x3, y3, color = "true distribution"), size = 1) +
      labs(x = "", y = "", title = "prob = 0.5") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-58-1.png)

``` r
set.seed(23)
ggplot() +
      geom_line(data = pCum(data.frame(x = rgeom(10, 0.1))), aes(x, p, color = "n =  10"), size = 1) +
      geom_line(data = pCum(data.frame(x = rgeom(100, 0.1))), aes(x, p, color = "n =  100"), size = 1) + 
      geom_line(data = pCum(data.frame(x = rgeom(1000, 0.1))), aes(x, p, color = "n =  1000"), size = 1) +
      geom_line(data = pCum(data.frame(x = rgeom(10000, 0.1))), aes(x, p, color = "n =  10000"), size = 1) +
      geom_line(data = data.frame(x3 = 0:25, y3 = dgeom(0:25, 0.1)),
             aes(x3, y3, color = "true distribution"), size = 1) +
      scale_x_continuous(limits = c(0, 25)) +
      labs(x = "", y = "", title = "prob = 0.1") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-59-1.png)

``` r
set.seed(23)
ggplot() +
      geom_line(data = pCum(data.frame(x = rgeom(10, 0.9))), aes(x, p, color = "n =  10"), size = 1) +
      geom_line(data = pCum(data.frame(x = rgeom(100, 0.9))), aes(x, p, color = "n =  100"), size = 1) + 
      geom_line(data = pCum(data.frame(x = rgeom(1000, 0.9))), aes(x, p, color = "n =  1000"), size = 1) +
      geom_line(data = pCum(data.frame(x = rgeom(10000, 0.9))), aes(x, p, color = "n =  10000"), size = 1) +
      geom_line(data = data.frame(x3 = 0:3, y3 = dgeom(0:3, 0.9)),
             aes(x3, y3, color = "true distribution"), size = 1) +
      labs(x = "", y = "", title = "prob = 0.9") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.box.margin = margin(-20, 0, 10, 0))
```

![](efek-parameter_files/figure-markdown_github/unnamed-chunk-60-1.png)

### Kesimpulan


Dari beberapa plot diatas diperoleh bahwa

1.  Nilai `prob` berpengaruh pada plot. Semakin besar nilai `prob`,
    kurva dimulai dari sumbu y pada titik mendekati 1 kemudian melandai.
2.  Untuk nilai `prob` yang besar dengan jumlah `n` yang kecil angka
    random yang diperoleh sudah memiliki kurva yang sangat mirip dengan
    distribusi sebenarnya. Semakin kecil nilai `prob` maka diperlukan
    semakain besar nilai `n`.

