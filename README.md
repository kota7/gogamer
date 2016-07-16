
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/kota7/gogamer.svg?branch=master)](https://travis-ci.org/kota7/gogamer)

gogamer: R package for go game data
===================================

![](readme-fig/README-unnamed-chunk-2-1.png)

How to use
----------

Install by

``` r
devtools::install_github("kota7/gogamer")
```

Use by

``` r
library(gogamer)
```

What you can do
---------------

You can read go game record file in SGF.

``` r
x <- read_sgf(system.file("extdata/mimiaka.sgf", package = "gogamer"))
class(x)
#> [1] "gogame"
print(x)
#> * Go game *
#> 
#>  White : Genan Inseki (8p)
#>  Black : Kuwabara Shusaku (4p)
#>  Result: B+2 (325 moves)
#> 
#>  komi        : 0
#>  handicap    : 0
#>  board size  : 19
#>  date        : 1846-07-21
```

You can print the board configuration on the console,

``` r
# argument 'at' specifies the move number
stateat(x, at = 127)
#>      A B C D E F G H J K L M N O P Q R S T
#>     --------------------------------------
#> 19|  + + + + + + + + + @ O O + + + + + + +
#> 18|  + + + @ + + + + + @ O + O + O O @ + +
#> 17|  + + O O + @ + + O @ @ O O + O @ + + +
#> 16|  + + + + + + + + + + + @ @ @ + + @ + +
#> 15|  + + + + + @ + + + + @ + + + + @ @ + +
#> 14|  + + O + + + + + + + + + + + + @ O O +
#> 13|  + + + + + + + + + + + + + O O O @ @ @
#> 12|  + + + + + + + + + + + + + + @ O O O @
#> 11|  + + + + + + + + + @ + + @ O O @ @ @ +
#> 10|  + + + + + + + + + + + + O O @ + @ O +
#>  9|  + + O + + + + + + + + + + + O @ @ O +
#>  8|  + + + + + + + + + + + + + + O @ O @ +
#>  7|  + + + + + + + + + + + + O + O @ O O +
#>  6|  + + O + + + + + + @ + @ O + O @ + + +
#>  5|  + + + + + + @ + O + + @ O @ O @ O + +
#>  4|  + + @ + @ + + @ + + + @ O O @ O O + +
#>  3|  + + + + + @ O @ O + O O @ @ @ @ O O +
#>  2|  + + + + + + @ O + O O + O @ @ + @ O +
#>  1|  + + + + + + + + O + + O + @ + @ + @ +
#> 
#>   black captured: 5   white captured: 4 
#>   last move: black K11
```

or draw as an image.

``` r
plotat(x, at = 127)
```

![](readme-fig/README-unnamed-chunk-7-1.png)

The image object inherits `ggplot`, hence you can save with `ggsave` function.

``` r
library(ggplot2)
ggsave("goimage.pdf", width = 5, height = 5)
```
