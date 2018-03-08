
<!-- README.md is generated from README.Rmd. Please edit that file -->
pkgattrs
========

The goal of pkgattrs is to get information on the contents of an R package. So far, we can obtain a summary of functions available in a package using the `get_pkg_fcn_info()` function. Here is an example of that for the `blastula` package.

``` r
library(pkgattrs)

blastula_fcn_info <-
  get_pkg_fcn_info(pkg_location = "~/Documents/R_oss_work/blastula/")
```

The resulting tibble contains a line for every function, which file each is located, starting/ending line numbers, and more.

``` r
blastula_fcn_info
#> # A tibble: 12 x 8
#>    fcn_name     r_file     ln_start ln_end lines exported n_pkg_fcns_call…
#>    <chr>        <chr>         <int>  <int> <int> <lgl>               <int>
#>  1 add_cta_but… add_cta_b…       43     66    24 T                       0
#>  2 add_ggplot   add_ggplo…       52     71    20 T                       1
#>  3 add_image    add_image…       40     45     6 T                       0
#>  4 blast_first  blast_fir…        9     52    44 T                       0
#>  5 compose_ema… compose_e…       86    275   190 T                       0
#>  6 create_emai… create_em…       47    123    77 T                       1
#>  7 prepare_tes… prepare_t…       30     78    49 T                       3
#>  8 preview_ema… preview_e…        8     11     4 T                       0
#>  9 send_by_mai… send_by_m…       56     87    32 T                       1
#> 10 send_by_mai… send_by_m…       44    114    71 T                       1
#> 11 send_email_… send_emai…       88    305   218 T                       2
#> 12 smtp_settin… utils.R           2     27    26 F                       0
#> # ... with 1 more variable: pkg_fcns_called <list>
```

Installation
------------

You can install pkgattrs from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("rich-iannone/pkgattrs")
```
