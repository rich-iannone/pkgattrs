
<!-- README.md is generated from README.Rmd. Please edit that file -->
pkgattrs
========

The goal of pkgattrs is to get information on the contents of an R package. So far, we can obtain a summary of functions available in one or more packages using the `get_pkg_fcn_info()` function. Here is an example that gets information about the functions in the `pkgattrs` and `blastula` packages that are hosted on GitHub.

``` r
library(pkgattrs)

fcn_info <-
  get_pkg_fcn_info(
    from_github("rich-iannone/pkgattrs"),
    from_github("rich-iannone/blastula"))
```

The resulting tibble contains the following information in each record:

-   the package name (`pkg_name`)
-   the function name (`fcn_name`)
-   the file that contains the function (`r_file`)
-   the line number in `r_file` where the function starts (`ln_start`) and ends (`ln_end`)
-   the number of lines used for the function (`lines`)
-   whether the function is exported or not (`exported`)
-   the number of package functions that are called in `fcn_name` (`n_pkg_fcns_called`)
-   a list column with the names of the package functions called in `fcn_name` (`pkg_fcns_called`)

``` r
fcn_info
#> # A tibble: 16 x 9
#>    pkg_name fcn_name         r_file         ln_start ln_end lines exported
#>    <chr>    <chr>            <chr>             <int>  <int> <int> <lgl>   
#>  1 pkgattrs from_github      from_github.R         9     16     8 T       
#>  2 pkgattrs get_pkg_fcn_info get_pkg_fcn_i…       17    269   253 T       
#>  3 pkgattrs are_github_paths utils.R               5     13     9 F       
#>  4 pkgattrs are_local_paths  utils.R              19     26     8 F       
#>  5 blastula add_cta_button   add_cta_butto…       43     66    24 T       
#>  6 blastula add_ggplot       add_ggplot.R         52     71    20 T       
#>  7 blastula add_image        add_image.R          40     45     6 T       
#>  8 blastula blast_first      blast_first.R         9     52    44 T       
#>  9 blastula compose_email    compose_email…       86    275   190 T       
#> 10 blastula create_email_cr… create_email_…       47    123    77 T       
#> 11 blastula prepare_test_me… prepare_test_…       30     78    49 T       
#> 12 blastula preview_email    preview_email…        8     11     4 T       
#> 13 blastula send_by_mailgun  send_by_mailg…       56     87    32 T       
#> 14 blastula send_by_mailr    send_by_mailr…       44    114    71 T       
#> 15 blastula send_email_out   send_email_ou…       88    305   218 T       
#> 16 blastula smtp_settings    utils.R               2     27    26 F       
#> # ... with 2 more variables: n_pkg_fcns_called <int>,
#> #   pkg_fcns_called <list>
```

Installation
------------

You can install pkgattrs from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("rich-iannone/pkgattrs")
```
