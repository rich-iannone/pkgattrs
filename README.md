
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/rich-iannone/pkgattrs.svg?branch=master)](https://travis-ci.org/rich-iannone/pkgattrs)

# pkgattrs

The **pkgattrs** package is useful for getting information on the
contents of any R package. One of the things that can be done is
generating a summary of functions available in one or more packages. We
can conveniently do this using the `get_pkg_fcn_info()` function. Here
is an example where we can create an informative table of the functions
in the `pkgattrs` and `blastula` packages (hosted on GitHub).

``` r
library(pkgattrs)

fcn_info <-
  get_pkg_fcn_info(
    from_github("rich-iannone/pkgattrs"),
    from_github("rich-iannone/blastula"))
```

The resulting tibble contains the following information in each record:

  - the package name (`pkg_name`)
  - the function name (`fcn_name`)
  - the file that contains the function (`r_file`)
  - the relative path from pkg root to `r_file` (`r_file_path`)
  - the line number in `r_file` where the function starts (`ln_start`)
    and ends (`ln_end`)
  - the number of lines used for the function (`lines`)
  - whether the function is exported or not (`exported`)
  - the number of package functions that are called in `fcn_name`
    (`n_pkg_fcns_called`)
  - a list column with the names of the package functions called in
    `fcn_name` (`pkg_fcns_called`)

<!-- end list -->

``` r
fcn_info
#> # A tibble: 20 x 10
#>    pkg_name fcn_name   r_file  r_file_path  ln_start ln_end lines exported
#>    <chr>    <chr>      <chr>   <chr>           <int>  <int> <int> <lgl>   
#>  1 pkgattrs from_gith… from_g… ./R/from_gi…        9     16     8 TRUE    
#>  2 pkgattrs create_fu… functi… ./R/functio…       17     62    46 TRUE    
#>  3 pkgattrs show_call… functi… ./R/functio…       75     84    10 TRUE    
#>  4 pkgattrs get_pkg_f… get_pk… ./R/get_pkg…       17    272   256 TRUE    
#>  5 pkgattrs are_githu… utils.R ./R/utils.R         5     13     9 FALSE   
#>  6 pkgattrs are_local… utils.R ./R/utils.R        19     26     8 FALSE   
#>  7 pkgattrs write_pkg… write_… ./R/write_p…       12     68    57 TRUE    
#>  8 blastula add_cta_b… add_ct… ./R/add_cta…       43     66    24 TRUE    
#>  9 blastula add_ggplot add_gg… ./R/add_ggp…       52     71    20 TRUE    
#> 10 blastula add_image  add_im… ./R/add_ima…       40     45     6 TRUE    
#> 11 blastula add_reada… add_re… ./R/add_rea…       24     64    41 TRUE    
#> 12 blastula blast_fir… blast_… ./R/blast_f…        9     52    44 TRUE    
#> 13 blastula compose_e… compos… ./R/compose…       86    275   190 TRUE    
#> 14 blastula create_em… create… ./R/create_…       47    123    77 TRUE    
#> 15 blastula get_html_… get_ht… ./R/get_htm…       13     16     4 TRUE    
#> 16 blastula prepare_t… prepar… ./R/prepare…       31     75    45 TRUE    
#> 17 blastula preview_e… previe… ./R/preview…        8     11     4 TRUE    
#> 18 blastula send_by_m… send_b… ./R/send_by…       56     88    33 TRUE    
#> 19 blastula send_emai… send_e… ./R/send_em…       88    305   218 TRUE    
#> 20 blastula smtp_sett… utils.R ./R/utils.R         2     27    26 FALSE   
#> # ... with 2 more variables: n_pkg_fcns_called <int>,
#> #   pkg_fcns_called <list>
```

The package also supplies functions for visualizing the relationships
between a package’s functions as a network graph. For example, we could
obtain a function information tibble for the **pointblank** package,
transform that to a graph, and then examine this network with the
`render_graph()` function.

``` r
get_pkg_fcn_info(from_github("rich-iannone/pointblank")) %>%
  create_function_graph() %>%
  render_graph(layout = "kk")
```

<img src="man/figures/pointblank_graph.png">

In this graph, the green nodes show the functions that are exported,
and, the relative sizing of nodes is scaled the number of package
functions called by each. Each edge represents the relationship
`called_in`.

We can also focus on a subgraph with a single function. The function
`show_called_functions()` can be used with the function graph object,
taking a function name to show all the package functions that function
does call. In the following example, we can examine which functions are
called by the `print.ptblank_agent()` method.

``` r
get_pkg_fcn_info(from_github("rich-iannone/pointblank")) %>%
  create_function_graph() %>%
  show_called_functions(caller_fcn = "print.ptblank_agent")
```

<img src="man/figures/pointblank_called_functions.png">

Finally, the package has a means to write out a given package’s API with
the `write_pkg_api()` function. For the **pkgattrs** package (this
package), we can generate a file that lists the exported functions along
with each of the function arguments and default values.

``` r
pkg_api(
  filename = "API",
  getwd())
```

This example generates the following text in the `API` file:

    create_function_graph(pkg_fcn_info, pkg_name)
    from_github(repo)
    get_pkg_fcn_info(...)
    pkg_api(filename, ...)
    show_called_functions(fcn_graph, caller_fcn)

## Installation

You can install pkgattrs from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("rich-iannone/pkgattrs")
```
