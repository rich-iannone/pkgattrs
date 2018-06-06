
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/rich-iannone/pkgattrs.svg?branch=master)](https://travis-ci.org/rich-iannone/pkgattrs)
[![CRAN
status](https://www.r-pkg.org/badges/version/pkgattrs)](https://cran.r-project.org/package=pkgattrs)

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
#> Skipping 1 packages ahead of CRAN: glue
#> Installing 2 packages: pillar, utf8
#> Skipping 1 packages ahead of CRAN: glue
#> Installing 2 packages: pillar, utf8
```

The resulting tibble contains the following information in each record:

  - the package name (`pkg_name`)
  - the package source location (`pkg_src`)
  - the function name (`fcn_name`)
  - whether the function is exported or not (`exported`)
  - the file that contains the function (`r_file`)
  - the relative path from pkg root to `r_file` (`r_file_path`)
  - the line number in `r_file` where the function starts (`ln_start`)
    and ends (`ln_end`)
  - the number of lines used for the function (`fcn_lines`)
  - the number of lines in the function used for code (`code`), for
    comments (`comment`), and for roxygen statements (`roxygen`), and,
    the `fcn_lines` lines that are blank (`blank`): the sum of all these
    is given in `total_lines`
  - the cyclomatic complexity of the function (`cyclocomp`)
  - the name of the package repository, if it was obtained from one
    (`pkg_repo`)
  - the name of the package path, if it a locally-available package
    (`pkg_path`)
  - the number of package functions that are called in `fcn_name`
    (`n_pkg_fcns_called`)
  - a list column with the names of the package functions called in
    `fcn_name` (`pkg_fcns_called`)

<!-- end list -->

``` r
fcn_info
#> # A tibble: 21 x 19
#>    pkg_name pkg_src fcn_name  exported r_file  r_file_path ln_start ln_end
#>    <chr>    <chr>   <chr>     <lgl>    <chr>   <chr>          <int>  <int>
#>  1 pkgattrs GitHub  are_gith… FALSE    utils.R ./R/utils.R        5     13
#>  2 pkgattrs GitHub  are_loca… FALSE    utils.R ./R/utils.R       19     26
#>  3 pkgattrs GitHub  create_f… TRUE     functi… ./R/functi…       17     66
#>  4 pkgattrs GitHub  from_git… TRUE     from_g… ./R/from_g…        9     16
#>  5 pkgattrs GitHub  get_fcn_… TRUE     get_fc… ./R/get_fc…       20    145
#>  6 pkgattrs GitHub  get_pkg_… TRUE     get_pk… ./R/get_pk…       23    347
#>  7 pkgattrs GitHub  show_cal… TRUE     functi… ./R/functi…       80     92
#>  8 pkgattrs GitHub  write_pk… TRUE     write_… ./R/write_…       16     77
#>  9 blastula GitHub  add_cta_… TRUE     add_ct… ./R/add_ct…       43     66
#> 10 blastula GitHub  add_ggpl… TRUE     add_gg… ./R/add_gg…       52     71
#> # ... with 11 more rows, and 11 more variables: fcn_lines <int>,
#> #   code <dbl>, comment <dbl>, blank <dbl>, roxygen <dbl>,
#> #   total_lines <dbl>, cyclocomp <int>, pkg_repo <chr>, pkg_path <chr>,
#> #   n_pkg_fcns_called <int>, pkg_fcns_called <list>
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
write_pkg_api(
  getwd(),
  filename = "API")
```

This example generates the following text in the `API` file:

    create_function_graph(pkg_fcn_info, pkg_name)
    from_github(repo)
    get_pkg_fcn_info(...)
    show_called_functions(fcn_graph, caller_fcn)
    write_pkg_api(..., filename)

## Installation

You can install pkgattrs from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("rich-iannone/pkgattrs")
```
