#' Prepare a package's API in a tibble
#'
#' Exported functions from a package (along with their arguments and any
#' associated default values) are provided in a tibble object.
#'
#' @param ... Any objects pointing to a package location. This can be a string
#'   with a path to a local package directory, or, an invocation of a helper
#'   function such as [from_github()].
#' @export
pkg_api <- function(...) {

  # Get a table of package function info
  pkg_fn_info <- pkgattrs(...)

  # Get API function listing
  exported_fns <-
    pkg_fn_info %>%
    dplyr::filter(exported == TRUE) %>%
    dplyr::arrange(fn_name) %>%
    dplyr::pull(fn_name)

  # Get a vector of package names
  pkg_names <-
    pkg_fn_info %>%
    dplyr::pull(pkg_name) %>%
    unique()

  fn_args_tbl <-
    seq(exported_fns) %>%
    purrr::map_df(.f = function(x) {

      if (!is.null(formals(exported_fns[x]))) {

        arg_names <- formals(exported_fns[x]) %>% names()

        arg_names_tbl <-
          dplyr::tibble(arg_name = formals(exported_fns[x]) %>% names())

        default_values <- c()

        for (i in seq(arg_names)) {

          if (typeof(formals(exported_fns[x])[[i]]) != "symbol" &&
              formals(exported_fns[x])[[i]] %>% is.character()) {

            default_value <-
              paste(
                paste0("\"", formals(exported_fns[x])[[i]], "\""),
                collapse = ", "
              )

          } else if (typeof(formals(exported_fns[x])[[i]]) == "NULL") {

            default_value <- "NULL"

          } else {

            default_value <-
              paste(
                as.character(formals(exported_fns[x])[[i]]),
                collapse = ", "
              )
          }

          default_values <- c(default_values, default_value)
        }

        fn_tbl <-
          dplyr::tibble(
            fn_name = exported_fns[x],
            arg_names = arg_names,
            default_values = default_values
          )

      } else {

        fn_tbl <-
          dplyr::tibble(
            fn_name = exported_fns[x],
            arg_names = NA_character_,
            default_values = NA_character_
            )
      }

      fn_tbl
    })

  fn_args_tbl
}
